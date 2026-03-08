#include "mycc/Sema/Sema.hpp"
#include "mycc/AST/ASTUtils.hpp"
#include "mycc/Sema/Analyses/LoopLabelAssigner.hpp"
#include <cassert>
#include <ranges>

using namespace mycc;


void Sema::assignLoopLabels(FunctionDeclaration &F) {
    LoopLabelAssigner(Labels, Diags, avoid_errors).run(F);
}

/**
 * @brief Enter a new function scope by clearing function-level label tracking.
 *
 * This method resets the FunctionLabels and GotoLabels sets to prepare
 * for semantic analysis of a new function.
 */
void Sema::enterFunction(StringRef name) {
    CurrentFunctionName = name.str();
    gotoLabelValidator.clearLabels();
}

/**
 * @brief Exit function scope and validate that all goto labels are defined.
 *
 * After processing a function, this validates that every goto statement
 * references a label that was actually defined in the function.
 */
void Sema::exitFunction() const {
    // Check that all goto labels are defined in the function
    if (!avoid_errors && gotoLabelValidator.validateGotoLabels(Diags)) {
        exit(1);
    }
}

Linkage Sema::computeLinkage(std::optional<StorageClass> sc, ScopeType scope) {
    if (scope == ScopeType::Global) {
        // global var/functions
        if (sc == StorageClass::SC_Static)
            return Linkage::Static; // internal linkage
        return Linkage::External; // extern or nothing specified
    }
    // a local variable/local definition
    if (sc == StorageClass::SC_Extern)
        return Linkage::External;
    return Linkage::None; // a local definition without storage class, has no linkage.
}

Expr *Sema::actOnStaticInit(Type *targetType, int64_t rawValue) const {
    if (targetType == Context.getIntTy())
        return Context.createExpression<IntInit>(static_cast<int32_t>(rawValue));
    return Context.createExpression<LongInit>(rawValue);
}

Expr *Sema::coerce(SMLoc Loc, Expr *expr, Type *targetType) const {
    auto exprType = typeExpressionInference->getType(expr, CurrentScope);
    if (exprType == nullptr) return nullptr;
    if (exprType == targetType) {
        return expr;
    }
    return actOnCastOperator(Loc, expr, targetType);
}

void Sema::enterScope() {
    CurrentScope = new Scope(CurrentScope);
}

void Sema::exitScope() {
    // check first there's a current scope
    assert(CurrentScope && "Can't exit non-existing scope");

    Scope *Parent = CurrentScope->getParentScope();
    delete CurrentScope;
    CurrentScope = Parent;
}

void Sema::initialize() {
    CurrentScope = nullptr;
    GlobalSymbolTable = new Scope(nullptr); // Separate table for IR generation
}

Program *Sema::actOnProgramDeclaration(DeclarationList &Decls) const {
    auto *p = Context.createProgram<Program>();
    p->add_functions(Decls);
    return p;
}

FunctionDeclaration *Sema::actOnFunctionDeclaration(SMLoc Loc, StringRef Name, ArgsList &args,
                                                    std::optional<StorageClass> storageClass,
                                                    FunctionType *funcType) {
    auto *FuncScope = CurrentScope->getParentScope();
    // Functions have linkage (external or internal/static)
    constexpr ScopeType scope = ScopeType::Global;

    //<--------------------------------------------------------------------------
    // Check if we're at block scope (inside a function, not at file-scope)
    bool isBlockScope = (FuncScope->getParentScope() != nullptr);

    // Static function declarations at block scope are not allowed in C
    if (shouldError(isBlockScope && storageClass == StorageClass::SC_Static,
                    [&] { Diags.report(Loc, diag::err_static_block_scope_function); }))
        return nullptr;
    //-------------------------------------------------------------------------->


    //<--------------------------------------------------------------------------
    // Check local scope for conflict with local variable (no linkage)
    // Book's rule: "if prev_entry.from_current_scope and (not prev_entry.has_linkage): fail"
    if (FuncScope->hasSymbolInCurrentScope(Name)) {
        const SymbolEntry *entry = FuncScope->lookupEntry(Name);
        if (shouldError(entry && entry->isLocalAttr(),
                        [&] { Diags.report(Loc, diag::err_linkage_conflict, Name.str()); }))
            return nullptr;
    }
    //-------------------------------------------------------------------------->


    //<--------------------------------------------------------------------------
    // Check GlobalSymbolTable for existing variable with same name
    // (function cannot redeclare a file-scope variable)
    if (shouldError(GlobalSymbolTable->lookupForVar(Name) != nullptr,
                    [&] { Diags.report(Loc, diag::err_variable_redeclared_as_function, Name.str()); }))
        return nullptr;
    //-------------------------------------------------------------------------->

    //<--------------------------------------------------------------------------
    // Check GlobalSymbolTable for existing function declaration
    // (catches conflicts between block-scope declarations in different functions)
    if (FunctionDeclaration *existing = GlobalSymbolTable->lookupForFunction(Name)) {
        // Check argument count matches
        if (shouldError(existing->getArgs().size() != args.size(),
                        [&] {
                            Diags.report(Loc, diag::err_wrong_argument_count, Name.str(), existing->getArgs().size(),
                                         args.size());
                        }))
            return nullptr;

        for (auto i = 0; i < args.size(); i++) {
            auto *currArg = args[i];
            auto *currArgType = typeExpressionInference->getType(currArg, CurrentScope);
            auto *existingArg = existing->getArgs()[i];
            auto *existingArgType = typeExpressionInference->getType(existingArg, CurrentScope);

            if (shouldError(currArgType != existingArgType, [&] {
                Diags.report(Loc, diag::err_non_compatible_types);
            }))
                return nullptr;
        }

        // Check linkage compatibility: "static follows non-static" is an error
        const SymbolEntry *oldEntry = GlobalSymbolTable->lookupEntry(Name);
        bool oldIsGlobal = oldEntry && oldEntry->isGlobal();
        bool newIsStatic = (storageClass == StorageClass::SC_Static);

        if (shouldError(oldIsGlobal && newIsStatic,
                        [&] { Diags.report(Loc, diag::err_static_follows_non_static, Name.str()); }))
            return nullptr;

        // Also insert into local scope if at block scope (for name lookup in this scope)
        if (isBlockScope) {
            FuncScope->addDeclaredIdentifier(Name);
            FuncScope->insert(existing, computeLinkage(storageClass, scope), scope);
        }

        // Multiple declarations are allowed - return existing
        return existing;
    }
    //-------------------------------------------------------------------------->

    // First declaration - create and insert
    auto *func = Context.createFunction<FunctionDeclaration>(Name, Loc, args);

    if (storageClass.has_value()) {
        func->setStorageClass(storageClass.value());
    }

    func->setFunctionType(funcType);
    for (size_t i = 0, e = func->getArgs().size(); i < e; i++) {
        auto *arg = func->getArg(i);
        auto *type = funcType->getArgTypes()[i];
        arg->setType(type);
    }

    // Insert into GlobalSymbolTable (for IR generation and cross-function visibility)
    GlobalSymbolTable->addDeclaredIdentifier(Name);
    GlobalSymbolTable->insert(func, computeLinkage(storageClass, scope), scope);

    // Also insert into local scope if at block scope (for local variable conflict detection)
    //if (isBlockScope) {
    FuncScope->insert(func, computeLinkage(storageClass, scope), scope);
    //}

    return func;
}

/**
 * @brief Process a function parameter declaration.
 *
 * Creates the parameter with its original name, adds it to the current scope,
 * and tracks it for variable name resolution. Reports an error if a
 * parameter with the same name already exists in the current scope.
 *
 * @param Loc Source location of the parameter declaration.
 * @param Name Parameter name.
 * @return Pointer to created Var node, or nullptr on error.
 */
Var *Sema::actOnParameterDeclaration(SMLoc Loc, StringRef Name, Type *type) const {
    // Create Var with original name (no renaming)
    auto *var = Context.createExpression<Var>(Loc, Name);

    // Add to current scope if it exists (parameters are in function scope)
    if (CurrentScope) {
        auto *decl = Context.createDeclaration<VarDeclaration>(Loc, var, nullptr, std::nullopt, type);

        if (shouldError(!CurrentScope->insert(Name, decl, Linkage::None, ScopeType::Local),
                        [&] { Diags.report(Loc, diag::err_duplicate_variable_declaration, Name.str()); }))
            return nullptr;

        CurrentScope->addDeclaredIdentifier(Name);
    }

    return var;
}

/**
 * @brief Process a local variable declaration and add it to the current scope.
 *
 * Adds the variable to the symbol table. The initializer validation
 * is done separately by actOnVarDeclarationInit after parsing the expression.
 *
 * @param Items Block items list to append the declaration to.
 * @param Loc Source location of the variable declaration.
 * @param Name Variable name.
 * @param storageClass type of storage (Static or Extern)
 * @param type type of the declared variable.
 * @return true if an error occurred, false on success.
 */
bool Sema::actOnVarDeclaration(BlockItems &Items, SMLoc Loc, StringRef Name,
                               std::optional<StorageClass> storageClass, Type *type) {
    if (!CurrentScope) {
        return true;
    }

    // Check for conflicts in current scope first
    if (shouldError(CurrentScope->hasSymbolInCurrentScope(Name) &&
                    CurrentScope->hasLinkageConflict(Name, storageClass),
                    [&] { Diags.report(Loc, diag::err_linkage_conflict, Name.str()); }))
        return true;

    // Check if another variable exists, and the type is different to the current one
    // e.g.
    /*
     * int foo = 3;
     *
     * long foo;
     */
    if (VarDeclaration *existingVar = CurrentScope->lookupForVar(Name)) {
        auto *existingVarType = existingVar->getType();
        if (shouldError(existingVarType != type, [&] {
            Diags.report(Loc, diag::err_var_with_other_type_declared, Name.str());
        }))
            return true;
    }

    auto *var = Context.createExpression<Var>(Loc, Name);
    auto *decl = Context.createDeclaration<VarDeclaration>(Loc, var, nullptr, storageClass);
    decl->setType(type);

    if (storageClass == StorageClass::SC_Extern) {
        // extern local variable
        // Step 1: Check CURRENT SCOPE ONLY for conflicts
        // (can't have both "extern int x" and "int x" in the same scope)
        if (CurrentScope->hasSymbolInCurrentScope(Name)) {
            const SymbolEntry *currentEntry = CurrentScope->lookupEntry(Name);
            if (currentEntry != nullptr) {
                if (shouldError(currentEntry->isFunction(), [&] {
                    Diags.report(Loc, diag::err_function_redeclared_as_variable, Name.str());
                }))
                    return true;
                // If there's already a local variable in current scope, it's a conflict
                if (shouldError(currentEntry->isLocalAttr(),
                                [&] { Diags.report(Loc, diag::err_conflicting_variable_linkage, Name.str()); }))
                    return true;
                // If there's already a static local in current scope, it's a conflict
                const StaticAttr *attrs = currentEntry->getStaticAttr();
                if (shouldError(attrs != nullptr && !attrs->global,
                                [&] { Diags.report(Loc, diag::err_conflicting_variable_linkage, Name.str()); }))
                    return true;
                // Previous extern in current scope - valid redeclaration, don't add again
                Items.emplace_back(decl);
                return false;
            }
        }

        // Step 2: Look at GlobalSymbolTable for existing declaration with linkage
        // (extern at block scope should refer to file-scope, not local vars in outer scopes)
        const SymbolEntry *globalEntry = GlobalSymbolTable->lookupEntry(Name);
        if (globalEntry != nullptr) {
            if (shouldError(globalEntry->isFunction(),
                            [&] { Diags.report(Loc, diag::err_function_redeclared_as_variable, Name.str()); }))
                return true;
            // Found existing declaration with linkage - inherit its attributes
        } else {
            // No prior declaration with linkage exists.
            // Block-scope extern creates a NEW identifier with external linkage.
            // Track this for conflict detection with later file-scope static declarations,
            // but DON'T add to global scope's visible symbols (name is only visible in block).
            BlockScopeExternLinkage.insert(Name.str());
        }

        // Add extern declaration to current scope for local name lookup
        CurrentScope->insert(Name, decl, Linkage::External, ScopeType::Local);
        CurrentScope->addDeclaredIdentifier(Name);
        StaticAttr attrs{InitialValue::NoInitializer, std::nullopt, true};
        CurrentScope->updateSymbolEntry(Name, attrs);
    } else if (storageClass == StorageClass::SC_Static) {
        // Static local - add to CurrentScope for local name lookup
        CurrentScope->insert(Name, decl, Linkage::Static, ScopeType::Local);
        CurrentScope->addDeclaredIdentifier(Name);
        // Temporarily set Initial(0), will be updated in actOnVarDeclarationInit
        StaticAttr attrs{InitialValue::Initial, 0, false};
        CurrentScope->updateSymbolEntry(Name, attrs);

        // Also add to GlobalSymbolTable with unique name for IR generation
        // Static locals are "moved to top level" with unique names like "functionName.varName"
        // If there's already a static local with the same base name, append a counter
        std::string baseName = CurrentFunctionName + "." + Name.str();
        std::string uniqueName = baseName;
        int counter = 1;
        while (GlobalSymbolTable->hasSymbolInCurrentScope(uniqueName)) {
            uniqueName = baseName + "." + std::to_string(counter++);
        }

        // Store the unique name in the declaration so IRGen can find it
        decl->setUniqueName(uniqueName);

        GlobalSymbolTable->insert(uniqueName, decl, Linkage::Static, ScopeType::Global);
        GlobalSymbolTable->updateSymbolEntry(uniqueName, attrs);
    } else {
        // Regular local variable (automatic storage)
        CurrentScope->insert(Name, decl, Linkage::None, ScopeType::Local);
        CurrentScope->addDeclaredIdentifier(Name);
        // LocalAttr is set automatically by SymbolEntry constructor
    }

    Items.emplace_back(decl);
    return false;
}

/**
 * @brief Validate and set the initializer for a variable declaration.
 *
 * Implements semantic checks for block-scope variable initializers:
 * - extern: no initializer allowed
 * - static: must have constant initializer (or defaults to 0)
 * - regular: any initializer allowed
 *
 * @param decl The variable declaration to validate.
 * @param initExpr Optional initializer expression.
 * @return true if an error occurred, false on success.
 */
bool Sema::actOnVarDeclarationInit(VarDeclaration *decl, Expr *initExpr) {
    if (!decl) return true;

    std::optional<StorageClass> storageClass = decl->getStorageClass();
    StringRef Name = decl->getVar()->getName();

    if (storageClass == StorageClass::SC_Extern) {
        // extern local variable - no initializer allowed
        if (shouldError(initExpr != nullptr,
                        [&] { Diags.report(SMLoc(), diag::err_extern_variable_has_initializer, Name.str()); }))
            return true;
        // Don't set expression for extern
        return false;
    }

    if (storageClass == StorageClass::SC_Static) {
        // static local variable - must have constant initializer
        InitialValue initialValue;
        std::optional<int64_t> constantValue = std::nullopt;

        if (initExpr != nullptr) {
            if (ASTUtils::isConstantExpression(initExpr)) {
                initialValue = InitialValue::Initial;
                int64_t rawValue = ASTUtils::evaluateConstantExpression(initExpr);
                constantValue = rawValue;
                initExpr = actOnStaticInit(decl->getType(), rawValue);
            } else {
                if (shouldError(true, [&] { Diags.report(SMLoc(), diag::err_non_constant_initializer); }))
                    return true;
                initialValue = InitialValue::Initial;
                constantValue = 0;
            }
        } else {
            // No initializer - defaults to 0
            initialValue = InitialValue::Initial;
            constantValue = 0;
            initExpr = actOnStaticInit(decl->getType(), 0);
        }

        // Update the symbol entry with the actual initial value
        StaticAttr attrs{initialValue, constantValue, false};
        CurrentScope->updateSymbolEntry(Name, attrs);

        // Also update GlobalSymbolTable with the unique name (stored in declaration)
        if (decl->getUniqueName().has_value()) {
            GlobalSymbolTable->updateSymbolEntry(decl->getUniqueName().value(), attrs);
        }
    }

    // Set the expression on the declaration
    decl->setExpr(initExpr);
    return false;
}

VarDeclaration *Sema::actOnGlobalVarDeclaration(SMLoc Loc, StringRef Name, Expr *initExpr,
                                                std::optional<StorageClass> storageClass, Type *type) {
    // Step 1-3: Determine initial_value based on initializer
    InitialValue initialValue;
    std::optional<int64_t> constantValue = std::nullopt;

    if (initExpr != nullptr) {
        // Has an initializer - must be a constant integer
        // Future improvements could be done here to allow
        // more expression types
        if (ASTUtils::isConstantExpression(initExpr)) {
            initialValue = InitialValue::Initial;
            int64_t rawValue = ASTUtils::evaluateConstantExpression(initExpr);
            constantValue = rawValue;
            initExpr = actOnStaticInit(type, rawValue);
        } else {
            // Non-constant initializer at file scope is an error
            if (shouldError(true, [&] { Diags.report(Loc, diag::err_non_constant_initializer); }))
                return nullptr;
            initialValue = InitialValue::Tentative; // Fallback for error recovery
        }
    } else {
        // No initializer
        if (storageClass == StorageClass::SC_Extern) {
            // If it is extern, this value could come from another file
            // we cannot provide a value 0 for initialization
            initialValue = InitialValue::NoInitializer;
        } else {
            // If it is not Extern, a tenative value could be 0
            initialValue = InitialValue::Tentative;
        }
    }

    // Step 4: Determine global linkage (external vs internal)
    bool global = (storageClass != StorageClass::SC_Static);

    // Check for conflict with block-scope extern that established external linkage
    if (shouldError(!global && BlockScopeExternLinkage.contains(Name.str()),
                    [&] { Diags.report(Loc, diag::err_conflicting_variable_linkage, Name.str()); }))
        return nullptr;

    // Check for conflict with previously declared function
    if (shouldError(GlobalSymbolTable->lookupForFunction(Name) != nullptr,
                    [&] { Diags.report(Loc, diag::err_function_redeclared_as_variable, Name.str()); }))
        return nullptr;

    // Step 5: Check if symbol already exists in the symbol table
    const SymbolEntry *oldEntry = CurrentScope->lookupEntry(Name);
    if (oldEntry != nullptr) {
        // Check if it's a function being redeclared as a variable
        if (shouldError(oldEntry->isFunction(),
                        [&] { Diags.report(Loc, diag::err_function_redeclared_as_variable, Name.str()); }))
            return nullptr;

        // Check if the existing variable has a different type
        if (VarDeclaration *existingVar = CurrentScope->lookupForVar(Name)) {
            if (shouldError(existingVar->getType() != type,
                            [&] { Diags.report(Loc, diag::err_var_with_other_type_declared, Name.str()); }))
                return nullptr;
        }

        // Get the old static attributes
        const StaticAttr *oldAttrs = oldEntry->getStaticAttr();
        if (oldAttrs == nullptr) {
            // It's a LocalAttr which shouldn't happen at file scope, treat as error
            if (shouldError(true,
                            [&] { Diags.report(Loc, diag::err_function_redeclared_as_variable, Name.str()); }))
                return nullptr;
        } else {
            // Handle extern: inherit global from previous declaration
            if (storageClass == StorageClass::SC_Extern) {
                global = oldAttrs->global;
            } else if (oldAttrs->global != global) {
                // Conflicting linkage (static vs non-static)
                if (shouldError(true,
                                [&] { Diags.report(Loc, diag::err_conflicting_variable_linkage, Name.str()); }))
                    return nullptr;
            }

            // Merge initialization values
            if (oldAttrs->init == InitialValue::Initial) {
                // Old declaration has a constant initializer
                if (initialValue == InitialValue::Initial) {
                    // Both have constant initializers - conflicting definitions
                    if (shouldError(true,
                                    [&] {
                                        Diags.report(Loc, diag::err_conflicting_file_scope_definitions, Name.str());
                                    }))
                        return nullptr;
                } else {
                    // Keep the old initializer
                    initialValue = oldAttrs->init;
                    constantValue = oldAttrs->value;
                }
            } else if (initialValue != InitialValue::Initial && oldAttrs->init == InitialValue::Tentative) {
                // Both are tentative or new is NoInitializer and old is Tentative
                initialValue = InitialValue::Tentative;
            }
        }

        // Update the existing symbol entry with merged attributes
        StaticAttr newAttrs{initialValue, constantValue, global};
        CurrentScope->updateSymbolEntry(Name, newAttrs);
        GlobalSymbolTable->updateSymbolEntry(Name, newAttrs);

        // Return the existing variable declaration (don't create a new one)
        VarDeclaration *existingDecl = CurrentScope->lookupForVar(Name);
        if (existingDecl && existingDecl->getExpr() == nullptr) {
            if (initExpr != nullptr) {
                // Update with the explicit initializer from this declaration
                existingDecl->setExpr(initExpr);
            } else if (initialValue != InitialValue::NoInitializer) {
                // Tentative definition: ensure zero initializer is present
                existingDecl->setExpr(actOnStaticInit(type, 0));
            }
        }
        return existingDecl;
    }

    // Step 7: Create new declaration and add to symbol table
    // Ensure tentative definitions always carry a zero initializer
    if (initExpr == nullptr && initialValue != InitialValue::NoInitializer)
        initExpr = actOnStaticInit(type, 0);

    auto *var = Context.createExpression<Var>(Loc, Name);
    auto *decl = Context.createDeclaration<VarDeclaration>(Loc, var, initExpr, storageClass);

    decl->setType(type);

    // Create the StaticAttr and insert into scope
    // Note: We manually construct the symbol entry since we need specific initial values
    StaticAttr attrs{initialValue, constantValue, global};
    Linkage linkage = global ? Linkage::External : Linkage::Static;

    // Add to CurrentScope (for scope chain lookups)
    CurrentScope->insert(Name, decl, linkage, ScopeType::Global);
    CurrentScope->addDeclaredIdentifier(Name);
    CurrentScope->updateSymbolEntry(Name, attrs);

    // Also add to GlobalSymbolTable (for IR generation)
    GlobalSymbolTable->insert(Name, decl, linkage, ScopeType::Global);
    GlobalSymbolTable->updateSymbolEntry(Name, attrs);

    return decl;
}

void Sema::actOnReturnStatement(BlockItems &Items, SMLoc Loc, Expr *RetVal) const {
    auto *currFunction = CurrentScope->lookupForFunction(CurrentFunctionName);
    if (currFunction == nullptr)
        currFunction = GlobalSymbolTable->lookupForFunction(CurrentFunctionName);
    auto *type = currFunction->getFunctionType()->getReturnType();
    RetVal = coerce(Loc, RetVal, type);
    Items.emplace_back(Context.createStatement<ReturnStatement>(RetVal));
}

void Sema::actOnNullStatement(BlockItems &Items, SMLoc Loc) const {
    Items.emplace_back(Context.createStatement<NullStatement>());
}

void Sema::actOnExprStatement(BlockItems &Items, SMLoc Loc, Expr *Expr) const {
    Items.emplace_back(Context.createStatement<ExpressionStatement>(Expr));
}

void Sema::actOnIfStatement(BlockItems &Items, SMLoc Loc, Expr *Cond, Statement *then_st, Statement *else_st) const {
    Items.emplace_back(Context.createStatement<IfStatement>(Cond, then_st, else_st));
}

void Sema::actOnCompoundStatement(BlockItems &Items, SMLoc Loc, BlockItems &compoundStatement) const {
    Items.emplace_back(Context.createStatement<CompoundStatement>(compoundStatement));
}

void Sema::actOnLabelStatement(BlockItems &Items, SMLoc Loc, StringRef Label) {
    if (!avoid_errors) {
        // The Labels are unique for each function, we must
        // ensure this property, throwing an error in case
        // an existing label has been declared again.
        if (gotoLabelValidator.FunctionLabelContains(Label)) {
            Diags.report(Loc, diag::err_existing_label, Label.str());
            exit(1);
        }
    }
    gotoLabelValidator.addFunctionLabel(Label);
    Items.emplace_back(Context.createStatement<LabelStatement>(Label));
}

void Sema::actOnGotoStatement(BlockItems &Items, SMLoc Loc, StringRef Label) {
    gotoLabelValidator.addGotoLabel(Label);
    Items.emplace_back(Context.createStatement<GotoStatement>(Label));
}

void Sema::actOnWhileStatement(BlockItems &Items, SMLoc Loc, Expr *Cond, Statement *Body) const {
    Items.emplace_back(Context.createStatement<WhileStatement>(Cond, Body));
}

void Sema::actOnDoWhileStatement(BlockItems &Items, SMLoc Loc, Statement *Body, Expr *Cond) const {
    Items.emplace_back(Context.createStatement<DoWhileStatement>(Body, Cond));
}

void Sema::actOnForStatement(BlockItems &Items, SMLoc Loc, ForInit &Init, Expr *Cond, Expr *Post,
                             Statement *Body) const {
    Items.emplace_back(Context.createStatement<ForStatement>(Init, Cond, Post, Body));
}

void Sema::actOnBreakStatement(BlockItems &Items, SMLoc Loc) const {
    Items.emplace_back(Context.createStatement<BreakStatement>());
}

void Sema::actOnContinueStatement(BlockItems &Items, SMLoc Loc) const {
    Items.emplace_back(Context.createStatement<ContinueStatement>());
}

void Sema::actOnDefaultStatement(BlockItems &Items, SMLoc Loc) const {
    Items.emplace_back(Context.createStatement<DefaultStatement>());
}

void Sema::actOnCaseStatement(BlockItems &Items, SMLoc Loc, Expr *Cond) const {
    Items.emplace_back(Context.createStatement<CaseStatement>(Cond));
}

void Sema::actOnSwitchStatement(BlockItems &Items, SMLoc Loc, Expr *Cond, Statement *Body) const {
    Items.emplace_back(Context.createStatement<SwitchStatement>(Cond, Body));
}

Expr *Sema::actOnConstLiteral(SMLoc Loc, StringRef Literal) const {
    const bool isLong = Literal.ends_with_insensitive("l");
    const StringRef digits = isLong ? Literal.drop_back() : Literal;

    // Parse into 65 bits to detect overflow beyond long
    llvm::APInt Value(65, digits, 10);
    if (Value.ugt(llvm::APInt(65, INT64_MAX))) {
        Diags.report(Loc, diag::err_long_literal_too_large);
        return nullptr;
    }

    Expr *expr = nullptr;
    // Unsuffixed and fits in int → IntegerLiteral, everything else → LongLiteral
    if (!isLong && Value.ule(llvm::APInt(65, INT32_MAX))) {
        expr = Context.createExpression<IntegerLiteral>(Loc, llvm::APSInt(Value.trunc(32), false));
    } else {
        expr = Context.createExpression<LongLiteral>(Loc, llvm::APSInt(Value.trunc(64), false));
    }
    typeExpressionInference->getType(expr, CurrentScope);
    return expr;
}


UnaryOperator *Sema::actOnUnaryOperator(SMLoc Loc, UnaryOperator::UnaryOperatorKind Kind, Expr *expr) const {
    auto *uop = Context.createExpression<UnaryOperator>(Loc, Kind, expr);
    typeExpressionInference->getType(uop, CurrentScope);
    return uop;
}

BinaryOperator *Sema::actOnBinaryOperator(SMLoc Loc, BinaryOperator::BinaryOpKind Kind, Expr *left, Expr *right) {
    auto *common = typeExpressionInference->commonType(typeExpressionInference->getType(left, CurrentScope),
                                                       typeExpressionInference->getType(right, CurrentScope));
    if (shouldError(!common, [&] { Diags.report(Loc, diag::err_non_compatible_types); }))
        return nullptr;
    left = coerce(Loc, left, common);
    right = coerce(Loc, right, common);
    auto *bop = Context.createExpression<BinaryOperator>(Loc, Kind, left, right);
    typeExpressionInference->getType(bop, CurrentScope);
    return bop;
}

AssignmentOperator *Sema::actOnAssignment(SMLoc Loc, Expr *left, Expr *right) {
    if (shouldError(left->getKind() != Expr::Ek_Var,
                    [&] { Diags.report(Loc, diag::err_incorrect_lvalue); }))
        return nullptr;
    if (shouldError(!typeExpressionInference->isAssignable(
                        typeExpressionInference->getType(right, CurrentScope),
                        typeExpressionInference->getType(left, CurrentScope)),
                    [&] { Diags.report(Loc, diag::err_non_compatible_types); }))
        return nullptr;
    right = coerce(Loc, right, left->getType());
    auto *aop = Context.createExpression<AssignmentOperator>(Loc, left, right);
    typeExpressionInference->getType(aop, CurrentScope);
    return aop;
}

PrefixOperator *Sema::actOnPrefixOperator(SMLoc Loc, PrefixOperator::PrefixOpKind Kind, Expr *expr) const {
    if (shouldError(expr->getKind() != Expr::Ek_Var,
                    [&] { Diags.report(Loc, diag::err_incorrect_lvalue); }))
        return nullptr;
    auto *preOp = Context.createExpression<PrefixOperator>(Loc, Kind, expr);
    typeExpressionInference->getType(preOp, CurrentScope);
    return preOp;
}

PostfixOperator *Sema::actOnPostfixOperator(SMLoc Loc, PostfixOperator::PostfixOpKind Kind, Expr *expr) const {
    if (shouldError(expr->getKind() != Expr::Ek_Var,
                    [&] { Diags.report(Loc, diag::err_incorrect_lvalue); }))
        return nullptr;
    auto *postOp = Context.createExpression<PostfixOperator>(Loc, Kind, expr);
    typeExpressionInference->getType(postOp, CurrentScope);
    return postOp;
}

/**
 * @brief Resolve an identifier to a variable.
 *
 * Performs variable name lookup through the scope chain. If the variable
 * is found in an enclosing scope, returns the Var from its declaration.
 * Reports an error if the variable is used before being declared.
 *
 * This method implements variable name shadowing by using the most recent
 * (innermost scope) declaration of a variable.
 *
 * @param Loc Source location of the identifier.
 * @param Name Variable name to look up.
 * @return Pointer to Var node from the declaration, or nullptr if undeclared.
 */
Var *Sema::actOnIdentifier(SMLoc Loc, StringRef Name) const {
    // Look up the variable in current scope
    if (CurrentScope) {
        // We make a lookup by name, this lookup will traverse
        // all the scopes from current through parents looking
        // for the variable.
        VarDeclaration *decl = CurrentScope->lookupForVar(Name);
        if (!decl) {
            if (shouldError(true,
                            [&] { Diags.report(Loc, diag::err_var_used_before_declared, Name.str()); }))
                return nullptr;
        } else {
            // Variable exists, return the Var from the declaration
            return decl->getVar();
        }
    }
    auto *var = Context.createExpression<Var>(Loc, Name);
    typeExpressionInference->getType(var, CurrentScope);
    return var;
}

ConditionalExpr *Sema::actOnTernaryOperator(SMLoc, Expr *left, Expr *middle, Expr *right) const {
    auto *conExpr = Context.createExpression<ConditionalExpr>(left, middle, right);
    typeExpressionInference->getType(conExpr, CurrentScope);
    return conExpr;
}

/**
 * @brief Create a function call expression with proper name resolution.
 *
 * Looks up the function name in scope. If found, uses the original function name.
 * Reports an error if the function hasn't been declared.
 *
 * @param Loc Source location of the function call.
 * @param name Function name to look up.
 * @param args List of argument expressions.
 * @return Pointer to the created FunctionCallExpr node, or nullptr on error.
 */
FunctionCallExpr *Sema::actOnFunctionCallOperator(SMLoc Loc, StringRef name, ExprList &args) const {
    FunctionDeclaration *func = CurrentScope->lookupForFunction(name);
    if (!func) {
        if (shouldError(CurrentScope->lookupForVar(name) != nullptr,
                        [&] { Diags.report(Loc, diag::err_definition_used_as_function, name.str()); }))
            return nullptr;
        func = GlobalSymbolTable->lookupForFunction(name);
    }

    if (func != nullptr) {
        // Check the argument size
        if (shouldError(func->getArgs().size() != args.size(),
                        [&] {
                            Diags.report(Loc, diag::err_wrong_argument_call, name.str(), args.size(),
                                         func->getArgs().size());
                        }))
            return nullptr;
        // Check the param types
        ExprList argCorrectTypes;
        const auto &paramTypes = func->getFunctionType()->getArgTypes();
        for (size_t i = 0; i < paramTypes.size(); ++i) {
            auto *paramType = paramTypes[i];
            auto *argProvided = args[i];
            auto *argProvidedType = typeExpressionInference->getType(argProvided, CurrentScope);
            if (shouldError(!typeExpressionInference->isAssignable(argProvidedType, paramType),
                            [&] { Diags.report(Loc, diag::err_non_compatible_types); }))
                return nullptr;
            argCorrectTypes.push_back(coerce(Loc, argProvided, paramType));
        }
        // Function found — set return type directly since we already have the declaration
        auto *funcCall = Context.createExpression<FunctionCallExpr>(name, argCorrectTypes);
        funcCall->setType(func->getFunctionType()->getReturnType());
        return funcCall;
    }
    if (shouldError(true, [&] { Diags.report(Loc, diag::err_func_used_before_declared, name.str()); }))
        return nullptr;
    auto *funcCall = Context.createExpression<FunctionCallExpr>(name, args);
    typeExpressionInference->getType(funcCall, CurrentScope);
    return funcCall;
}

CastExpr *Sema::actOnCastOperator(SMLoc Loc, Expr *expr, Type *type) const {
    auto *cast = Context.createExpression<CastExpr>(expr, type);
    typeExpressionInference->getType(cast, CurrentScope);
    return cast;
}
