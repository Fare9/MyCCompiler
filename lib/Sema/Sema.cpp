#include "mycc/Sema/Sema.hpp"
#include <cassert>
#include <ranges>

using namespace mycc;

/**
 * @brief Enter a new function scope by clearing function-level label tracking.
 *
 * This method resets the FunctionLabels and GotoLabels sets to prepare
 * for semantic analysis of a new function.
 */
void Sema::enterFunction() {
    FunctionLabels.clear();
    GotoLabels.clear();
}

/**
 * @brief Exit function scope and validate that all goto labels are defined.
 *
 * After processing a function, this validates that every goto statement
 * references a label that was actually defined in the function.
 */
void Sema::exitFunction() {
    // Check that all goto labels are defined in the function
    if (!avoid_errors) {
        checkGotoLabelsCorrectlyPointToFunction();
    }
}

void Sema::checkGotoLabelsCorrectlyPointToFunction() const {
    for (const auto &label: GotoLabels) {
        if (!FunctionLabels.contains(label)) {
            Diags.report(SMLoc(), diag::err_undefined_label, label.str());
            exit(1);
        }
    }
}

void Sema::enterScope() {
    CurrentScope = new Scope(CurrentScope);
}

void Sema::exitScope() {
    // check first there's a current scope
    assert(CurrentScope && "Can't exit non-existing scope");

    Scope *Parent = CurrentScope->getParentScope();
    // delete current scope
    delete CurrentScope;
    CurrentScope = Parent;
}

std::string Sema::generateLoopLabel() {
    return "loop_" + std::to_string(LoopLabelCounter++);
}

/**
 * @brief Assign unique labels to all loops, breaks, and continues in a function.
 *
 * This method traverses the function body and assigns labels to:
 * - Loop statements (while, do-while, for) for break and continue targets
 * - Switch statements for break targets
 * - Break and continue statements to reference their appropriate targets
 *
 * @param F The function to process.
 */
void Sema::assignLoopLabels(FunctionDeclaration &F) {
    // Labels for the breaks
    std::vector<BreakableContext> breakableStack;

    // Traverse all items in the function body
    for (auto &item: F) {
        traverseBlockItem(item, breakableStack);
    }
}

void Sema::traverseBlockItem(BlockItem &item, std::vector<BreakableContext> &breakableStack) {
    if (std::holds_alternative<Statement *>(item)) {
        traverseStatement(std::get<Statement *>(item), breakableStack);
    }
}

std::string Sema::generateSwitchLabel() {
    return "switch_" + std::to_string(SwitchLabelCounter++);
}

std::string Sema::generateCaseLabel() {
    return "case_" + std::to_string(CaseLabelCounter++);
}

std::string Sema::generateDefaultLabel() {
    return "default_" + std::to_string(DefaultLabelCounter++);
}

bool Sema::isConstantExpression(Expr *expr) {
    return expr->getKind() == Expr::Ek_Int;
}

int64_t Sema::evaluateConstantExpression(Expr *expr) {
    if (auto *intLit = dynamic_cast<IntegerLiteral *>(expr)) {
        return intLit->getValue().getSExtValue();
    }
    // this should never be reached since we only allow constant integers
    return 0;
}

/**
 * @brief Validate the body of a switch statement recursively.
 *
 * Performs the following validations:
 * 1. Case values must be constant expressions
 * 2. No duplicate case values
 * 3. At most one default case
 * 4. No variable declarations immediately after case/default labels
 *
 * @param body Switch body statement to validate.
 * @param seenCaseValues Set to track and detect duplicate case values.
 * @param hasDefault Flag indicating if a default case has been seen.
 */
void Sema::validateSwitchBody(Statement *body,
                              std::set<int64_t> &seenCaseValues,
                              bool &hasDefault) {
    if (!body) return;

    switch (body->getKind()) {
        case Statement::SK_Case: {
            auto *caseStmt = dynamic_cast<CaseStatement *>(body);

            // First check, case value must be a constant integer
            Expr *value = caseStmt->getValue();
            if (!isConstantExpression(value)) {
                Diags.report(SMLoc(), diag::err_case_value_not_constant);
                exit(1);
            }

            // Second check, look for duplicated cases
            int64_t caseValue = evaluateConstantExpression(value);
            if (seenCaseValues.contains(caseValue)) {
                Diags.report(SMLoc(), diag::err_duplicate_case_value, std::to_string(caseValue));
                exit(1);
            }
            seenCaseValues.insert(caseValue);
            break;
        }

        case Statement::SK_Default: {
            // Check 3: It has multiple defaults
            if (hasDefault) {
                Diags.report(SMLoc(), diag::err_multiple_default_in_switch);
                exit(1);
            }
            hasDefault = true;
            break;
        }

        case Statement::SK_Compound: {
            auto *compound = dynamic_cast<CompoundStatement *>(body);
            Statement *prevStmt = nullptr;

            // Go over each item to validate the body
            for (auto &item: *compound) {
                // Check if current item is a Declaration following case/default
                if (std::holds_alternative<VarDeclaration *>(item)) {
                    if (prevStmt &&
                        (prevStmt->getKind() == Statement::SK_Case ||
                         prevStmt->getKind() == Statement::SK_Default)) {
                        if (!avoid_errors) {
                            Diags.report(SMLoc(), diag::err_declaration_after_case_label);
                            exit(1);
                        }
                    }
                    // Reset prevStmt since a declaration is not a statement
                    prevStmt = nullptr;
                } else if (std::holds_alternative<Statement *>(item)) {
                    Statement *stmt = std::get<Statement *>(item);
                    validateSwitchBody(stmt, seenCaseValues, hasDefault);
                    prevStmt = stmt;
                }
            }
            break;
        }

        // Recursively check nested statements
        case Statement::SK_If: {
            auto *ifStmt = dynamic_cast<IfStatement *>(body);
            validateSwitchBody(ifStmt->getThenSt(), seenCaseValues, hasDefault);
            if (ifStmt->getElseSt())
                validateSwitchBody(ifStmt->getElseSt(), seenCaseValues, hasDefault);
            break;
        }

        case Statement::SK_While: {
            auto *whileStmt = dynamic_cast<WhileStatement *>(body);
            validateSwitchBody(whileStmt->getBody(), seenCaseValues, hasDefault);
            break;
        }
        case Statement::SK_DoWhile: {
            auto *doWhileStmt = dynamic_cast<DoWhileStatement *>(body);
            validateSwitchBody(doWhileStmt->getBody(), seenCaseValues, hasDefault);
            break;
        }
        case Statement::SK_For: {
            auto *forStmt = dynamic_cast<ForStatement *>(body);
            validateSwitchBody(forStmt->getBody(), seenCaseValues, hasDefault);
            break;
        }
    }
}

/**
 * @brief Recursively traverse a statement and assign labels for control flow.
 *
 * This method handles:
 * - Loops (while, do-while, for): assigns base labels and processes break/continue
 * - Switch statements: assigns break label and validates cases
 * - Break statements: links to innermost breakable context
 * - Continue statements: links to innermost loop context
 * - Compound and conditional statements: recursively processes children
 *
 * @param stmt Statement to traverse.
 * @param breakableStack Stack of enclosing breakable contexts for break/continue resolution.
 */
void Sema::traverseStatement(Statement *stmt, std::vector<BreakableContext> &breakableStack) {
    if (!stmt) return;

    switch (stmt->getKind()) {
        case Statement::SK_While: {
            auto *whileStmt = dynamic_cast<WhileStatement *>(stmt);
            std::string baseLabel = generateLoopLabel();
            whileStmt->set_label(baseLabel);

            // Push base label onto stack
            BreakableContext ctx;
            ctx.base_label = baseLabel;
            ctx.is_loop = true;
            breakableStack.push_back(ctx);

            // Traverse body
            traverseStatement(whileStmt->getBody(), breakableStack);

            // Pop from stack
            breakableStack.pop_back();
            break;
        }

        case Statement::SK_DoWhile: {
            auto *doWhileStmt = dynamic_cast<DoWhileStatement *>(stmt);
            std::string baseLabel = generateLoopLabel();
            doWhileStmt->set_label(baseLabel);

            // Push base label onto stack
            BreakableContext ctx;
            ctx.base_label = baseLabel;
            ctx.is_loop = true;
            breakableStack.push_back(ctx);

            // Traverse body
            traverseStatement(doWhileStmt->getBody(), breakableStack);

            // Pop from stack
            breakableStack.pop_back();
            break;
        }

        case Statement::SK_For: {
            auto *forStmt = dynamic_cast<ForStatement *>(stmt);
            std::string baseLabel = generateLoopLabel();
            forStmt->set_label(baseLabel);

            // Push base label onto stack
            BreakableContext ctx;
            ctx.base_label = baseLabel;
            ctx.is_loop = true;
            breakableStack.push_back(ctx);

            // Traverse body
            traverseStatement(forStmt->getBody(), breakableStack);

            // Pop from stack
            breakableStack.pop_back();
            break;
        }
        case Statement::SK_Switch: {
            auto *switchStmt = dynamic_cast<SwitchStatement *>(stmt);
            std::string switchLabel = generateSwitchLabel();
            switchStmt->set_break_label(switchLabel + "_end");

            std::set<int64_t> seenCaseValues;
            bool hasDefault = false;
            validateSwitchBody(switchStmt->get_body(), seenCaseValues, hasDefault);

            // Push switch context
            BreakableContext ctx;
            ctx.base_label = switchLabel;
            ctx.is_loop = false;
            breakableStack.push_back(ctx);

            traverseStatement(switchStmt->get_body(), breakableStack);

            breakableStack.pop_back();
            break;
        }
        case Statement::SK_Case: {
            auto *caseStmt = dynamic_cast<CaseStatement *>(stmt);

            // Check if we're inside any switch by searching the stack
            bool insideSwitch = false;
            for (const auto &ctx: breakableStack) {
                if (!ctx.is_loop) {
                    // Found a switch
                    insideSwitch = true;
                    break;
                }
            }

            if (!insideSwitch) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_case_not_in_switch);
                    exit(1);
                }
            } else {
                std::string caseLabel = generateCaseLabel();
                caseStmt->set_label(caseLabel);
            }
            break;
        }
        case Statement::SK_Default: {
            auto *defaultStmt = dynamic_cast<DefaultStatement *>(stmt);

            // Check if we're inside any switch by searching the stack
            bool insideSwitch = false;
            for (const auto &ctx: breakableStack) {
                if (!ctx.is_loop) {
                    // Found a switch
                    insideSwitch = true;
                    break;
                }
            }

            if (!insideSwitch) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_default_not_in_switch);
                    exit(1);
                }
            } else {
                std::string defaultLabel = generateDefaultLabel();
                defaultStmt->set_label(defaultLabel);
            }
            break;
        }
        case Statement::SK_Break: {
            auto *breakStmt = dynamic_cast<BreakStatement *>(stmt);

            if (breakableStack.empty()) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_break_not_in_loop);
                    exit(1);
                }
            } else {
                // Break jumps to the innermost loop or switch
                std::string targetLabel = breakableStack.back().get_break_label();
                breakStmt->set_label(targetLabel);
            }
            break;
        }

        case Statement::SK_Continue: {
            auto *continueStmt = dynamic_cast<ContinueStatement *>(stmt);

            if (breakableStack.empty()) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_continue_not_in_loop);
                    exit(1);
                }
            } else {
                bool foundLoop = false;
                for (auto &breakable: std::ranges::reverse_view(breakableStack)) {
                    if (breakable.is_loop) {
                        // Continue jumps to continue label of innermost LOOP
                        std::string targetLabel = breakable.get_continue_label();
                        continueStmt->set_label(targetLabel);
                        foundLoop = true;
                        break;
                    }
                }

                if (!foundLoop && !avoid_errors) {
                    Diags.report(SMLoc(), diag::err_continue_not_in_loop);
                    exit(1);
                }
            }
            break;
        }

        case Statement::SK_If: {
            auto *ifStmt = dynamic_cast<IfStatement *>(stmt);
            traverseStatement(ifStmt->getThenSt(), breakableStack);
            if (ifStmt->getElseSt()) {
                traverseStatement(ifStmt->getElseSt(), breakableStack);
            }
            break;
        }

        case Statement::SK_Compound: {
            auto *compoundStmt = dynamic_cast<CompoundStatement *>(stmt);
            for (auto &item: *compoundStmt) {
                traverseBlockItem(item, breakableStack);
            }
            break;
        }

        // Other statements (Return, Expression, Null, Label, Goto) don't need processing
        default:
            break;
    }
}

void Sema::initialize() {
    CurrentScope = nullptr;
}

Program *Sema::actOnProgramDeclaration(FuncList &Funcs) const {
    auto *p = Context.createProgram<Program>();
    p->add_functions(Funcs);
    return p;
}

FunctionDeclaration *Sema::actOnFunctionDeclaration(SMLoc Loc, StringRef Name, ArgsList &args) const {
    auto *func = Context.createFunction<FunctionDeclaration>(Name, Loc, args);

    // Function declarations always go into the global/file scope
    // Even if declared inside a block, they have external linkage
    // Loop up to find the global scope (the one with no parent)
    Scope *globalScope = CurrentScope;
    while (globalScope->getParentScope() != nullptr) {
        globalScope = globalScope->getParentScope();
    }
    Scope *parentScope = CurrentScope->getParentScope();


    if (parentScope && parentScope->hasLinkageConflict(Name, Linkage::External)) {
        if (!avoid_errors) {
            Diags.report(SMLoc(), diag::err_linkage_conflict, Name);
            return nullptr;
        }
    }
    // Check if already declared in the target scope
    if (globalScope->hasSymbolInCurrentScope(Name)) {
        // Function redeclaration - lookup existing function
        FunctionDeclaration *existing = globalScope->lookupForFunction(Name);
        if (existing) {
            // We check now the number of parameters
            if (existing->getArgs().size() != args.size() && !avoid_errors) {
                Diags.report(Loc, diag::err_wrong_argument_count, Name, existing->getArgs().size(), args.size());
                return nullptr;
            }

            // Multiple declarations are allowed in C (e.g., forward declarations)
            // TODO: Later, check type compatibility between declarations
            // Return the EXISTING declaration so the body gets attached to the same object
            return existing;
        }
        // Symbol exists but it's not a function (should have been caught by linkage check)
        if (!avoid_errors) {
            Diags.report(Loc, diag::erro_func_already_declared, Name.str());
            return nullptr;
        }
    }

    // First declaration - insert into target scope
    globalScope->addDeclaredIdentifier(Name);
    globalScope->insert(func, Linkage::External);
    if (parentScope && globalScope != parentScope) {
        parentScope->addDeclaredIdentifier(Name);
        parentScope->insert(func, Linkage::External);
    }

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
Var *Sema::actOnParameterDeclaration(SMLoc Loc, StringRef Name) const {
    // Create Var with original name (no renaming)
    auto *var = Context.createExpression<Var>(Loc, Name);

    // Add to current scope if it exists (parameters are in function scope)
    if (CurrentScope) {
        // For parameters, we create a minimal VarDeclaration just for scope tracking
        auto *decl = Context.createDeclaration<VarDeclaration>(Loc, var);

        if (!CurrentScope->insert(Name, decl, Linkage::None)) {
            if (!avoid_errors) {
                Diags.report(Loc, diag::err_duplicate_variable_declaration, Name.str());
                return nullptr;
            }
        }

        CurrentScope->addDeclaredIdentifier(Name);
    }

    return var;
}

/**
 * @brief Process a variable declaration and add it to the current scope.
 *
 * Creates the variable with its original name, adds it to the current scope,
 * and appends the declaration to the block items. Reports an error if
 * a variable with the same name already exists in the current scope.
 *
 * @param Items Block items list to append the declaration to.
 * @param Loc Source location of the variable declaration.
 * @param Name Variable name.
 * @return true if a duplicate declaration error occurred, false on success.
 */
bool Sema::actOnVarDeclaration(BlockItems &Items, SMLoc Loc, StringRef Name) const {
    // Create declaration with original name (no renaming)
    auto *var = Context.createExpression<Var>(Loc, Name);
    auto *decl = Context.createDeclaration<VarDeclaration>(Loc, var);


    // Add to current scope if it exists
    if (CurrentScope) {
        // Check if there's already a symbol with a different linkage, to show a different error.
        if (CurrentScope->hasLinkageConflict(Name, Linkage::None)) {
            if (!avoid_errors) {
                Diags.report(SMLoc(), diag::err_linkage_conflict, Name);
                return true;
            }
        }
        // We try to insert it, but another declaration exists
        // this is an error, in the same scope (block) two variables
        // cannot have the same name.
        if (!CurrentScope->insert(Name, decl, Linkage::None)) {
            if (!avoid_errors) {
                Diags.report(Loc, diag::err_duplicate_variable_declaration, Name.str());
                return true;
            }
        }

        // Track that this variable was declared in the current scope
        CurrentScope->addDeclaredIdentifier(Name);
    }

    Items.emplace_back(decl);
    return false;
}

void Sema::actOnReturnStatement(BlockItems &Items, SMLoc Loc, Expr *RetVal) const {
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
        if (FunctionLabels.contains(Label)) {
            Diags.report(Loc, diag::err_existing_label, Label.str());
            exit(1);
        }
    }
    FunctionLabels.insert(Label);
    Items.emplace_back(Context.createStatement<LabelStatement>(Label));
}

void Sema::actOnGotoStatement(BlockItems &Items, SMLoc Loc, StringRef Label) {
    GotoLabels.insert(Label);
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

IntegerLiteral *Sema::actOnIntegerLiteral(SMLoc Loc, StringRef Literal) const {
    uint8_t Radix = 10;

    llvm::APInt Value(64, Literal, Radix);
    return Context.createExpression<IntegerLiteral>(Loc, llvm::APSInt(Value, false));
}

UnaryOperator *Sema::actOnUnaryOperator(SMLoc Loc, UnaryOperator::UnaryOperatorKind Kind, Expr *expr) const {
    return Context.createExpression<UnaryOperator>(Loc, Kind, expr);
}

BinaryOperator *Sema::actOnBinaryOperator(SMLoc Loc, BinaryOperator::BinaryOpKind Kind, Expr *left, Expr *right) const {
    return Context.createExpression<BinaryOperator>(Loc, Kind, left, right);
}

AssignmentOperator *Sema::actOnAssignment(SMLoc Loc, Expr *left, Expr *right) const {
    if (!avoid_errors) {
        if (left->getKind() != Expr::Ek_Var) {
            Diags.report(Loc, diag::err_incorrect_lvalue);
            return nullptr;
        }
        // TODO: Type checking - for now all variables are int
    }

    return Context.createExpression<AssignmentOperator>(Loc, left, right);
}

PrefixOperator *Sema::actOnPrefixOperator(SMLoc Loc, PrefixOperator::PrefixOpKind Kind, Expr *expr) const {
    if (!avoid_errors) {
        if (expr->getKind() != Expr::Ek_Var) {
            Diags.report(Loc, diag::err_incorrect_lvalue);
            return nullptr;
        }
    }

    return Context.createExpression<PrefixOperator>(Loc, Kind, expr);
}

PostfixOperator *Sema::actOnPostfixOperator(SMLoc Loc, PostfixOperator::PostfixOpKind Kind, Expr *expr) const {
    if (!avoid_errors) {
        if (expr->getKind() != Expr::Ek_Var) {
            Diags.report(Loc, diag::err_incorrect_lvalue);
            return nullptr;
        }
    }

    return Context.createExpression<PostfixOperator>(Loc, Kind, expr);
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
            if (!avoid_errors) {
                // Issue error for potentially undefined variable
                Diags.report(Loc, diag::err_var_used_before_declared, Name.str());
                return nullptr;
            }
        } else {
            // Variable exists, return the Var from the declaration
            return decl->getVar();
        }
    }

    return Context.createExpression<Var>(Loc, Name);
}

ConditionalExpr *Sema::actOnTernaryOperator(SMLoc, Expr *left, Expr *middle, Expr *right) const {
    return Context.createExpression<ConditionalExpr>(left, middle, right);
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
    if (func != nullptr) {
        if (func->getArgs().size() != args.size() && !avoid_errors) {
            Diags.report(Loc, diag::err_wrong_argument_call, name, args.size(), func->getArgs().size());
            return nullptr;
        }
        // Function found, use its name (original name)
        return Context.createExpression<FunctionCallExpr>(func->getName(), args);
    }
    if (!avoid_errors) {
        // Issue error for potentially undefined function
        Diags.report(Loc, diag::err_func_used_before_declared, name.str());
        return nullptr;
    }
    return Context.createExpression<FunctionCallExpr>(name, args);
}
