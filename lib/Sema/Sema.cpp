
#include "mycc/Sema/Sema.hpp"
#include <cassert>

using namespace mycc;

void Sema::enterFunction()
{
    FunctionLabels.clear();
    GotoLabels.clear();
}

void Sema::exitFunction()
{
    // Check that all goto labels are defined in the function
    if (!avoid_errors) {
        checkGotoLabelsCorrectlyPointToFunction();
    }
}

void Sema::checkGotoLabelsCorrectlyPointToFunction()
{
    for (const auto& label : GotoLabels) {
        if (!FunctionLabels.contains(label)) {
            Diags.report(SMLoc(), diag::err_undefined_label, label.str());
            exit(1);
        }
    }
}

void Sema::enterScope()
{
    CurrentScope = new Scope(CurrentScope);
}

void Sema::exitScope()
{
    // check first there's a current scope
    assert(CurrentScope && "Can't exit non-existing scope");

    // Pop variables that were declared in this scope
    popVariablesFromScope(CurrentScope->getDeclaredVariables());

    Scope *Parent = CurrentScope->getParentScope();
    // delete current scope
    delete CurrentScope;
    CurrentScope = Parent;
}

std::string Sema::generateLoopLabel() {
    return "loop_" + std::to_string(LoopLabelCounter++);
}

void Sema::assignLoopLabels(Function& F) {
    // Stack holds base loop labels for nested loops
    std::vector<std::string> loopStack;

    // Traverse all items in the function body
    for (auto& item : F) {
        traverseBlockItem(item, loopStack);
    }
}

void Sema::traverseBlockItem(BlockItem& item, std::vector<std::string>& loopStack) {
    if (std::holds_alternative<Statement*>(item)) {
        traverseStatement(std::get<Statement*>(item), loopStack);
    }
}

void Sema::traverseStatement(Statement* stmt, std::vector<std::string>& loopStack) {
    if (!stmt) return;

    switch (stmt->getKind()) {
        case Statement::SK_While: {
            auto* whileStmt = static_cast<WhileStatement*>(stmt);
            std::string baseLabel = generateLoopLabel();
            whileStmt->set_label(baseLabel);

            // Push base label onto stack
            loopStack.push_back(baseLabel);

            // Traverse body
            traverseStatement(whileStmt->getBody(), loopStack);

            // Pop from stack
            loopStack.pop_back();
            break;
        }

        case Statement::SK_DoWhile: {
            auto* doWhileStmt = static_cast<DoWhileStatement*>(stmt);
            std::string baseLabel = generateLoopLabel();
            doWhileStmt->set_label(baseLabel);

            loopStack.push_back(baseLabel);
            traverseStatement(doWhileStmt->getBody(), loopStack);
            loopStack.pop_back();
            break;
        }

        case Statement::SK_For: {
            auto* forStmt = static_cast<ForStatement*>(stmt);
            std::string baseLabel = generateLoopLabel();
            forStmt->set_label(baseLabel);

            loopStack.push_back(baseLabel);
            traverseStatement(forStmt->getBody(), loopStack);
            loopStack.pop_back();
            break;
        }

        case Statement::SK_Break: {
            auto* breakStmt = static_cast<BreakStatement*>(stmt);
            if (loopStack.empty()) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_break_not_in_loop);
                    exit(1);
                }
            } else {
                // Break jumps to end of loop: loop_N_end
                std::string targetLabel = loopStack.back() + "_end";
                breakStmt->set_label(targetLabel);
            }
            break;
        }

        case Statement::SK_Continue: {
            auto* continueStmt = static_cast<ContinueStatement*>(stmt);
            if (loopStack.empty()) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_continue_not_in_loop);
                    exit(1);
                }
            } else {
                // Continue jumps to start of loop: loop_N_start
                std::string targetLabel = loopStack.back() + "_start";
                continueStmt->set_label(targetLabel);
            }
            break;
        }

        case Statement::SK_If: {
            auto* ifStmt = static_cast<IfStatement*>(stmt);
            traverseStatement(ifStmt->getThenSt(), loopStack);
            if (ifStmt->getElseSt()) {
                traverseStatement(ifStmt->getElseSt(), loopStack);
            }
            break;
        }

        case Statement::SK_Compound: {
            auto* compoundStmt = static_cast<CompoundStatement*>(stmt);
            for (auto& item : *compoundStmt) {
                traverseBlockItem(item, loopStack);
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
    VariableCounter = 0;
}

Program * Sema::actOnProgramDeclaration(FuncList &Funcs) {
    auto * p = Context.createProgram<Program>();
    p->add_functions(Funcs);
    return p;
}

Function * Sema::actOnFunctionDeclaration(SMLoc Loc, StringRef Name) {
    auto* func = Context.createFunction<Function>(Name, Loc);

    return func;
}

bool Sema::actOnVarDeclaration(BlockItems& Items, SMLoc Loc, StringRef Name) {
    // Generate unique name and track it
    StringRef originalName = Name;
    std::string uniqueName = generateUniqueVarName(originalName);
    pushVariableName(originalName, uniqueName);

    // Create declaration with unique name
    auto* var = Context.createExpression<Var>(Loc, uniqueName);
    auto* decl = Context.createDeclaration<Declaration>(Loc, var);

    // Add to current scope if it exists, using original name as key
    if (CurrentScope) {
        // We try to insert it, but another declaration exists
        // this is an error, in the same scope (block) two variables
        // cannot have the same name.
        if (!CurrentScope->insert(originalName, decl)) {
            if (!avoid_errors) {
                Diags.report(Loc, diag::err_duplicate_variable_declaration, originalName.str());
                return true;
            }
        }

        // Track that this variable was declared in the current scope
        CurrentScope->addDeclaredVariable(originalName);
    }

    Items.push_back(decl);
    return false;
}

void Sema::actOnReturnStatement(BlockItems& Items, SMLoc Loc, Expr *RetVal) {
    Items.push_back(Context.createStatement<ReturnStatement>(RetVal));
}

void Sema::actOnNullStatement(BlockItems& Items, SMLoc Loc) {
    Items.push_back(Context.createStatement<NullStatement>());
}

void Sema::actOnExprStatement(BlockItems& Items, SMLoc Loc, Expr *Expr) {
    Items.push_back(Context.createStatement<ExpressionStatement>(Expr));
}

void Sema::actOnIfStatement(BlockItems& Items, SMLoc Loc, Expr *Cond, Statement *then_st, Statement *else_st) {
    Items.push_back(Context.createStatement<IfStatement>(Cond, then_st, else_st));
}

void Sema::actOnCompoundStatement(BlockItems& Items, SMLoc Loc, BlockItems& compoundStatement) {
    Items.push_back(Context.createStatement<CompoundStatement>(compoundStatement));
}

void Sema::actOnLabelStatement(BlockItems& Items, SMLoc Loc, StringRef Label)
{
    if (!avoid_errors)
    {
        // The Labels are unique for each function, we must
        // ensure this property, throwing an error in case
        // an existing label has been declared again.
        if (FunctionLabels.contains(Label))
        {
            Diags.report(Loc, diag::err_existing_label, Label.str());
            exit(1);
        }
    }
    FunctionLabels.insert(Label);
    Items.push_back(Context.createStatement<LabelStatement>(Label));
}

void Sema::actOnGotoStatement(BlockItems& Items, SMLoc Loc, StringRef Label)
{
    GotoLabels.insert(Label);
    Items.push_back(Context.createStatement<GotoStatement>(Label));
}

void Sema::actOnWhileStatement(BlockItems& Items, SMLoc Loc, Expr *Cond, Statement *Body)
{
    Items.push_back(Context.createStatement<WhileStatement>(Cond, Body));
}

void Sema::actOnDoWhileStatement(BlockItems& Items, SMLoc Loc, Statement *Body, Expr *Cond)
{
    Items.push_back(Context.createStatement<DoWhileStatement>(Body, Cond));
}

void Sema::actOnForStatement(BlockItems& Items, SMLoc Loc, ForInit& Init, Expr *Cond, Expr *Post, Statement *Body)
{
    Items.push_back(Context.createStatement<ForStatement>(Init, Cond, Post, Body));
}

void Sema::actOnBreakStatement(BlockItems& Items, SMLoc Loc)
{
    Items.push_back(Context.createStatement<BreakStatement>());
}

void Sema::actOnContinueStatement(BlockItems& Items, SMLoc Loc)
{
    Items.push_back(Context.createStatement<ContinueStatement>());
}

IntegerLiteral* Sema::actOnIntegerLiteral(SMLoc Loc, StringRef Literal) {
    uint8_t Radix = 10;

    llvm::APInt Value(64, Literal, Radix);
    return Context.createExpression<IntegerLiteral>(Loc, llvm::APSInt(Value, false));
}

UnaryOperator* Sema::actOnUnaryOperator(SMLoc Loc, UnaryOperator::UnaryOperatorKind Kind, Expr* expr) {
    return Context.createExpression<UnaryOperator>(Loc, Kind, expr);
}

BinaryOperator* Sema::actOnBinaryOperator(SMLoc Loc, BinaryOperator::BinaryOpKind Kind, Expr* left, Expr* right) {
    return Context.createExpression<BinaryOperator>(Loc, Kind, left, right);
}

AssignmentOperator* Sema::actOnAssignment(SMLoc Loc, Expr* left, Expr* right) {
    if (!avoid_errors) {
        if (left->getKind() != Expr::Ek_Var) {
            Diags.report(Loc, diag::err_incorrect_lvalue);
            return nullptr;
        }
    }

    return Context.createExpression<AssignmentOperator>(Loc, left, right);
}

PrefixOperator* Sema::actOnPrefixOperator(SMLoc Loc, PrefixOperator::PrefixOpKind Kind, Expr* expr) {
    if (!avoid_errors) {
        if (expr->getKind() != Expr::Ek_Var) {
            Diags.report(Loc, diag::err_incorrect_lvalue);
            return nullptr;
        }
    }

    return Context.createExpression<PrefixOperator>(Loc, Kind, expr);
}

PostfixOperator* Sema::actOnPostfixOperator(SMLoc Loc, PostfixOperator::PostfixOpKind Kind, Expr* expr) {
    if (!avoid_errors) {
        if (expr->getKind() != Expr::Ek_Var) {
            Diags.report(Loc, diag::err_incorrect_lvalue);
            return nullptr;
        }
    }

    return Context.createExpression<PostfixOperator>(Loc, Kind, expr);
}

Var* Sema::actOnIdentifier(SMLoc Loc, StringRef Name) {
    // Look up the variable in current scope
    if (CurrentScope) {
        // We make a lookup by name, this lookup will traverse
        // all the scopes from current through parents looking
        /// for the variable.
        Declaration* decl = CurrentScope->lookup(Name);
        if (!decl) {
            if (!avoid_errors) {
                // Issue error for potentially undefined variable
                Diags.report(Loc, diag::err_var_used_before_declared, Name.str());
                return nullptr;
            }
        }
        else {
            // Variable exists, get the unique name for it
            std::string uniqueName = getCurrentUniqueVarName(Name);
            return Context.createExpression<Var>(Loc, uniqueName);
        }
    }

    return Context.createExpression<Var>(Loc, Name);
}

ConditionalExpr * Sema::actOnTernaryOperator(SMLoc, Expr* left, Expr* middle, Expr* right) {
    return Context.createExpression<ConditionalExpr>(left, middle, right);
}

std::string Sema::generateUniqueVarName(StringRef originalName) {
    return originalName.str() + "_" + std::to_string(VariableCounter++);
}

void Sema::pushVariableName(StringRef originalName, const std::string& uniqueName) {
    // We keep unique names instead of the original ones
    // it will be easier for later generating the intermmediate
    // representation
    VariableNameStacks[originalName].push_back(uniqueName);
}

std::string Sema::getCurrentUniqueVarName(StringRef originalName) {
    // Look in the map of variable names, look for the last one.
    auto it = VariableNameStacks.find(originalName);
    if (it != VariableNameStacks.end() && !it->second.empty()) {
        return it->second.back();
    }
    return originalName.str();
}

void Sema::popVariablesFromScope(const std::vector<std::string>& declaredVars) {
    // Once we go out from a scope (a block), we have to
    // remove all the declared variables from the variable
    // name stacks, so we do not keep the unique generated
    // names.
    for (const std::string& varName : declaredVars) {
        auto it = VariableNameStacks.find(varName);
        if (it != VariableNameStacks.end() && !it->second.empty()) {
            it->second.pop_back();
        }
    }
}