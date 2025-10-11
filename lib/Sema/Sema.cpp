
#include "mycc/Sema/Sema.hpp"
#include <cassert>

using namespace mycc;

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
    VariableNameStacks[originalName].push_back(uniqueName);
}

std::string Sema::getCurrentUniqueVarName(StringRef originalName) {
    auto it = VariableNameStacks.find(originalName);
    if (it != VariableNameStacks.end() && !it->second.empty()) {
        return it->second.back();
    }
    return originalName.str();
}

void Sema::popVariablesFromScope(const std::vector<std::string>& declaredVars) {
    for (const std::string& varName : declaredVars) {
        auto it = VariableNameStacks.find(varName);
        if (it != VariableNameStacks.end() && !it->second.empty()) {
            it->second.pop_back();
        }
    }
}