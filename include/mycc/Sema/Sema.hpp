#pragma once

#include "mycc/AST/AST.hpp"
#include "mycc/Sema/Scope.hpp"
#include "mycc/Basic/Diagnostic.hpp"
#include "llvm/ADT/StringMap.h"
#include <vector>
#include <string>

namespace mycc {
class Sema {
    friend class EnterDeclScope;

    DiagnosticsEngine &Diags;
    Scope *CurrentScope;
    bool avoid_errors = false;

    // Variable name mapping: original_name -> stack of unique_names
    StringMap<std::vector<std::string>> VariableNameStacks;

    // Counter for generating unique variable names
    unsigned int VariableCounter;

    // Helper methods for variable name management
    std::string generateUniqueVarName(StringRef originalName);
    void pushVariableName(StringRef originalName, const std::string& uniqueName);
    std::string getCurrentUniqueVarName(StringRef originalName);
    void popVariablesFromScope(const std::vector<std::string>& declaredVars);

public:
    explicit Sema(DiagnosticsEngine &Diags) : Diags(Diags) {
        initialize();
    }

    void avoidErrors() {
        avoid_errors = true;
    }

    void initialize();

    void enterScope();
    void exitScope();

    Program * actOnProgramDeclaration(FuncList &Funcs);
    Function * actOnFunctionDeclaration(SMLoc Loc, StringRef Name);

    bool actOnVarDeclaration(BlockItems& Items, SMLoc Loc, StringRef Name);

    void actOnReturnStatement(BlockItems& Items, SMLoc Loc, Expr *RetVal);
    void actOnNullStatement(BlockItems& Items, SMLoc Loc);
    void actOnExprStatement(BlockItems& Items, SMLoc Loc, Expr *Expr);

    IntegerLiteral* actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
    UnaryOperator* actOnUnaryOperator(SMLoc, UnaryOperator::UnaryOperatorKind Kind, Expr* expr);
    BinaryOperator* actOnBinaryOperator(SMLoc, BinaryOperator::BinaryOpKind Kind, Expr* left, Expr* right);
    AssignmentOperator* actOnAssignment(SMLoc, Expr* left, Expr* right);
    Var* actOnIdentifier(SMLoc, StringRef Name);
};

class EnterDeclScope {
    Sema &Semantics;
public:
    EnterDeclScope(Sema &Semantics) : Semantics(Semantics) {
        Semantics.enterScope();
    }

    ~EnterDeclScope() {
        Semantics.exitScope();
    }
};
}