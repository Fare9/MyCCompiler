#pragma once

#include "mycc/AST/AST.hpp"
#include "mycc/Basic/Diagnostic.hpp"

namespace mycc {
class Sema {
    DiagnosticsEngine &Diags;

public:
    explicit Sema(DiagnosticsEngine &Diags) : Diags(Diags) {
        initialize();
    }

    void initialize();

    Program * actOnProgramDeclaration(FuncList &Funcs);
    Function * actOnFunctionDeclaration(SMLoc Loc, StringRef Name);
    void actOnFunctionDeclaration(Function *F, SMLoc Loc, StringRef Name, StmtList &Stmts);

    void actOnReturnStatement(StmtList &Stmts, SMLoc Loc, Expr *RetVal);
    Expr* actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
    Expr* actOnUnaryOperator(SMLoc, UnaryOperator::UnaryOperatorKind Kind, Expr *expr);
    Expr* actOnBinaryOperator(SMLoc, BinaryOperator::BinaryOpKind Kind, Expr *left, Expr *right);
};
}