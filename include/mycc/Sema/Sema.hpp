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
    void actOnFunctionDeclaration(Function *F, SMLoc Loc, StringRef Name, BlockItems& Items);

    void actOnVarDeclaration(BlockItems& Items, SMLoc Loc, Var* Name, Expr* assignment);

    void actOnReturnStatement(BlockItems& Items, SMLoc Loc, Expr *RetVal);
    void actOnNullStatement(BlockItems& Items, SMLoc Loc);
    void actOnExprStatement(BlockItems& Items, SMLoc Loc, Expr *Expr);

    IntegerLiteral* actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
    UnaryOperator* actOnUnaryOperator(SMLoc, UnaryOperator::UnaryOperatorKind Kind, Expr* expr);
    BinaryOperator* actOnBinaryOperator(SMLoc, BinaryOperator::BinaryOpKind Kind, Expr* left, Expr* right);
    AssignmentOperator* actOnAssignment(SMLoc, Expr* left, Expr* right);
    Var* actOnIdentifier(SMLoc, StringRef Name);
};
}