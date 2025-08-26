
#include "mycc/Sema/Sema.hpp"

using namespace mycc;

void Sema::initialize() {
}

Program * Sema::actOnProgramDeclaration(FuncList &Funcs) {
    auto * p = new Program();
    p->add_functions(Funcs);
    return p;
}

Function * Sema::actOnFunctionDeclaration(SMLoc Loc, StringRef Name) {
    return new Function(Name, Loc);
}

void Sema::actOnFunctionDeclaration(Function *F, SMLoc Loc, StringRef Name, StmtList &Stmts) {
    F->setStmts(Stmts);
}

void Sema::actOnReturnStatement(StmtList &Stmts, SMLoc Loc, Expr *RetVal) {
    Stmts.push_back(new ReturnStatement(RetVal));
}

Expr* Sema::actOnIntegerLiteral(SMLoc Loc, StringRef Literal) {
    uint8_t Radix = 10;

    llvm::APInt Value(64, Literal, Radix);
    return new IntegerLiteral(Loc, llvm::APSInt(Value, false));
}

Expr* Sema::actOnUnaryOperator(SMLoc Loc, UnaryOperator::UnaryOperatorKind Kind, Expr *expr) {
    return new UnaryOperator(Loc, Kind, expr);
}