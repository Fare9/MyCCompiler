
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

void Sema::actOnFunctionDeclaration(Function *F, SMLoc Loc, StringRef Name, BlockItems& Items) {
    F->setBody(Items);
}

void Sema::actOnVarDeclaration(BlockItems& Items, SMLoc Loc, Var* Name, Expr* assignment) {
    Items.push_back(new Declaration(Loc, Name, assignment));
}

void Sema::actOnReturnStatement(BlockItems& Items, SMLoc Loc, Expr *RetVal) {
    Items.push_back(new ReturnStatement(RetVal));
}

void Sema::actOnNullStatement(BlockItems& Items, SMLoc Loc) {
    Items.push_back(new NullStatement());
}

void Sema::actOnExprStatement(BlockItems& Items, SMLoc Loc, Expr *Expr) {
    Items.push_back(new ExpressionStatement(Expr));
}

IntegerLiteral* Sema::actOnIntegerLiteral(SMLoc Loc, StringRef Literal) {
    uint8_t Radix = 10;

    llvm::APInt Value(64, Literal, Radix);
    return new IntegerLiteral(Loc, llvm::APSInt(Value, false));
}

UnaryOperator* Sema::actOnUnaryOperator(SMLoc Loc, UnaryOperator::UnaryOperatorKind Kind, Expr* expr) {
    return new UnaryOperator(Loc, Kind, expr);
}

BinaryOperator* Sema::actOnBinaryOperator(SMLoc Loc, BinaryOperator::BinaryOpKind Kind, Expr* left, Expr* right) {
    return new BinaryOperator(Loc, Kind, left, right);
}

AssignmentOperator* Sema::actOnAssignment(SMLoc Loc, Expr* left, Expr* right) {
    return new AssignmentOperator(Loc, left, right);
}

Var* Sema::actOnIdentifier(SMLoc Loc, StringRef Name) {
    return new Var(Loc, Name);
}