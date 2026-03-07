#pragma once

#include "mycc/AST/AST.hpp"
#include "mycc/AST/ASTContext.hpp"
#include "mycc/Sema/Scope.hpp"

namespace mycc {
class TypeExpressionInference {
    ASTContext& Context;
public:
    explicit TypeExpressionInference(ASTContext& Context) : Context(Context) {
    }

    Type* commonType(Type*, Type*) const;

    Type* getType(Expr*, Scope*);
    Type* getType(IntegerLiteral*, Scope*);
    Type* getType(LongLiteral*, Scope*);
    Type* getType(Var*, Scope*);
    Type* getType(UnaryOperator*, Scope*);
    Type* getType(BinaryOperator*, Scope*);
    Type* getType(AssignmentOperator*, Scope*);
    Type* getType(PrefixOperator*, Scope*);
    Type* getType(PostfixOperator*, Scope*);
    Type* getType(ConditionalExpr*, Scope*);
    Type* getType(FunctionCallExpr*, Scope*);
    Type* getType(CastExpr*, Scope*);
};
}
