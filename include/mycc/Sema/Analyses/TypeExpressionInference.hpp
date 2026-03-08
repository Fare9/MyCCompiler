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

    /// Usual arithmetic conversions — the common type for a binary expression.
    /// Returns nullptr if the types are not arithmetically compatible.
    Type* commonType(Type* lhs, Type* rhs) const;

    /// Returns true if a value of type `from` can be assigned/passed into
    /// a slot of type `to`. For builtins this means same kind or a
    /// widening/narrowing integer conversion. Extend here for pointers.
    bool isAssignable(Type* from, Type* to) const;

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
