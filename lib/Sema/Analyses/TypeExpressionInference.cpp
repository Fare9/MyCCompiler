#include "mycc/Sema/Analyses/TypeExpressionInference.hpp"
#include "mycc/Sema/Scope.hpp"

using namespace mycc;

// ============================================================================
// Helpers — integer promotion rank (C11 §6.3.1.1)
// ============================================================================

namespace {
    /// Usual arithmetic conversions for two builtin integer types (C11 §6.3.1.8).
    /// Returns nullptr if either type is non-arithmetic (e.g. void).
    Type *promoteBuiltins(const BuiltinType *bt1, const BuiltinType *bt2,
                          ASTContext &ctx) {
        int r1 = BuiltinType::integerRank(bt1->getBuiltinKind());
        int r2 = BuiltinType::integerRank(bt2->getBuiltinKind());

        // Either operand is void or otherwise non-integer, not promotable.
        if (r1 < 0 || r2 < 0)
            return nullptr;

        // Integer promotion: sub-int types (bool, char, short) first promote to int.
        const int intRank = BuiltinType::integerRank(BuiltinType::Int);
        r1 = std::max(r1, intRank);
        r2 = std::max(r2, intRank);

        // The operand with higher rank wins.
        BuiltinType::BuiltinKind winner =
                (r1 >= r2) ? bt1->getBuiltinKind() : bt2->getBuiltinKind();
        return ctx.getBuiltInType(winner);
    }

    bool isComparisonOrLogical(BinaryOperator::BinaryOpKind kind) {
        switch (kind) {
            case BinaryOperator::BoK_LowerThan:
            case BinaryOperator::BoK_LowerEqual:
            case BinaryOperator::BoK_GreaterThan:
            case BinaryOperator::BoK_GreaterEqual:
            case BinaryOperator::Bok_Equal:
            case BinaryOperator::Bok_NotEqual:
            case BinaryOperator::Bok_And:
            case BinaryOperator::Bok_Or:
                return true;
            default:
                return false;
        }
    }
} // namespace

// ============================================================================
// TypeExpressionInference
// ============================================================================

Type *TypeExpressionInference::commonType(Type *left, Type *right) const {
    if (!left || !right)
        return nullptr;

    if (left->getKind() == Type::TK_Builtin &&
        right->getKind() == Type::TK_Builtin) {
        return promoteBuiltins(llvm::cast<BuiltinType>(left),
                               llvm::cast<BuiltinType>(right), Context);
    }

    // Pointer/function types: no implicit common type — callers must handle.
    return nullptr;
}

bool TypeExpressionInference::isAssignable(Type *from, Type *to) const {
    if (!from || !to) return false;

    // Identical types (works by pointer because ASTContext interns them).
    if (from == to) return true;

    // Arithmetic types: any integer type is assignable to any other integer
    // type (narrowing is allowed in C, just potentially lossy).
    if (from->getKind() == Type::TK_Builtin &&
        to->getKind() == Type::TK_Builtin) {
        auto fromRank = BuiltinType::integerRank(llvm::cast<BuiltinType>(from)->getBuiltinKind());
        auto toRank = BuiltinType::integerRank(llvm::cast<BuiltinType>(to)->getBuiltinKind());
        // Both must be integer types (rank >= 0); void is not assignable.
        return fromRank >= 0 && toRank >= 0;
    }

    // TODO: pointer assignability (T* -> T*, void* -> T*, null constant → T*)
    //      once we have added the pointers, we will add these rules
    return false;
}

Type *TypeExpressionInference::getType(Expr *expr, Scope *scope) {
    switch (expr->getKind()) {
        case Expr::Ek_Int:
            return getType(llvm::cast<IntegerLiteral>(expr), scope);
        case Expr::Ek_Long:
            return getType(llvm::cast<LongLiteral>(expr), scope);
        case Expr::Ek_Var:
            return getType(llvm::cast<Var>(expr), scope);
        case Expr::Ek_UnaryOperator:
            return getType(llvm::cast<UnaryOperator>(expr), scope);
        case Expr::Ek_BinaryOperator:
            return getType(llvm::cast<BinaryOperator>(expr), scope);
        case Expr::Ek_AssignmentOperator:
            return getType(llvm::cast<AssignmentOperator>(expr), scope);
        case Expr::Ek_PrefixOperator:
            return getType(llvm::cast<PrefixOperator>(expr), scope);
        case Expr::Ek_PostfixOperator:
            return getType(llvm::cast<PostfixOperator>(expr), scope);
        case Expr::Ek_ConditionalOperator:
            return getType(llvm::cast<ConditionalExpr>(expr), scope);
        case Expr::Ek_FunctionCallOperator:
            return getType(llvm::cast<FunctionCallExpr>(expr), scope);
        case Expr::Ek_Cast:
            return getType(llvm::cast<CastExpr>(expr), scope);
        case Expr::Ek_IntInit:
            expr->setType(Context.getIntTy());
            return expr->getType();
        case Expr::Ek_LongInit:
            expr->setType(Context.getLongTy());
            return expr->getType();
        default:
            return nullptr;
    }
}

Type *TypeExpressionInference::getType(IntegerLiteral *il, Scope *) {
    if (!il->getType())
        il->setType(Context.getIntTy());
    return il->getType();
}

Type *TypeExpressionInference::getType(LongLiteral *ll, Scope *) {
    if (!ll->getType())
        ll->setType(Context.getLongTy());
    return ll->getType();
}

Type *TypeExpressionInference::getType(Var *v, Scope *scope) {
    if (!v->getType()) {
        if (auto *decl = scope->lookupForVar(v->getName()))
            v->setType(decl->getType());
    }
    return v->getType();
}

Type *TypeExpressionInference::getType(UnaryOperator *uop, Scope *scope) {
    if (uop->getType())
        return uop->getType();

    if (uop->getOperatorKind() == UnaryOperator::UopK_Not) {
        // Logical not always produces int regardless of operand type.
        uop->setType(Context.getIntTy());
    } else {
        uop->setType(getType(uop->getExpr(), scope));
    }
    return uop->getType();
}

Type *TypeExpressionInference::getType(BinaryOperator *bop, Scope *scope) {
    if (bop->getType())
        return bop->getType();

    auto *t1 = getType(bop->getLeft(), scope);
    auto *t2 = getType(bop->getRight(), scope);
    if (!t1 || !t2)
        return nullptr;

    if (isComparisonOrLogical(bop->getOperatorKind())) {
        // Relational and logical operators always produce int (C11 §6.5.8).
        bop->setType(Context.getIntTy());
    } else if (t1->getKind() == Type::TK_Builtin &&
               t2->getKind() == Type::TK_Builtin) {
        bop->setType(promoteBuiltins(llvm::cast<BuiltinType>(t1),
                                     llvm::cast<BuiltinType>(t2), Context));
    }
    // TODO: pointer arithmetic (ptr ± integer → pointer, ptr − ptr → ptrdiff_t)

    return bop->getType();
}

Type *TypeExpressionInference::getType(AssignmentOperator *aop, Scope *scope) {
    if (aop->getType())
        return aop->getType();

    auto *lhsType = getType(aop->getLeft(), scope);
    [[maybe_unused]] auto *rhsType = getType(aop->getRight(), scope);

    // TODO: verify RHS is assignable to LHS; insert ImplicitCastExpr on RHS
    //       when types differ (e.g. int → long requires sext in codegen).

    // An assignment expression has the type of its LHS (C11 §6.5.16).
    aop->setType(lhsType);
    return aop->getType();
}

Type *TypeExpressionInference::getType(PrefixOperator *preOp, Scope *scope) {
    if (preOp->getType())
        return preOp->getType();

    preOp->setType(getType(preOp->getExpr(), scope));
    return preOp->getType();
}

Type *TypeExpressionInference::getType(PostfixOperator *posOp, Scope *scope) {
    if (posOp->getType())
        return posOp->getType();

    posOp->setType(getType(posOp->getExpr(), scope));
    return posOp->getType();
}

Type *TypeExpressionInference::getType(ConditionalExpr *cExpr, Scope *scope) {
    if (cExpr->getType())
        return cExpr->getType();

    auto *t1 = getType(cExpr->getLeft(), scope);
    auto *t2 = getType(cExpr->getRight(), scope);
    if (!t1 || !t2)
        return nullptr;

    if (t1->getKind() == Type::TK_Builtin && t2->getKind() == Type::TK_Builtin) {
        cExpr->setType(promoteBuiltins(llvm::cast<BuiltinType>(t1),
                                       llvm::cast<BuiltinType>(t2), Context));
    }
    // TODO: C11 §6.5.15 - one branch may be void, or both pointers to
    //       compatible types. We will handle those cases here, but not yet.

    return cExpr->getType();
}

Type *TypeExpressionInference::getType(FunctionCallExpr *func, Scope *scope) {
    if (func->getType())
        return func->getType();

    if (auto *decl = scope->lookupForFunction(func->getIdentifier()))
        func->setType(decl->getFunctionType()->getReturnType());

    return func->getType();
}

Type *TypeExpressionInference::getType(CastExpr *cast, Scope *) {
    // TODO: validate that the cast is legal (e.g. void is not castable to int,
    //       pointer<->integer casts require explicit acknowledgement).
    return cast->getCastedType();
}
