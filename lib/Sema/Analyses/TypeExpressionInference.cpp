#include "mycc/Sema/Analyses/TypeExpressionInference.hpp"
#include "mycc/Sema/Scope.hpp"
#include <unordered_map>

using namespace mycc;

namespace {
    const std::unordered_map<int, BuiltinType::BuiltinKind>
    typesCastMap{
        {BuiltinType::Void, BuiltinType::Void},
        {BuiltinType::Int, BuiltinType::Int},
        {BuiltinType::Long, BuiltinType::Long},
        {BuiltinType::Int | BuiltinType::Long, BuiltinType::Long},
    };

    Type *promoteBuiltins(const BuiltinType *bt1, const BuiltinType *bt2, ASTContext &Context) {
        if (bt1 == nullptr || bt2 == nullptr) return nullptr;
        int mixed = bt1->getBuiltinKind() | bt2->getBuiltinKind();
        auto it = typesCastMap.find(mixed);
        if (it != typesCastMap.end())
            return Context.getBuiltInType(it->second);
        return nullptr;
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
}

Type *TypeExpressionInference::commonType(Type *left, Type *right) const {
    if (left == nullptr || right == nullptr) return nullptr;
    if (left->getKind() == Type::TK_Builtin && right->getKind() == Type::TK_Builtin) {
        auto *lBK = llvm::cast<BuiltinType>(left);
        auto *rBK = llvm::cast<BuiltinType>(right);
        return promoteBuiltins(lBK, rBK, Context);
    }
    return nullptr;
}

Type *TypeExpressionInference::getType(Expr *expr, Scope *scope) {
    switch (expr->getKind()) {
        case Expr::Ek_Int:                 return getType(llvm::cast<IntegerLiteral>(expr), scope);
        case Expr::Ek_Long:                return getType(llvm::cast<LongLiteral>(expr), scope);
        case Expr::Ek_Var:                 return getType(llvm::cast<Var>(expr), scope);
        case Expr::Ek_UnaryOperator:       return getType(llvm::cast<UnaryOperator>(expr), scope);
        case Expr::Ek_BinaryOperator:      return getType(llvm::cast<BinaryOperator>(expr), scope);
        case Expr::Ek_AssignmentOperator:  return getType(llvm::cast<AssignmentOperator>(expr), scope);
        case Expr::Ek_PrefixOperator:      return getType(llvm::cast<PrefixOperator>(expr), scope);
        case Expr::Ek_PostfixOperator:     return getType(llvm::cast<PostfixOperator>(expr), scope);
        case Expr::Ek_ConditionalOperator: return getType(llvm::cast<ConditionalExpr>(expr), scope);
        case Expr::Ek_FunctionCallOperator:return getType(llvm::cast<FunctionCallExpr>(expr), scope);
        case Expr::Ek_Cast:                return getType(llvm::cast<CastExpr>(expr), scope);
        default:                           return nullptr;
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
    if (uop->getType() == nullptr) {
        if (uop->getOperatorKind() == UnaryOperator::UopK_Not) {
            uop->setType(Context.getIntTy());
        } else {
            auto *type = getType(uop->getExpr(), scope);
            uop->setType(type);
        }
    }
    return uop->getType();
}

Type *TypeExpressionInference::getType(BinaryOperator *bop, Scope *scope) {
    if (bop->getType() == nullptr) {
        auto *t1 = getType(bop->getLeft(), scope);
        auto *t2 = getType(bop->getRight(), scope);
        if (t1 == nullptr || t2 == nullptr) return nullptr;
        if (isComparisonOrLogical(bop->getOperatorKind())) { // the result of this binary operation is always an integer
            bop->setType(Context.getIntTy());
        } else if (t1->getKind() == Type::TK_Builtin && t2->getKind() == Type::TK_Builtin) { // in other case, check if we are working with builtin types
            auto *bt1 = llvm::cast<BuiltinType>(t1);
            auto *bt2 = llvm::cast<BuiltinType>(t2);
            bop->setType(promoteBuiltins(bt1, bt2, Context));
        }
    }
    return bop->getType();
}

Type *TypeExpressionInference::getType(AssignmentOperator *aop, Scope *scope) {
    if (aop->getType() == nullptr) {
        auto *type = getType(aop->getLeft(), scope);
        aop->setType(type);
    }
    return aop->getType();
}

Type *TypeExpressionInference::getType(PrefixOperator *preOp, Scope *scope) {
    if (preOp->getType() == nullptr) {
        auto *type = getType(preOp->getExpr(), scope);
        preOp->setType(type);
    }
    return preOp->getType();
}

Type *TypeExpressionInference::getType(PostfixOperator *posOp, Scope *scope) {
    if (posOp->getType() == nullptr) {
        auto *type = getType(posOp->getExpr(), scope);
        posOp->setType(type);
    }
    return posOp->getType();
}

Type *TypeExpressionInference::getType(ConditionalExpr *cExpr, Scope *scope) {
    if (cExpr->getType() == nullptr) {
        auto *t1 = getType(cExpr->getLeft(), scope);
        auto *t2 = getType(cExpr->getRight(), scope);
        if (t1 == nullptr || t2 == nullptr) return nullptr;
        if (t1->getKind() == Type::TK_Builtin && t2->getKind() == Type::TK_Builtin) {
            auto *bt1 = llvm::cast<BuiltinType>(t1);
            auto *bt2 = llvm::cast<BuiltinType>(t2);
            cExpr->setType(promoteBuiltins(bt1, bt2, Context));
        }
    }
    return cExpr->getType();
}

Type *TypeExpressionInference::getType(FunctionCallExpr *func, Scope *scope) {
    if (func->getType() == nullptr) {
        if (auto *decl = scope->lookupForFunction(func->getIdentifier())) {
            auto *retType = decl->getFunctionType()->getReturnType();
            func->setType(retType);
        }
    }
    return func->getType();
}

Type *TypeExpressionInference::getType(CastExpr *cast, Scope *) {
    return cast->getCastedType();
}
