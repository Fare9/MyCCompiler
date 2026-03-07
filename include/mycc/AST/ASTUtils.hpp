#pragma once
#include "mycc/AST/AST.hpp"

namespace mycc
{
    class ASTUtils
    {
    public:
        static bool isConstantExpression(Expr* expr)
        {
            switch (expr->getKind()) {
                case Expr::Ek_Int:
                case Expr::Ek_Long:
                case Expr::Ek_IntInit:
                case Expr::Ek_LongInit:
                    return true;
                default:
                    return false;
            }
        }

        static int64_t evaluateConstantExpression(Expr* expr)
        {
            if (auto* intLit = llvm::dyn_cast<IntegerLiteral>(expr))
                return intLit->getValue().getSExtValue();
            if (auto* longLit = llvm::dyn_cast<LongLiteral>(expr))
                return longLit->getValue().getSExtValue();
            if (auto* intInit = llvm::dyn_cast<IntInit>(expr))
                return intInit->getValue();
            if (auto* longInit = llvm::dyn_cast<LongInit>(expr))
                return longInit->getValue();
            return 0;
        }
    };
}
