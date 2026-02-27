#pragma once
#include "mycc/AST/AST.hpp"

namespace mycc
{
    class ASTUtils
    {
    public:
        static bool isConstantExpression(Expr* expr)
        {
            return expr->getKind() == Expr::Ek_Int;
        }

        static int64_t evaluateConstantExpression(Expr* expr)
        {
            if (auto* intLit = llvm::dyn_cast<IntegerLiteral>(expr))
            {
                return intLit->getValue().getSExtValue();
            }
            // this should never be reached since we only allow constant integers
            return 0;
        }
    };
}
