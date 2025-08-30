#include "mycc/CodeGen/IRGen.hpp"
#include "llvm/Support/Casting.h"

using namespace mycc;
using namespace mycc::codegen;

void IRGenerator::generateIR(const Program& ASTProgram) {
    // Convert each AST function to IR function
    for (const Function* ASTFunc : ASTProgram) {
        ir::Function* IRFunc = generateFunction(*ASTFunc);
        IRProg.add_function(IRFunc);
    }
}

ir::Function* IRGenerator::generateFunction(const Function& ASTFunc) {
    // Create new IR function
    ir::InstList instructions;
    auto* IRFunc = new ir::Function(instructions, ASTFunc.getName());
    
    // Generate IR for each statement in the function
    for (const Statement* Stmt : ASTFunc) {
        generateStatement(*Stmt, IRFunc);
    }
    
    return IRFunc;
}

void IRGenerator::generateStatement(const Statement& Stmt, ir::Function* IRFunc) {
    switch (Stmt.getKind()) {
        case Statement::SK_Return: {
            const auto& RetStmt = dynamic_cast<const ReturnStatement&>(Stmt);
            
            // Generate IR for the return value expression
            ir::Value* RetVal = generateExpression(*RetStmt.getRetVal(), IRFunc);
            
            // Create return instruction
            ir::Ret* RetInst = Ctx.createRet(RetVal);
            IRFunc->add_instruction(RetInst);
            break;
        }
        // Add more statement types as needed
    }
}

ir::Value* IRGenerator::generateExpression(const Expr& Expr, ir::Function * IRFunc) {
    switch (Expr.getKind()) {
        case Expr::Ek_Int: {
            const auto& IntLit = dynamic_cast<const IntegerLiteral&>(Expr);
            
            // Create integer constant in IR
            return Ctx.createInt(IntLit.getValue());
        }
        case Expr::Ek_UnaryOperator: {
            const auto& UnaryOp = dynamic_cast<const UnaryOperator&>(Expr);

            ir::UnaryOp::UnaryOpKind Kind;
            switch (UnaryOp.getOperatorKind()) {
                case UnaryOperator::UnaryOperatorKind::UopK_Negate:
                    Kind = ir::UnaryOp::UnaryOpKind::Neg;
                    break;
                case UnaryOperator::UnaryOperatorKind::UopK_Complement:
                    Kind = ir::UnaryOp::UnaryOpKind::Complement;
                    break;
            }
            ir::Value * source = generateExpression(*UnaryOp.getExpr(), IRFunc);
            ir::UnaryOp * UnOp = Ctx.createUnaryOp(dynamic_cast<ir::Operand*>(source), Kind);
            if (IRFunc != nullptr)
                IRFunc->add_instruction(UnOp);
            return UnOp->getDestination();
        }
        case Expr::Ek_BinaryOperator: {
            const auto& BinaryOp = dynamic_cast<const BinaryOperator&>(Expr);

            ir::BinaryOp::BinaryOpKind Kind;
            switch (BinaryOp.getOperatorKind()) {
                case BinaryOperator::BoK_Add:
                    Kind = ir::BinaryOp::BinaryOpKind::Add;
                    break;
                case BinaryOperator::BoK_Subtract:
                    Kind = ir::BinaryOp::BinaryOpKind::Sub;
                    break;
                case BinaryOperator::BoK_Multiply:
                    Kind = ir::BinaryOp::BinaryOpKind::Mul;
                    break;
                case BinaryOperator::BoK_Divide:
                    Kind = ir::BinaryOp::BinaryOpKind::Div;
                    break;
                case BinaryOperator::BoK_Remainder:
                    Kind = ir::BinaryOp::BinaryOpKind::Rem;
                    break;
                case BinaryOperator::BoK_BitwiseAnd:
                    Kind = ir::BinaryOp::BinaryOpKind::And;
                    break;
                case BinaryOperator::BoK_BitwiseOr:
                    Kind = ir::BinaryOp::BinaryOpKind::Or;
                    break;
                case BinaryOperator::BoK_BitwiseXor:
                    Kind = ir::BinaryOp::BinaryOpKind::Xor;
                    break;
                case BinaryOperator::BoK_LeftShift:
                    Kind = ir::BinaryOp::BinaryOpKind::Sal;
                    break;
                case BinaryOperator::BoK_RightShift:
                    Kind = ir::BinaryOp::BinaryOpKind::Sar;
                    break;
            }
            // According to C standard the subexpressions of the same operation
            // are usually unsequenced, they can be evaluated in any order.
            ir::Value * left = generateExpression(*BinaryOp.getLeft(), IRFunc);
            ir::Value * right = generateExpression(*BinaryOp.getRight(), IRFunc);
            ir::BinaryOp *BinOp = Ctx.createBinaryOp(dynamic_cast<ir::Operand*>(left), dynamic_cast<ir::Operand*>(right), Kind);
            if (IRFunc != nullptr)
                IRFunc->add_instruction(BinOp);
            return BinOp->getDestination();
        }
    }
    
    return nullptr; // Should not reach here
}