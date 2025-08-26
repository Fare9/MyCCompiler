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
    ir::Function* IRFunc = new ir::Function(instructions, ASTFunc.getName());
    
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
    }
    
    return nullptr; // Should not reach here
}