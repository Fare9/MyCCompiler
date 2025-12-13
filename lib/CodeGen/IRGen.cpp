#include "mycc/CodeGen/IRGen.hpp"
#include "llvm/Support/Casting.h"

using namespace mycc;
using namespace mycc::codegen;

void IRGenerator::generateIR(const Program& ASTProgram)
{
    // Convert each AST function to IR function
    for (const Function* ASTFunc : ASTProgram)
    {
        ir::Function* IRFunc = generateFunction(*ASTFunc);
        IRProg.add_function(IRFunc);
    }
}

ir::Function* IRGenerator::generateFunction(const Function& ASTFunc)
{
    // Create new IR function
    ir::InstList instructions;
    auto* IRFunc = new ir::Function(instructions, ASTFunc.getName());

    // simple fix for now, we generate a Return(0) if no Return exists
    bool containsReturn = false;

    // Generate IR for each statement in the function
    for (const BlockItem& Item : ASTFunc)
    {
        containsReturn |= generateBlockItem(Item, IRFunc);
    }

    if (!containsReturn)
    {
        ir::Int* RetVal = Ctx.createInt(llvm::APSInt::get(0));
        ir::Ret* RetInst = Ctx.createRet(RetVal);
        IRFunc->add_instruction(RetInst);
    }

    return IRFunc;
}

bool IRGenerator::generateBlockItem(const BlockItem& Item, ir::Function* IRFunc)
{
    if (std::holds_alternative<Statement*>(Item))
    {
        Statement* Stmt = std::get<Statement*>(Item);
        generateStatement(*Stmt, IRFunc);
        if (Stmt->getKind() == Statement::SK_Return)
            return true;
    }
    else if (std::holds_alternative<Declaration*>(Item))
    {
        Declaration* Decl = std::get<Declaration*>(Item);
        generateDeclaration(*Decl, IRFunc);
    }
    return false;
}

void IRGenerator::generateStatement(const Statement& Stmt, ir::Function* IRFunc)
{
    switch (Stmt.getKind())
    {
    case Statement::SK_Return:
        {
            generateReturnStmt(Stmt, IRFunc);
            break;
        }
    case Statement::SK_Expression:
        {
            const auto& ExprStmt = dynamic_cast<const ExpressionStatement&>(Stmt);
            // Generate the expression
            generateExpression(*ExprStmt.getExpr(), IRFunc);
            break;
        }
    case Statement::SK_If:
        {
            generateIfStmt(Stmt, IRFunc);
            break;
        }
    case Statement::SK_Label:
        {
            generateLabelStmt(Stmt, IRFunc);
            break;
        }
    case Statement::SK_Goto:
        {
            generateGotoStmt(Stmt, IRFunc);
            break;
        }
    case Statement::SK_Null:
        break;
    case Statement::SK_Compound:
        {
            generateCompoundStmt(Stmt, IRFunc);
            break;
        }
    case Statement::SK_Break:
        {
            const auto & BreakStmt = dynamic_cast<const BreakStatement&>(Stmt);
            auto * label =
                Ctx.getOrCreateLabel(std::string(BreakStmt.get_label()), true);
            IRFunc->add_instruction(Ctx.createJump(label));
            break;
        }
    case Statement::SK_Continue:
        {
            const auto & ContinueStmt = dynamic_cast<const ContinueStatement&>(Stmt);
            auto * label =
                Ctx.getOrCreateLabel(std::string(ContinueStmt.get_label()), true);
            IRFunc->add_instruction(Ctx.createJump(label));
            break;
        }
    case Statement::SK_While:
        {
            generateWhileStmt(Stmt, IRFunc);
            break;
        }
    case Statement::SK_DoWhile:
        {
            generateDoWhileStmt(Stmt, IRFunc);
            break;
        }
    case Statement::SK_For:
        {
            generateForStmt(Stmt, IRFunc);
            break;
        }
    }
}

void IRGenerator::generateDeclaration(const Declaration& Decl, ir::Function* IRFunc)
{
    if (Decl.getExpr() == nullptr) return;
    const auto* left = Decl.getVar();
    const auto* right = Decl.getExpr();

    // We create a Declaration like an assignment in case
    // this one has an expression

    // First we emit the right part of the assignment
    auto* result = generateExpression(*right, IRFunc);
    // Now we create a copy that we include in functions
    auto* varop = generateExpression(*left, IRFunc);
    IRFunc->add_instruction(Ctx.createCopy(result, varop));
}

void IRGenerator::generateReturnStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& RetStmt = dynamic_cast<const ReturnStatement&>(Stmt);

    // Generate IR for the return value expression
    ir::Value* RetVal = generateExpression(*RetStmt.getRetVal(), IRFunc);

    // Create return instruction
    ir::Ret* RetInst = Ctx.createRet(RetVal);
    IRFunc->add_instruction(RetInst);
}

void IRGenerator::generateIfStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& IfStmt = dynamic_cast<const IfStatement&>(Stmt);
    // generate conditional code
    auto result = generateExpression(*IfStmt.getCondition(), IRFunc);
    // generate labels
    auto * else_label = Ctx.getOrCreateLabel("else_label");
    auto * end_label = Ctx.getOrCreateLabel("end_label");
    if (IfStmt.getElseSt() != nullptr)
        IRFunc->add_instruction(Ctx.createJZ(result, else_label));
    else
        IRFunc->add_instruction(Ctx.createJZ(result, end_label));
    // generate the code of `then`
    generateStatement(*IfStmt.getThenSt(), IRFunc);
    if (IfStmt.getElseSt() != nullptr)
    {
        // generate a JUMP to END label from the IF statement
        IRFunc->add_instruction(Ctx.createJump(end_label));
        // Now generate else
        IRFunc->add_instruction(else_label);
        generateStatement(*IfStmt.getElseSt(), IRFunc);
    }
    // Now we generate the END label
    IRFunc->add_instruction(end_label);
}

void IRGenerator::generateLabelStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& Label = dynamic_cast<const LabelStatement&>(Stmt);
    const auto ILabel = Ctx.getOrCreateLabel(Label.getLabel().str(), true);
    IRFunc->add_instruction(ILabel);
}

void IRGenerator::generateGotoStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& Goto = dynamic_cast<const GotoStatement&>(Stmt);
    const auto ILabel = Ctx.getOrCreateLabel(Goto.getLabel().str(), true);
    IRFunc->add_instruction(Ctx.createJump(ILabel));
}

void IRGenerator::generateCompoundStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& Compound = dynamic_cast<const CompoundStatement&>(Stmt);
    // managing compound statement is exactly the same
    // as managing a function block.
    for (const BlockItem& Item : Compound)
    {
        generateBlockItem(Item, IRFunc);
    }
}

void IRGenerator::generateWhileStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& While = dynamic_cast<const WhileStatement&>(Stmt);

    std::string while_start = std::string(While.get_label()) + "_start";
    std::string while_continue = std::string(While.get_label()) + "_continue";
    std::string while_end = std::string(While.get_label()) + "_end";

    auto label_start = Ctx.getOrCreateLabel(while_start, true);
    auto label_continue = Ctx.getOrCreateLabel(while_continue, true);
    auto label_end = Ctx.getOrCreateLabel(while_end, true);

    // Place start label
    IRFunc->add_instruction(label_start);
    IRFunc->add_instruction(label_continue);

    // Evaluate condition
    auto result = generateExpression(*While.getCondition(), IRFunc);

    // Jump to end if condition is false (zero)
    IRFunc->add_instruction(Ctx.createJZ(result, label_end));

    // Generate loop body
    generateStatement(*While.getBody(), IRFunc);

    // Jump back to start
    IRFunc->add_instruction(Ctx.createJump(label_start));

    // Place end label
    IRFunc->add_instruction(label_end);
}

void IRGenerator::generateDoWhileStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& DoWhile = dynamic_cast<const DoWhileStatement&>(Stmt);

    std::string do_while_start = std::string(DoWhile.get_label()) + "_start";
    std::string do_while_continue = std::string(DoWhile.get_label()) + "_continue";
    std::string do_while_end = std::string(DoWhile.get_label()) + "_end";

    auto label_start = Ctx.getOrCreateLabel(do_while_start, true);
    auto label_continue = Ctx.getOrCreateLabel(do_while_continue, true);
    auto label_end = Ctx.getOrCreateLabel(do_while_end, true);

    // Place label start
    IRFunc->add_instruction(label_start);

    // Generate the body of the do/while
    generateStatement(*DoWhile.getBody(), IRFunc);

    // In opposite to While, the label continue must jump to the expression check
    IRFunc->add_instruction(label_continue);

    // Now generate the expression
    auto result = generateExpression(*DoWhile.getCondition(), IRFunc);

    // Jump to end if condition is false (zero)
    IRFunc->add_instruction(Ctx.createJZ(result, label_end));

    // Jump back to start
    IRFunc->add_instruction(Ctx.createJump(label_start));

    // Place end label
    IRFunc->add_instruction(label_end);
}

void IRGenerator::generateForStmt(const Statement& Stmt, ir::Function* IRFunc)
{
    const auto& For = dynamic_cast<const ForStatement&>(Stmt);

    std::string for_label_start = std::string(For.get_label()) + "_start";
    std::string for_label_continue = std::string(For.get_label()) + "_continue";
    std::string for_end = std::string(For.get_label()) + "_end";

    auto label_start = Ctx.getOrCreateLabel(for_label_start, true);
    auto label_continue = Ctx.getOrCreateLabel(for_label_continue, true);
    auto label_end = Ctx.getOrCreateLabel(for_end, true);

    // Now we generate a declaration or an expression
    if (std::holds_alternative<Declaration *>(For.getInit()))
    {
        auto * decl = std::get<Declaration *>(For.getInit());
        generateDeclaration(*decl, IRFunc);
    } else if (std::holds_alternative<Expr *>(For.getInit()))
    {
        auto * expr = std::get<Expr *>(For.getInit());
        generateExpression(*expr, IRFunc);
    }

    // Place start label
    IRFunc->add_instruction(label_start);

    // Evaluate condition
    if (For.getCondition() != nullptr)
    {
        auto result = generateExpression(*For.getCondition(), IRFunc);

        // Jump to end if condition is false (zero)
        IRFunc->add_instruction(Ctx.createJZ(result, label_end));
    }
    // Generate loop body
    generateStatement(*For.getBody(), IRFunc);

    // Different to While and DoWhile, in For we need
    // to set the label_continue, right before the
    // post processing in the update of the variables.
    IRFunc->add_instruction(label_continue);

    // Generate update of the variables
    if (For.getPost() != nullptr)
    {
        generateExpression(*For.getPost(), IRFunc);
    }

    // Jump back to start
    IRFunc->add_instruction(Ctx.createJump(label_start));

    // Place end label
    IRFunc->add_instruction(label_end);
}

ir::Value* IRGenerator::generateExpression(const Expr& Expr, ir::Function* IRFunc)
{
    switch (Expr.getKind())
    {
    case Expr::Ek_Var:
        {
            const auto& var = dynamic_cast<const Var&>(Expr);
            // Create a Var with the information from the one
            // of the AST
            return Ctx.getOrCreateVar(var.getName());
        }
    case Expr::Ek_AssignmentOperator:
        {
            const auto& assignment = dynamic_cast<const AssignmentOperator&>(Expr);
            const auto* left = assignment.getLeft();
            const auto* right = assignment.getRight();
            // First we emit the right part of the assignment
            auto* result = generateExpression(*right, IRFunc);
            // Now we create a copy that we include in functions
            auto* varop = generateExpression(*left, IRFunc);
            IRFunc->add_instruction(Ctx.createCopy(result, varop));
            return varop;
        }
    case Expr::Ek_Int:
        {
            const auto& IntLit = dynamic_cast<const IntegerLiteral&>(Expr);

            // Create integer constant in IR
            return Ctx.createInt(IntLit.getValue());
        }
    case Expr::Ek_UnaryOperator:
        {
            const auto& UnaryOp = dynamic_cast<const UnaryOperator&>(Expr);

            ir::UnaryOp::UnaryOpKind Kind;
            switch (UnaryOp.getOperatorKind())
            {
            case UnaryOperator::UnaryOperatorKind::UopK_Negate:
                Kind = ir::UnaryOp::UnaryOpKind::Neg;
                break;
            case UnaryOperator::UnaryOperatorKind::UopK_Complement:
                Kind = ir::UnaryOp::UnaryOpKind::Complement;
                break;
            case UnaryOperator::UnaryOperatorKind::UopK_Not:
                Kind = ir::UnaryOp::UnaryOpKind::Not;
                break;
            }
            ir::Value* source = generateExpression(*UnaryOp.getExpr(), IRFunc);
            ir::UnaryOp* UnOp = Ctx.createUnaryOp(dynamic_cast<ir::Operand*>(source), Kind);
            if (IRFunc != nullptr)
                IRFunc->add_instruction(UnOp);
            return UnOp->getDestination();
        }
    case Expr::Ek_BinaryOperator:
        {
            const auto& BinaryOp = dynamic_cast<const BinaryOperator&>(Expr);

            ir::BinaryOp::BinaryOpKind Kind = ir::BinaryOp::BinaryOpKind::none;
            ir::ICmpOp::CmpOpKind CmpKind = ir::ICmpOp::CmpOpKind::none;

            switch (BinaryOp.getOperatorKind())
            {
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
            case BinaryOperator::BoK_LowerThan:
                CmpKind = ir::ICmpOp::lt;
                break;
            case BinaryOperator::BoK_LowerEqual:
                CmpKind = ir::ICmpOp::le;
                break;
            case BinaryOperator::BoK_GreaterThan:
                CmpKind = ir::ICmpOp::gt;
                break;
            case BinaryOperator::BoK_GreaterEqual:
                CmpKind = ir::ICmpOp::ge;
                break;
            case BinaryOperator::Bok_Equal:
                CmpKind = ir::ICmpOp::eq;
                break;
            case BinaryOperator::Bok_NotEqual:
                CmpKind = ir::ICmpOp::neq;
                break;
            case BinaryOperator::Bok_And:
            case BinaryOperator::Bok_Or:
            case BinaryOperator::BoK_None:
                break;
            }
            // According to C standard the subexpressions of the same operation
            // are usually unsequenced, they can be evaluated in any order.

            // We will generate left and right before they are used because
            // we use `generateExpression` for them, that will add instructions
            // to the IR Function.

            if (Kind != ir::BinaryOp::BinaryOpKind::none)
            {
                ir::Value* left = generateExpression(*BinaryOp.getLeft(), IRFunc);
                ir::Value* right = generateExpression(*BinaryOp.getRight(), IRFunc);

                ir::BinaryOp* BinOp = Ctx.createBinaryOp(dynamic_cast<ir::Operand*>(left),
                                                         dynamic_cast<ir::Operand*>(right), Kind);
                if (IRFunc != nullptr)
                    IRFunc->add_instruction(BinOp);
                return BinOp->getDestination();
            }
            if (CmpKind != ir::ICmpOp::CmpOpKind::none)
            {
                ir::Value* left = generateExpression(*BinaryOp.getLeft(), IRFunc);
                ir::Value* right = generateExpression(*BinaryOp.getRight(), IRFunc);

                ir::ICmpOp* CmpOp = Ctx.createICmpOp(dynamic_cast<ir::Operand*>(left),
                                                     dynamic_cast<ir::Operand*>(right), CmpKind);
                if (IRFunc != nullptr)
                    IRFunc->add_instruction(CmpOp);
                return CmpOp->getDestination();
            }

            // Short-Circuiting for && instruction
            if (BinaryOp.getOperatorKind() == BinaryOperator::Bok_And)
            {
                auto* false_label = Ctx.getOrCreateLabel("false_label");
                auto* end_label = Ctx.getOrCreateLabel("end_label");
                auto* result = Ctx.createReg();
                auto* temp_left = Ctx.createReg();
                auto* temp_right = Ctx.createReg();
                auto* val_1 = Ctx.createInt(llvm::APSInt(llvm::APInt(32, abs(1))));
                auto* val_0 = Ctx.createInt(llvm::APSInt(32, abs(0)));

                ir::Value* left = generateExpression(*BinaryOp.getLeft(), IRFunc);

                if (IRFunc != nullptr)
                {
                    auto* mov_left = Ctx.createCopy(left, temp_left);
                    IRFunc->add_instruction(mov_left);

                    auto* jump_if_zero_left = Ctx.createJZ(temp_left, false_label);
                    IRFunc->add_instruction(jump_if_zero_left);

                    ir::Value* right = generateExpression(*BinaryOp.getRight(), IRFunc);
                    auto* mov_right = Ctx.createCopy(right, temp_right);
                    IRFunc->add_instruction(mov_right);

                    auto* jump_if_zero_right = Ctx.createJZ(temp_right, false_label);
                    IRFunc->add_instruction(jump_if_zero_right);

                    auto* set_result_to_1 = Ctx.createCopy(val_1, result);
                    IRFunc->add_instruction(set_result_to_1);

                    auto* jump_end = Ctx.createJump(end_label);
                    IRFunc->add_instruction(jump_end);
                    auto* set_result_to_0 = Ctx.createCopy(val_0, result);
                    IRFunc->add_instruction(false_label);

                    IRFunc->add_instruction(set_result_to_0);
                    IRFunc->add_instruction(end_label);
                }

                return result;
            }

            // Short-circuiting for || instruction
            if (BinaryOp.getOperatorKind() == BinaryOperator::Bok_Or)
            {
                auto* true_label = Ctx.getOrCreateLabel("true_label");
                auto* end_label = Ctx.getOrCreateLabel("end_label");
                auto* result = Ctx.createReg();
                auto* temp_left = Ctx.createReg();
                auto* temp_right = Ctx.createReg();
                auto* val_1 = Ctx.createInt(llvm::APSInt(llvm::APInt(32, abs(1))));
                auto* val_0 = Ctx.createInt(llvm::APSInt(32, abs(0)));

                ir::Value* left = generateExpression(*BinaryOp.getLeft(), IRFunc);

                if (IRFunc != nullptr)
                {
                    auto* mov_left = Ctx.createCopy(left, temp_left);
                    IRFunc->add_instruction(mov_left);

                    auto* jump_if_not_zero_left = Ctx.createJNZ(temp_left, true_label);
                    IRFunc->add_instruction(jump_if_not_zero_left);

                    ir::Value* right = generateExpression(*BinaryOp.getRight(), IRFunc);

                    auto* mov_right = Ctx.createCopy(right, temp_right);
                    IRFunc->add_instruction(mov_right);

                    auto* jump_if_not_zero_right = Ctx.createJNZ(temp_right, true_label);
                    IRFunc->add_instruction(jump_if_not_zero_right);

                    auto* set_result_to_0 = Ctx.createCopy(val_0, result);
                    IRFunc->add_instruction(set_result_to_0);

                    auto* jump_end = Ctx.createJump(end_label);
                    IRFunc->add_instruction(jump_end);

                    auto* set_result_to_1 = Ctx.createCopy(val_1, result);
                    IRFunc->add_instruction(true_label);

                    IRFunc->add_instruction(set_result_to_1);
                    IRFunc->add_instruction(end_label);
                }

                return result;
            }

            break;
        }
    case Expr::Ek_PrefixOperator:
        {
            const auto& PrefixOp = dynamic_cast<const PrefixOperator&>(Expr);

            // Get the variable being operated on
            ir::Value* var = generateExpression(*PrefixOp.getExpr(), IRFunc);
            auto* one = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 1)));

            ir::BinaryOp* increment_instruction = nullptr;

            // For prefix operators, we first update the variable, then return its new value
            switch (PrefixOp.getOperatorKind())
            {
            case PrefixOperator::POK_PreIncrement:
                increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand*>(var),
                                                           one, ir::BinaryOp::Add);
                break;
            case PrefixOperator::POK_PreDecrement:
                increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand*>(var),
                                                           one, ir::BinaryOp::Sub);
                break;
            }

            if (IRFunc != nullptr && increment_instruction != nullptr)
            {
                IRFunc->add_instruction(increment_instruction);
                // Copy the result back to the variable
                IRFunc->add_instruction(Ctx.createCopy(increment_instruction->getDestination(), var));
            }

            return var; // Return the updated variable
        }
    case Expr::Ek_PostfixOperator:
        {
            const auto& PostfixOp = dynamic_cast<const PostfixOperator&>(Expr);

            // Get the variable being operated on
            ir::Value* var = generateExpression(*PostfixOp.getExpr(), IRFunc);
            auto* one = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 1)));

            // For postfix operators, we save the old value, update the variable, then return the old value
            auto* old_value = Ctx.createReg();
            ir::BinaryOp* increment_instruction = nullptr;

            if (IRFunc != nullptr)
            {
                // Save the current value
                IRFunc->add_instruction(Ctx.createCopy(var, old_value));
            }

            switch (PostfixOp.getOperatorKind())
            {
            case PostfixOperator::POK_PostIncrement:
                increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand*>(var),
                                                           one, ir::BinaryOp::Add);
                break;
            case PostfixOperator::POK_PostDecrement:
                increment_instruction = Ctx.createBinaryOp(dynamic_cast<ir::Operand*>(var),
                                                           one, ir::BinaryOp::Sub);
                break;
            }

            if (IRFunc != nullptr && increment_instruction != nullptr)
            {
                IRFunc->add_instruction(increment_instruction);
                // Copy the result back to the variable
                IRFunc->add_instruction(Ctx.createCopy(increment_instruction->getDestination(), var));
            }

            return old_value; // Return the old value (before increment/decrement)
        }
    case Expr::Ek_ConditionalOperator:
        {
            const auto& cond_expr = dynamic_cast<const ConditionalExpr&>(Expr);
            // Results that will be returned
            ir::Value* Result, *temp1, *temp2;
            // labels
            auto * e2_label = Ctx.getOrCreateLabel("e2_label");
            auto * end_label = Ctx.getOrCreateLabel("end_label");
            // Result must be a register
            Result = Ctx.createReg();

            // first generate the conditional statement
            auto * cond_result = generateExpression(*cond_expr.getCondition(), IRFunc);
            // If met, jump to the e2
            IRFunc->add_instruction(Ctx.createJZ(cond_result, e2_label));
            // now we generate the code for the first statement, and obtain
            // the result
            temp1 = generateExpression(*cond_expr.getLeft(), IRFunc);
            IRFunc->add_instruction(Ctx.createMov(temp1, Result));
            IRFunc->add_instruction(Ctx.createJump(end_label));
            // now we generate the second statement
            IRFunc->add_instruction(e2_label);
            temp2 = generateExpression(*cond_expr.getRight(), IRFunc);
            IRFunc->add_instruction(Ctx.createMov(temp2, Result));
            // finally the end label
            IRFunc->add_instruction(end_label);
            return Result;
        }
    }

    return nullptr; // Should not reach here
}
