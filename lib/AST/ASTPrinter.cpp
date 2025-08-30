#include "mycc/AST/ASTPrinter.hpp"
#include "mycc/AST/AST.hpp"
#include <string>

using namespace mycc;

std::string ASTPrinter::print(const Program* program) {
    return print(program, 0);
}

std::string ASTPrinter::print(const Program* program, int indent) {
    if (!program) return "null";
    
    std::string output = getIndent(indent) + "Program\n";
    for (auto* func : *program) {
        output += print(func, indent + 1);
    }
    return output;
}

std::string ASTPrinter::print(const Function* function) {
    return print(function, 0);
}

std::string ASTPrinter::print(const Function* function, int indent) {
    if (!function) return getIndent(indent) + "null\n";
    
    std::string output = getIndent(indent) + "Function: " + function->getName().str() + "\n";
    for (auto* stmt : *function) {
        output += print(stmt, indent + 1);
    }
    return output;
}

std::string ASTPrinter::print(const Statement* statement) {
    return print(statement, 0);
}

std::string ASTPrinter::print(const Statement* statement, int indent) {
    if (!statement) return getIndent(indent) + "null\n";
    
    switch (statement->getKind()) {
        case Statement::SK_Return:
            return printReturnStatement(dynamic_cast<const ReturnStatement*>(statement), indent);
    }
    return getIndent(indent) + "Unknown Statement\n";
}

std::string ASTPrinter::print(const Expr* expr) {
    return print(expr, 0);
}

std::string ASTPrinter::print(const Expr* expr, int indent) {
    if (!expr) return getIndent(indent) + "null\n";
    
    switch (expr->getKind()) {
        case Expr::Ek_Int:
            return printIntegerLiteral(dynamic_cast<const IntegerLiteral*>(expr), indent);
        case Expr::Ek_UnaryOperator:
            return printUnaryOperator(dynamic_cast<const UnaryOperator*>(expr), indent);
        case Expr::Ek_BinaryOperator:
            return printBinaryOperator(dynamic_cast<const BinaryOperator*>(expr), indent);
    }
    return getIndent(indent) + "Unknown Expression\n";
}

std::string ASTPrinter::printReturnStatement(const ReturnStatement* stmt, int indent) {
    std::string output = getIndent(indent) + "ReturnStatement\n";
    if (stmt->getRetVal()) {
        output += print(stmt->getRetVal(), indent + 1);
    }
    return output;
}

std::string ASTPrinter::printIntegerLiteral(const IntegerLiteral* expr, int indent) {
    return getIndent(indent) + "IntegerLiteral: " + std::to_string(expr->getValue().getSExtValue()) + "\n";
}

std::string ASTPrinter::printUnaryOperator(const UnaryOperator* expr, int indent) {
    std::string kind_unary_operator;
    if (expr->getOperatorKind() == UnaryOperator::UnaryOperatorKind::UopK_Negate)
        kind_unary_operator = "Negate";
    else if (expr->getOperatorKind() == UnaryOperator::UnaryOperatorKind::UopK_Complement)
        kind_unary_operator = "Complement";
    
    std::string output = getIndent(indent) + "UnaryOperator: " + kind_unary_operator + "\n";
    output += print(expr->getExpr(), indent + 1);
    return output;
}

std::string ASTPrinter::printBinaryOperator(const BinaryOperator* expr, int indent) {
    std::string kind_binary_operator;
    switch (expr->getOperatorKind()) {
        case BinaryOperator::BinaryOpKind::BoK_Add:
            kind_binary_operator = "Add";
            break;
        case BinaryOperator::BinaryOpKind::BoK_Subtract:
            kind_binary_operator = "Sub";
            break;
        case BinaryOperator::BinaryOpKind::BoK_Divide:
            kind_binary_operator = "Div";
            break;
        case BinaryOperator::BinaryOpKind::BoK_Multiply:
            kind_binary_operator = "Mul";
            break;
        case BinaryOperator::BinaryOpKind::BoK_Remainder:
            kind_binary_operator = "Rem";
            break;
        case BinaryOperator::BinaryOpKind::BoK_LeftShift:
            kind_binary_operator = "LeftShift";
            break;
        case BinaryOperator::BinaryOpKind::BoK_RightShift:
            kind_binary_operator = "RightShift";
            break;
        case BinaryOperator::BinaryOpKind::BoK_BitwiseAnd:
            kind_binary_operator = "BitwiseAnd";
            break;
        case BinaryOperator::BinaryOpKind::BoK_BitwiseXor:
            kind_binary_operator = "BitwiseXor";
            break;
        case BinaryOperator::BinaryOpKind::BoK_BitwiseOr:
            kind_binary_operator = "BitwiseOr";
            break;
        default:
            kind_binary_operator = "Unknown";
    }

    std::string output = getIndent(indent) + "BinaryOperator: " + kind_binary_operator + "\n";
    output += getIndent(indent + 1) + "Left:\n";
    output += print(expr->getLeft(), indent + 2);
    output += getIndent(indent + 1) + "Right:\n";
    output += print(expr->getRight(), indent + 2);
    return output;
}

std::string ASTPrinter::getIndent(int level) {
    return std::string(level * 2, ' ');
}