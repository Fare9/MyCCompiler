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
    for (auto item : *function) {
        if (std::holds_alternative<Statement*>(item))
            output += print(std::get<Statement*>(item), indent + 1);
        else if (std::holds_alternative<Declaration*>(item))
            output += print(std::get<Declaration*>(item), indent + 1);
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
        case Statement::SK_Expression:
            return printExpressionStatement(dynamic_cast<const ExpressionStatement*>(statement), indent);
        case Statement::SK_Null:
            return printNullStatement(dynamic_cast<const NullStatement*>(statement), indent);
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
        case Expr::Ek_Var:
            return printVar(dynamic_cast<const Var*>(expr), indent);
        case Expr::Ek_UnaryOperator:
            return printUnaryOperator(dynamic_cast<const UnaryOperator*>(expr), indent);
        case Expr::Ek_BinaryOperator:
            return printBinaryOperator(dynamic_cast<const BinaryOperator*>(expr), indent);
        case Expr::Ek_AssignmentOperator:
            return printAssignmentOperator(dynamic_cast<const AssignmentOperator*>(expr), indent);
        case Expr::Ek_PrefixOperator:
            return printPrefixOperator(dynamic_cast<const PrefixOperator*>(expr), indent);
        case Expr::Ek_PostfixOperator:
            return printPostfixOperator(dynamic_cast<const PostfixOperator*>(expr), indent);
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
    switch (expr->getKind()) {
        case UnaryOperator::UopK_Negate:
            kind_unary_operator = "Negate";
            break;
        case UnaryOperator::UopK_Complement:
            kind_unary_operator = "Complement";
            break;
        case UnaryOperator::UopK_Not:
            kind_unary_operator = "Not";
            break;
    }

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
        case BinaryOperator::BinaryOpKind::BoK_LowerThan:
            kind_binary_operator = "LowerThan";
            break;
        case BinaryOperator::BinaryOpKind::BoK_LowerEqual:
            kind_binary_operator = "LowerEqual";
            break;
        case BinaryOperator::BinaryOpKind::BoK_GreaterThan:
            kind_binary_operator = "GreaterThan";
            break;
        case BinaryOperator::BinaryOpKind::BoK_GreaterEqual:
            kind_binary_operator = "GreaterEqual";
            break;
        case BinaryOperator::BinaryOpKind::Bok_Equal:
            kind_binary_operator = "Equal";
            break;
        case BinaryOperator::BinaryOpKind::Bok_NotEqual:
            kind_binary_operator = "NotEqual";
            break;
        case BinaryOperator::BinaryOpKind::Bok_And:
            kind_binary_operator = "And";
            break;
        case BinaryOperator::BinaryOpKind::Bok_Or:
            kind_binary_operator = "Or";
            break;
        case BinaryOperator::BinaryOpKind::Bok_Assign:
            kind_binary_operator = "Assign";
            break;
        case BinaryOperator::BinaryOpKind::BoK_None:
            kind_binary_operator = "None";
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

std::string ASTPrinter::print(const Declaration* decl) {
    return print(decl, 0);
}

std::string ASTPrinter::print(const Declaration* decl, int indent) {
    if (!decl) return getIndent(indent) + "null\n";
    return printDeclaration(decl, indent);
}

std::string ASTPrinter::printExpressionStatement(const ExpressionStatement* stmt, int indent) {
    std::string output = getIndent(indent) + "ExpressionStatement\n";
    if (stmt->getExpr()) {
        output += print(stmt->getExpr(), indent + 1);
    }
    return output;
}

std::string ASTPrinter::printNullStatement(const NullStatement* stmt, int indent) {
    return getIndent(indent) + "NullStatement\n";
}

std::string ASTPrinter::printVar(const Var* expr, int indent) {
    return getIndent(indent) + "Var: " + expr->getName().str() + "\n";
}

std::string ASTPrinter::printAssignmentOperator(const AssignmentOperator* expr, int indent) {
    std::string output = getIndent(indent) + "AssignmentOperator\n";
    output += getIndent(indent + 1) + "Left:\n";
    output += print(expr->getLeft(), indent + 2);
    output += getIndent(indent + 1) + "Right:\n";
    output += print(expr->getRight(), indent + 2);
    return output;
}

std::string ASTPrinter::printPrefixOperator(const PrefixOperator* expr, int indent) {
    std::string kind_prefix_operator;
    switch (expr->getOperatorKind()) {
        case PrefixOperator::POK_PreIncrement:
            kind_prefix_operator = "PreIncrement";
            break;
        case PrefixOperator::POK_PreDecrement:
            kind_prefix_operator = "PreDecrement";
            break;
    }

    std::string output = getIndent(indent) + "PrefixOperator: " + kind_prefix_operator + "\n";
    output += print(expr->getExpr(), indent + 1);
    return output;
}

std::string ASTPrinter::printPostfixOperator(const PostfixOperator* expr, int indent) {
    std::string kind_postfix_operator;
    switch (expr->getOperatorKind()) {
        case PostfixOperator::POK_PostIncrement:
            kind_postfix_operator = "PostIncrement";
            break;
        case PostfixOperator::POK_PostDecrement:
            kind_postfix_operator = "PostDecrement";
            break;
    }

    std::string output = getIndent(indent) + "PostfixOperator: " + kind_postfix_operator + "\n";
    output += print(expr->getExpr(), indent + 1);
    return output;
}

std::string ASTPrinter::printDeclaration(const Declaration* decl, int indent) {
    std::string output = getIndent(indent) + "Declaration\n";
    output += getIndent(indent + 1) + "Name:\n";
    output += printVar(decl->getVar(), indent + 2);
    if (decl->getExpr()) {
        output += getIndent(indent + 1) + "Initializer:\n";
        output += print(decl->getExpr(), indent + 2);
    }
    return output;
}

std::string ASTPrinter::getIndent(int level) {
    return std::string(level * 2, ' ');
}