#include "mycc/AST/ASTPrinter.hpp"
#include "mycc/AST/AST.hpp"
#include <string>

using namespace mycc;

std::string ASTPrinter::print(const Program* program) {
    if (!program) return "null";
    
    std::string output = "Program(";
    for (auto* func : *program) {
        output += print(func);
    }
    output += ")";
    return output;
}

std::string ASTPrinter::print(const Function* function) {
    if (!function) return "null";
    
    std::string output = "Function(" + function->getName().str() + ", {";
    for (auto* stmt : *function) {
        output += print(stmt);
    }
    output += "})";
    return output;
}

std::string ASTPrinter::print(const Statement* statement) {
    if (!statement) return "null";
    
    switch (statement->getKind()) {
        case Statement::SK_Return:
            return printReturnStatement(static_cast<const ReturnStatement*>(statement));
    }
    return "Unknown Statement";
}

std::string ASTPrinter::print(const Expr* expr) {
    if (!expr) return "null";
    
    switch (expr->getKind()) {
        case Expr::Ek_Int:
            return printIntegerLiteral(static_cast<const IntegerLiteral*>(expr));
    }
    return "Unknown Expression";
}

std::string ASTPrinter::printReturnStatement(const ReturnStatement* stmt) {
    std::string output = "ReturnStatement(";
    if (stmt->getRetVal()) {
        output += print(stmt->getRetVal());
    }
    output += ")";
    return output;
}

std::string ASTPrinter::printIntegerLiteral(const IntegerLiteral* expr) {
    return "IntegerLiteral(" + std::to_string(expr->getValue().getSExtValue()) + ")";
}