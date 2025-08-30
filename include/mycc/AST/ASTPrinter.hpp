#pragma once

#include "mycc/AST/AST.hpp"
#include <string>

namespace mycc {

class ASTPrinter {
public:
    static std::string print(const Program* program);
    static std::string print(const Function* function);
    static std::string print(const Statement* statement);
    static std::string print(const Expr* expr);

private:
    static std::string print(const Program* program, int indent);
    static std::string print(const Function* function, int indent);
    static std::string print(const Statement* statement, int indent);
    static std::string print(const Expr* expr, int indent);
    
    static std::string printReturnStatement(const ReturnStatement* stmt, int indent);
    static std::string printIntegerLiteral(const IntegerLiteral* expr, int indent);
    static std::string printUnaryOperator(const UnaryOperator* expr, int indent);
    static std::string printBinaryOperator(const BinaryOperator* expr, int indent);
    
    static std::string getIndent(int level);
};

}