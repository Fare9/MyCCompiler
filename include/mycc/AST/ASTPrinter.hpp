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
    static std::string printReturnStatement(const ReturnStatement* stmt);
    static std::string printIntegerLiteral(const IntegerLiteral* expr);
};

}