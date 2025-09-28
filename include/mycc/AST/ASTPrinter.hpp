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
    
    static std::string print(const Declaration* decl);
    static std::string print(const Declaration* decl, int indent);

    static std::string printReturnStatement(const ReturnStatement* stmt, int indent);
    static std::string printExpressionStatement(const ExpressionStatement* stmt, int indent);
    static std::string printNullStatement(const NullStatement* stmt, int indent);
    static std::string printIntegerLiteral(const IntegerLiteral* expr, int indent);
    static std::string printVar(const Var* expr, int indent);
    static std::string printUnaryOperator(const UnaryOperator* expr, int indent);
    static std::string printBinaryOperator(const BinaryOperator* expr, int indent);
    static std::string printAssignmentOperator(const AssignmentOperator* expr, int indent);
    static std::string printDeclaration(const Declaration* decl, int indent);
    
    static std::string getIndent(int level);
};

}