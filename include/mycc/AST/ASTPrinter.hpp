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
    static std::string printIfStatement(const IfStatement* stmt, int indent);
    static std::string printCompoundStatement(const CompoundStatement* stmt, int indent);
    static std::string printLabelStatement(const LabelStatement* stmt, int indent);
    static std::string printGotoStatement(const GotoStatement* stmt, int indent);
    static std::string printBreakStatement(const BreakStatement* stmt, int indent);
    static std::string printContinueStatement(const ContinueStatement* stmt, int indent);
    static std::string printWhileStatement(const WhileStatement* stmt, int indent);
    static std::string printDoWhileStatement(const DoWhileStatement* stmt, int indent);
    static std::string printForStatement(const ForStatement* stmt, int indent);
    static std::string printIntegerLiteral(const IntegerLiteral* expr, int indent);
    static std::string printVar(const Var* expr, int indent);
    static std::string printUnaryOperator(const UnaryOperator* expr, int indent);
    static std::string printBinaryOperator(const BinaryOperator* expr, int indent);
    static std::string printAssignmentOperator(const AssignmentOperator* expr, int indent);
    static std::string printPrefixOperator(const PrefixOperator* expr, int indent);
    static std::string printPostfixOperator(const PostfixOperator* expr, int indent);
    static std::string printConditionalExpr(const ConditionalExpr* expr, int indent);
    static std::string printDeclaration(const Declaration* decl, int indent);
    
    static std::string getIndent(int level);
};

}