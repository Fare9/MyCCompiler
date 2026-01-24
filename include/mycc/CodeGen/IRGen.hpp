#pragma once

#include "mycc/AST/AST.hpp"
#include "mycc/IR/SimpleIR.hpp"

namespace mycc::codegen {

class IRGenerator {
    ir::Context& Ctx;
    ir::Program& IRProg;

    // Variable renaming for SSA-style naming
    unsigned int VariableCounter = 0;
    StringMap<std::vector<std::string>> VariableRenameStack;

    struct CaseInfo {
        Expr * value;
        std::string label;
    };

    void collectSwitchCases(
        const Statement *stmt,
        std::vector<CaseInfo>& cases,
        std::string& defaultLabel,
        bool& hasDefault);

    // Variable renaming helper methods
    std::string generateUniqueVarName(StringRef originalName);
    std::string getIRName(StringRef originalName);
    void enterScope();
    void exitScope(const std::vector<std::string> &declaredVars);

public:
    IRGenerator(ir::Context& Ctx, ir::Program& IRProg)
        : Ctx(Ctx), IRProg(IRProg) {}
    
    // Convert AST Program to IR Program
    void generateIR(const Program& ASTProgram);
    
private:
    // Convert AST Function to IR Function
    ir::Function* generateFunction(const FunctionDeclaration& ASTFunc);

    bool generateBlockItem(const BlockItem& Item, ir::Function* IRFunc);
    // Convert AST Statement to IR Instructions
    void generateStatement(const Statement& Stmt, ir::Function* IRFunc);
    void generateDeclaration(const VarDeclaration& Decl, ir::Function* IRFunc);

    // Convert Statements to IR values
    void generateReturnStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateIfStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateLabelStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateGotoStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateCompoundStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateWhileStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateDoWhileStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateForStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateSwitchStmt(const Statement& Stmt, ir::Function* IRFunc);
    void generateCaseStmt(const Statement& Stmt, ir::Function* IRFunc) const;
    void generateDefaultStmt(const Statement& Stmt, ir::Function* IRFunc) const;

    // Convert AST Expression to IR Value
    ir::Value* generateExpression(const Expr& Expr, ir::Function * IRFunc = nullptr);

    // Expression-specific generation methods
    ir::Value* generateVarExpression(const Var& VarExpr, ir::Function* IRFunc);
    ir::Value* generateAssignmentExpression(const AssignmentOperator& Assignment, ir::Function* IRFunc);
    ir::Value* generateIntExpression(const IntegerLiteral& IntLit, ir::Function* IRFunc) const;
    ir::Value* generateUnaryExpression(const UnaryOperator& UnaryOp, ir::Function* IRFunc);
    ir::Value* generateBinaryExpression(const BinaryOperator& BinaryOp, ir::Function* IRFunc);
    ir::Value* generatePrefixExpression(const PrefixOperator& PrefixOp, ir::Function* IRFunc);
    ir::Value* generatePostfixExpression(const PostfixOperator& PostfixOp, ir::Function* IRFunc);
    ir::Value* generateConditionalExpression(const ConditionalExpr& CondExpr, ir::Function* IRFunc);
    ir::Value* generateFunctionCallExpression(const FunctionCallExpr& FuncCallExpr, ir::Function* IRFunc);
};

}
