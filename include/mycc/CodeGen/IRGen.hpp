#pragma once

#include "mycc/AST/AST.hpp"
#include "mycc/IR/SimpleIR.hpp"

namespace mycc {
namespace codegen {

class IRGenerator {
    ir::Context& Ctx;
    ir::Program& IRProg;
    
public:
    IRGenerator(ir::Context& Ctx, ir::Program& IRProg) 
        : Ctx(Ctx), IRProg(IRProg) {}
    
    // Convert AST Program to IR Program
    void generateIR(const Program& ASTProgram);
    
private:
    // Convert AST Function to IR Function
    ir::Function* generateFunction(const Function& ASTFunc);

    bool generateBlockItem(const BlockItem& Item, ir::Function* IRFunc);
    // Convert AST Statement to IR Instructions
    void generateStatement(const Statement& Stmt, ir::Function* IRFunc);
    void generateDeclaration(const Declaration& Decl, ir::Function* IRFunc);
    
    // Convert AST Expression to IR Value
    ir::Value* generateExpression(const Expr& Expr, ir::Function * IRFunc = nullptr);
};

}
}