#pragma once

#include "mycc/AST/AST.hpp"
#include "mycc/Sema/Scope.hpp"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <memory>
#include <string>

namespace mycc::codegen::llvmbackend {

/// @brief Code generator that lowers the AST directly to LLVM IR,
/// bypassing the SimpleIR intermediate representation.
class LLVMIRGenerator {
    /// @brief LLVM context owning types and constants.
    llvm::LLVMContext &Ctx;
    /// @brief The LLVM module being generated (holds functions, globals).
    std::unique_ptr<llvm::Module> Module;
    /// @brief IR builder for emitting instructions into basic blocks.
    llvm::IRBuilder<> Builder;
    /// @brief Symbol table from semantic analysis for linkage/storage info.
    const Scope *Symbols = nullptr;

    /// @brief Map from variable names to their LLVM alloca/global addresses.
    llvm::StringMap<llvm::Value *> NamedValues;

public:

     /// @brief Constructor of the LLVM IR Generator, we need a context and the name
     /// of the module.
     /// @param Ctx Context from LLVM.
     /// @param ModuleName Name of the module to generate.
    explicit LLVMIRGenerator(llvm::LLVMContext &Ctx, llvm::StringRef ModuleName);

    /// @brief Generate LLVM IR for the entire translation unit.
    /// @param ASTProgram AST from the Module to transform.
    /// @param symbols Symbol table from the Module.
    void generateIR(const Program &ASTProgram, const Scope &symbols);

    /// @brief Get the generated LLVM module (transfers ownership).
    std::unique_ptr<llvm::Module> takeModule() { return std::move(Module); }

    /// @brief Print the generated LLVM IR to a raw_ostream.
    void print(llvm::raw_ostream &OS) const;

private:
    // Top-level generation

    /// @brief Function to generate all the top symbols from the module.
    /// these are retrieved from the symbol table.
    /// @param symbols reference to the global symbol table.
    void generateGlobals(const Scope &symbols);
    void generateFunction(const FunctionDeclaration &Func, const SymbolEntry& Entry);
    void generateGlobalVar(const VarDeclaration &Var, const SymbolEntry& Entry) const;

    // Statements
    void generateBlockItem(const BlockItem &Item);
    void generateStatement(const Statement &Stmt);
    void generateReturnStmt(const ReturnStatement &Stmt);
    void generateIfStmt(const IfStatement &Stmt);
    void generateCompoundStmt(const CompoundStatement &Stmt);
    void generateWhileStmt(const WhileStatement &Stmt);
    void generateDoWhileStmt(const DoWhileStatement &Stmt);
    void generateForStmt(const ForStatement &Stmt);
    void generateSwitchStmt(const SwitchStatement &Stmt);
    void generateDeclaration(const VarDeclaration &Decl);

    // Expressions
    llvm::Value *generateExpression(const Expr &E);
    llvm::Value *generateIntLiteral(const IntegerLiteral &Lit);
    llvm::Value *generateVarExpr(const Var &V);
    llvm::Value *generateUnaryExpr(const UnaryOperator &Op);
    llvm::Value *generateBinaryExpr(const BinaryOperator &Op);
    llvm::Value *generateAssignmentExpr(const AssignmentOperator &Op);
    llvm::Value *generatePrefixExpr(const PrefixOperator &Op);
    llvm::Value *generatePostfixExpr(const PostfixOperator &Op);
    llvm::Value *generateConditionalExpr(const ConditionalExpr &Op);
    llvm::Value *generateFunctionCallExpr(const FunctionCallExpr &Call);

    // Helpers
    llvm::Type *getLLVMType(const Type *T) const;

    static llvm::GlobalValue::LinkageTypes getLinkage(const SymbolEntry &Entry);

    static llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *F,
                                                    llvm::StringRef Name,
                                                    llvm::Type *Ty);
};

} // namespace mycc::codegen::llvmbackend
