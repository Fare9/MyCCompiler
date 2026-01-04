
#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/Support/SourceMgr.h"
#include <memory>
#include <vector>
#include <deque>

namespace mycc {

// Forward declarations
class Statement;
class Expr;
class VarDeclaration;
class Function;
class Program;

class ASTContext {
    llvm::SourceMgr &SrcMgr;
    StringRef FileName;
    
    // Storage for all AST nodes - the context owns all nodes
    // Using deque to ensure pointer stability (no reallocation)
    std::deque<std::unique_ptr<Statement>> Statements;
    std::deque<std::unique_ptr<Expr>> Expressions;
    std::deque<std::unique_ptr<VarDeclaration>> Declarations;
    std::deque<std::unique_ptr<Function>> Functions;
    std::deque<std::unique_ptr<Program>> Programs;
    
public:
    ASTContext(llvm::SourceMgr &SrcMgr, StringRef FileName)
        : SrcMgr(SrcMgr), FileName(FileName) {
    }

    // Non-copyable, non-movable to ensure stable addresses
    ASTContext(const ASTContext&) = delete;
    ASTContext& operator=(const ASTContext&) = delete;
    ASTContext(ASTContext&&) = delete;
    ASTContext& operator=(ASTContext&&) = delete;

    StringRef getFileName() const {
        return FileName;
    }

    llvm::SourceMgr & getSourceMgr() {
        return SrcMgr;
    }

    const llvm::SourceMgr & getSourceMgr() const {
        return SrcMgr;
    }
    
    // Factory methods to create AST nodes
    template<typename T, typename... Args>
    T* createStatement(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        Statements.emplace_back(std::move(node));
        return ptr;
    }
    
    template<typename T, typename... Args>
    T* createExpression(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        Expressions.emplace_back(std::move(node));
        return ptr;
    }
    
    template<typename T, typename... Args>
    T* createDeclaration(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        Declarations.emplace_back(std::move(node));
        return ptr;
    }
    
    template<typename T, typename... Args>
    T* createFunction(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        Functions.emplace_back(std::move(node));
        return ptr;
    }
    
    template<typename T, typename... Args>
    T* createProgram(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        Programs.emplace_back(std::move(node));
        return ptr;
    }
};

}