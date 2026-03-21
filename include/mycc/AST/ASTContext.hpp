
#pragma once

#include "mycc/Basic/LLVM.hpp"
#include <memory>
#include <deque>

namespace mycc {

// Forward declarations
class Statement;
class Expr;
class VarDeclaration;
class FunctionDeclaration;
class Program;

/**
 * @brief ASTContext class keeps the memory from all the possibles nodes
 * that the AST can have. In this way, any node we have will be released once
 * this ASTContext is destroyed.
 */
class ASTContext {
    SourceMgr &SrcMgr;
    StringRef FileName;
    
    // Storage for all AST nodes - the context owns all nodes
    // Using deque to ensure pointer stability (no reallocation)
    std::deque<std::unique_ptr<Statement>> Statements;
    std::deque<std::unique_ptr<Expr>> Expressions;
    std::deque<std::unique_ptr<VarDeclaration>> Declarations;
    std::deque<std::unique_ptr<FunctionDeclaration>> Functions;
    std::deque<std::unique_ptr<Program>> Programs;

    // We create a pool of types, the ASTContext keeps the
    // types centered and every node will not need to own the
    // type
    std::deque<std::unique_ptr<Type>> Types;

    // Some of the canonical types, never duplicated
    BuiltinType *IntTy;
    BuiltinType *LongTy;
    BuiltinType *VoidTy;
    
public:
    ASTContext(llvm::SourceMgr &SrcMgr, StringRef FileName)
        : SrcMgr(SrcMgr), FileName(FileName) {
        auto i = std::make_unique<BuiltinType>(BuiltinType::Int);
        IntTy = i.get(); Types.push_back(std::move(i));
        auto l = std::make_unique<BuiltinType>(BuiltinType::Long);
        LongTy = l.get(); Types.push_back(std::move(l));
        auto v = std::make_unique<BuiltinType>(BuiltinType::Void);
        VoidTy = v.get(); Types.push_back(std::move(v));
    }

    // Non-copyable, non-movable to ensure stable addresses
    ASTContext(const ASTContext&) = delete;
    ASTContext& operator=(const ASTContext&) = delete;
    ASTContext(ASTContext&&) = delete;
    ASTContext& operator=(ASTContext&&) = delete;

    [[nodiscard]] StringRef getFileName() const {
        return FileName;
    }

    SourceMgr & getSourceMgr() {
        return SrcMgr;
    }

    [[nodiscard]] const SourceMgr & getSourceMgr() const {
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

    template<typename T, typename... Args>
    T* createType(Args&&... args) {
        auto node = std::make_unique<T>(std::forward<Args>(args)...);
        T* ptr = node.get();
        Types.emplace_back(std::move(node));
        return ptr;
    }

    BuiltinType* getBuiltInType(BuiltinType::BuiltinKind Kind) {
        if (Kind == BuiltinType::Int) return IntTy;
        if (Kind == BuiltinType::Long) return LongTy;
        if (Kind == BuiltinType::Void) return VoidTy;
        return nullptr;
    }

    BuiltinType* getIntTy()  { return IntTy;  }
    BuiltinType* getLongTy() { return LongTy; }
    BuiltinType* getVoidTy() { return VoidTy; }
};

}