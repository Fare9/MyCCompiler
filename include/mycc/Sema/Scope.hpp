#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringMap.h"
#include <vector>
#include <string>
#include <variant>

namespace mycc {

class VarDeclaration;
class FunctionDeclaration;

using symbol_t = std::variant<FunctionDeclaration*, VarDeclaration *>;

enum class Linkage {
    External,   // Functions and file-scope variables (global vars)
    None
};

struct SymbolEntry {
    std::variant<FunctionDeclaration*, VarDeclaration*> decl;
    Linkage linkage;

    SymbolEntry(symbol_t symbol, Linkage linkage) : decl(symbol), linkage(linkage) {}
};

class Scope {
    Scope * parentScope;
    StringMap<SymbolEntry> Symbols;

    // Track variables declared in this scope (original names)
    std::vector<std::string> DeclaredIdentifiers;

public:
    Scope(Scope * parentScope = nullptr) : parentScope(parentScope) {}

    bool insert(VarDeclaration *declaration, Linkage linkage);
    bool insert(StringRef key, VarDeclaration *declaration, Linkage linkage);
    bool insert(FunctionDeclaration *funcDeclaration, Linkage linkage);
    bool insert(StringRef key, FunctionDeclaration *funcDeclaration, Linkage linkage);

    // Lookup through scope chain (current scope + parent scopes)
    VarDeclaration * lookupForVar(StringRef Name);
    FunctionDeclaration* lookupForFunction(StringRef Name);

    // Check if a symbol exists in the current scope only (not parent scopes)
    [[nodiscard]] bool hasSymbolInCurrentScope(StringRef Name) const;

    // Check if a symbol has a linkage conflict with an existing symbol
    [[nodiscard]] bool hasLinkageConflict(StringRef Name, Linkage newLinkage) const;

    void addDeclaredIdentifier(StringRef originalName);
    [[nodiscard]] const std::vector<std::string>& getDeclaredIdentifiers() const { return DeclaredIdentifiers; }

    Scope * getParentScope() { return parentScope; }
};
}
