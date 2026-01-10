#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringMap.h"
#include <vector>
#include <string>
#include <variant>

namespace mycc {

class VarDeclaration;
class FunctionDeclaration;

class Scope {
public:
    using symbol_t = std::variant<FunctionDeclaration*, VarDeclaration *>;
private:
    Scope * parentScope;
    StringMap<symbol_t> Symbols;

    // Track variables declared in this scope (original names)
    std::vector<std::string> DeclaredIdentifiers;

public:
    Scope(Scope * parentScope = nullptr) : parentScope(parentScope) {}

    bool insert(VarDeclaration *declaration);
    bool insert(StringRef key, VarDeclaration *declaration);
    bool insert(FunctionDeclaration *funcDeclaration);
    bool insert(StringRef key, FunctionDeclaration *funcDeclaration);

    // Lookup through scope chain (current scope + parent scopes)
    VarDeclaration * lookupForVar(StringRef Name);
    FunctionDeclaration* lookupForFunction(StringRef Name);

    // Check if a symbol exists in the current scope only (not parent scopes)
    bool hasSymbolInCurrentScope(StringRef Name) const;

    void addDeclaredIdentifier(StringRef originalName);
    const std::vector<std::string>& getDeclaredIdentifiers() const { return DeclaredIdentifiers; }

    Scope * getParentScope() { return parentScope; }
};
}
