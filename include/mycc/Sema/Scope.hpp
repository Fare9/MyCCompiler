#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringMap.h"
#include <vector>
#include <string>
#include <variant>

namespace mycc {

class VarDeclaration;
class Function;

class Scope {
public:
    using symbol_t = std::variant<Function*, VarDeclaration *>;
private:
    Scope * parentScope;
    StringMap<symbol_t> Symbols;

    // Track variables declared in this scope (original names)
    std::vector<std::string> DeclaredIdentifiers;

public:
    Scope(Scope * parentScope = nullptr) : parentScope(parentScope) {}

    bool insert(VarDeclaration *declaration);
    bool insert(StringRef key, VarDeclaration *declaration);
    bool insert(Function *funcDeclaration);
    bool insert(StringRef key, Function *funcDeclaration);
    VarDeclaration * lookupForVar(StringRef Name);
    Function* lookupForFunction(StringRef Name);

    void addDeclaredIdentifier(StringRef originalName);
    const std::vector<std::string>& getDeclaredIdentifiers() const { return DeclaredIdentifiers; }

    Scope * getParentScope() { return parentScope; }
};
}
