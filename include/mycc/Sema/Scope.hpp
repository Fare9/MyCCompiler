#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringMap.h"
#include <vector>
#include <string>

namespace mycc {

class VarDeclaration;

class Scope {
    Scope * parentScope;
    StringMap<VarDeclaration*> Symbols;

    // Track variables declared in this scope (original names)
    std::vector<std::string> DeclaredVariables;

public:
    Scope(Scope * parentScope = nullptr) : parentScope(parentScope) {}

    bool insert(VarDeclaration *declaration);
    bool insert(StringRef key, VarDeclaration *declaration);
    VarDeclaration * lookup(StringRef Name);

    void addDeclaredVariable(StringRef originalName);
    const std::vector<std::string>& getDeclaredVariables() const { return DeclaredVariables; }

    Scope * getParentScope() { return parentScope; }
};
}