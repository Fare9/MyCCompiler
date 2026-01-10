#include "mycc/Sema/Scope.hpp"
#include "mycc/AST/AST.hpp"

using namespace mycc;


bool Scope::insert(VarDeclaration *declaration)
{
    StringRef name = declaration->getVar()->getName();
    return Symbols.insert(std::pair(name, declaration)).second;
}

bool Scope::insert(StringRef key, VarDeclaration *declaration)
{
    return Symbols.insert(std::pair(key, declaration)).second;
}

bool Scope::insert(FunctionDeclaration *funcDeclaration) {
    StringRef name = funcDeclaration->getName();
    return Symbols.insert(std::pair(name, funcDeclaration)).second;
}

bool Scope::insert(StringRef key, FunctionDeclaration *funcDeclaration) {
    return Symbols.insert(std::pair(key, funcDeclaration)).second;
}

VarDeclaration * Scope::lookupForVar(StringRef Name) {
    Scope * S = this;
    while (S)
    {
        StringMap<symbol_t>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            // Check if the symbol is actually a VarDeclaration
            if (std::holds_alternative<VarDeclaration*>(I->second)) {
                return std::get<VarDeclaration*>(I->second);
            }
            // Symbol exists but it's a Function, not a variable
            // In C, variables and functions have separate namespaces at the same scope level,
            // but for simplicity we treat finding a function as "not found" for variable lookup
        }
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

FunctionDeclaration* Scope::lookupForFunction(StringRef Name) {
    Scope * S = this;
    while (S)
    {
        StringMap<symbol_t>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            // Check if the symbol is actually a Function
            if (std::holds_alternative<FunctionDeclaration*>(I->second)) {
                return std::get<FunctionDeclaration*>(I->second);
            }
            // Symbol exists but it's a VarDeclaration, not a function
            // Continue searching in parent scopes
        }
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

bool Scope::hasSymbolInCurrentScope(StringRef Name) const {
    return Symbols.find(Name) != Symbols.end();
}

void Scope::addDeclaredIdentifier(StringRef originalName) {
    DeclaredIdentifiers.push_back(originalName.str());
}