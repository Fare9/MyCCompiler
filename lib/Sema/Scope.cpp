#include "mycc/Sema/Scope.hpp"
#include "mycc/AST/AST.hpp"

using namespace mycc;


bool Scope::insert(VarDeclaration *declaration, Linkage linkage)
{
    StringRef name = declaration->getVar()->getName();

    return Symbols.insert(std::pair(name, SymbolEntry{declaration, linkage})).second;
}

bool Scope::insert(StringRef key, VarDeclaration *declaration, Linkage linkage)
{
    return Symbols.insert(std::pair(key, SymbolEntry{declaration, linkage})).second;
}

bool Scope::insert(FunctionDeclaration *funcDeclaration, Linkage linkage) {
    StringRef name = funcDeclaration->getName();
    return Symbols.insert(std::pair(name, SymbolEntry{funcDeclaration, linkage})).second;
}

bool Scope::insert(StringRef key, FunctionDeclaration *funcDeclaration, Linkage linkage) {
    return Symbols.insert(std::pair(key, SymbolEntry{funcDeclaration, linkage})).second;
}

VarDeclaration * Scope::lookupForVar(StringRef Name) {
    Scope * S = this;
    while (S)
    {
        StringMap<SymbolEntry>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            // Check if the symbol is actually a VarDeclaration
            if (std::holds_alternative<VarDeclaration*>(I->second.decl)) {
                return std::get<VarDeclaration*>(I->second.decl);
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
        StringMap<SymbolEntry>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            // Check if the symbol is actually a Function
            if (std::holds_alternative<FunctionDeclaration*>(I->second.decl)) {
                return std::get<FunctionDeclaration*>(I->second.decl);
            }
            // Another thing was declared... So we should return nullptr
            // because someone might be calling something else as a function...
            return nullptr;
        }
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

bool Scope::hasSymbolInCurrentScope(StringRef Name) const {
    return Symbols.find(Name) != Symbols.end();
}

bool Scope::hasLinkageConflict(StringRef Name, Linkage newLinkage) const {
    auto I = Symbols.find(Name);
    if (I == Symbols.end()) {
        return false; // No conflict if symbol doesn't exist
    }

    // Check if linkages differ
    return I->second.linkage != newLinkage;
}

void Scope::addDeclaredIdentifier(StringRef originalName) {
    DeclaredIdentifiers.push_back(originalName.str());
}