#include "mycc/Sema/Scope.hpp"
#include "mycc/AST/AST.hpp"

using namespace mycc;


bool Scope::insert(VarDeclaration *declaration, Linkage linkage, ScopeType scope) {
    StringRef name = declaration->getVar()->getName();

    return Symbols.insert(std::pair(name, SymbolEntry{declaration, linkage, scope})).second;
}

bool Scope::insert(StringRef key, VarDeclaration *declaration, Linkage linkage, ScopeType scope) {
    return Symbols.insert(std::pair(key, SymbolEntry{declaration, linkage, scope})).second;
}

bool Scope::insert(FunctionDeclaration *funcDeclaration, Linkage linkage, ScopeType scope) {
    StringRef name = funcDeclaration->getName();
    return Symbols.insert(std::pair(name, SymbolEntry{funcDeclaration, linkage, scope})).second;
}

bool Scope::insert(StringRef key, FunctionDeclaration *funcDeclaration, Linkage linkage, ScopeType scope) {
    return Symbols.insert(std::pair(key, SymbolEntry{funcDeclaration, linkage, scope})).second;
}

VarDeclaration *Scope::lookupForVar(StringRef Name) {
    Scope *S = this;
    while (S) {
        StringMap<SymbolEntry>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            // Check if the symbol is actually a VarDeclaration
            if (std::holds_alternative<VarDeclaration *>(I->second.decl)) {
                return std::get<VarDeclaration *>(I->second.decl);
            }
            // Symbol exists, but it's a Function, not a variable
            // In C, variables and functions have separate namespaces at the same scope level,
            // but for simplicity we treat finding a function as "not found" for variable lookup
        }
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

FunctionDeclaration *Scope::lookupForFunction(StringRef Name) {
    Scope *S = this;
    while (S) {
        StringMap<SymbolEntry>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            // Check if the symbol is actually a Function
            if (std::holds_alternative<FunctionDeclaration *>(I->second.decl)) {
                return std::get<FunctionDeclaration *>(I->second.decl);
            }
            // If someone is calling a symbol and another symbol with the same name is in current
            // scope, we return nullptr...
            return nullptr;
        }
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

const SymbolEntry *Scope::lookupEntry(StringRef Name) const {
    const Scope *S = this;
    while (S) {
        auto I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            return &I->second;
        }
        S = S->getParentScope();
    }
    return nullptr;
}

bool Scope::hasSymbolInCurrentScope(StringRef Name) const {
    return Symbols.find(Name) != Symbols.end();
}

bool Scope::hasLinkageConflict(StringRef Name, std::optional<StorageClass> newStorageClass) const {
    auto I = Symbols.find(Name);
    if (I == Symbols.end()) {
        return false; // No conflict if symbol doesn't exist
    }

    // Check if previous entry has linkage (FunAttr or StaticAttr with global=true, or any StaticAttr)
    // FunAttr always has linkage, StaticAttr always has linkage, LocalAttr has no linkage
    bool prevHasLinkage = !I->second.isLocalAttr();

    // New has storage class equals to extern
    bool newIsExtern = (newStorageClass == StorageClass::SC_Extern);

    // There is a conflict if NOT (prev has linkage AND new is extern)
    return !(prevHasLinkage && newIsExtern);
}

bool Scope::updateSymbolEntry(StringRef Name, const StaticAttr &newAttrs) {
    auto I = Symbols.find(Name);
    if (I == Symbols.end()) {
        return false;
    }

    // Update the attrs with the new StaticAttr
    I->second.attrs = newAttrs;
    return true;
}

void Scope::addDeclaredIdentifier(StringRef originalName) {
    DeclaredIdentifiers.push_back(originalName.str());
}
