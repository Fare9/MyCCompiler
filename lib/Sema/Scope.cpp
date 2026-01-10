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

bool Scope::insert(Function *funcDeclaration) {
    StringRef name = funcDeclaration->getName();
    return Symbols.insert(std::pair(name, funcDeclaration)).second;
}

bool Scope::insert(StringRef key, Function *funcDeclaration) {
    return Symbols.insert(std::pair(key, funcDeclaration)).second;
}

VarDeclaration * Scope::lookupForVar(StringRef Name) {
    Scope * S = this;
    while (S)
    {
        StringMap<symbol_t>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end())
            return std::get<VarDeclaration*>(I->second);
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

Function* Scope::lookupForFunction(StringRef Name) {
    Scope * S = this;
    while (S)
    {
        StringMap<symbol_t>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end())
            return std::get<Function*>(I->second);
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

void Scope::addDeclaredIdentifier(StringRef originalName) {
    DeclaredIdentifiers.push_back(originalName.str());
}