#include "mycc/Sema/Scope.hpp"
#include "mycc/AST/AST.hpp"

using namespace mycc;


bool Scope::insert(Declaration *declaration)
{
    StringRef name = declaration->getVar()->getName();
    return Symbols.insert(std::pair(name, declaration)).second;
}

bool Scope::insert(StringRef key, Declaration *declaration)
{
    return Symbols.insert(std::pair(key, declaration)).second;
}

Declaration * Scope::lookup(StringRef Name) {
    Scope * S = this;
    while (S)
    {
        StringMap<Declaration *>::const_iterator I = S->Symbols.find(Name);
        if (I != S->Symbols.end())
            return I->second;
        S = S->getParentScope(); // look for the symbol in parent scope
    }

    return nullptr;
}

void Scope::addDeclaredVariable(StringRef originalName) {
    DeclaredVariables.push_back(originalName.str());
}