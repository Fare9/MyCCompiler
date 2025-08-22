#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "mycc/Basic/TokenKinds.hpp"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

#include <string>
#include <utility>
#include <vector>

namespace mycc {

class Program;
class Function;
class Statement;
class Expr;


using ExprList = std::vector<Expr*>;
using StmtList = std::vector<Statement*>;
using FuncList = std::vector<Function*>;

class Program {
    FuncList functions;
public:
    Program() = default;
    
    ~Program() {
        for (Function* func : functions) {
            delete func;
        }
    }

    void add_functions(FuncList & funcs) {
        functions = std::move(funcs);
    }

    void add_function(Function * func) {
        functions.push_back(func);
    }

    [[nodiscard]] size_t get_number_of_functions() const {
        return functions.size();
    }

    Function * get_function(size_t i) {
        return i >= functions.size() ? nullptr : functions[i];
    }

    // Iterator support for range-based for loops
    FuncList::iterator begin() {
        return functions.begin();
    }

    FuncList::iterator end() {
        return functions.end();
    }

    [[nodiscard]] FuncList::const_iterator begin() const {
        return functions.begin();
    }

    [[nodiscard]] FuncList::const_iterator end() const {
        return functions.end();
    }
    
};

class Function {
    SMLoc Loc;
    StringRef Name;
    StmtList body;
public:
    Function(StringRef Name, SMLoc Loc) : Name(Name), Loc(Loc) {
    }
    
    ~Function() {
        for (Statement* stmt : body) {
            delete stmt;
        }
    }

    [[nodiscard]] StringRef getName() const {
        return Name;
    }

    void setStmts(StmtList &s) {
        body = std::move(s);
    }

    void add_statement(Statement * s) {
        body.push_back(s);
    }

    Statement * get_statement(size_t i) {
        return i >= body.size() ? nullptr : body[i];
    }

    StmtList::iterator begin() {
        return body.begin();
    }

    StmtList::iterator end() {
        return body.end();
    }

    [[nodiscard]] StmtList::const_iterator begin() const {
        return body.begin();
    }

    [[nodiscard]] StmtList::const_iterator end() const {
        return body.end();
    }
};

class Statement {
public:
    enum StmtKind {
        SK_Return
    };
private:
    const StmtKind Kind;
protected:
    explicit Statement(StmtKind Kind) : Kind(Kind) {
    }
public:
    virtual ~Statement() = default;
    
    [[nodiscard]] StmtKind getKind() const {
        return Kind;
    }
};

class ReturnStatement : public Statement {
    Expr * RetVal;
public:
    explicit ReturnStatement(Expr * RetVal) : Statement(SK_Return), RetVal(RetVal) {
    }
    
    ~ReturnStatement() override {
        delete RetVal;
    }

    Expr * getRetVal() const {
        return RetVal;
    }

    static bool classof(const Statement * S) {
        return S->getKind() == SK_Return;
    }
};

class Expr {
public:
    enum ExprKind {
        Ek_Int,
    };
private:
    const ExprKind Kind;

protected:
    explicit Expr(ExprKind Kind) : Kind(Kind) {
    }

public:
    virtual ~Expr() = default;
    
    [[nodiscard]] ExprKind getKind() const
    {
        return Kind;
    }
};

class IntegerLiteral : public Expr {
    SMLoc Loc;
    llvm::APSInt Value;
public:
    IntegerLiteral(SMLoc Loc, llvm::APSInt Value) :
        Expr(Ek_Int), Loc(Loc), Value(std::move(Value)) {
    }

    llvm::APSInt& getValue() {
        return Value;
    }

    [[nodiscard]] const llvm::APSInt& getValue() const {
        return Value;
    }

    static bool classof(const Expr *E) {
        return E->getKind() == Ek_Int;
    }
};

}