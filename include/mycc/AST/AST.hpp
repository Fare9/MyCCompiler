#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

#include <utility>
#include <variant>
#include <vector>
#include <optional>

namespace mycc {

class Program;
class Function;
class Statement;
class Declaration;
class Expr;

using ExprList = std::vector<Expr*>;
using StmtList = std::vector<Statement*>;
using BlockItem = std::variant<Statement *, Declaration *, std::monostate>;
using BlockItems = std::vector<BlockItem>;
using FuncList = std::vector<Function*>;

// Base classes

class Statement {
public:
    enum StmtKind {
        SK_Return,
        SK_Expression,
        SK_Null,
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

class Expr {
public:
    enum ExprKind {
        Ek_Int,
        Ek_Var,
        Ek_UnaryOperator,
        Ek_BinaryOperator,
        Ek_AssignmentOperator,
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

// Statements and Expressions

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

class ExpressionStatement : public Statement {
    Expr * expr;
public:
    explicit ExpressionStatement(Expr * expr) : Statement(SK_Expression), expr(expr) {}

    ~ExpressionStatement() override {
        delete expr;
    }

    Expr * getExpr() const {
        return expr;
    }

    static bool classof(const Statement * S) {
        return S->getKind() == SK_Expression;
    }
};

class NullStatement : public Statement {
public:
    explicit NullStatement() : Statement(SK_Null) {}

    ~NullStatement() override = default;

    static bool classof(const Statement * S) {
        return S->getKind() == SK_Null;
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

class Var : public Expr {
    SMLoc Loc;
    StringRef Name;
public:
    Var(SMLoc Loc, StringRef Name) : Expr(Ek_Var), Loc(Loc), Name(Name) {
    }

    ~Var() override = default;

    [[nodiscard]] StringRef getName() const {
        return Name;
    }

    static bool classof(const Expr *E) {
        return E->getKind() == Ek_Var;
    }
};

class UnaryOperator : public Expr {
public:
    enum UnaryOperatorKind {
        UopK_Complement,
        UopK_Negate,
        UopK_Not,
    };
private:
    SMLoc Loc;
    const UnaryOperatorKind UnaryKind;
    Expr * expr;
public:
    UnaryOperator(SMLoc Loc, UnaryOperatorKind UnaryKind, Expr * expr) :
        Expr(Ek_UnaryOperator), Loc(Loc), UnaryKind(UnaryKind), expr(expr) {
    }

    ~UnaryOperator() override {
        delete expr;
    }

    [[nodiscard]] UnaryOperatorKind getOperatorKind() const {
        return UnaryKind;
    }

    Expr * getExpr() {
        return expr;
    }

    [[nodiscard]] const Expr * getExpr() const {
        return expr;
    }

    static bool classof(const Expr *E) {
        return E->getKind() == Ek_UnaryOperator;
    }
};

class BinaryOperator : public Expr {
public:
    enum BinaryOpKind {
        // Chapter 3
        BoK_Add,
        BoK_Subtract,
        BoK_Multiply,
        BoK_Divide,
        BoK_Remainder,
        BoK_LeftShift,
        BoK_RightShift,
        BoK_BitwiseAnd,
        BoK_BitwiseXor,
        BoK_BitwiseOr,
        // Chapter 4
        BoK_LowerThan,
        BoK_LowerEqual,
        BoK_GreaterThan,
        BoK_GreaterEqual,
        Bok_Equal,
        Bok_NotEqual,
        Bok_And,
        Bok_Or,
        // Chapter 5
        // equals is not a Binary operation, but
        // we will add it here for not moving out
        // these values, or to create a variant
        // of two possible values
        Bok_Assign,
        BoK_None
    };
private:
    SMLoc Loc;
    const BinaryOpKind BinaryKind;
    Expr * left;
    Expr * right;
public:
    BinaryOperator(SMLoc Loc, BinaryOpKind BinaryKind, Expr *left, Expr *right)
        : Expr(Ek_BinaryOperator), Loc(Loc), BinaryKind(BinaryKind), left(left), right(right) {
    }

    ~BinaryOperator() override {
        delete left;
        delete right;
    }

    [[nodiscard]] BinaryOpKind getOperatorKind() const {
        return BinaryKind;
    }

    [[nodiscard]] Expr * getLeft() const {
        return left;
    }

    [[nodiscard]] Expr * getRight() const {
        return right;
    }

    static bool classof(const Expr *E) {
        return E->getKind() == Ek_BinaryOperator;
    }
};

class AssignmentOperator : public Expr {
    SMLoc Loc;
    Expr * left;
    Expr * right;
public:
    AssignmentOperator(SMLoc Loc, Expr * left, Expr * right) :
        Expr(Ek_AssignmentOperator), Loc(Loc), left(left), right(right) {}

    ~AssignmentOperator() override {
        delete left;
        delete right;
    }

    Expr * getLeft() const
    {
        return left;
    }

    Expr * getRight() const
    {
        return right;
    }

    static bool classof(const Expr *E) {
        return E->getKind() == Ek_AssignmentOperator;
    }
};

// Declaration

class Declaration {
    SMLoc Loc;
    Var* Name;
    // In a declaration, an expression can be null
    Expr * expr = nullptr;
public:
    Declaration(SMLoc Loc, Var* Name) : Loc(Loc), Name(Name) {}
    Declaration(SMLoc Loc, Var* Name, Expr * expr) : Loc(Loc), Name(Name), expr(expr) {}
    ~Declaration() {
        delete Name;

        if (expr)
            delete expr;
    }

    [[nodiscard]] Var * getName() const
    {
        return Name;
    }

    Expr * getExpr() const
    {
        return expr;
    }
};

class Function {
    SMLoc Loc;
    StringRef Name;
    BlockItems body;
public:
    Function(StringRef Name, SMLoc Loc) : Name(Name), Loc(Loc) {
    }

    ~Function() {
        for (auto& item : body) {
            if (std::holds_alternative<Statement *>(item)) {
                delete std::get<Statement *>(item);
            }
            else if (std::holds_alternative<Declaration *>(item)) {
                delete std::get<Declaration *>(item);
            }
        }
        body.clear();
    }

    [[nodiscard]] StringRef getName() const {
        return Name;
    }

    void setBody(BlockItems &s) {
        body = std::move(s);
    }

    void add_statement(Statement* s) {
        body.push_back(s);
    }

    void add_declaration(Declaration* s) {
        body.push_back(s);
    }

    BlockItem get_item(size_t i) {
        return i >= body.size() ? std::monostate{} : body[i];
    }

    BlockItems::iterator begin() {
        return body.begin();
    }

    BlockItems::iterator end() {
        return body.end();
    }

    [[nodiscard]] BlockItems::const_iterator begin() const {
        return body.begin();
    }

    [[nodiscard]] BlockItems::const_iterator end() const {
        return body.end();
    }
};

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
}