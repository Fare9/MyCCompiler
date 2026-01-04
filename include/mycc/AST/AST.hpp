#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

#include <utility>
#include <variant>
#include <vector>
#include <optional>
#include <fmt/core.h>

#include "AST.hpp"
#include "AST.hpp"

namespace mycc {
    class Program;
    class Function;
    class Statement;
    class VarDeclaration;
    class Expr;

    using ExprList = std::vector<Expr *>;
    using StmtList = std::vector<Statement *>;
    using BlockItem = std::variant<Statement *, VarDeclaration *, Function*, std::monostate>;
    using ForInit = std::variant<VarDeclaration *, Expr *, std::monostate>;
    using BlockItems = std::vector<BlockItem>;
    using FuncList = std::vector<Function *>;

    // Base classes

    class Statement {
    public:
        enum StmtKind {
            SK_Return,
            SK_Expression,
            SK_If,
            SK_Compound,
            SK_Label,
            SK_Goto,
            SK_Break,
            SK_Continue,
            SK_While,
            SK_DoWhile,
            SK_For,
            SK_Case,
            SK_Default,
            SK_Switch,
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
            Ek_PrefixOperator,
            Ek_PostfixOperator,
            Ek_ConditionalOperator,
            Ek_FunctionCallOperator,
        };

    private:
        const ExprKind Kind;

    protected:
        explicit Expr(ExprKind Kind) : Kind(Kind) {
        }

    public:
        virtual ~Expr() = default;

        [[nodiscard]] ExprKind getKind() const {
            return Kind;
        }
    };

    // Statements and Expressions

    class ReturnStatement : public Statement {
        Expr *RetVal;

    public:
        explicit ReturnStatement(Expr *RetVal) : Statement(SK_Return), RetVal(RetVal) {
        }

        ~ReturnStatement() override = default;

        Expr *getRetVal() const {
            return RetVal;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Return;
        }
    };

    class ExpressionStatement : public Statement {
        Expr *expr;

    public:
        explicit ExpressionStatement(Expr *expr) : Statement(SK_Expression), expr(expr) {
        }

        ~ExpressionStatement() override = default;

        Expr *getExpr() const {
            return expr;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Expression;
        }
    };

    class IfStatement : public Statement {
        Expr *condition;
        // mandatory in if
        Statement *then_st;
        // optional statement
        Statement *else_st;

    public:
        IfStatement(Expr *condition, Statement *then_st) : Statement(SK_If), condition(condition), then_st(then_st) {
        }

        IfStatement(Expr *condition, Statement *then_st, Statement *else_st) : Statement(SK_If), condition(condition),
                                                                               then_st(then_st), else_st(else_st) {
        }

        ~IfStatement() override = default;

        Expr *getCondition() const {
            return condition;
        }

        Statement *getThenSt() const {
            return then_st;
        }

        Statement *getElseSt() const {
            return else_st;
        }

        void setElseSt(Statement *st) {
            this->else_st = st;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_If;
        }
    };

    class CompoundStatement : public Statement {
        BlockItems block;

    public:
        CompoundStatement(BlockItems &block) : Statement(SK_Compound), block(std::move(block)) {
        }

        ~CompoundStatement() override = default;

        BlockItems &getBlock() {
            return block;
        }

        const BlockItems &getBlock() const {
            return block;
        }

        BlockItem get_item(size_t i) {
            return i >= block.size() ? std::monostate{} : block[i];
        }

        BlockItems::iterator begin() {
            return block.begin();
        }

        BlockItems::iterator end() {
            return block.end();
        }

        [[nodiscard]] BlockItems::const_iterator begin() const {
            return block.begin();
        }

        [[nodiscard]] BlockItems::const_iterator end() const {
            return block.end();
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Compound;
        }
    };

    class LabelStatement : public Statement {
        StringRef Label;

    public:
        LabelStatement(StringRef Label) : Statement(SK_Label), Label(Label) {
        }

        ~LabelStatement() override = default;

        StringRef getLabel() const {
            return Label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Label;
        }
    };

    class GotoStatement : public Statement {
        StringRef Label;

    public:
        GotoStatement(StringRef Label) : Statement(SK_Goto), Label(Label) {
        }

        ~GotoStatement() override = default;

        StringRef getLabel() const {
            return Label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Goto;
        }
    };

    class BreakStatement : public Statement {
    private:
        std::string label;

    public:
        BreakStatement() : Statement(SK_Break) {
        }

        ~BreakStatement() override = default;

        void set_label(std::string label) {
            this->label = label;
        }

        std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Break;
        }
    };

    class ContinueStatement : public Statement {
    private:
        std::string label;

    public:
        ContinueStatement() : Statement(SK_Continue) {
        }

        ~ContinueStatement() override = default;

        void set_label(std::string label) {
            this->label = label;
        }

        std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Continue;
        }
    };

    class WhileStatement : public Statement {
    private:
        // condition inside of while statement
        Expr *Condition;
        // Body of the while condition
        Statement *Body;
        // An identifier label for the while loop
        std::string label;

    public:
        WhileStatement(Expr *condition, Statement *body) : Statement(SK_While), Condition(condition), Body(body) {
        }

        ~WhileStatement() override = default;

        Expr *getCondition() const {
            return Condition;
        }

        Statement *getBody() const {
            return Body;
        }

        void set_label(std::string label) {
            this->label = label;
        }

        std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_While;
        }
    };

    class DoWhileStatement : public Statement {
    private:
        // Body of the do/while condition
        Statement *Body;
        // condition inside of while statement
        Expr *Condition;
        // An identifier label for the do-while loop
        std::string label;

    public:
        DoWhileStatement(Statement *body, Expr *condition) : Statement(SK_DoWhile), Body(body), Condition(condition) {
        }

        ~DoWhileStatement() override = default;

        Statement *getBody() const {
            return Body;
        }

        Expr *getCondition() const {
            return Condition;
        }

        void set_label(std::string label) {
            this->label = label;
        }

        std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_DoWhile;
        }
    };

    class ForStatement : public Statement {
    private:
        ForInit Init; // This can be a Declaration, Expr, or nothing
        Expr *Condition; // Optional condition
        Expr *Post; // Optional post expression
        Statement *Body; // Body of For loop
        // An identifier label for the do-while loop
        std::string label;

    public:
        ForStatement(ForInit init, Expr *condition, Expr *post, Statement *body) : Statement(SK_For),
            Init(std::move(init)), Condition(condition),
            Post(post), Body(body) {
        }

        ~ForStatement() override = default;

        const ForInit &getInit() const {
            return Init;
        }

        Expr *getCondition() const { return Condition; }

        Expr *getPost() const { return Post; }

        Statement *getBody() const { return Body; }

        void set_label(std::string label) {
            this->label = label;
        }

        std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_For;
        }
    };

    class CaseStatement : public Statement {
        Expr *Value; // Constant expression checked
        std::string label; // the label of the case for jumping
    public:
        CaseStatement(Expr *value) : Statement(SK_Case), Value(value) {
        }

        ~CaseStatement() override = default;

        void set_label(std::string label) {
            this->label = label;
        }

        std::string_view get_label() const {
            return label;
        }

        Expr *getValue() const { return Value; }
    };

    class DefaultStatement : public Statement {
        std::string label; // Internal label for Jumping
    public:
        DefaultStatement() : Statement(SK_Default) {
        }

        ~DefaultStatement() override = default;

        void set_label(std::string label) {
            this->label = label;
        }

        std::string_view get_label() const {
            return label;
        }
    };


    class SwitchStatement : public Statement {
        Expr *Condition; // Controlling expression
        Statement *Body; // Usually a switch will be a CompoundStatement containing case labels
        std::string break_label; // Label to jump to when breaking
    public:
        SwitchStatement(Expr *condition, Statement *body) : Statement(SK_Switch), Condition(condition), Body(body) {
        }

        Expr *get_condition() const {
            return Condition;
        }

        Statement *get_body() const {
            return Body;
        }

        void set_break_label(std::string label) {
            this->break_label = label;
        }

        std::string_view get_break_label() const {
            return this->break_label;
        }
    };

    class NullStatement : public Statement {
    public:
        explicit NullStatement() : Statement(SK_Null) {
        }

        ~NullStatement() override = default;

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Null;
        }
    };

    class IntegerLiteral : public Expr {
        SMLoc Loc;
        llvm::APSInt Value;

    public:
        IntegerLiteral(SMLoc Loc, llvm::APSInt Value) : Expr(Ek_Int), Loc(Loc), Value(std::move(Value)) {
        }

        llvm::APSInt &getValue() {
            return Value;
        }

        [[nodiscard]] const llvm::APSInt &getValue() const {
            return Value;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_Int;
        }
    };

    class Var : public Expr {
        SMLoc Loc;
        std::string Name;

    public:
        Var(SMLoc Loc, StringRef Name) : Expr(Ek_Var), Loc(Loc), Name(Name) {
        }

        ~Var() override = default;

        [[nodiscard]] StringRef getName() const {
            return Name;
        }

        void setName() const {
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
        Expr *expr;

    public:
        UnaryOperator(SMLoc Loc, UnaryOperatorKind UnaryKind, Expr *expr) : Expr(Ek_UnaryOperator), Loc(Loc),
                                                                            UnaryKind(UnaryKind), expr(expr) {
        }

        ~UnaryOperator() override = default;

        [[nodiscard]] UnaryOperatorKind getOperatorKind() const {
            return UnaryKind;
        }

        Expr *getExpr() {
            return expr;
        }

        [[nodiscard]] const Expr *getExpr() const {
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
            // Chapter 6
            // Same as Assign, it is not a binary operation
            // but we will add it for not moving out these
            // values
            Bok_Interrogation,
            BoK_None
        };

    private:
        SMLoc Loc;
        const BinaryOpKind BinaryKind;
        Expr *left;
        Expr *right;

    public:
        BinaryOperator(SMLoc Loc, BinaryOpKind BinaryKind, Expr *left, Expr *right)
            : Expr(Ek_BinaryOperator), Loc(Loc), BinaryKind(BinaryKind), left(left), right(right) {
        }

        ~BinaryOperator() override = default;

        [[nodiscard]] BinaryOpKind getOperatorKind() const {
            return BinaryKind;
        }

        [[nodiscard]] Expr *getLeft() const {
            return left;
        }

        [[nodiscard]] Expr *getRight() const {
            return right;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_BinaryOperator;
        }
    };

    class AssignmentOperator : public Expr {
        SMLoc Loc;
        Expr *left;
        Expr *right;

    public:
        AssignmentOperator(SMLoc Loc, Expr *left, Expr *right) : Expr(Ek_AssignmentOperator), Loc(Loc), left(left),
                                                                 right(right) {
        }

        ~AssignmentOperator() override = default;

        Expr *getLeft() const {
            return left;
        }

        Expr *getRight() const {
            return right;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_AssignmentOperator;
        }
    };

    class PrefixOperator : public Expr {
    public:
        enum PrefixOpKind {
            POK_PreIncrement,
            POK_PreDecrement,
        };

    private:
        SMLoc Loc;
        const PrefixOpKind OpKind;
        Expr *expr;

    public:
        PrefixOperator(SMLoc Loc, PrefixOpKind OpKind, Expr *expr) : Expr(Ek_PrefixOperator), Loc(Loc), OpKind(OpKind),
                                                                     expr(expr) {
        }

        ~PrefixOperator() override = default;

        [[nodiscard]] PrefixOpKind getOperatorKind() const {
            return OpKind;
        }

        Expr *getExpr() {
            return expr;
        }

        [[nodiscard]] const Expr *getExpr() const {
            return expr;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_PrefixOperator;
        }
    };

    class PostfixOperator : public Expr {
    public:
        enum PostfixOpKind {
            POK_PostIncrement,
            POK_PostDecrement,
        };

    private:
        SMLoc Loc;
        const PostfixOpKind OpKind;
        Expr *expr;

    public:
        PostfixOperator(SMLoc Loc, PostfixOpKind OpKind, Expr *expr) : Expr(Ek_PostfixOperator), Loc(Loc),
                                                                       OpKind(OpKind), expr(expr) {
        }

        ~PostfixOperator() override = default;

        [[nodiscard]] PostfixOpKind getOperatorKind() const {
            return OpKind;
        }

        Expr *getExpr() {
            return expr;
        }

        [[nodiscard]] const Expr *getExpr() const {
            return expr;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_PostfixOperator;
        }
    };

    class ConditionalExpr : public Expr {
        Expr *condition;
        Expr *left;
        Expr *right;

    public:
        ConditionalExpr(Expr *condition, Expr *left, Expr *right) : Expr(Ek_ConditionalOperator), condition(condition),
                                                                    left(left), right(right) {
        }

        ~ConditionalExpr() override = default;

        Expr *getCondition() const {
            return condition;
        }

        Expr *getLeft() const {
            return left;
        }

        Expr *getRight() const {
            return right;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_ConditionalOperator;
        }
    };

    class FunctionCallExpr : public Expr {
        SMLoc Loc;
        std::string identifier;
        ExprList args;

    public:
        FunctionCallExpr(StringRef identifier, ExprList args) : Expr(Ek_FunctionCallOperator), identifier(identifier),
                                                                args(std::move(args)) {
        }

        ~FunctionCallExpr() override = default;

        [[nodiscard]] StringRef getIdentifier() const {
            return identifier;
        }

        [[nodiscard]] const ExprList &getArgs() const {
            return args;
        }

        [[nodiscard]] ExprList &getArguments() {
            return args;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_FunctionCallOperator;
        }
    };

    // Declaration

    using ArgsList = std::vector<Var *>;

    class VarDeclaration {
        SMLoc Loc;
        Var *Name;
        // In a declaration, an expression can be null
        Expr *expr = nullptr;

    public:
        VarDeclaration(SMLoc Loc, Var *Name) : Loc(Loc), Name(Name) {
        }

        VarDeclaration(SMLoc Loc, Var *Name, Expr *expr) : Loc(Loc), Name(Name), expr(expr) {
        }

        ~VarDeclaration() = default;

        [[nodiscard]] Var *getVar() const {
            return Name;
        }

        Expr *getExpr() const {
            return expr;
        }

        void setExpr(Expr *e) {
            expr = e;
        }
    };

    class Function {
        SMLoc Loc;
        StringRef Name;
        ArgsList args;
        BlockItems body;

    public:
        Function(StringRef Name, SMLoc Loc, ArgsList args) : Name(Name), Loc(Loc), args(std::move(args)) {
        }

        ~Function() = default;

        [[nodiscard]] StringRef getName() const {
            return Name;
        }

        void setBody(BlockItems &s) {
            body = std::move(s);
        }

        void add_statement(Statement *s) {
            body.emplace_back(s);
        }

        void add_declaration(VarDeclaration *s) {
            body.emplace_back(s);
        }

        Var *getArg(size_t i) {
            return i >= args.size() ? nullptr : args.at(i);
        }

        [[nodiscard]] const ArgsList &getArgs() const {
            return args;
        }

        ArgsList &getArguments() {
            return args;
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

        ~Program() = default;

        void add_functions(FuncList &funcs) {
            functions = std::move(funcs);
        }

        void add_function(Function *func) {
            functions.push_back(func);
        }

        [[nodiscard]] size_t get_number_of_functions() const {
            return functions.size();
        }

        Function *get_function(size_t i) {
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
