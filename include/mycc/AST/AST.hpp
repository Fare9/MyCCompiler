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

namespace mycc {
    class Program;
    class FunctionDeclaration;
    class Statement;
    class VarDeclaration;
    class Expr;

    using ExprList = std::vector<Expr *>;
    using StmtList = std::vector<Statement *>;
    using BlockItem = std::variant<Statement *, VarDeclaration *, FunctionDeclaration *, std::monostate>;
    using ForInit = std::variant<VarDeclaration *, Expr *, std::monostate>;
    using BlockItems = std::vector<BlockItem>;
    using DeclarationList = std::vector<std::variant<FunctionDeclaration *, VarDeclaration *> >;

    /// @brief Storage class specifiers that can appear on declarations
    /// (`static` or `extern`).
    enum class StorageClass {
        SC_Static,
        SC_Extern,
    };

    /// @brief Base class for all types in the AST. Uses a discriminator kind
    /// to support LLVM-style RTTI (`classof`).
    class Type {
    public:
        enum TypeKind {
            TK_Builtin,
            TK_Pointer,
            TK_Function,
        };

    private:
        const TypeKind Kind;

    protected:
        explicit Type(TypeKind Kind) : Kind(Kind) {
        }

    public:
        virtual ~Type() = default;

        [[nodiscard]] TypeKind getKind() const { return Kind; }
    };

    /// @brief Represents a built-in primitive type (e.g. `int`, `void`).
    class BuiltinType : public Type {
    public:
        enum BuiltinKind {
            Int,
            Void,
            // For the moment, we can include in the future more
        };

    private:
        BuiltinKind BuiltinK;

    public:
        explicit BuiltinType(BuiltinKind K)
            : Type(TK_Builtin), BuiltinK(K) {
        }

        [[nodiscard]] BuiltinKind getBuiltinKind() const { return BuiltinK; }

        static bool classof(const Type *T) {
            return T->getKind() == TK_Builtin;
        }
    };

    /// @brief Base class for all statement AST nodes. Each concrete statement
    /// carries a StmtKind discriminator for LLVM-style RTTI.
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

    /// @brief Base class for all expression AST nodes. Each concrete expression
    /// carries an ExprKind discriminator for LLVM-style RTTI.
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

    /// @brief AST node for a `return` statement, optionally carrying a
    /// return-value expression.
    class ReturnStatement : public Statement {
        Expr *RetVal;

    public:
        explicit ReturnStatement(Expr *RetVal) : Statement(SK_Return), RetVal(RetVal) {
        }

        ~ReturnStatement() override = default;

        [[nodiscard]] Expr *getRetVal() const {
            return RetVal;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Return;
        }
    };

    /// @brief AST node for an expression used as a statement (e.g. `foo();`).
    class ExpressionStatement : public Statement {
        Expr *expr;

    public:
        explicit ExpressionStatement(Expr *expr) : Statement(SK_Expression), expr(expr) {
        }

        ~ExpressionStatement() override = default;

        [[nodiscard]] Expr *getExpr() const {
            return expr;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Expression;
        }
    };

    /// @brief AST node for an `if` statement with mandatory then-branch
    /// and optional else-branch.
    class IfStatement : public Statement {
        Expr *condition;
        // mandatory in if
        Statement *then_st;
        // optional statement
        Statement *else_st;

    public:
        IfStatement(Expr *condition, Statement *then_st) : Statement(SK_If), condition(condition), then_st(then_st),
                                                           else_st(nullptr) {
        }

        IfStatement(Expr *condition, Statement *then_st, Statement *else_st) : Statement(SK_If), condition(condition),
                                                                               then_st(then_st), else_st(else_st) {
        }

        ~IfStatement() override = default;

        [[nodiscard]] Expr *getCondition() const {
            return condition;
        }

        [[nodiscard]] Statement *getThenSt() const {
            return then_st;
        }

        [[nodiscard]] Statement *getElseSt() const {
            return else_st;
        }

        void setElseSt(Statement *st) {
            this->else_st = st;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_If;
        }
    };

    /// @brief AST node for a compound statement (block): a brace-enclosed
    /// sequence of statements and declarations.
    class CompoundStatement : public Statement {
        BlockItems block;

    public:
        CompoundStatement(BlockItems &block) : Statement(SK_Compound), block(std::move(block)) {
        }

        ~CompoundStatement() override = default;

        BlockItems &getBlock() {
            return block;
        }

        [[nodiscard]] const BlockItems &getBlock() const {
            return block;
        }

        [[nodiscard]] BlockItem get_item(size_t i) const {
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

    /// @brief AST node for a label statement (`label:`), used as a target
    /// for `goto`.
    class LabelStatement : public Statement {
        StringRef Label;

    public:
        LabelStatement(StringRef Label) : Statement(SK_Label), Label(Label) {
        }

        ~LabelStatement() override = default;

        [[nodiscard]] StringRef getLabel() const {
            return Label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Label;
        }
    };

    /// @brief AST node for a `goto label;` statement.
    class GotoStatement : public Statement {
        StringRef Label;

    public:
        GotoStatement(StringRef Label) : Statement(SK_Goto), Label(Label) {
        }

        ~GotoStatement() override = default;

        [[nodiscard]] StringRef getLabel() const {
            return Label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Goto;
        }
    };

    /// @brief AST node for a `break;` statement. The label is resolved
    /// during semantic analysis to identify the enclosing loop or switch.
    class BreakStatement : public Statement {
        std::string label;

    public:
        BreakStatement() : Statement(SK_Break) {
        }

        ~BreakStatement() override = default;

        void set_label(const std::string &label) {
            this->label = label;
        }

        [[nodiscard]] std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Break;
        }
    };

    /// @brief AST node for a `continue;` statement. The label is resolved
    /// during semantic analysis to identify the enclosing loop.
    class ContinueStatement : public Statement {
        std::string label;

    public:
        ContinueStatement() : Statement(SK_Continue) {
        }

        ~ContinueStatement() override = default;

        void set_label(const std::string &label) {
            this->label = label;
        }

        [[nodiscard]] std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Continue;
        }
    };

    /// @brief AST node for a `while (condition) body` loop.
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

        [[nodiscard]] Expr *getCondition() const {
            return Condition;
        }

        [[nodiscard]] Statement *getBody() const {
            return Body;
        }

        void set_label(const std::string &label) {
            this->label = label;
        }

        [[nodiscard]] std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_While;
        }
    };

    /// @brief AST node for a `do body while (condition);` loop.
    class DoWhileStatement : public Statement {
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

        [[nodiscard]] Statement *getBody() const {
            return Body;
        }

        [[nodiscard]] Expr *getCondition() const {
            return Condition;
        }

        void set_label(const std::string &label) {
            this->label = label;
        }

        [[nodiscard]] std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_DoWhile;
        }
    };

    /// @brief AST node for a `for (init; condition; post) body` loop.
    /// The init clause may be a declaration, an expression, or empty.
    class ForStatement : public Statement {
        ForInit Init; // This can be a Declaration, Expr, or nothing
        Expr *Condition; // Optional condition
        Expr *Post; // Optional post expression
        Statement *Body; // Body of For loop
        // An identifier label for the do-while loop
        std::string label;

    public:
        ForStatement(ForInit init, Expr *condition, Expr *post, Statement *body) : Statement(SK_For),
            Init(init), Condition(condition),
            Post(post), Body(body) {
        }

        ~ForStatement() override = default;

        [[nodiscard]] const ForInit &getInit() const {
            return Init;
        }

        [[nodiscard]] Expr *getCondition() const { return Condition; }

        [[nodiscard]] Expr *getPost() const { return Post; }

        [[nodiscard]] Statement *getBody() const { return Body; }

        void set_label(const std::string &label) {
            this->label = label;
        }

        [[nodiscard]] std::string_view get_label() const {
            return label;
        }

        static bool classof(const Statement *S) {
            return S->getKind() == SK_For;
        }
    };

    /// @brief AST node for a `case expr:` label inside a switch statement.
    class CaseStatement : public Statement {
        Expr *Value; // Constant expression checked
        std::string label; // the label of the case for jumping
    public:
        CaseStatement(Expr *value) : Statement(SK_Case), Value(value) {
        }

        ~CaseStatement() override = default;

        void set_label(const std::string &label) {
            this->label = label;
        }

        [[nodiscard]] std::string_view get_label() const {
            return label;
        }

        [[nodiscard]] Expr *getValue() const { return Value; }
    };

    /// @brief AST node for a `default:` label inside a switch statement.
    class DefaultStatement : public Statement {
        std::string label; // Internal label for Jumping
    public:
        DefaultStatement() : Statement(SK_Default) {
        }

        ~DefaultStatement() override = default;

        void set_label(const std::string &label) {
            this->label = label;
        }

        [[nodiscard]] std::string_view get_label() const {
            return label;
        }
    };

    /// @brief AST node for a `switch (expr) { ... }` statement. The body
    /// is typically a CompoundStatement containing case/default labels.
    class SwitchStatement : public Statement {
        Expr *Condition; // Controlling expression
        Statement *Body; // Usually a switch will be a CompoundStatement containing case labels
        std::string break_label; // Label to jump to when breaking
    public:
        SwitchStatement(Expr *condition, Statement *body) : Statement(SK_Switch), Condition(condition), Body(body) {
        }

        [[nodiscard]] Expr *get_condition() const {
            return Condition;
        }

        [[nodiscard]] Statement *get_body() const {
            return Body;
        }

        void set_break_label(const std::string &label) {
            this->break_label = label;
        }

        [[nodiscard]] std::string_view get_break_label() const {
            return this->break_label;
        }
    };

    /// @brief AST node for a null (empty) statement: a bare `;`.
    class NullStatement : public Statement {
    public:
        explicit NullStatement() : Statement(SK_Null) {
        }

        ~NullStatement() override = default;

        static bool classof(const Statement *S) {
            return S->getKind() == SK_Null;
        }
    };

    /// @brief AST node for an integer literal constant (e.g. `42`).
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

    /// @brief AST node for a variable reference expression (an identifier
    /// that refers to a declared variable).
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

    /// @brief AST node for a unary operator expression (complement `~`,
    /// negate `-`, logical not `!`).
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

    /// @brief AST node for a binary operator expression (arithmetic, bitwise,
    /// relational, and logical operators like `+`, `<<`, `<`, `&&`, etc.).
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

    /// @brief AST node for an assignment expression (`lvalue = rvalue`),
    /// including compound assignments that have been desugared.
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

    /// @brief AST node for a prefix increment/decrement expression
    /// (`++x` or `--x`).
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

    /// @brief AST node for a postfix increment/decrement expression
    /// (`x++` or `x--`).
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
        PostfixOperator(const SMLoc Loc, const PostfixOpKind OpKind, Expr *expr) : Expr(Ek_PostfixOperator), Loc(Loc),
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

    /// @brief AST node for a ternary conditional expression
    /// (`condition ? left : right`).
    class ConditionalExpr : public Expr {
        Expr *condition;
        Expr *left;
        Expr *right;

    public:
        ConditionalExpr(Expr *condition, Expr *left, Expr *right) : Expr(Ek_ConditionalOperator), condition(condition),
                                                                    left(left), right(right) {
        }

        ~ConditionalExpr() override = default;

        [[nodiscard]] Expr *getCondition() const {
            return condition;
        }

        [[nodiscard]] Expr *getLeft() const {
            return left;
        }

        [[nodiscard]] Expr *getRight() const {
            return right;
        }

        static bool classof(const Expr *E) {
            return E->getKind() == Ek_ConditionalOperator;
        }
    };

    /// @brief AST node for a function call expression (`identifier(args...)`).
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

    using ArgsList = std::vector<Var *>;

    /// @brief AST node for a variable declaration, with optional initializer
    /// and storage class. Static local variables may carry a unique name
    /// for code generation.
    class VarDeclaration {
        SMLoc Loc;
        Var *Name;
        // In a declaration, an expression can be null
        Expr *expr = nullptr;
        std::optional<StorageClass> storageClass;
        // For static local variables, stores the unique global name (e.g., "func.var.1")
        std::optional<std::string> uniqueName;

    public:
        VarDeclaration(const SMLoc Loc, Var *Name) : Loc(Loc), Name(Name) {
        }

        VarDeclaration(const SMLoc Loc, Var *Name, Expr *expr) : Loc(Loc), Name(Name), expr(expr) {
        }

        VarDeclaration(const SMLoc Loc, Var *Name, Expr *expr, std::optional<StorageClass> storageClass)
            : Loc(Loc), Name(Name), expr(expr), storageClass(storageClass) {
        }

        ~VarDeclaration() = default;

        [[nodiscard]] Var *getVar() const {
            return Name;
        }

        [[nodiscard]] Expr *getExpr() const {
            return expr;
        }

        void setExpr(Expr *e) {
            expr = e;
        }

        [[nodiscard]] std::optional<StorageClass> getStorageClass() const {
            return storageClass;
        }

        void setStorageClass(StorageClass sc) {
            storageClass = sc;
        }

        [[nodiscard]] const std::optional<std::string>& getUniqueName() const {
            return uniqueName;
        }

        void setUniqueName(const std::string& name) {
            uniqueName = name;
        }
    };

    /// @brief AST node for a function declaration or definition. A declaration
    /// has no body; a definition has a body (list of block items). Supports
    /// storage class specifiers for linkage control.
    class FunctionDeclaration {
        SMLoc Loc;
        StringRef Name;
        ArgsList args;
        bool IsDefinition = false; // True if function has a body (even if empty)
        BlockItems body;
        std::optional<StorageClass> storageClass;

    public:
        FunctionDeclaration(const StringRef Name, const SMLoc Loc, ArgsList args)
            : Loc(Loc), Name(Name), args(std::move(args)) {
        }

        FunctionDeclaration(StringRef Name, SMLoc Loc, ArgsList args, std::optional<StorageClass> storageClass)
            : Loc(Loc), Name(Name), args(std::move(args)), storageClass(storageClass) {
        }

        ~FunctionDeclaration() = default;

        [[nodiscard]] StringRef getName() const {
            return Name;
        }

        void setBody(BlockItems &s) {
            body = std::move(s);
            IsDefinition = true; // Mark as definition when body is set
        }

        void setArgs(ArgsList &newArgs) {
            args = std::move(newArgs);
        }

        [[nodiscard]] bool hasBody() const {
            return IsDefinition;
        }

        [[nodiscard]] bool isDeclaration() const {
            return !IsDefinition;
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

        [[nodiscard]] BlockItem get_item(size_t i) const {
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

        [[nodiscard]] std::optional<StorageClass> getStorageClass() const {
            return storageClass;
        }

        void setStorageClass(StorageClass sc) {
            storageClass = sc;
        }

        /// Returns true if the function has external linkage (is NOT static)
        [[nodiscard]] bool isGlobal() const {
            return !storageClass.has_value() || storageClass.value() != StorageClass::SC_Static;
        }
    };

    /// @brief Root AST node representing a full translation unit (compilation
    /// unit). Contains all top-level declarations: functions and global variables.
    class Program {
        DeclarationList declarations;

    public:
        Program() = default;

        ~Program() = default;

        void add_functions(DeclarationList &funcs) {
            declarations = std::move(funcs);
        }

        void add_function(FunctionDeclaration *func) {
            declarations.emplace_back(func);
        }

        void add_variable(VarDeclaration *var) {
            declarations.emplace_back(var);
        }

        [[nodiscard]] size_t get_number_of_declarations() const {
            return declarations.size();
        }

        [[nodiscard]] std::variant<FunctionDeclaration *, VarDeclaration *> get_declaration(size_t i) const {
            return declarations.at(i);
        }

        // Iterator support for range-based for loops
        DeclarationList::iterator begin() {
            return declarations.begin();
        }

        DeclarationList::iterator end() {
            return declarations.end();
        }

        [[nodiscard]] DeclarationList::const_iterator begin() const {
            return declarations.begin();
        }

        [[nodiscard]] DeclarationList::const_iterator end() const {
            return declarations.end();
        }
    };
}
