#pragma once

#include "mycc/AST/AST.hpp"
#include "mycc/AST/ASTContext.hpp"
#include "mycc/Sema/Scope.hpp"
#include "mycc/Basic/Diagnostic.hpp"
#include "llvm/ADT/StringMap.h"
#include <vector>
#include <string>
#include <set>
#include <map>

namespace mycc {
    /**
     * @class Sema
     * @brief Semantic analysis class for the C compiler.
     *
     * The Sema class performs semantic analysis on the AST, including:
     * - Variable name resolution and uniqueness
     * - Scope management
     * - Label validation for goto statements
     * - Loop and switch statement validation
     * - Type checking and constant expression evaluation
     */
    class Sema {
        friend class EnterDeclScope;

        DiagnosticsEngine &Diags;
        ASTContext &Context;
        Scope *CurrentScope;
        bool avoid_errors = false;


        // Set that contains for a method the labels
        std::set<StringRef> FunctionLabels;
        // Set that contains all the jumped labels by Goto
        std::set<StringRef> GotoLabels;

        // Counter for generating unique loop labels
        unsigned int LoopLabelCounter = 0;
        // Counter to generate Switch and Case labels
        unsigned int SwitchLabelCounter = 0;
        unsigned int CaseLabelCounter = 0;
        unsigned int DefaultLabelCounter = 0;

        /**
         * @brief We keep this structure to maintain the context for the break instructions.
         * with the `base_level` we generate the break and the continue label. We also
         * keep metadata to know if we are inside a loop.
         */
        struct BreakableContext {
            std::string base_label;
            bool is_loop; // metadata, it contains true for loops, and false for switches

            [[nodiscard]] std::string get_break_label() const {
                return base_label + "_end";
            }

            [[nodiscard]] std::string get_continue_label() const {
                return base_label + "_continue";
            }
        };


        // Loop label assignment helpers

        /**
         * @brief Generate a unique label for a loop construct.
         * @return Unique loop label string.
         */
        std::string generateLoopLabel();

        /**
         * @brief Traverse a statement and assign labels to break/continue targets.
         * @param stmt Statement to traverse.
         * @param breakableStack Stack of breakable contexts (loops and switches).
         */
        void traverseStatement(Statement *stmt, std::vector<BreakableContext> &breakableStack);

        /**
         * @brief Traverse a block item (statement or declaration) for label assignment.
         * @param item Block item to traverse.
         * @param breakableStack Stack of breakable contexts (loops and switches).
         */
        void traverseBlockItem(BlockItem &item, std::vector<BreakableContext> &breakableStack);

        // Switch label assignment helpers

        /**
         * @brief Generate a unique label for a switch statement.
         * @return Unique switch label string.
         */
        std::string generateSwitchLabel();

        /**
         * @brief Generate a unique label for a case statement.
         * @return Unique case label string.
         */
        std::string generateCaseLabel();

        /**
         * @brief Generate a unique label for a default statement.
         * @return Unique default label string.
         */
        std::string generateDefaultLabel();

        /**
         * @brief Check if an expression is a constant expression.
         * @param expr Expression to check.
         * @return true if the expression is a constant, false otherwise.
         */
        static bool isConstantExpression(Expr *expr);

        /**
         * @brief Evaluate a constant expression to an integer value.
         * @param expr Constant expression to evaluate.
         * @return Integer value of the constant expression.
         */
        static int64_t evaluateConstantExpression(Expr *expr);

        /**
         * @brief Validate the body of a switch statement.
         *
         * Checks for:
         * - Case values are constant expressions
         * - No duplicate case values
         * - At most one default case
         * - No declarations immediately after case/default labels
         *
         * @param body Switch body statement.
         * @param seenCaseValues Set to track duplicate case values.
         * @param hasDefault Flag to track if default case was seen.
         */
        void validateSwitchBody(Statement *body,
                                std::set<int64_t> &seenCaseValues,
                                bool &hasDefault);

        // Final passes from Semantic Analysis

        /**
         * @brief Verify all goto labels reference defined labels in the function.
         */
        void checkGotoLabelsCorrectlyPointToFunction() const;

        /**
         * Compute the Linkage depending on the Storage class and the scope.
         * @param sc optional value of storage class.
         * @param scope scope of the declaration.
         * @return linkage type.
         */
        static Linkage computeLinkage(std::optional<StorageClass> sc, ScopeType scope);

    public:
        /**
         * @brief Constructs a Sema object for semantic analysis.
         * @param Diags Diagnostics engine for error reporting.
         * @param Context AST context for creating AST nodes.
         */
        explicit Sema(DiagnosticsEngine &Diags, ASTContext &Context) : Diags(Diags), Context(Context), CurrentScope(nullptr) {
            initialize();
        }

        /**
         * @brief Disable error reporting (useful for testing).
         */
        void avoidErrors() {
            avoid_errors = true;
        }

        [[nodiscard]] bool is_avoid_errors_active() const {
            return avoid_errors;
        }

        /**
         * @brief Initialize semantic analysis state.
         */
        void initialize();

        /**
         * @brief Enter a new function scope, clearing function-level state.
         */
        void enterFunction();

        /**
         * @brief Exit function scope and validate goto labels.
         */
        void exitFunction() const;

        /**
         * @brief Enter a new lexical scope for variable declarations.
         */
        void enterScope();

        /**
         * @brief Exit current lexical scope, popping variable declarations.
         */
        void exitScope();

        /**
         * @brief Assign unique labels to loops, breaks, and continues in a function.
         * @param F The function to process.
         */
        void assignLoopLabels(FunctionDeclaration &F);

        /**
         * @brief Create a Program node from a list of functions.
         * @param Funcs List of functions in the program.
         * @return Pointer to the created Program node.
         */
        Program *actOnProgramDeclaration(DeclarationList &Funcs) const;

        /**
         * @brief Create a Function node.
         * @param Loc Source location of the function declaration.
         * @param Name Function name.
         * @param args Function parameters.
         * @return Pointer to the created Function node.
         */
        FunctionDeclaration *actOnFunctionDeclaration(SMLoc Loc, StringRef Name, ArgsList &args, std::optional<StorageClass> storageClass) const;

        /**
         * @brief Process a parameter declaration and create a Var node with unique name.
         * @param Loc Source location of the parameter.
         * @param Name Parameter name.
         * @return Pointer to the created Var node, or nullptr on error.
         */
        [[nodiscard]] Var *actOnParameterDeclaration(SMLoc Loc, StringRef Name) const;

        /**
         * @brief Process a variable declaration and add it to the current scope.
         * @param Items Block items list to append the declaration to.
         * @param Loc Source location of the declaration.
         * @param Name Variable name.
         * @param storageClass type of storage (Static or Extern)
         * @return true on error (duplicate declaration), false on success.
         */
        bool actOnVarDeclaration(BlockItems &Items, SMLoc Loc, StringRef Name, std::optional<StorageClass> storageClass) const;

        /**
         * @brief Create a return statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the return statement.
         * @param RetVal Return value expression (may be null for void return).
         */
        void actOnReturnStatement(BlockItems &Items, SMLoc Loc, Expr *RetVal) const;

        /**
         * @brief Create a null (empty) statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the null statement.
         */
        void actOnNullStatement(BlockItems &Items, SMLoc Loc) const;

        /**
         * @brief Create an expression statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the expression statement.
         * @param Expr Expression to evaluate.
         */
        void actOnExprStatement(BlockItems &Items, SMLoc Loc, Expr *Expr) const;

        /**
         * @brief Create an if statement with optional else clause.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the if statement.
         * @param Cond Condition expression.
         * @param then_st Then branch statement.
         * @param else_st Else branch statement (may be null).
         */
        void actOnIfStatement(BlockItems &Items, SMLoc Loc, Expr *Cond, Statement *then_st, Statement *else_st) const;

        /**
         * @brief Create a compound statement (block).
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the compound statement.
         * @param compoundStatement Block items contained in the compound statement.
         */
        void actOnCompoundStatement(BlockItems &Items, SMLoc Loc, BlockItems &compoundStatement) const;

        /**
         * @brief Create a label statement and register it in the function.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the label.
         * @param Label Label identifier.
         */
        void actOnLabelStatement(BlockItems &Items, SMLoc Loc, StringRef Label);

        /**
         * @brief Create a goto statement and register the target label.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the goto statement.
         * @param Label Target label identifier.
         */
        void actOnGotoStatement(BlockItems &Items, SMLoc Loc, StringRef Label);

        /**
         * @brief Create a while loop statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the while statement.
         * @param Cond Loop condition expression.
         * @param Body Loop body statement.
         */
        void actOnWhileStatement(BlockItems &Items, SMLoc Loc, Expr *Cond, Statement *Body) const;

        /**
         * @brief Create a do-while loop statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the do-while statement.
         * @param Body Loop body statement.
         * @param Cond Loop condition expression.
         */
        void actOnDoWhileStatement(BlockItems &Items, SMLoc Loc, Statement *Body, Expr *Cond) const;

        /**
         * @brief Create a for loop statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the for statement.
         * @param Init Loop initialization (declaration or expression).
         * @param Cond Loop condition expression.
         * @param Post Post-iteration expression.
         * @param Body Loop body statement.
         */
        void actOnForStatement(BlockItems &Items, SMLoc Loc, ForInit &Init, Expr *Cond, Expr *Post, Statement *Body) const;

        /**
         * @brief Create a break statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the break statement.
         */
        void actOnBreakStatement(BlockItems &Items, SMLoc Loc) const;

        /**
         * @brief Create a continue statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the continue statement.
         */
        void actOnContinueStatement(BlockItems &Items, SMLoc Loc) const;

        /**
         * @brief Create a default case statement for switch.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the default statement.
         */
        void actOnDefaultStatement(BlockItems &Items, SMLoc Loc) const;

        /**
         * @brief Create a case statement for switch.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the case statement.
         * @param Cond Case value expression (must be constant).
         */
        void actOnCaseStatement(BlockItems &Items, SMLoc Loc, Expr *Cond) const;

        /**
         * @brief Create a switch statement.
         * @param Items Block items list to append the statement to.
         * @param Loc Source location of the switch statement.
         * @param Cond Switch condition expression.
         * @param Body Switch body statement (typically a compound statement with cases).
         */
        void actOnSwitchStatement(BlockItems &Items, SMLoc Loc, Expr *Cond, Statement *Body) const;


        /**
         * @brief Create an integer literal expression.
         * @param Loc Source location of the literal.
         * @param Literal String representation of the integer.
         * @return Pointer to the created IntegerLiteral node.
         */
        [[nodiscard]] IntegerLiteral *actOnIntegerLiteral(SMLoc Loc, StringRef Literal) const;

        /**
         * @brief Create a unary operator expression.
         * @param Loc Source location of the operator.
         * @param Kind Kind of unary operator (negation, complement, logical not).
         * @param expr Operand expression.
         * @return Pointer to the created UnaryOperator node.
         */
        UnaryOperator *actOnUnaryOperator(SMLoc Loc, UnaryOperator::UnaryOperatorKind Kind, Expr *expr) const;

        /**
         * @brief Create a binary operator expression.
         * @param Loc Source location of the operator.
         * @param Kind Kind of binary operator (add, subtract, multiply, etc.).
         * @param left Left operand expression.
         * @param right Right operand expression.
         * @return Pointer to the created BinaryOperator node.
         */
        BinaryOperator *actOnBinaryOperator(SMLoc Loc, BinaryOperator::BinaryOpKind Kind, Expr *left, Expr *right) const;

        /**
         * @brief Create an assignment expression and validate the lvalue.
         * @param Loc Source location of the assignment.
         * @param left Left-hand side expression (must be an lvalue).
         * @param right Right-hand side expression.
         * @return Pointer to the created AssignmentOperator node, or nullptr if left is not an lvalue.
         */
        AssignmentOperator *actOnAssignment(SMLoc Loc, Expr *left, Expr *right) const;

        /**
         * @brief Create a prefix increment/decrement expression and validate the lvalue.
         * @param Loc Source location of the operator.
         * @param Kind Kind of prefix operator (increment or decrement).
         * @param expr Operand expression (must be an lvalue).
         * @return Pointer to the created PrefixOperator node, or nullptr if expr is not an lvalue.
         */
        PrefixOperator *actOnPrefixOperator(SMLoc Loc, PrefixOperator::PrefixOpKind Kind, Expr *expr) const;

        /**
         * @brief Create a postfix increment/decrement expression and validate the lvalue.
         * @param Loc Source location of the operator.
         * @param Kind Kind of postfix operator (increment or decrement).
         * @param expr Operand expression (must be an lvalue).
         * @return Pointer to the created PostfixOperator node, or nullptr if expr is not an lvalue.
         */
        PostfixOperator *actOnPostfixOperator(SMLoc Loc, PostfixOperator::PostfixOpKind Kind, Expr *expr) const;

        /**
         * @brief Resolve an identifier to its unique variable name.
         * @param Loc Source location of the identifier.
         * @param Name Variable name to look up.
         * @return Pointer to Var node with the unique name, or nullptr if undeclared.
         */
        [[nodiscard]] Var *actOnIdentifier(SMLoc Loc, StringRef Name) const;

        /**
         * @brief Create a conditional (ternary) expression.
         * @param Loc Source location of the operator.
         * @param left Condition expression.
         * @param middle True branch expression.
         * @param right False branch expression.
         * @return Pointer to the created ConditionalExpr node.
         */
        ConditionalExpr *actOnTernaryOperator(SMLoc Loc, Expr *left, Expr *middle, Expr *right) const;

        /**
         * @brief Create a function call expression. The calling function
         * already checks for the argument expressions.
         *
         * @param Loc Source location of the function call.
         * @param name Function name.
         * @param args List of argument expressions.
         * @return Pointer to the created FunctionCallExpr node.
         */
        FunctionCallExpr *actOnFunctionCallOperator(SMLoc Loc, StringRef name, ExprList &args) const;
    };

    /**
     * @class EnterDeclScope
     * @brief RAII helper class for scope management.
     *
     * Automatically enters a scope on construction and exits on destruction,
     * ensuring proper scope cleanup even in the presence of exceptions.
     */
    class EnterDeclScope {
        Sema &Semantics;

    public:
        EnterDeclScope(Sema &Semantics) : Semantics(Semantics) {
            Semantics.enterScope();
        }

        ~EnterDeclScope() {
            Semantics.exitScope();
        }
    };
}
