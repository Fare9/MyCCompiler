#pragma once

#include "mycc/Basic/Diagnostic.hpp"
#include "mycc/Lexer/Lexer.hpp"
#include "mycc/Sema/Sema.hpp"
#include "mycc/AST/AST.hpp"
#include "mycc/AST/ASTContext.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>
#include <optional>

namespace mycc {
    class Parser {
        /// @brief parser will use the lexer to create the AST nodes
        Lexer &Lex;
        /// @brief Sema object will apply semantic checks later, for now
        /// it will just create objects
        Sema &Actions;
        /// @brief AST context for memory management
        ASTContext &Context;
        /// @brief Current Token
        Token Tok;

        /// @brief Get the current diagnostic engine from lexer
        /// @return diagnostic engine for errors
        [[nodiscard]] DiagnosticsEngine &getDiagnostics() const {
            return Lex.getDiagnostics();
        }

        /// @brief make lexer read the next token
        void advance() {
            Lex.next(Tok);
        }

        /// @brief look for an expected token, in case an unexpected token is found report it
        /// @param ExpectedTok token we expect now
        /// @return true in case there was an error, false everything fine
        [[nodiscard]] bool expect(tok::TokenKind ExpectedTok) const {
            if (Tok.is(ExpectedTok)) // check if is expected token
                return false; // no error

            const char *Expected = tok::getPunctuatorSpelling(ExpectedTok);

            if (!Expected)
                Expected = tok::getKeywordSpelling(ExpectedTok);
            if (!Expected)
                Expected = tok::getTokenName(ExpectedTok);

            StringRef Actual(Tok.getLocation().getPointer(), Tok.getLength());

            getDiagnostics().report(Tok.getLocation(), diag::err_expected, Expected, Actual.str());

            return true;
        }

        /// @brief Consume a current expected token, once consumed, advance to the next token.
        /// @param ExpectedTok token we expect
        /// @return false in case everything is fine, true in case of error
        bool consume(tok::TokenKind ExpectedTok) {
            if (Tok.is(ExpectedTok)) {
                advance();
                return false;
            }

            return true;
        }

        /***
         * Full implementation of a Recursive descent parser
         * for each one of the non-terminals and terminals
         * parts of the EBNF. Since the parser is for a LL(1)
         * grammar, we must avoid left recursion, so something
         * like:
         *
         * stmnt : stmnt | ...
         *
         * Must be avoided, since that is translated to:
         *
         * bool parseStatement(StmtList &Stmts)
         * {
         *      parseStatement(Stmts);
         *      ...
         * }
         *
         * Finishing in an infinite recursion.
        */

        /// @brief Parse a compilation unit (Program)
        /// a compilation unit will be a list of declarations
        /// (functions and variables)
        /// @param P program to parse
        /// @return `true` if parsing was well, `false` otherwise
        bool parseProgram(Program *&P);

        /// @brief Parse the possible declarations that form a Program,
        /// these are functions and variables.
        /// @param Decls the full list of declarations.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseDeclaration(DeclarationList &Decls);

        /// @brief Parse declaration header: [storage-class]* type identifier
        /// Stops after identifier, before '(' or '=' or ';'. In that way parseDeclaration
        /// can parse the correct type of the AST.
        /// @param storageClass to store `extern` or `static` if present.
        /// @param type type of the variable/function return type (only `int` for the moment).
        /// @param name reference to the declaration name.
        /// @param loc line
        /// @param allowStorageClass flag to allow or not the declaration
        /// of storage class (e.g. declaration in `for` loop cannot have it.
        bool parseDeclarationHeader(std::optional<StorageClass> &storageClass,
                                    Type *&type,
                                    StringRef &name,
                                    SMLoc &loc,
                                    bool allowStorageClass = true);

        /// @brief Parse the rest of a function after 'int name' has been consumed
        bool parseFunctionRest(FunctionDeclaration *&F, SMLoc Loc, StringRef Name,
                               std::optional<StorageClass> storageClass, std::unique_ptr<Type> retType);

        /// @brief Parse the rest of a global variable after 'int name' has been consumed
        bool parseGlobalVarRest(VarDeclaration *&V, SMLoc Loc, StringRef Name,
                                std::optional<StorageClass> storageClass, std::unique_ptr<Type> type);

        /// @brief Parse for a possible declaration of a variable, or a function
        /// and if not parse a statement.
        /// @param Items all the items from the block.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseBlock(BlockItems &Items);

        /// @brief Parse a declaration of a variable, it handles the storage class analysis
        /// the type, and finally name and optionally initialization of a variable.
        bool parseVarDeclaration(BlockItems &Items, bool allowStorageClass = true);

        /// @brief Parse a statement (return, if, compound, goto, while, do-while,
        /// for, switch, break, continue, or expression statement).
        /// @param Items block items list to append the parsed statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseStatement(BlockItems &Items);

        /// @brief Parse a return statement: `return [expr];`
        /// @param Items block items list to append the return statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseReturnStmt(BlockItems &Items);

        /// @brief Parse an expression statement: `expr;`
        /// @param Items block items list to append the expression statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseExprStmt(BlockItems &Items);

        /// @brief Parse an if statement: `if (expr) stmt [else stmt]`
        /// @param Items block items list to append the if statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseIfStmt(BlockItems &Items);

        /// @brief Parse a compound statement (block): `{ block_item* }`
        /// @param Items block items list to append the compound statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseCompoundStmt(BlockItems &Items);

        /// @brief Parse a goto statement: `goto label;`
        /// @param Items block items list to append the goto statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseGotoStmt(BlockItems &Items);

        /// @brief Parse a while statement: `while (expr) stmt`
        /// @param Items block items list to append the while statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseWhileStmt(BlockItems &Items);

        /// @brief Parse a do-while statement: `do stmt while (expr);`
        /// @param Items block items list to append the do-while statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseDoWhileStmt(BlockItems &Items);

        /// @brief Parse a for statement: `for ([init]; [cond]; [inc]) stmt`
        /// @param Items block items list to append the for statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseForStmt(BlockItems &Items);

        /// @brief Parse a default label inside a switch: `default: stmt`
        /// @param Items block items list to append the default statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseDefaultStatement(BlockItems &Items);

        /// @brief Parse a case label inside a switch: `case const_expr: stmt`
        /// @param Items block items list to append the case statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseCaseStatement(BlockItems &Items);

        /// @brief Parse a switch statement: `switch (expr) { case/default* }`
        /// @param Items block items list to append the switch statement to.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseSwitchStatement(BlockItems &Items);

        /// @brief Parse the rest of a function declaration statement after the
        /// declaration header has been consumed.
        /// @param Items block items list to append the declaration to.
        /// @param Loc source location of the declaration.
        /// @param Name name of the function.
        /// @param retType return type of the function
        /// @param storageClass optional storage class (extern/static).
        /// @return `false` if parse was well, `true` otherwise.
        bool parseFunctionDeclarationStmt(BlockItems &Items, SMLoc Loc, StringRef Name, std::unique_ptr<Type> retType,
                                          std::optional<StorageClass> storageClass = std::nullopt);

        /// @brief Parse the rest of a variable declaration inline (inside a block)
        /// after the declaration header has been consumed.
        /// @param Items block items list to append the declaration to.
        /// @param Loc source location of the declaration.
        /// @param Name name of the variable.
        /// @param storageClass optional storage class (extern/static).
        /// @return `false` if parse was well, `true` otherwise.
        bool parseVariableDeclInline(BlockItems &Items, SMLoc Loc, StringRef Name, std::unique_ptr<Type> type,
                                     std::optional<StorageClass> storageClass = std::nullopt);

        /// @brief Parse an expression using precedence climbing.
        /// @param E reference to the resulting expression AST node.
        /// @param min_precedence minimum precedence level for binding.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseExpr(Expr *&E, int min_precedence = 0);

        /// @brief Parse the middle operand of a ternary conditional expression.
        /// @param Middle reference to the resulting middle expression AST node.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseMiddle(Expr *&Middle);

        /// @brief Parse a factor (primary expression): literals, unary operators,
        /// identifiers, function calls, or parenthesized expressions.
        /// @param E reference to the resulting expression AST node.
        /// @return `false` if parse was well, `true` otherwise.
        bool parseFactor(Expr *&E);

        /// @brief Map a token to its corresponding binary operator kind.
        /// @param Tok the token to inspect.
        /// @return the binary operator kind for the token.
        static BinaryOperator::BinaryOpKind parseBinOp(Token &Tok);

        /// @brief Get the compound-assignment operator kind that corresponds
        /// to a given expression's operator (e.g. `+=` yields `+`).
        /// @param expr the expression to inspect.
        /// @return the operator kind for the underlying binary operation.
        static int get_operator_kind_by_expr(Expr *expr);

    public:
        /// @brief Construct a new Parser.
        /// @param Lex lexer providing the token stream.
        /// @param Actions semantic analysis interface.
        /// @param Context AST context for memory management.
        Parser(Lexer &Lex, Sema &Actions, ASTContext &Context);

        /// @brief Entry point: parse the full translation unit.
        /// @return the parsed Program AST node, or nullptr on failure.
        Program *parse();
    };
}
