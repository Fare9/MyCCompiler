#pragma once

#include "mycc/Basic/Diagnostic.hpp"
#include "mycc/Lexer/Lexer.hpp"
#include "mycc/Sema/Sema.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>

namespace mycc {

class Parser {
    // @brief parser will use the lexer to create the AST nodes
    Lexer &Lex;
    // @brief Sema object will apply semantic checks later, for now
    // it will just create objects
    Sema &Actions;
    // @brief Current Token
    Token Tok;

    /// @brief Get the current diagnostic engine from lexer
    /// @return diagnostic engine for errors
    [[nodiscard]] DiagnosticsEngine &getDiagnostics() const {
        return Lex.getDiagnostics();
    }

    /// @brief make lexer read the next token
    void advance()
    {
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
    bool consume(tok::TokenKind ExpectedTok)
    {
        if (Tok.is(ExpectedTok))
        {
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
    /// @param P program to parse
    /// @return `true` if parsing was well, `false` otherwise
    bool parseProgram(Program *&P);
    bool parseFunction(Function *&F);

    bool parseStatementSequence(StmtList &Stmts);
    bool parseStatement(StmtList &Stmts);
    bool parseReturnStmt(StmtList &Stmts);

    bool parseExprList(ExprList &Exprs);
    bool parseExpr(Expr *&E, int min_precedence = 0);
    bool parseFactor(Expr *&E);

    static BinaryOperator::BinaryOpKind parseBinOp(Token& Tok);
public:
    Parser(Lexer &Lex, Sema &Actions);

    std::unique_ptr<Program> parse();
};


}