
#include "mycc/Parser/Parser.hpp"

using namespace mycc;

Parser::Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
}

Program * Parser::parse() {
    Program * p = nullptr;
    parseProgram(p);
    return p;
}

bool Parser::parseProgram(Program *&P) {
    FuncList Funcs;

    // advance to the first token
    advance();
    // now start consuming functions
    while (Tok.isNot(tok::eof)) {
        Function * Func = nullptr;
        if (!parseFunction(Func)) {
            return false;
        } else {
            Funcs.push_back(Func);
        }
    }

    P = Actions.actOnProgramDeclaration(Funcs);
    return false;
}

bool Parser::parseFunction(Function *&F) {
    auto _errorhandler = [this] {
        while (!Tok.is(tok::eof))
            advance();
        return false;
    };

    if (consume(tok::kw_int))
        return _errorhandler();
    if (expect(tok::identifier))
        return _errorhandler();

    F = Actions.actOnFunctionDeclaration(Tok.getLocation(), Tok.getIdentifier());

    // advance to next token
    advance();

    // for now consume "(void)"
    if (consume(tok::l_paren))
        return _errorhandler();
    if (consume(tok::kw_void))
        return _errorhandler();
    if (consume(tok::r_paren))
        return _errorhandler();

    // consume body
    StmtList body;
    if (consume(tok::l_brace))
        return _errorhandler();
    // Parse statement sequence - fail immediately on error
    if (parseStatementSequence(body))
        return _errorhandler();
    if (consume(tok::r_brace))
        return _errorhandler();
    F->setStmts(body);

    return true;
}

bool Parser::parseStatementSequence(StmtList &Stmts) {
    // No error handler - let errors bubble up immediately
    
    if (parseStatement(Stmts))
        return true; // Immediate failure

    while (Tok.is(tok::semi)) {
        advance();
        if (parseStatement(Stmts))
            return true; // Immediate failure
    }
    return false;
}

bool Parser::parseStatement(StmtList &Stmts) {

    if (Tok.is(tok::kw_return)) {
        if (parseReturnStmt(Stmts))
            return true;
    }

    return false;
}

bool Parser::parseReturnStmt(mycc::StmtList &Stmts) {

    Expr * E = nullptr;
    SMLoc Loc = Tok.getLocation();
    if (consume(tok::kw_return))
        return true;

    // Try to parse expression - let parseExpr handle invalid tokens
    if (parseExpr(E))
        return true;

    if (consume(tok::semi))
        return true;
    Actions.actOnReturnStatement(Stmts, Loc, E);
    return false;
}

bool Parser::parseExprList(ExprList &Exprs) {
    auto _errorhandler = [this] {
        while (!Tok.is(tok::r_paren)) {
            advance();
            if (Tok.is(tok::eof))
                return true;
        }
        return false;
    };

    Expr * E = nullptr;
    if (parseExpr(E))
        return _errorhandler();
    if (E)
        Exprs.push_back(E);
    while (Tok.is(tok::comma)) {
        E = nullptr;
        advance();
        if (parseExpr(E))
            return _errorhandler();
        if (E)
            Exprs.push_back(E);
    }
    return false;
}

bool Parser::parseExpr(Expr *&E) {
    auto _errorhandler = [this] {
        while (!Tok.is(tok::r_brace)) {
            advance();
            if (Tok.is(tok::eof))
                return true;
        }
        return false;
    };


    if (!Tok.is(tok::integer_literal))
        return _errorhandler();
    E = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
    advance();

    return false;
}