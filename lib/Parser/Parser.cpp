
#include "mycc/Parser/Parser.hpp"

using namespace mycc;

Parser::Parser(Lexer &Lex, Sema &Actions) : Lex(Lex), Actions(Actions) {
}

std::unique_ptr<Program> Parser::parse() {
    Program * p = nullptr;
    parseProgram(p);
    return std::unique_ptr<Program>(p);
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
    if (parseExpr(E, 0))
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

std::unordered_map<BinaryOperator::BinaryOpKind, int> binary_operators_precedence = {
        {BinaryOperator::BinaryOpKind::BoK_Multiply, 50},
        {BinaryOperator::BinaryOpKind::BoK_Divide, 50},
        {BinaryOperator::BinaryOpKind::BoK_Remainder, 50},
        {BinaryOperator::BinaryOpKind::BoK_Add, 45},
        {BinaryOperator::BinaryOpKind::BoK_Subtract, 45}
};

bool Parser::parseExpr(Expr *&E, int min_precedence) {
    auto _errorhandler = [this] {
        while (!Tok.is(tok::r_brace)) {
            advance();
            if (Tok.is(tok::eof))
                return true;
        }
        return false;
    };

    // We will modify the way we parse expressions to
    // provide support for binary expressions. We will
    // use Precedence climbing together with Recursive
    // Descent.
    Expr *left;
    Expr *right;

    // Parse as a left part a factor
    parseFactor(left);

    while (Tok.isOneOf(tok::plus, tok::minus, tok::star, tok::slash, tok::percent)) {
        BinaryOperator::BinaryOpKind Kind = parseBinOp(Tok);
        int precedence = binary_operators_precedence[Kind];
        if (precedence < min_precedence) break;
        SMLoc Loc = Tok.getLocation();
        advance();
        parseExpr(right, precedence+1);
        left = Actions.actOnBinaryOperator(Loc, Kind, left, right);
    }

    E = left;

    return false;
}

bool Parser::parseFactor(Expr *&E) {
    auto _errorhandler = [this] {
        while (!Tok.is(tok::r_brace)) {
            advance();
            if (Tok.is(tok::eof))
                return true;
        }
        return false;
    };

    if (Tok.is(tok::integer_literal)) {
        E = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
        advance();
    }
    else if (Tok.isOneOf(tok::minus, tok::tilde)) {
        tok::TokenKind OpKind = Tok.getKind();
        SMLoc OpLoc = Tok.getLocation();
        advance();
        Expr * internalExpr = nullptr;
        if (parseFactor(internalExpr))
            return _errorhandler();
        if (internalExpr == nullptr)
            return _errorhandler();
        if (OpKind == tok::minus)
            E = Actions.actOnUnaryOperator(OpLoc, UnaryOperator::UnaryOperatorKind::UopK_Negate, internalExpr);
        else if (OpKind == tok::tilde)
            E = Actions.actOnUnaryOperator(OpLoc, UnaryOperator::UnaryOperatorKind::UopK_Complement, internalExpr);
    }
    else if (Tok.is(tok::l_paren)) {
        advance();
        if (parseExpr(E, 0))
            return _errorhandler();
        if (consume(tok::r_paren))
            return _errorhandler();
    }
    else
        return _errorhandler();
    return false;
}

BinaryOperator::BinaryOpKind Parser::parseBinOp(Token& Tok) {
    switch (Tok.getKind()) {
        case tok::minus:
            return BinaryOperator::BinaryOpKind::BoK_Subtract;
        case tok::plus:
            return BinaryOperator::BinaryOpKind::BoK_Add;
        case tok::star:
            return BinaryOperator::BinaryOpKind::BoK_Multiply;
        case tok::slash:
            return BinaryOperator::BinaryOpKind::BoK_Divide;
        case tok::percent:
            return BinaryOperator::BinaryOpKind::BoK_Remainder;
        default:
            return BinaryOperator::BinaryOpKind::BoK_None;
    }
}