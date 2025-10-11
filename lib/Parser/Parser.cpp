
#include "mycc/Parser/Parser.hpp"

#include "mycc/AST/AST.hpp"
#include "mycc/AST/AST.hpp"

using namespace mycc;

Parser::Parser(Lexer &Lex, Sema &Actions, ASTContext &Context) : Lex(Lex), Actions(Actions), Context(Context) {
}

Program* Parser::parse() {
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
        }
        Funcs.push_back(Func);
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
    BlockItems body;
    if (consume(tok::l_brace))
        return _errorhandler();
    Actions.enterScope();
    // Parse statement sequence - fail immediately on error
    if (parseBlock(body))
        return _errorhandler();
    Actions.exitScope();
    if (consume(tok::r_brace))
        return _errorhandler();
    F->setBody(body);

    return true;
}

bool Parser::parseBlock(BlockItems& Items) {
    while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
        if (Tok.is(tok::kw_int)) {
            if (parseDeclaration(Items))
                return true;
        }
        else {
            if (parseStatement(Items))
                return true;
        }
    }
    return false;
}

bool Parser::parseDeclaration(BlockItems& Items) {
    if (Tok.is(tok::kw_int)) {
        SMLoc Loc = Tok.getLocation();
        // type is correct, advance it
        advance();
        if (expect(tok::identifier))
            return true;
        StringRef var = Tok.getIdentifier();
        advance();

        // We generate the declaration first, so the variable
        // exists in the scope
        if (Actions.actOnVarDeclaration(Items, Loc, var))
            return true;
        // now we can generate the expression,
        // if the variable is used in the declaration
        // this is compliant with the standard, but it
        // is an undefined behavior.
        Declaration * decl = std::get<Declaration*>(Items.back());
        Expr * exp = nullptr;
        // the assignment to the declaration is
        // optional
        if (Tok.is(tok::equal)) {
            advance();
            if (parseExpr(exp))
                return true;
        }
        if (consume(tok::semi))
            return true;

        decl->setExpr(exp);
        return false;
    }

    return true;
}

bool Parser::parseStatement(BlockItems& Items) {

    if (Tok.is(tok::kw_return)) {
        if (parseReturnStmt(Items))
            return true;
        return false;
    } else if (Tok.is(tok::semi)) {
        Actions.actOnNullStatement(Items, Tok.getLocation());
        consume(tok::semi);
        return false;
    } else {
        if (parseExprStmt(Items))
            return true;
        return false;
    }

    return false;
}

bool Parser::parseReturnStmt(BlockItems& Items) {

    Expr * E = nullptr;
    SMLoc Loc = Tok.getLocation();
    if (consume(tok::kw_return))
        return true;

    // Try to parse expression - let parseExpr handle invalid tokens
    if (parseExpr(E, 0))
        return true;

    if (consume(tok::semi))
        return true;
    Actions.actOnReturnStatement(Items, Loc, E);
    return false;
}

bool Parser::parseExprStmt(BlockItems& Items) {
    Expr * E = nullptr;
    SMLoc Loc = Tok.getLocation();

    // Try to parse expression - let parseExpr handle invalid tokens
    if (parseExpr(E, 0))
        return true;

    if (consume(tok::semi))
        return true;
    Actions.actOnExprStatement(Items, Loc, E);
    return false;
}

std::unordered_map<BinaryOperator::BinaryOpKind, int> binary_operators_precedence = {
{BinaryOperator::BinaryOpKind::BoK_Multiply, 50},
{BinaryOperator::BinaryOpKind::BoK_Divide, 50},
{BinaryOperator::BinaryOpKind::BoK_Remainder, 50},
{BinaryOperator::BinaryOpKind::BoK_Add, 45},
{BinaryOperator::BinaryOpKind::BoK_Subtract, 45},
   // Bitwise shift operators (precedence 5 in C standard)
{BinaryOperator::BinaryOpKind::BoK_LeftShift, 40},
{BinaryOperator::BinaryOpKind::BoK_RightShift, 40},
   // minor than, minor equal, greater than, greater equal
    {BinaryOperator::BinaryOpKind::BoK_LowerThan, 35},
{BinaryOperator::BinaryOpKind::BoK_LowerEqual, 35},
{BinaryOperator::BinaryOpKind::BoK_GreaterThan, 35},
{BinaryOperator::BinaryOpKind::BoK_GreaterEqual, 35},
{BinaryOperator::BinaryOpKind::Bok_Equal, 30},
{BinaryOperator::BinaryOpKind::Bok_NotEqual, 30},
  // Relational operators would be ~35 (precedence 6-7)
  // Equality operators would be ~30 (precedence 7)
  // Bitwise AND (precedence 8 in C standard)
{BinaryOperator::BinaryOpKind::BoK_BitwiseAnd, 25},
  // Bitwise XOR (precedence 9 in C standard)
{BinaryOperator::BinaryOpKind::BoK_BitwiseXor, 20},
  // Bitwise OR (precedence 10 in C standard)
{BinaryOperator::BinaryOpKind::BoK_BitwiseOr, 15},
    // Logical AND and OR
{BinaryOperator::BinaryOpKind::Bok_And, 10},
{BinaryOperator::BinaryOpKind::Bok_Or, 5},
{BinaryOperator::BinaryOpKind::Bok_Assign, 1},
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
    if (parseFactor(left))
        _errorhandler();

    while (Tok.isOneOf(
            // Chapter 3
            tok::plus,
            tok::minus,
            tok::lessless,
            tok::greatergreater,
            tok::star,
            tok::slash,
            tok::percent,
            tok::amp,
            tok::caret,
            tok::pipe,
            // Chapter 4
            tok::less,
            tok::greater,
            tok::lessequal,
            tok::greaterequal,
            tok::equalequal,
            tok::exclaimequal,
            tok::ampamp,
            tok::pipepipe,
            // Chapter 5
            tok::equal,
            tok::compoundadd,
            tok::compoundsub,
            tok::compoundmul,
            tok::compounddiv,
            tok::compoundrem,
            tok::compoundand,
            tok::compoundor,
            tok::compoundxor,
            tok::compoundshl,
            tok::compoundshr)) {
        if (Tok.isOneOf(tok::compoundadd,
            tok::compoundsub,
            tok::compoundmul,
            tok::compounddiv,
            tok::compoundrem,
            tok::compoundand,
            tok::compoundor,
            tok::compoundxor,
            tok::compoundshl,
            tok::compoundshr)) {
            tok::TokenKind compoundToken = Tok.getKind();
            BinaryOperator::BinaryOpKind Kind = BinaryOperator::BinaryOpKind::Bok_Assign;
            int precedence = binary_operators_precedence[Kind];
            if (precedence < min_precedence) break;
            SMLoc Loc = Tok.getLocation();
            // consume the compound token
            advance();
            // parse the expression
            // now parse the expression, this time
            // we do not add +1 so we can have right-precedence
            // in opposite to parsing a binary expression
            parseExpr(right, precedence);
            Expr *tempResult = nullptr;
            if (compoundToken == tok::compoundadd) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_Add, left, right);
            } else if (compoundToken == tok::compoundsub) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_Subtract, left, right);
            } else if (compoundToken == tok::compoundmul) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_Multiply, left, right);
            } else if (compoundToken == tok::compounddiv) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_Divide, left, right);
            } else if (compoundToken == tok::compoundrem) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_Remainder, left, right);
            } else if (compoundToken == tok::compoundand) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_BitwiseAnd, left, right);
            } else if (compoundToken == tok::compoundor) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_BitwiseOr, left, right);
            } else if (compoundToken == tok::compoundxor) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_BitwiseXor, left, right);
            } else if (compoundToken == tok::compoundshl) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_LeftShift, left, right);
            } else if (compoundToken == tok::compoundshr) {
                tempResult = Actions.actOnBinaryOperator(Loc, BinaryOperator::BoK_RightShift, left, right);
            }
            // Final assignment step: left = tempResult
            left = Actions.actOnAssignment(Loc, left, tempResult);
            if (left == nullptr)
                _errorhandler();

        }
        else if (Tok.is(tok::equal))
        {
            BinaryOperator::BinaryOpKind Kind = BinaryOperator::BinaryOpKind::Bok_Assign;
            int precedence = binary_operators_precedence[Kind];
            if (precedence < min_precedence) break;

            SMLoc Loc = Tok.getLocation();
            // consume '=' token
            advance();
            // now parse the expression, this time
            // we do not add +1 so we can have right-precedence
            // in opposite to parsing a binary expression
            parseExpr(right, precedence);
            left = Actions.actOnAssignment(Loc, left, right);
            if (left == nullptr)
                _errorhandler();
        }
        else
        {
            BinaryOperator::BinaryOpKind Kind = parseBinOp(Tok);
            int precedence = binary_operators_precedence[Kind];
            if (precedence < min_precedence) break;
            SMLoc Loc = Tok.getLocation();
            advance();
            parseExpr(right, precedence+1);
            left = Actions.actOnBinaryOperator(Loc, Kind, left, right);
        }
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
    else if (Tok.is(tok::identifier)) {
        E = Actions.actOnIdentifier(Tok.getLocation(), Tok.getIdentifier());
        if (!E)
            return true;
        advance();

        // Check for postfix operators
        if (Tok.isOneOf(tok::increment, tok::decrement)) {
            tok::TokenKind OpKind = Tok.getKind();
            SMLoc OpLoc = Tok.getLocation();
            advance();
            if (OpKind == tok::increment)
                E = Actions.actOnPostfixOperator(OpLoc, PostfixOperator::PostfixOpKind::POK_PostIncrement, E);
            else if (OpKind == tok::decrement)
                E = Actions.actOnPostfixOperator(OpLoc, PostfixOperator::PostfixOpKind::POK_PostDecrement, E);
        }
    }
    else if (Tok.isOneOf(tok::minus, tok::tilde, tok::exclaim, tok::increment, tok::decrement)) {
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
        else if (OpKind == tok::exclaim)
            E = Actions.actOnUnaryOperator(OpLoc, UnaryOperator::UnaryOperatorKind::UopK_Not, internalExpr);
        else if (OpKind == tok::increment)
            E = Actions.actOnPrefixOperator(OpLoc, PrefixOperator::PrefixOpKind::POK_PreIncrement, internalExpr);
        else if (OpKind == tok::decrement)
            E = Actions.actOnPrefixOperator(OpLoc, PrefixOperator::PrefixOpKind::POK_PreDecrement, internalExpr);
    }
    else if (Tok.is(tok::l_paren)) {
        advance();
        if (parseExpr(E, 0))
            return _errorhandler();
        if (consume(tok::r_paren))
            return _errorhandler();

        // Check for postfix operators after parenthesized expressions
        if (Tok.isOneOf(tok::increment, tok::decrement)) {
            tok::TokenKind OpKind = Tok.getKind();
            SMLoc OpLoc = Tok.getLocation();
            advance();
            if (OpKind == tok::increment)
                E = Actions.actOnPostfixOperator(OpLoc, PostfixOperator::PostfixOpKind::POK_PostIncrement, E);
            else if (OpKind == tok::decrement)
                E = Actions.actOnPostfixOperator(OpLoc, PostfixOperator::PostfixOpKind::POK_PostDecrement, E);
        }
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
        case tok::lessless:
            return BinaryOperator::BinaryOpKind::BoK_LeftShift;
        case tok::greatergreater:
            return BinaryOperator::BinaryOpKind::BoK_RightShift;
        case tok::amp:
            return BinaryOperator::BinaryOpKind::BoK_BitwiseAnd;
        case tok::caret:
            return BinaryOperator::BinaryOpKind::BoK_BitwiseXor;
        case tok::pipe:
            return BinaryOperator::BinaryOpKind::BoK_BitwiseOr;
        case tok::less:
            return BinaryOperator::BinaryOpKind::BoK_LowerThan;
        case tok::lessequal:
            return BinaryOperator::BinaryOpKind::BoK_LowerEqual;
        case tok::greater:
            return BinaryOperator::BinaryOpKind::BoK_GreaterThan;
        case tok::greaterequal:
            return BinaryOperator::BinaryOpKind::BoK_GreaterEqual;
        case tok::equalequal:
            return BinaryOperator::BinaryOpKind::Bok_Equal;
        case tok::exclaimequal:
            return BinaryOperator::BinaryOpKind::Bok_NotEqual;
        case tok::ampamp:
            return BinaryOperator::BinaryOpKind::Bok_And;
        case tok::pipepipe:
            return BinaryOperator::BinaryOpKind::Bok_Or;
        default:
            return BinaryOperator::BinaryOpKind::BoK_None;
    }
}