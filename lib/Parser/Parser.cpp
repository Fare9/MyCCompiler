#include "mycc/Parser/Parser.hpp"

#include "mycc/AST/AST.hpp"

using namespace mycc;

Parser::Parser(Lexer &Lex, Sema &Actions, ASTContext &Context) : Lex(Lex), Actions(Actions), Context(Context) {
}

Program *Parser::parse() {
    Program *p = nullptr;
    parseProgram(p);
    return p;
}

bool Parser::parseProgram(Program *&P) {
    DeclarationList Decls;

    // Enter a global scope for function declarations and global variables
    Actions.enterScope();

    // advance to the first token
    advance();
    // now start consuming declarations (functions or variables)
    while (Tok.isNot(tok::eof)) {
        if (parseDeclaration(Decls)) {
            return true;
        }
    }

    P = Actions.actOnProgramDeclaration(Decls);

    // Exit global scope after all declarations parsed
    Actions.exitScope();
    return false;
}

std::vector specifiers = {
    tok::kw_extern,
    tok::kw_static,
};

std::vector types = {
    tok::kw_int
};

bool Parser::parseDeclarationHeader(std::optional<StorageClass> &storageClass,
                                    Type *&type,
                                    StringRef &name,
                                    SMLoc &loc,
                                    bool allowStorageClass) {
    storageClass = std::nullopt;
    type = nullptr;

    while (Tok.isOneOf(specifiers) || Tok.isOneOf(types)) {
        if (Tok.isOneOf(specifiers)) {
            if (!allowStorageClass && !Actions.is_avoid_errors_active()) {
                getDiagnostics().report(Tok.getLocation(),
                                        diag::err_for_loop_cannot_have_storage_class);
                return true;
            }
            if (Tok.is(tok::kw_extern)) {
                if (storageClass.has_value() && storageClass.value() == StorageClass::SC_Static) {
                    getDiagnostics().report(Tok.getLocation(),
                                            diag::err_declaration_already_static);
                    return true;
                }
                storageClass = StorageClass::SC_Extern;
            } else if (Tok.is(tok::kw_static)) {
                if (storageClass.has_value() && storageClass.value() == StorageClass::SC_Extern) {
                    getDiagnostics().report(Tok.getLocation(),
                                            diag::err_declaration_already_extern);
                    return true;
                }
                storageClass = StorageClass::SC_Static;
            }
        } else if (Tok.is(tok::kw_int)) {
            if (type != nullptr) {
                getDiagnostics().report(Tok.getLocation(),
                                        diag::err_declaration_has_type);
                delete type;
                return true;
            }
            type = new BuiltinType(BuiltinType::Int);
        }
        advance();
    }

    // Expect identifier
    if (expect(tok::identifier))
        return true;

    if (type == nullptr) {
        getDiagnostics().report(Tok.getLocation(),
                                diag::err_declaration_has_no_type,
                                Tok.getIdentifier().str());
        return true;
    }

    name = Tok.getIdentifier();
    loc = Tok.getLocation();
    advance();

    return false;
}

bool Parser::parseDeclaration(DeclarationList &Decls) {
    std::optional<StorageClass> storageClass;
    Type *type = nullptr;
    StringRef name;
    SMLoc loc;

    if (parseDeclarationHeader(storageClass, type, name, loc, true))
        return true;

    // Check if it's a function (has '(') or variable (has '=' or ';')
    if (Tok.is(tok::l_paren)) {
        // Function declaration
        FunctionDeclaration *Func = nullptr;
        if (!parseFunctionRest(Func, loc, name, storageClass))
            return true;

        // Check for duplicate declarations
        bool add_to_decls = true;
        for (const auto &decl: Decls) {
            if (const auto *existing_func = std::get_if<FunctionDeclaration *>(&decl)) {
                if (Func->getName() == (*existing_func)->getName() &&
                    Func->getArgs().size() == (*existing_func)->getArgs().size()) {
                    add_to_decls = false;
                    break;
                }
            }
        }
        if (add_to_decls)
            Decls.emplace_back(Func);
    } else {
        // Variable declaration
        VarDeclaration *Var = nullptr;
        if (!parseGlobalVarRest(Var, loc, name, storageClass))
            return true;

        Decls.emplace_back(Var);
    }
    return false;
}

bool Parser::parseFunctionRest(FunctionDeclaration *&F, SMLoc funcLoc, StringRef funcName,
                               std::optional<StorageClass> storageClass) {
    auto _errorhandler = [this] {
        while (!Tok.is(tok::eof))
            advance();
        return false;
    };

    ArgsList args;
    BlockItems body;
    bool parsedBody = false;

    // consume '('
    if (consume(tok::l_paren))
        return _errorhandler();

    // Enter function-level state (labels, etc.)
    Actions.enterFunction();

    // Enter function scope (for parameters AND body)
    Actions.enterScope();

    // Check for void (no parameters) vs parameter list
    if (Tok.is(tok::kw_void)) {
        // int foo(void) - no parameters
        advance();
    } else if (!Tok.is(tok::r_paren)) {
        // int foo(int x, int y, ...) - has parameters
        if (consume(tok::kw_int))
            return _errorhandler();
        if (expect(tok::identifier))
            return _errorhandler();

        Var *param = Actions.actOnParameterDeclaration(Tok.getLocation(), Tok.getIdentifier());
        if (param)
            args.push_back(param);
        advance();

        while (Tok.is(tok::comma)) {
            advance(); // consume comma
            if (consume(tok::kw_int))
                return _errorhandler();
            if (expect(tok::identifier))
                return _errorhandler();

            param = Actions.actOnParameterDeclaration(Tok.getLocation(), Tok.getIdentifier());
            if (!param)
                return _errorhandler();
            args.push_back(param);
            advance();
        }
    }

    if (consume(tok::r_paren))
        return _errorhandler();

    F = Actions.actOnFunctionDeclaration(funcLoc, funcName, args, storageClass);
    if (!F)
        return _errorhandler();

    if (Tok.is(tok::semi)) {
        advance();
    } else {
        // consume body
        if (consume(tok::l_brace))
            return _errorhandler();

        // Parse statement sequence - fail immediately on error
        if (parseBlock(body))
            return _errorhandler();

        if (consume(tok::r_brace))
            return _errorhandler();

        parsedBody = true;
    }

    Actions.exitFunction();

    Actions.exitScope();

    if (parsedBody) {
        // Check for multiple definitions (redefinition error)
        if (F->hasBody() && !Actions.is_avoid_errors_active()) {
            getDiagnostics().report(funcLoc, diag::err_function_redefinition, funcName);
            return _errorhandler();
        }

        // Update args for the definition (parameter names from definition take precedence)
        F->setArgs(args);
        F->setBody(body);
    }

    // Once we have finished with the function,
    // we can assign the labels to the loop instructions
    // and the break/continue
    Actions.assignLoopLabels(*F);

    return true;
}

bool Parser::parseGlobalVarRest(VarDeclaration *&V, SMLoc loc, StringRef name,
                                std::optional<StorageClass> storageClass) {
    auto _errorhandler = [this] {
        while (!Tok.is(tok::eof))
            advance();
        return false;
    };

    Expr *initExpr = nullptr;

    // Check for optional initializer
    if (Tok.is(tok::equal)) {
        advance();
        if (parseExpr(initExpr, 0))
            return _errorhandler();
    }

    if (consume(tok::semi))
        return _errorhandler();

    // Perform semantic analysis for the global variable declaration
    V = Actions.actOnGlobalVarDeclaration(loc, name, initExpr, storageClass);
    if (!V)
        return _errorhandler();

    return true;
}


bool Parser::parseBlock(BlockItems &Items) {
    while (Tok.isNot(tok::r_brace) && Tok.isNot(tok::eof)) {
        // Check if this is a declaration (starts with type or storage class specifier)
        if (Tok.isOneOf(specifiers) || Tok.isOneOf(types)) {
            std::optional<StorageClass> storageClass;
            Type *type = nullptr;
            StringRef name;
            SMLoc loc;

            if (parseDeclarationHeader(storageClass, type, name, loc, true))
                return true;

            // Check if it's a function (has '(') or variable (has '=' or ';')
            if (Tok.is(tok::l_paren)) {
                // Function declaration in block
                if (parseFunctionDeclarationStmt(Items, loc, name, storageClass))
                    return true;
            } else {
                // Variable declaration
                if (parseVariableDeclInline(Items, loc, name, storageClass))
                    return true;
            }
        } else {
            if (parseStatement(Items))
                return true;
        }
    }
    return false;
}

bool Parser::parseVarDeclaration(BlockItems &Items, const bool allowStorageClass) {
    std::optional<StorageClass> storageClass;
    Type *type = nullptr;
    StringRef name;
    SMLoc loc;

    if (parseDeclarationHeader(storageClass, type, name, loc, allowStorageClass))
        return true;

    // Add variable to scope first (so it can be referenced in initializer)
    if (Actions.actOnVarDeclaration(Items, loc, name, storageClass))
        return true;

    VarDeclaration *decl = std::get<VarDeclaration *>(Items.back());

    // Parse optional initializer (variable now exists in scope)
    Expr *initExpr = nullptr;
    if (Tok.is(tok::equal)) {
        advance();
        if (parseExpr(initExpr))
            return true;
    }

    // Validate and set the initializer
    if (Actions.actOnVarDeclarationInit(decl, initExpr))
        return true;

    return false;
}

bool Parser::parseStatement(BlockItems &Items) {
    if (Tok.is(tok::kw_return)) {
        return parseReturnStmt(Items);
    }
    if (Tok.is(tok::semi)) {
        Actions.actOnNullStatement(Items, Tok.getLocation());
        consume(tok::semi);
        return false;
    }
    if (Tok.is(tok::kw_if)) {
        return parseIfStmt(Items);
    }
    if (Tok.is(tok::l_brace)) {
        return parseCompoundStmt(Items);
    }
    if (Tok.is(tok::kw_goto)) {
        return parseGotoStmt(Items);
    }
    if (Tok.is(tok::kw_while)) {
        return parseWhileStmt(Items);
    }
    if (Tok.is(tok::kw_do)) {
        return parseDoWhileStmt(Items);
    }
    if (Tok.is(tok::kw_for) || Tok.is(tok::kw_tree)) {
        return parseForStmt(Items);
    }
    if (Tok.is(tok::kw_break)) {
        SMLoc Loc = Tok.getLocation();
        if (consume(tok::kw_break))
            return true;
        if (consume(tok::semi))
            return true;
        Actions.actOnBreakStatement(Items, Loc);
        return false;
    }
    if (Tok.is(tok::kw_continue)) {
        SMLoc Loc = Tok.getLocation();
        if (consume(tok::kw_continue))
            return true;
        if (consume(tok::semi))
            return true;
        Actions.actOnContinueStatement(Items, Loc);
        return false;
    }
    if (Tok.is(tok::kw_case)) {
        return parseCaseStatement(Items);
    }
    if (Tok.is(tok::kw_default)) {
        return parseDefaultStatement(Items);
    }
    if (Tok.is(tok::kw_switch)) {
        return parseSwitchStatement(Items);
    }
    if (Tok.is(tok::identifier)) {
        // Peek ahead to see if the next token is a colon
        Token nextTok;
        Lex.peek(nextTok);

        // check if we have `:`, in that case, there's a label
        if (nextTok.is(tok::colon)) {
            StringRef labelName = Tok.getIdentifier();
            SMLoc loc = Tok.getLocation();
            advance(); // consume identifier
            advance(); // consume colon
            Actions.actOnLabelStatement(Items, loc, labelName);

            if (Tok.is(tok::kw_int)) {
                getDiagnostics().report(Tok.getLocation(), diag::err_expected_statement_after_label);
                return true;
            }
            if (Tok.is(tok::r_brace)) {
                getDiagnostics().report(Tok.getLocation(), diag::err_label_cannot_end_block);
                return true;
            }

            return false;
        }

        // if it is not a label, it's a regular identifier, so parse it as an expression
        // (fall through to parseExprStmt)
    }
    if (parseExprStmt(Items))
        return true;
    return false;
}

bool Parser::parseReturnStmt(BlockItems &Items) {
    Expr *E = nullptr;
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

bool Parser::parseExprStmt(BlockItems &Items) {
    Expr *E = nullptr;
    SMLoc Loc = Tok.getLocation();

    // Try to parse expression - let parseExpr handle invalid tokens
    if (parseExpr(E, 0))
        return true;

    if (consume(tok::semi))
        return true;
    Actions.actOnExprStatement(Items, Loc, E);
    return false;
}

bool Parser::parseIfStmt(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();

    Expr *Cond = nullptr;
    BlockItems then_sts;
    BlockItems else_sts;

    if (consume(tok::kw_if))
        return true;
    // Parse expression in between parenthesis "(" <Exp> ")"
    if (consume(tok::l_paren))
        return true;
    if (parseExpr(Cond, 0))
        return true;
    if (consume(tok::r_paren))
        return true;

    // Parse the then statement (handles both single statements and compound statements)
    if (parseStatement(then_sts))
        return true;

    // if there's an else statement
    if (Tok.is(tok::kw_else)) {
        advance();
        if (parseStatement(else_sts))
            return true;
    }

    Statement *then_st = then_sts.empty() ? nullptr : std::get<Statement *>(then_sts.back());
    Statement *else_st = else_sts.empty() ? nullptr : std::get<Statement *>(else_sts.back());

    Actions.actOnIfStatement(Items, Loc, Cond, then_st, else_st);
    return false;
}

bool Parser::parseCompoundStmt(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();

    if (consume(tok::l_brace))
        return true;

    Actions.enterScope();

    bool result = [&]() {
        BlockItems compound_stmts;

        if (parseBlock(compound_stmts))
            return true;

        if (consume(tok::r_brace))
            return true;

        Actions.actOnCompoundStatement(Items, Loc, compound_stmts);
        return false;
    }();

    Actions.exitScope();

    return result;
}

bool Parser::parseGotoStmt(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();
    advance();

    if (expect(tok::identifier))
        return true;
    // get the identifier
    StringRef Identifier = Tok.getIdentifier();
    // consume the identifier token
    advance();
    // generate a Goto statement
    Actions.actOnGotoStatement(Items, Loc, Identifier);

    if (consume(tok::semi))
        return true;

    return false;
}

bool Parser::parseWhileStmt(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();

    Expr *Cond = nullptr;
    Statement *Body = nullptr;

    BlockItems body_sts;

    if (consume(tok::kw_while))
        return true;

    // Parse expression in between parenthesis "(" <Exp> ")"
    if (consume(tok::l_paren))
        return true;
    if (parseExpr(Cond, 0))
        return true;
    if (consume(tok::r_paren))
        return true;

    if (parseStatement(body_sts))
        return true;

    if (!body_sts.empty())
        Body = std::get<Statement *>(body_sts.back());

    Actions.actOnWhileStatement(Items, Loc, Cond, Body);
    return false;
}

bool Parser::parseDoWhileStmt(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();

    Statement *Body = nullptr;
    Expr *Cond = nullptr;

    BlockItems body_sts;

    if (consume(tok::kw_do))
        return true;

    if (parseStatement(body_sts))
        return true;

    if (consume(tok::kw_while))
        return true;

    // Consume the expression from the do/while
    if (consume(tok::l_paren))
        return true;
    if (parseExpr(Cond, 0))
        return true;
    if (consume(tok::r_paren))
        return true;

    if (consume(tok::semi))
        return true;

    if (!body_sts.empty())
        Body = std::get<Statement *>(body_sts.back());

    Actions.actOnDoWhileStatement(Items, Loc, Body, Cond);
    return false;
}

bool Parser::parseForStmt(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();
    ForInit init = std::monostate{};
    Expr *Cond = nullptr;
    Expr *Post = nullptr;
    Statement *Body = nullptr;

    if (consume(tok::kw_for) && consume(tok::kw_tree))
        return true;

    if (consume(tok::l_paren))
        return true;

    // We always enter the scope here, so if any declaration...
    Actions.enterScope();
    // Parse init (Declaration | Expr | nothing)
    if (Tok.isOneOf(specifiers) || Tok.isOneOf(types)) {
        // Use parseVarDeclaration with allowStorageClass=false for for-loops
        BlockItems tempItems;
        if (parseVarDeclaration(tempItems, false))
            return true;
        // We extract the declaration from tempItems
        init = std::get<VarDeclaration *>(tempItems.back());
    } else if (!Tok.is(tok::semi)) {
        // Handle expression: i = 0
        Expr *initExpr = nullptr;
        if (parseExpr(initExpr, 0))
            return true;
        init = initExpr;
    }
    // If it's just ';', init stays as std::monostate{}
    if (consume(tok::semi))
        return true;

    // Now we need to detect if we need to parse a
    // conditional expression
    if (!Tok.is(tok::semi)) {
        if (parseExpr(Cond, 0))
            return true;
    }

    if (consume(tok::semi))
        return true;

    // Now check if we have a post loop
    if (!Tok.is(tok::r_paren)) {
        if (parseExpr(Post, 0))
            return true;
    }

    if (consume(tok::r_paren))
        return true;

    BlockItems body_sts;
    if (parseStatement(body_sts))
        return true;
    if (!body_sts.empty())
        Body = std::get<Statement *>(body_sts.back());

    // Always exit scope since we entered at l_paren
    Actions.exitScope();

    Actions.actOnForStatement(Items, Loc, init, Cond, Post, Body);
    return false;
}

bool Parser::parseDefaultStatement(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();

    if (consume(tok::kw_default))
        return true;

    if (consume(tok::colon))
        return true;

    Actions.actOnDefaultStatement(Items, Loc);
    return false;
}

bool Parser::parseCaseStatement(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();

    if (consume(tok::kw_case))
        return true;

    Expr *CaseExpr = nullptr;
    if (parseExpr(CaseExpr, 0))
        return true;

    if (consume(tok::colon))
        return true;

    Actions.actOnCaseStatement(Items, Loc, CaseExpr);
    return false;
}

bool Parser::parseSwitchStatement(BlockItems &Items) {
    SMLoc Loc = Tok.getLocation();

    Expr *Cond = nullptr;
    Statement *Body = nullptr;

    BlockItems body_sts;

    if (consume(tok::kw_switch))
        return true;

    // Parse the conditional expression "(" <Exp> ")"
    if (consume(tok::l_paren))
        return true;
    if (parseExpr(Cond, 0))
        return true;
    if (consume(tok::r_paren))
        return true;

    // Parse the body (this will typically be a compound statement with case labels)
    if (parseStatement(body_sts))
        return true;
    if (!body_sts.empty())
        Body = std::get<Statement *>(body_sts.back());

    Actions.actOnSwitchStatement(Items, Loc, Cond, Body);
    return false;
}

bool Parser::parseFunctionDeclarationStmt(BlockItems &Items, SMLoc Loc, StringRef Name,
                                          std::optional<StorageClass> storageClass) {
    ArgsList args;
    BlockItems body;
    bool parsedBody = false;

    if (consume(tok::l_paren))
        return true;

    Actions.enterScope();

    if (Tok.is(tok::kw_void)) {
        advance();
    } else if (!Tok.is(tok::r_paren)) {
        // int foo(int x, int y, ...) - has parameters
        if (consume(tok::kw_int))
            return true;
        if (expect(tok::identifier))
            return true;

        Var *param = Actions.actOnParameterDeclaration(Tok.getLocation(), Tok.getIdentifier());
        if (param)
            args.push_back(param);
        advance();

        while (Tok.is(tok::comma)) {
            advance(); // consume comma
            if (consume(tok::kw_int))
                return true;
            if (expect(tok::identifier))
                return true;

            param = Actions.actOnParameterDeclaration(Tok.getLocation(), Tok.getIdentifier());
            if (param)
                args.push_back(param);
            advance();
        }
    }

    if (consume(tok::r_paren))
        return true;

    // Create a Function object with no body (declaration only)
    FunctionDeclaration *F = Actions.actOnFunctionDeclaration(Loc, Name, args, storageClass);
    if (!F)
        return true;

    if (Tok.is(tok::semi)) {
        advance();
    } else {
        if (!Actions.is_avoid_errors_active())
            return true;
        // consume body
        if (consume(tok::l_brace))
            return true;
        // Parse statement sequence - fail immediately on error
        if (parseBlock(body))
            return true;
        if (consume(tok::r_brace))
            return true;

        parsedBody = true;
    }

    Actions.exitScope();

    if (parsedBody) {
        // Check for multiple definitions (redefinition error)
        if (F->hasBody()) {
            getDiagnostics().report(Loc, diag::err_function_redefinition, Name);
            return true;
        }

        // Update args for the definition (parameter names from definition take precedence)
        F->setArgs(args);
        F->setBody(body);
    }

    Items.emplace_back(F);
    return false;
}

bool Parser::parseVariableDeclInline(BlockItems &Items, SMLoc Loc, StringRef Name,
                                     std::optional<StorageClass> storageClass) {
    // We've already consumed 'int' and identifier
    // Now at '=' or ';'

    // Add variable to scope first (so it can be referenced in initializer)
    if (Actions.actOnVarDeclaration(Items, Loc, Name, storageClass))
        return true;

    VarDeclaration *decl = std::get<VarDeclaration *>(Items.back());

    // Parse optional initializer (variable now exists in scope)
    Expr *initExpr = nullptr;
    if (Tok.is(tok::equal)) {
        advance();
        if (parseExpr(initExpr))
            return true;
    }

    if (consume(tok::semi))
        return true;

    // Validate and set the initializer
    if (Actions.actOnVarDeclarationInit(decl, initExpr))
        return true;

    return false;
}


namespace {
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
        // ? expression for ternary operators
        {BinaryOperator::BinaryOpKind::Bok_Interrogation, 3},
        // = for assignments
        {BinaryOperator::BinaryOpKind::Bok_Assign, 1},
    };
}

static constexpr tok::TokenKind exprOps[] = {
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
    tok::compoundshr,
    // Chapter 6
    tok::interrogation
};

static constexpr tok::TokenKind compoundExprs[] = {
    tok::compoundadd,
    tok::compoundsub,
    tok::compoundmul,
    tok::compounddiv,
    tok::compoundrem,
    tok::compoundand,
    tok::compoundor,
    tok::compoundxor,
    tok::compoundshl,
    tok::compoundshr
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
        return _errorhandler();

    while (Tok.isOneOf(exprOps)) {
        // Compound statements
        if (Tok.isOneOf(compoundExprs)) {
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
        // Assignment operator
        else if (Tok.is(tok::equal)) {
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
        // ternary operator
        else if (Tok.is(tok::interrogation)) {
            SMLoc Loc = Tok.getLocation();
            BinaryOperator::BinaryOpKind Kind = BinaryOperator::BinaryOpKind::Bok_Interrogation;
            int precedence = binary_operators_precedence[Kind];
            if (precedence < min_precedence) break;

            advance(); // consume '?'

            Expr *middle, *right;
            // first we parse a middle expression
            parseMiddle(middle);
            // then we parse the right one after ":"
            parseExpr(right, precedence);

            left = Actions.actOnTernaryOperator(Loc, left, middle, right);
        }
        // any other binary operator
        else {
            BinaryOperator::BinaryOpKind Kind = parseBinOp(Tok);
            int precedence = binary_operators_precedence[Kind];
            if (precedence < min_precedence) break;
            SMLoc Loc = Tok.getLocation();
            advance();
            parseExpr(right, precedence + 1);
            left = Actions.actOnBinaryOperator(Loc, Kind, left, right);
        }
    }

    E = left;

    return false;
}

bool Parser::parseMiddle(Expr *&Middle) {
    if (parseExpr(Middle, 0))
        return true;
    if (consume(tok::colon))
        return true;
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
    // the factor is an integer
    if (Tok.is(tok::integer_literal)) {
        E = Actions.actOnIntegerLiteral(Tok.getLocation(), Tok.getLiteralData());
        advance();
    }
    // the factor is an identifier (it can contain postfix operator)
    else if (Tok.is(tok::identifier)) {
        StringRef name = Tok.getIdentifier();
        SMLoc Loc = Tok.getLocation();
        advance();

        // First we check if there's a parentheses
        // in that case, it's a function call
        if (Tok.is(tok::l_paren)) {
            advance(); // consume '('
            ExprList args;

            if (!Tok.is(tok::r_paren)) {
                // check we are not in the end of parameters
                Expr *arg = nullptr;
                if (parseExpr(arg, 0))
                    return _errorhandler();
                args.push_back(arg);

                while (Tok.is(tok::comma)) {
                    advance();
                    if (parseExpr(arg, 0))
                        return _errorhandler();
                    args.push_back(arg);
                }
            }
            if (consume(tok::r_paren))
                return _errorhandler();

            E = Actions.actOnFunctionCallOperator(Loc, name, args);
            if (!E)
                return true;
        } else {
            E = Actions.actOnIdentifier(Loc, name);
            if (!E)
                return true;

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
    }

    // Look for unary operators or for prefix operators
    else if
    (Tok
        .
        isOneOf(tok::minus, tok::tilde, tok::exclaim, tok::increment, tok::decrement)
    ) {
        tok::TokenKind OpKind = Tok.getKind();
        SMLoc OpLoc = Tok.getLocation();
        advance();
        Expr *internalExpr = nullptr;
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
    // Parentheses expression (<expr>)
    else if
    (Tok
        .
        is(tok::l_paren)
    ) {
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
    } else
        return
                _errorhandler();

    return
            false;
}

int Parser::get_operator_kind_by_expr(Expr *expr) {
    if (BinaryOperator *binOp = reinterpret_cast<BinaryOperator *>(expr)) {
        return binary_operators_precedence[binOp->getOperatorKind()];
    }
    if (AssignmentOperator *assignOp = reinterpret_cast<AssignmentOperator *>(expr)) {
        return binary_operators_precedence[BinaryOperator::BinaryOpKind::Bok_Assign];
    }
    if (ConditionalExpr *condExpr = reinterpret_cast<ConditionalExpr *>(expr)) {
        return binary_operators_precedence[BinaryOperator::BinaryOpKind::Bok_Interrogation];
    }
    return 0;
}

BinaryOperator::BinaryOpKind Parser::parseBinOp(Token &Tok) {
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
