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
class Sema {
    friend class EnterDeclScope;

    DiagnosticsEngine &Diags;
    ASTContext &Context;
    Scope *CurrentScope;
    bool avoid_errors = false;

    // Variable name mapping: original_name -> stack of unique_names
    StringMap<std::vector<std::string>> VariableNameStacks;

    // Counter for generating unique variable names
    unsigned int VariableCounter;

    // Set that contains for a method the labels
    std::set<StringRef> FunctionLabels;
    // Set that contains all the jumped labels by Goto
    std::set<StringRef> GotoLabels;

    // Counter for generating unique loop labels
    unsigned int LoopLabelCounter = 0;

    // Helper methods for variable name management
    std::string generateUniqueVarName(StringRef originalName);
    void pushVariableName(StringRef originalName, const std::string& uniqueName);
    std::string getCurrentUniqueVarName(StringRef originalName);
    void popVariablesFromScope(const std::vector<std::string>& declaredVars);

    // Loop label assignment helpers
    std::string generateLoopLabel();
    void traverseStatement(Statement* stmt, std::vector<std::string>& loopStack);
    void traverseBlockItem(BlockItem& item, std::vector<std::string>& loopStack);

    // Final passes from Semantic Analysis
    void checkGotoLabelsCorrectlyPointToFunction();

public:
    explicit Sema(DiagnosticsEngine &Diags, ASTContext &Context) : Diags(Diags), Context(Context) {
        initialize();
    }

    void avoidErrors() {
        avoid_errors = true;
    }

    void initialize();

    void enterFunction();
    void exitFunction();
    void enterScope();
    void exitScope();

    void assignLoopLabels(Function& F);

    Program * actOnProgramDeclaration(FuncList &Funcs);
    Function * actOnFunctionDeclaration(SMLoc Loc, StringRef Name);

    bool actOnVarDeclaration(BlockItems& Items, SMLoc Loc, StringRef Name);

    void actOnReturnStatement(BlockItems& Items, SMLoc Loc, Expr *RetVal);
    void actOnNullStatement(BlockItems& Items, SMLoc Loc);
    void actOnExprStatement(BlockItems& Items, SMLoc Loc, Expr *Expr);
    void actOnIfStatement(BlockItems& Items, SMLoc Loc, Expr *Cond, Statement *then_st, Statement *else_st);
    void actOnCompoundStatement(BlockItems& Items, SMLoc Loc, BlockItems& compoundStatement);
    void actOnLabelStatement(BlockItems& Items, SMLoc Loc, StringRef Label);
    void actOnGotoStatement(BlockItems& Items, SMLoc Loc, StringRef Label);
    void actOnWhileStatement(BlockItems& Items, SMLoc Loc, Expr *Cond, Statement *Body);
    void actOnDoWhileStatement(BlockItems& Items, SMLoc Loc, Statement *Body, Expr *Cond);
    void actOnForStatement(BlockItems& Items, SMLoc Loc, ForInit& Init, Expr *Cond, Expr *Post, Statement *Body);
    void actOnBreakStatement(BlockItems& Items, SMLoc Loc);
    void actOnContinueStatement(BlockItems& Items, SMLoc Loc);

    IntegerLiteral* actOnIntegerLiteral(SMLoc Loc, StringRef Literal);
    UnaryOperator* actOnUnaryOperator(SMLoc, UnaryOperator::UnaryOperatorKind Kind, Expr* expr);
    BinaryOperator* actOnBinaryOperator(SMLoc, BinaryOperator::BinaryOpKind Kind, Expr* left, Expr* right);
    AssignmentOperator* actOnAssignment(SMLoc, Expr* left, Expr* right);
    PrefixOperator* actOnPrefixOperator(SMLoc, PrefixOperator::PrefixOpKind Kind, Expr* expr);
    PostfixOperator* actOnPostfixOperator(SMLoc, PostfixOperator::PostfixOpKind Kind, Expr* expr);
    Var* actOnIdentifier(SMLoc, StringRef Name);
    ConditionalExpr * actOnTernaryOperator(SMLoc, Expr* left, Expr* middle, Expr* right);
};

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