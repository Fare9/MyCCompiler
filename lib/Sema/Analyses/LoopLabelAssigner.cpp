#include "mycc/Sema/Analyses/LoopLabelAssigner.hpp"
#include "mycc/AST/ASTUtils.hpp"

#include <ranges>

using namespace mycc;

LoopLabelAssigner::LoopLabelAssigner(LabelGenerator& Label, DiagnosticsEngine& Diags, bool avoid_errors) :
    Label(Label), Diags(Diags), avoid_errors(avoid_errors) {
}

void LoopLabelAssigner::run(FunctionDeclaration &F)
{
    // Labels for the breaks
    std::vector<BreakableContext> breakableStack;

    // Traverse all items in the function body
    for (auto &item: F) {
        traverseBlockItem(item, breakableStack);
    }
}

void LoopLabelAssigner::traverseBlockItem(BlockItem &item, std::vector<BreakableContext> &breakableStack) {
    if (std::holds_alternative<Statement *>(item)) {
        traverseStatement(std::get<Statement *>(item), breakableStack);
    }
}

void LoopLabelAssigner::traverseStatement(Statement *stmt, std::vector<BreakableContext> &breakableStack) {
    if (!stmt) return;

    switch (stmt->getKind()) {
        case Statement::SK_While: {
            auto *whileStmt = llvm::cast<WhileStatement>(stmt);
            std::string baseLabel = Label.generateLoopLabel();
            whileStmt->set_label(baseLabel);

            // Push base label onto stack
            BreakableContext ctx;
            ctx.base_label = baseLabel;
            ctx.is_loop = true;
            breakableStack.push_back(ctx);

            // Traverse body
            traverseStatement(whileStmt->getBody(), breakableStack);

            // Pop from stack
            breakableStack.pop_back();
            break;
        }

        case Statement::SK_DoWhile: {
            auto *doWhileStmt = llvm::cast<DoWhileStatement>(stmt);
            std::string baseLabel = Label.generateLoopLabel();
            doWhileStmt->set_label(baseLabel);

            // Push base label onto stack
            BreakableContext ctx;
            ctx.base_label = baseLabel;
            ctx.is_loop = true;
            breakableStack.push_back(ctx);

            // Traverse body
            traverseStatement(doWhileStmt->getBody(), breakableStack);

            // Pop from stack
            breakableStack.pop_back();
            break;
        }

        case Statement::SK_For: {
            auto *forStmt = llvm::cast<ForStatement>(stmt);
            std::string baseLabel = Label.generateLoopLabel();
            forStmt->set_label(baseLabel);

            // Push base label onto stack
            BreakableContext ctx;
            ctx.base_label = baseLabel;
            ctx.is_loop = true;
            breakableStack.push_back(ctx);

            // Traverse body
            traverseStatement(forStmt->getBody(), breakableStack);

            // Pop from stack
            breakableStack.pop_back();
            break;
        }

        case Statement::SK_Switch: {
            auto *switchStmt = llvm::cast<SwitchStatement>(stmt);
            std::string switchLabel = Label.generateSwitchLabel();
            switchStmt->set_break_label(switchLabel + "_end");

            std::set<int64_t> seenCaseValues;
            bool hasDefault = false;
            validateSwitchBody(switchStmt->get_body(), seenCaseValues, hasDefault);

            // Push switch context
            BreakableContext ctx;
            ctx.base_label = switchLabel;
            ctx.is_loop = false;
            breakableStack.push_back(ctx);

            traverseStatement(switchStmt->get_body(), breakableStack);

            breakableStack.pop_back();
            break;
        }
        case Statement::SK_Case: {
            auto *caseStmt = llvm::cast<CaseStatement>(stmt);

            // Check if we're inside any switch by searching the stack
            bool insideSwitch = false;
            for (const auto &ctx: breakableStack) {
                if (!ctx.is_loop) {
                    // Found a switch
                    insideSwitch = true;
                    break;
                }
            }

            if (!insideSwitch) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_case_not_in_switch);
                    exit(1);
                }
            } else {
                std::string caseLabel = Label.generateCaseLabel();
                caseStmt->set_label(caseLabel);
            }
            break;
        }
        case Statement::SK_Default: {
            auto *defaultStmt = llvm::cast<DefaultStatement>(stmt);

            // Check if we're inside any switch by searching the stack
            bool insideSwitch = false;
            for (const auto &ctx: breakableStack) {
                if (!ctx.is_loop) {
                    // Found a switch
                    insideSwitch = true;
                    break;
                }
            }

            if (!insideSwitch) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_default_not_in_switch);
                    exit(1);
                }
            } else {
                std::string defaultLabel = Label.generateDefaultLabel();
                defaultStmt->set_label(defaultLabel);
            }
            break;
        }
        case Statement::SK_Break: {
            auto *breakStmt = llvm::cast<BreakStatement>(stmt);

            if (breakableStack.empty()) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_break_not_in_loop);
                    exit(1);
                }
            } else {
                // Break jumps to the innermost loop or switch
                std::string targetLabel = breakableStack.back().get_break_label();
                breakStmt->set_label(targetLabel);
            }
            break;
        }

        case Statement::SK_Continue: {
            auto *continueStmt = llvm::cast<ContinueStatement>(stmt);

            if (breakableStack.empty()) {
                if (!avoid_errors) {
                    Diags.report(SMLoc(), diag::err_continue_not_in_loop);
                    exit(1);
                }
            } else {
                bool foundLoop = false;
                for (auto &breakable: std::ranges::reverse_view(breakableStack)) {
                    if (breakable.is_loop) {
                        // Continue jumps to continue label of innermost LOOP
                        std::string targetLabel = breakable.get_continue_label();
                        continueStmt->set_label(targetLabel);
                        foundLoop = true;
                        break;
                    }
                }

                if (!foundLoop && !avoid_errors) {
                    Diags.report(SMLoc(), diag::err_continue_not_in_loop);
                    exit(1);
                }
            }
            break;
        }

        case Statement::SK_If: {
            auto *ifStmt = llvm::cast<IfStatement>(stmt);
            traverseStatement(ifStmt->getThenSt(), breakableStack);
            if (ifStmt->getElseSt()) {
                traverseStatement(ifStmt->getElseSt(), breakableStack);
            }
            break;
        }

        case Statement::SK_Compound: {
            auto *compoundStmt = llvm::cast<CompoundStatement>(stmt);
            for (auto &item: *compoundStmt) {
                traverseBlockItem(item, breakableStack);
            }
            break;
        }

        // Other statements (Return, Expression, Null, Label, Goto) don't need processing
        default:
            break;
    }
}


/**
 * @brief Validate the body of a switch statement recursively. These validations
 * are applied according to the C standard.
 *
 * Performs the following validations:
 * 1. Case values must be constant expressions
 * 2. No duplicate case values
 * 3. At most one default case
 * 4. No variable declarations immediately after case/default labels
 *
 * @param body Switch body statement to validate.
 * @param seenCaseValues Set to track and detect duplicate case values.
 * @param hasDefault Flag indicating if a default case has been seen.
 */
void LoopLabelAssigner::validateSwitchBody(Statement *body,
                              std::set<int64_t> &seenCaseValues,
                              bool &hasDefault) {
    if (!body) return;

    switch (body->getKind()) {
        case Statement::SK_Case: {
            const auto *caseStmt = llvm::cast<CaseStatement>(body);

            // First check, case value must be a constant integer
            Expr *value = caseStmt->getValue();
            if (!ASTUtils::isConstantExpression(value)) {
                Diags.report(SMLoc(), diag::err_case_value_not_constant);
                exit(1);
            }

            // Second check, look for duplicated cases
            int64_t caseValue = ASTUtils::evaluateConstantExpression(value);
            if (seenCaseValues.contains(caseValue)) {
                Diags.report(SMLoc(), diag::err_duplicate_case_value, std::to_string(caseValue));
                exit(1);
            }
            seenCaseValues.insert(caseValue);
            break;
        }

        case Statement::SK_Default: {
            // Check 3: It has multiple defaults
            if (hasDefault) {
                Diags.report(SMLoc(), diag::err_multiple_default_in_switch);
                exit(1);
            }
            hasDefault = true;
            break;
        }

        case Statement::SK_Compound: {
            auto *compound = llvm::cast<CompoundStatement>(body);
            Statement *prevStmt = nullptr;

            // Go over each item to validate the body
            for (auto &item: *compound) {
                // Check if current item is a Declaration following case/default
                if (std::holds_alternative<VarDeclaration *>(item)) {
                    if (prevStmt &&
                        (prevStmt->getKind() == Statement::SK_Case ||
                         prevStmt->getKind() == Statement::SK_Default)) {
                        if (!avoid_errors) {
                            Diags.report(SMLoc(), diag::err_declaration_after_case_label);
                            exit(1);
                        }
                    }
                    // Reset prevStmt since a declaration is not a statement
                    prevStmt = nullptr;
                } else if (std::holds_alternative<Statement *>(item)) {
                    Statement *stmt = std::get<Statement *>(item);
                    validateSwitchBody(stmt, seenCaseValues, hasDefault);
                    prevStmt = stmt;
                }
            }
            break;
        }

        // Recursively check nested statements
        case Statement::SK_If: {
            auto *ifStmt = llvm::cast<IfStatement>(body);
            validateSwitchBody(ifStmt->getThenSt(), seenCaseValues, hasDefault);
            if (ifStmt->getElseSt())
                validateSwitchBody(ifStmt->getElseSt(), seenCaseValues, hasDefault);
            break;
        }

        case Statement::SK_While: {
            auto *whileStmt = llvm::cast<WhileStatement>(body);
            validateSwitchBody(whileStmt->getBody(), seenCaseValues, hasDefault);
            break;
        }
        case Statement::SK_DoWhile: {
            auto *doWhileStmt = llvm::cast<DoWhileStatement>(body);
            validateSwitchBody(doWhileStmt->getBody(), seenCaseValues, hasDefault);
            break;
        }
        case Statement::SK_For: {
            auto *forStmt = llvm::cast<ForStatement>(body);
            validateSwitchBody(forStmt->getBody(), seenCaseValues, hasDefault);
            break;
        }
        default:
            break;
    }
}
