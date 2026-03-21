#pragma once
#include <set>

#include "mycc/Sema/Analyses/BreakableContext.hpp"
#include "mycc/Sema/Analyses/LabelGenerator.hpp"
#include "mycc/Basic/Diagnostic.hpp"
#include "mycc/AST/AST.hpp"

namespace mycc {
    class LoopLabelAssigner {
        LabelGenerator& Label;
        DiagnosticsEngine& Diags;
        bool avoid_errors;

        /**
         * @brief From what a block item can be, just analyze those that are statements, this analysis
         * is used to assign loop labels.
         *
         * @param item a block item reference, only handle those that are statements.
         * @param breakableStack vector containing all the break statements information.
         */
        void traverseBlockItem(BlockItem &item, std::vector<BreakableContext> &breakableStack);

        /**
         * @brief Recursively traverse a statement and assign labels for control flow.
         *
         * This method handles:
         * - Loops (while, do-while, for): assigns base labels and processes break/continue
         * - Switch statements: assigns break label and validates cases
         * - Break statements: links to innermost breakable context
         * - Continue statements: links to innermost loop context
         * - Compound and conditional statements: recursively processes children
         *
         * @param stmt Statement to traverse.
         * @param breakableStack Stack of enclosing breakable contexts for break/continue resolution.
         */
        void traverseStatement(Statement *stmt, std::vector<BreakableContext> &breakableStack);

        void validateSwitchBody(Statement *body,
                              std::set<int64_t> &seenCaseValues,
                              bool &hasDefault);

    public:
        LoopLabelAssigner(LabelGenerator& Label, DiagnosticsEngine& Diags, bool avoid_errors);

        /**
         * @brief Assign unique labels to all loops, breaks, and continues in a function.
         *
         * This method traverses the function body and assigns labels to:
         * - Loop statements (while, do-while, for) for break and continue targets
         * - Switch statements for break targets
         * - Break and continue statements to reference their appropriate targets
         *
         * @param F The function to process.
         */
        void run(FunctionDeclaration &F);
    };
}
