#pragma once

#include "llvm/IR/PassManager.h"

namespace mycc::passes {

/// @brief ShuffleBlocksPass is an LLVM function pass that obfuscates control
/// flow by randomising the order of basic blocks inside each function.
///
/// The transformation performs three steps:
///   1. A new entry block ("obf_entry") is prepended to the function body.
///      It contains a single unconditional branch to the original entry block,
///      so semantics are fully preserved.
///   2. All original basic blocks are shuffled into a random order.
///   3. Any block that is missing a terminator (defensive — valid LLVM IR
///      always has one) receives an explicit unconditional branch or
///      an `unreachable` instruction as a fallback.
///
/// The pass is registered under the name "shuffle-blocks" and is also
/// inserted automatically at the OptimizerLast extension point when the
/// plugin is loaded.
struct ShuffleBlocksPass : public llvm::PassInfoMixin<ShuffleBlocksPass> {
    llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM);

    /// @brief Mark the pass as required so the pass manager never skips it.
    static bool isRequired() { return true; }
};

} // namespace mycc::passes
