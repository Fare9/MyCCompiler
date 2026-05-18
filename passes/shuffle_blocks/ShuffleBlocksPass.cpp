#include "ShuffleBlocksPass.hpp"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

#include <algorithm>
#include <random>
#include <vector>

using namespace llvm;
using namespace mycc::passes;

PreservedAnalyses ShuffleBlocksPass::run(Function &F, FunctionAnalysisManager &) {
    // Nothing to shuffle in declarations or single-block functions
    if (F.isDeclaration() || F.size() < 2)
        return PreservedAnalyses::all();

    // Collect all existing basic blocks
    std::vector<BasicBlock *> Blocks;
    Blocks.reserve(F.size());
    for (auto &BB : F)
        Blocks.push_back(&BB);

    BasicBlock *OriginalEntry = Blocks.front();

    // Step 1 — create a new entry block that jumps to the original entry.
    // Inserted at the very front of the function so it becomes the new entry.
    BasicBlock *NewEntry = BasicBlock::Create(
        F.getContext(), "obf_entry", &F, OriginalEntry);
    BranchInst::Create(OriginalEntry, NewEntry);

    // Step 2 — shuffle the original blocks into a random order
    std::mt19937 RNG{std::random_device{}()};
    std::shuffle(Blocks.begin(), Blocks.end(), RNG);

    // Step 3 — reorder blocks in the function:
    //   NewEntry is already the first block (inserted before OriginalEntry).
    //   Move each shuffled block after the previous one.
    BasicBlock *Prev = NewEntry;
    for (auto *BB : Blocks) {
        BB->moveAfter(Prev);
        Prev = BB;
    }

    // Step 4 — defensive: add a fallback terminator to any block that lacks one.
    // Valid LLVM IR always has terminators, but newly created or split blocks
    // may not. Use direct instruction APIs to avoid IRBuilder's vtable dependency.
    for (auto &BB : F) {
        if (BB.getTerminator() != nullptr)
            continue;

        auto It = BB.getIterator();
        ++It;
        if (It != F.end())
            BranchInst::Create(&*It, &BB);
        else
            new UnreachableInst(BB.getContext(), &BB);
    }

    return PreservedAnalyses::none();
}

// ============================================================
// Plugin entry point — required by LLVM's plugin infrastructure.
// The host process (mycc or opt) calls llvmGetPassPluginInfo()
// after dlopen-ing this .so to register our passes.
// ============================================================

llvm::PassPluginLibraryInfo getShuffleBlocksPassPluginInfo() {
    return {
        LLVM_PLUGIN_API_VERSION, "ShuffleBlocksPass", "v0.1",
        [](PassBuilder &PB) {
            // Registration 1 — explicit pipeline string: "shuffle-blocks"
            // Allows: opt -passes="shuffle-blocks" or pipeline strings.
            PB.registerPipelineParsingCallback(
                [](StringRef Name, FunctionPassManager &FPM,
                   ArrayRef<PassBuilder::PipelineElement>) -> bool {
                    if (Name == "shuffle-blocks") {
                        FPM.addPass(ShuffleBlocksPass());
                        return true;
                    }
                    return false;
                });

            // Registration 2 — automatic insertion at the end of any
            // standard optimisation pipeline (O1/O2/O3/Os/Oz).
            // When the plugin is loaded via --llvm-plugin, this ensures
            // the pass runs without the user needing to name it explicitly.
            PB.registerOptimizerLastEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel) {
                    FunctionPassManager FPM;
                    FPM.addPass(ShuffleBlocksPass());
                    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
                });
        }
    };
}

extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
    return getShuffleBlocksPassPluginInfo();
}
