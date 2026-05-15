#include <mycc/Opt/LLVMOptimizer.hpp>

// Plugin loading and error handling
#include "llvm/Support/Error.h"

// Analysis managers
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/IR/PassManager.h"

// Pass infrastructure
#include "llvm/Passes/PassPlugin.h"

#include <filesystem>

using namespace mycc::opt;

llvm::OptimizationLevel LLVMOptimizer::toLLVMLevel(OptLevel optLevel) {
    switch (optLevel) {
        case OptLevel::O0:
            return llvm::OptimizationLevel::O0;
        case OptLevel::O1:
            return llvm::OptimizationLevel::O1;
        case OptLevel::O2:
            return llvm::OptimizationLevel::O2;
        case OptLevel::O3:
            return llvm::OptimizationLevel::O3;
        case OptLevel::Os:
            return llvm::OptimizationLevel::Os;
        case OptLevel::Oz:
            return llvm::OptimizationLevel::Oz;
        default:
            llvm_unreachable("Unknown Optimization Level");
    }
}

LLVMOptimizer::LLVMOptimizer(Config cfg) : Cfg(std::move(cfg)) {
    // Check first for every path to exists
    for (const auto& file : Cfg.pluginPaths) {
        auto Plugin = llvm::PassPlugin::Load(file);
        if (!Plugin) {
            PluginLoadError = llvm::toString(Plugin.takeError());
            PluginsLoaded = false;
            return;
        }
        // Success, register the pass builder callback
        Plugin->registerPassBuilderCallbacks(Builder);
    }
}

bool LLVMOptimizer::optimize(llvm::Module& M, std::string& errMsg) {
    if (!PluginsLoaded) {
        errMsg = PluginLoadError;
        return false;
    }

    // Create the four analysis manager
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    // Cross-register the analysis manager via Builder
    Builder.registerModuleAnalyses(MAM);
    Builder.registerCGSCCAnalyses(CGAM);
    Builder.registerFunctionAnalyses(FAM);
    Builder.registerLoopAnalyses(LAM);
    Builder.crossRegisterProxies(LAM, FAM, CGAM,  MAM);

    llvm::ModulePassManager MPM = Builder.buildPerModuleDefaultPipeline(toLLVMLevel(Cfg.optLevel));
    MPM.run(M, MAM);
    return true;
}
