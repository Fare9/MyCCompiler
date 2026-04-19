#pragma once

#include <llvm/IR/Module.h>
#include <llvm/Passes/PassBuilder.h>

#include <vector>
#include <string>

namespace mycc::opt {
    /**
     * This class provides access to different optimizations from
     * LLVM. The idea behind this class is also allowing a user to
     * provide its own shared objects with optimizations.
     */
    class LLVMOptimizer {
    public:
        // Public definitions we will need to allow the command line
        // tool to configure the optimizer.

        /// @brief OptLevel represent the different levels of optimization
        /// present in LLVM
        enum class OptLevel { O0, O1, O2, O3, Os, Oz };

        /// @brief public configuration structure that contains the optimization
        /// level to run with the optimizer and the path to user provided plugins
        struct Config {
            OptLevel optLevel = OptLevel::O0;       // OptLevel by default 0
            std::vector<std::string> pluginPaths;   // Path to user provided plugins
        };

        explicit LLVMOptimizer(Config cfg);

        /**
         * Main function from the optimizer, it will transform the
         * provided module, running the provided configuration.
         * @param M Module where to run the optimizations
         * @param errMsg message error in case there was an error
         * @return boolean indicating if there was an error
         */
        bool optimize(llvm::Module& M, std::string& errMsg);

    private:
        Config Cfg;
        llvm::PassBuilder Builder;
        bool PluginsLoaded = true;
        std::string PluginLoadError;

        /**
         * Transform the provided level of optimization following the
         * configuration of this pass manager, to one from LLVM.
         * @param optLevel the optimization level to transform
         * @return an optimization level from LLVM.
         */
        static llvm::OptimizationLevel toLLVMLevel(OptLevel optLevel);
    };
}
