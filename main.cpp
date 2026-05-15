#include "mycc/Lexer/Lexer.hpp"
#include "mycc/Lexer/Token.hpp"

#include "mycc/Parser/Parser.hpp"

#include "mycc/AST/AST.hpp"
#include "mycc/AST/ASTContext.hpp"
#include "mycc/AST/ASTPrinter.hpp"

#include "mycc/Sema/Sema.hpp"

#include "mycc/IR/SimpleIR.hpp"
#include "mycc/CodeGen/IRGen.hpp"
#include "mycc/CodeGen/x64/X64CodeGen.hpp"
#include "mycc/CodeGen/llvm/LLVMIRGen.hpp"

#include "mycc/Opt/LLVMOptimizer.hpp"


#include "mycc/Basic/Diagnostic.hpp"
#include "mycc/Basic/TokenKinds.hpp"

#include <iostream>
#include <vector>
#include <unordered_map>
#include <fstream>
#include <cstdlib>
#include <filesystem>


bool lexer = false;
bool parser = false;
bool semantic = false;
bool tacky = false;
bool codegen = false;
bool llvm_gen = false;
bool object = false;
bool compile = false;
bool print_output = false;

bool tool_exists(const std::string &tool) {
    std::string check_cmd = "command -v " + tool + " >/dev/null 2>&1";
    return std::system(check_cmd.c_str()) == 0;
}


void print_help() {
    std::cout <<
        "mycc - A C Compiler\n"
        "Usage: mycc [options] <file>\n"
        "\nCompilation steps (mutually exclusive, run all steps up to and including the selected one):\n"
        "  --lex               Run the lexer only\n"
        "  --parse             Run the lexer and parser\n"
        "  --validate          Run lexer, parser, and semantic analysis\n"
        "  --tacky             Run lexer, parser, semantic analysis, and IR generation\n"
        "  --codegen           Run full compilation without assembling\n"
        "  --llvm              Generate LLVM IR instead of x86-64 assembly (produces <file>.ll)\n"
        "\nCode generation options:\n"
        "  -O0                 No optimization (default)\n"
        "  -O1                 Basic optimizations\n"
        "  -O2                 Standard optimizations\n"
        "  -O3                 Aggressive optimizations\n"
        "  -Os                 Optimize for size\n"
        "  -Oz                 Optimize aggressively for size\n"
        "  --llvm-plugin <path> Load a user-provided LLVM pass plugin (.so)\n"
        "                       Can be specified multiple times. Requires --llvm.\n"
        "\nOutput options:\n"
        "  -c                  Generate an object file instead of an executable\n"
        "  --print             Print output to stdout\n"
        "\nOther:\n"
        "  --help, -h          Show this help message\n"
        "\nIf no compilation step is specified, the compiler runs all steps and produces an executable.\n";
}

int main(int argc, char **argv) {
    std::vector<std::string> InputFiles;
    std::vector<std::string> args{argv + 1, argv + argc};

    size_t i = 0, max_size = args.size();

    std::optional<mycc::opt::LLVMOptimizer::OptLevel> optimizationLevel;
    std::vector<std::string> llvmPluginsPaths;

    std::unique_ptr<mycc::opt::LLVMOptimizer> llvmOptimizer = nullptr;

    std::unordered_map<std::string, std::function<void()> > options{
        // main options
        {"--lex", [&]() { lexer = true; }},
        {
            "--parse", [&]() {
                parser = true;
            }
        },
        {
            "--validate", [&]() {
                parser = true;
                semantic = true;
            }
        },
        {
            "--tacky", [&]() {
                tacky = true;
            }
        },
        {
            "--codegen", [&]() {
                codegen = true;
            }
        },
        {"--llvm", [&]() { llvm_gen = true; }},
        {"-c", [&]() { object = true; }},
        {"--print", [&]() { print_output = true; }},
        // other options
        {"--help", [&]() { print_help(); }},
        {"-h", [&]() { print_help(); }},
        {
            "-O0", [&]() {
                if (optimizationLevel.has_value()) {
                    std::cerr << "Error, an optimization level was already provided\n";
                    exit(1);
                }
                optimizationLevel = mycc::opt::LLVMOptimizer::OptLevel::O0;
            }
        },
        {
            "-O1", [&]() {
                if (optimizationLevel.has_value()) {
                    std::cerr << "Error, an optimization level was already provided\n";
                    exit(1);
                }
                optimizationLevel = mycc::opt::LLVMOptimizer::OptLevel::O1;
            }
        },
        {
            "-O2", [&]() {
                if (optimizationLevel.has_value()) {
                    std::cerr << "Error, an optimization level was already provided\n";
                    exit(1);
                }
                optimizationLevel = mycc::opt::LLVMOptimizer::OptLevel::O2;
            }
        },
        {
            "-O3", [&]() {
                if (optimizationLevel.has_value()) {
                    std::cerr << "Error, an optimization level was already provided\n";
                    exit(1);
                }
                optimizationLevel = mycc::opt::LLVMOptimizer::OptLevel::O3;
            }
        },
        {
            "-Os", [&]() {
                if (optimizationLevel.has_value()) {
                    std::cerr << "Error, an optimization level was already provided\n";
                    exit(1);
                }
                optimizationLevel = mycc::opt::LLVMOptimizer::OptLevel::Os;
            }
        },
        {
            "-Oz", [&]() {
                if (optimizationLevel.has_value()) {
                    std::cerr << "Error, an optimization level was already provided\n";
                    exit(1);
                }
                optimizationLevel = mycc::opt::LLVMOptimizer::OptLevel::Oz;
            }
        },
        {
            "--llvm-plugin", [&]() {
                size_t nextI = i + 1;
                if (nextI >= max_size) {
                    std::cerr << "Error, --llvm-plugin needs a path to the plugin as argument.\n";
                    exit(1);
                }
                llvmPluginsPaths.emplace_back(args[nextI]);
                i = nextI;
            }
        }
    };

    for (i = 0; i < max_size; i++) {
        const auto &s = args[i];
        if (auto it = options.find(s); it != options.end())
            it->second();
        else
            InputFiles.push_back(s);
    }

    if (!lexer && !parser && !tacky && !codegen && !llvm_gen) compile = true;

    if (llvm_gen && (optimizationLevel.has_value() || !llvmPluginsPaths.empty())) {
        if (!optimizationLevel.has_value()) {
            optimizationLevel = mycc::opt::LLVMOptimizer::OptLevel::O0;
        }

        llvmOptimizer = std::make_unique<mycc::opt::LLVMOptimizer>(
            mycc::opt::LLVMOptimizer::Config{optimizationLevel.value(), std::move(llvmPluginsPaths)}
        );
    }

    for (const auto &F: InputFiles) {
        llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer> >
                FileOrErr = llvm::MemoryBuffer::getFile(F);

        llvm::SourceMgr SrcMgr;
        mycc::DiagnosticsEngine Diags(SrcMgr);

        SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
        auto Lexer = mycc::Lexer(SrcMgr, Diags);
        auto ASTContext = mycc::ASTContext(SrcMgr, F);
        auto Sema = mycc::Sema(Lexer.getDiagnostics(), ASTContext);
        auto Parser = mycc::Parser(Lexer, Sema, ASTContext);
        mycc::ir::Context Context;
        mycc::ir::Program Program;
        auto irGen = mycc::codegen::IRGenerator(Context, Program);
        auto x64CodeGen = mycc::codegen::x64::X64CodeGenerator();

        if (lexer) {
            mycc::Token Tok;
            if (print_output)
                std::cout << "Tokens: ";
            while (true) {
                Lexer.next(Tok);
                if (Tok.is(mycc::tok::TokenKind::unknown)) {
                    std::cerr << "mycc: error: unrecognized token in input\n";
                    std::cerr << "compilation terminated.\n";
                    return 1;
                }
                if (print_output) {
                    std::cout << Tok.to_string() << " ";
                }
                if (Tok.is(mycc::tok::TokenKind::eof)) {
                    std::cout << std::endl;
                    break;
                }
            }
            Lexer.reset();
        }
        if (parser) {
            // if --validate is provided, parser is
            // run with semantic analysis with errors
            if (!semantic)
                Sema.avoidErrors();
            mycc::Program *p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: no AST generated due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 2;
            }
            if (print_output) {
                std::cout << "AST: " << mycc::ASTPrinter::print(p) << std::endl;
            }
        }
        if (tacky) {
            mycc::Program *p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: code generation failed due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 3;
            }
            irGen.generateIR(*p, Sema.getGlobalSymbolTable());
            if (print_output) {
                std::cout << "IR output:\n" << Program.to_string() << std::endl;
            }
        }
        if (codegen) {
            mycc::Program *p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: code generation failed due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 3;
            }
            irGen.generateIR(*p, Sema.getGlobalSymbolTable());
            if (print_output) {
                std::cout << "IR output:\n" << Program.to_string() << std::endl;
            }
            x64CodeGen.generateX64AST(Program);
            if (print_output) {
                std::cout << "Assembly output:\n" << x64CodeGen.generateAssembly() << std::endl;
            }
        }
        if (llvm_gen) {
            mycc::Program *p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: code generation failed due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 3;
            }
            llvm::LLVMContext LLVMCtx;
            mycc::codegen::llvmbackend::LLVMIRGenerator LLVMGen(LLVMCtx, F);
            LLVMGen.generateIR(*p, Sema.getGlobalSymbolTable());

            auto Module = LLVMGen.takeModule();

            // If the user specified any kind of optimization, apply the optimization
            if (llvmOptimizer) {
                std::string errMsg;
                if (!llvmOptimizer->optimize(*Module, errMsg)) {
                    std::cerr << "mycc: error: optimizer failed: " << errMsg << "\n";
                    return 5;
                }
            }


            if (print_output) {
                Module->print(llvm::outs(), nullptr);
            }

            // Write LLVM IR to .ll file
            std::string ll_name = F;
            if (ll_name.ends_with(".c"))
                ll_name.replace(ll_name.length() - 2, 2, ".ll");
            else
                ll_name += ".ll";

            std::error_code EC;
            llvm::raw_fd_ostream OutFile(ll_name, EC);
            if (EC) {
                std::cerr << "mycc: error: could not open output file '" << ll_name << "': " << EC.message() << "\n";
                return 4;
            }
            Module->print(OutFile, nullptr);
            std::cout << "LLVM output: " << ll_name << '\n';
        }
        if (compile) {
            mycc::Program *p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: code generation failed due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 3;
            }
            irGen.generateIR(*p, Sema.getGlobalSymbolTable());
            if (print_output) {
                std::cout << "IR output:\n" << Program.to_string() << std::endl;
            }
            x64CodeGen.generateX64AST(Program);
            if (print_output) {
                std::cout << "Assembly output:\n" << x64CodeGen.generateAssembly() << std::endl;
            }

            // Create assembly filename by replacing .c with .s
            std::string assembly_name = F;
            if (assembly_name.ends_with(".c")) {
                assembly_name.replace(assembly_name.length() - 2, 2, ".s");
            } else {
                assembly_name += ".s";
            }

            // Write assembly to file
            std::ofstream assembly_file(assembly_name);
            if (assembly_file.is_open()) {
                assembly_file << x64CodeGen.generateAssembly();
                assembly_file.close();
                if (print_output) {
                    std::cout << "Assembly written to: " << assembly_name << std::endl;
                }
            } else {
                std::cerr << "mycc: error: could not open output file '" << assembly_name << "'\n";
                std::cerr << "compilation terminated.\n";
                return 4;
            }

            std::string executable_name = assembly_name;
            if (executable_name.ends_with(".s"))
                executable_name.erase(executable_name.length() - 2, 2);

            if (object)
                executable_name += ".o";

            std::vector<std::string> compilers = {"clang", "gcc"};
            bool compiled = false;

            for (const auto &compiler: compilers) {
                if (tool_exists(compiler)) {
                    std::string compile_cmd = compiler;
                    compile_cmd += object ? " -c" : "";
                    compile_cmd += " -o \"";
                    compile_cmd += executable_name;
                    compile_cmd += "\" \"";
                    compile_cmd += assembly_name;
                    compile_cmd += "\"";

                    if (std::system(compile_cmd.c_str()) == 0) {
                        compiled = true;
                        if (print_output) {
                            std::cout << "Compiled with " << compiler << ": " << executable_name << std::endl;
                        }
                        break;
                    }
                }
            }

            if (!compiled) {
                std::cerr << "mycc: error: no suitable compiler found or compilation failed\n";
                return 5;
            }
        }
    }


    return 0;
}
