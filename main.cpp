#include "mycc/Lexer/Lexer.hpp"
#include "mycc/Lexer/Token.hpp"
#include "mycc/Sema/Sema.hpp"
#include "mycc/Parser/Parser.hpp"
#include "mycc/IR/SimpleIR.hpp"
#include "mycc/CodeGen/IRGen.hpp"
#include "mycc/CodeGen/x64/X64CodeGen.hpp"


#include "mycc/AST/AST.hpp"
#include "mycc/AST/ASTPrinter.hpp"

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
bool codegen = false;
bool compile = false;
bool print_output = false;

bool tool_exists(const std::string& tool) {
    std::string check_cmd = "command -v " + tool + " >/dev/null 2>&1";
    return std::system(check_cmd.c_str()) == 0;
}

void print_help() {
    std::cout << "mycc - C Compiler\n"
              << "Usage: mycc [options] <file>\n"
              << "\nOptions:\n"
              << "  --lex      Run lexer only\n"
              << "  --parse    Run lexer and parser\n"
              << "  --codegen  Run full compilation (lexer, parser, codegen)\n"
              << "  -S         Generate assembly file\n"
              << "  --help     Show this help message\n";
}

int main(int argc, char **argv) {
    std::vector<std::string> InputFiles;
    std::vector<std::string> args{argv + 1, argv + argc};

    std::unordered_map<std::string, std::function<void()>> options{
            // main options
            {"--lex",     [&]() { lexer = true; }},
            {"--parse",   [&]() {
                parser = true;
            }},
            {"--codegen", [&]() {
                codegen = true;
            }},
            {"--print",   [&]() { print_output = true; }},
            // other options
            {"--help",    [&]() { print_help(); }},
            {"-h",        [&]() { print_help(); }}
    };

    for (const auto &s: args) {
        if (auto it = options.find(s); it != options.end())
            it->second();
        else {
            InputFiles.push_back(s);
        }
    }

    if (!lexer && !parser && !codegen) compile = true;

    for (const auto &F: InputFiles) {
        llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
                FileOrErr = llvm::MemoryBuffer::getFile(F);

        llvm::SourceMgr SrcMgr;
        mycc::DiagnosticsEngine Diags(SrcMgr);

        SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
        auto Lexer = mycc::Lexer(SrcMgr, Diags);
        auto Sema = mycc::Sema(Lexer.getDiagnostics());
        auto Parser = mycc::Parser(Lexer, Sema);
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
            auto p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: no AST generated due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 2;
            }
            if (print_output) {
                std::cout << "AST: " << mycc::ASTPrinter::print(p.get()) << std::endl;
            }
        }
        if (codegen) {
            auto p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: code generation failed due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 3;
            }
            irGen.generateIR(*p);
            if (print_output) {
                std::cout << "IR output:\n" << Program.to_string() << std::endl;
            }
            x64CodeGen.generateAssembly(Program);
            if (print_output) {
                std::cout << "Assembly output:\n" << x64CodeGen.getAssembly() << std::endl;
            }
        }
        if (compile) {
            auto p = Parser.parse();
            Lexer.reset();
            if (!p) {
                std::cerr << "mycc: error: parse error encountered\n";
                std::cerr << "mycc: fatal error: code generation failed due to parse errors\n";
                std::cerr << "compilation terminated.\n";
                return 3;
            }
            irGen.generateIR(*p);
            if (print_output) {
                std::cout << "IR output:\n" << Program.to_string() << std::endl;
            }
            x64CodeGen.generateAssembly(Program);
            if (print_output) {
                std::cout << "Assembly output:\n" << x64CodeGen.getAssembly() << std::endl;
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
                assembly_file << x64CodeGen.getAssembly();
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

            std::vector<std::string> compilers = {"clang", "gcc"};
            bool compiled = false;

            for (const auto& compiler : compilers) {
                if (tool_exists(compiler)) {
                    std::string compile_cmd = compiler + " -o \"" + executable_name +
                                              "\" \"" + assembly_name + "\"";

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