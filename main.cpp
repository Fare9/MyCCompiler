#include "mycc/Lexer/Lexer.hpp"
#include "mycc/Lexer/Token.hpp"
#include "mycc/Sema/Sema.hpp"
#include "mycc/Parser/Parser.hpp"

#include "mycc/AST/AST.hpp"
#include "mycc/AST/ASTPrinter.hpp"

#include "mycc/Basic/Diagnostic.hpp"
#include "mycc/Basic/TokenKinds.hpp"

#include <iostream>
#include <vector>
#include <unordered_map>


bool lexer = false;
bool parser = false;
bool codegen = false;
bool print_output = false;

bool generate_assembly_file = false;

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
            {"-S",        [&]() { generate_assembly_file = true; }},
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

    for (const auto &F: InputFiles) {
        llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>
                FileOrErr = llvm::MemoryBuffer::getFile(F);

        llvm::SourceMgr SrcMgr;
        mycc::DiagnosticsEngine Diags(SrcMgr);

        SrcMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
        auto Lexer = mycc::Lexer(SrcMgr, Diags);
        auto Sema = mycc::Sema(Lexer.getDiagnostics());
        auto Parser = mycc::Parser(Lexer, Sema);

        if (lexer) {
            mycc::Token Tok;
            if (print_output)
                std::cout << "Tokens: ";
            while (true) {
                Lexer.next(Tok);
                if (Tok.is(mycc::tok::TokenKind::unknown)) {
                    std::cout << std::endl;
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
            mycc::Program *p = Parser.parse();
            if (!p) {
                std::cout << "Error while parsing, no AST has been generated.\n";
                return 2;
            }
            if (print_output) {
                std::cout << "AST: " << mycc::ASTPrinter::print(p) << std::endl;
            }

            Lexer.reset();
        }
    }


    return 0;
}