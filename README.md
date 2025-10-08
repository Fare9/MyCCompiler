# MyCCompiler

## Overview

MyCCompiler is a C compiler implementation based on the book "Writing a C Compiler" by Nora Sandler. This project is developed in C++ and utilizes LLVM libraries for various compiler infrastructure components, including abstract syntax tree (AST) manipulation, source location tracking, and utility data structures.

The compiler follows a traditional multi-phase compilation approach, transforming C source code through several intermediate representations before generating x86-64 assembly code.

## Project Structure

The project is organized into several key components, each responsible for a specific phase of the compilation process:

### Core Components

- **`include/mycc/`** - Header files containing interface declarations
- **`lib/`** - Implementation files for all compiler components
- **`main.cpp`** - Entry point and command-line interface

### Compilation Phases

#### Lexical Analysis
- **Location**: `include/mycc/Lexer/` and `lib/Lexer/`
- **Files**: `Lexer.hpp`, `Token.hpp`, `Lexer.cpp`
- **Purpose**: Tokenizes input C source code into a stream of tokens

#### Parsing
- **Location**: `include/mycc/Parser/` and `lib/Parser/`
- **Files**: `Parser.hpp`, `Parser.cpp`
- **Purpose**: Constructs an Abstract Syntax Tree (AST) from the token stream

#### Semantic Analysis
- **Location**: `include/mycc/Sema/` and `lib/Sema/`
- **Files**: `Sema.hpp`, `Scope.hpp`, `Sema.cpp`, `Scope.cpp`
- **Purpose**: Performs semantic validation and scope resolution

#### Abstract Syntax Tree
- **Location**: `include/mycc/AST/` and `lib/AST/`
- **Files**: `AST.hpp`, `ASTContext.hpp`, `ASTPrinter.hpp`, `ASTPrinter.cpp`
- **Purpose**: Defines AST node structures and provides tree manipulation utilities

#### Intermediate Representation
- **Location**: `include/mycc/IR/` and `lib/IR/`
- **Files**: `SimpleIR.hpp`
- **Purpose**: Provides a low-level intermediate representation for optimization and code generation

#### Code Generation
- **Location**: `include/mycc/CodeGen/` and `lib/CodeGen/`
- **Files**: `IRGen.hpp`, `IRGen.cpp`
- **Subdirectory**: `x64/` containing `X64CodeGen.hpp`, `x64AST.hpp`, `X64CodeGen.cpp`
- **Purpose**: Transforms IR to target-specific assembly code (x86-64)

### Support Components

#### Basic Utilities
- **Location**: `include/mycc/Basic/` and `lib/Basic/`
- **Files**: `LLVM.hpp`, `TokenKinds.def`, `TokenKinds.hpp`, `Diagnostic.def`, `Diagnostic.hpp`
- **Purpose**: Provides fundamental data types, diagnostics, and LLVM integration

### Testing

- **`test/`** - Test cases for various compiler components
- **`writing-a-c-compiler-tests/`** - Official test suite from the book

## Build System

The project uses CMake as its build system with modular library organization:

- **Main executable**: `mycc`
- **Component libraries**: `libLexer.a`, `libParser.a`, `libAST.a`, `libCodeGen.a`, `libSema.a`, `libBasic.a`

## Usage

```bash
# Build the project
mkdir build && cd build
cmake ..
make

# Compile a C source file
./mycc input.c -o output.s
```

## Dependencies

- **LLVM**: Provides core data structures (APSInt, StringRef, StringMap) and utility classes
- **CMake**: Build system
- **C++17 or later**: Required for modern C++ features used throughout the codebase

## Current Implementation Status

The compiler currently supports:
- Basic C syntax parsing
- Variable declarations and assignments
- Arithmetic and logical expressions
- Control flow statements
- Function definitions
- Short-circuit evaluation for logical operators
- x86-64 assembly generation

For detailed technical information about the compiler's architecture and implementation, see [TECHNICAL.md](TECHNICAL.md).