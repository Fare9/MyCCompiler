#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "mycc/CodeGen/x64/x64AST.hpp"

#include <variant>

namespace mycc::codegen::x64 {

/**
 * @brief Represents a variable (or constant) in the backend symbol table.
 * Stores the assembly-level type and whether it has static storage duration.
 */
struct ObjEntry {
    X64Type AsmType;
    bool IsStatic;
};

/**
 * @brief Represents a function in the backend symbol table.
 * Tracks whether the function is defined in this translation unit.
 * Undefined functions are called via @PLT.
 */
struct FunEntry {
    bool Defined;
};

using BackendSymTabEntry = std::variant<ObjEntry, FunEntry>;

/**
 * @brief Backend symbol table mapping names to their assembly-level entries.
 * Built during Phase 1 (IR -> X64AST) and consulted in fixup and emission.
 *
 * Unlike the frontend symbol table which tracks source types, this stores
 * X64Type — the assembly size of each object. In this compiler, assembly
 * types are derived directly from IR operand types (similar to LLVM IR),
 * so only static variables and functions need entries here.
 */
using BackendSymTab = llvm::StringMap<BackendSymTabEntry>;

} // namespace mycc::codegen::x64
