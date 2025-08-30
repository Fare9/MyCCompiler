#pragma once

#include "mycc/IR/SimpleIR.hpp"
#include "mycc/CodeGen/x64/x64AST.hpp"
#include "llvm/ADT/StringRef.h"

#include <memory>
#include <string>

namespace mycc {
namespace codegen {
namespace x64 {

class X64CodeGenerator {
    std::unique_ptr<X64Program> Program;
    
public:
    X64CodeGenerator() = default;
    
    // Main generation pipeline
    void generateX64AST(const ir::Program& IRProg);

    std::string generateAssembly();
    
private:
    // Phase 1: Generate X64AST from IR with pseudo-registers
    void generateProgram(const ir::Program& IRProg);
    void generateFunction(const ir::Function& IRFunc, X64Function* X64Func);
    void generateInstruction(const ir::Instruction& Inst, X64Function* X64Func);
    
    // Instruction generation methods
    void generateMov(const ir::Mov& MovInst, X64Function* X64Func);
    void generateRet(const ir::Ret& RetInst, X64Function* X64Func);
    void generateUnary(const ir::UnaryOp& UnaryInst, X64Function* X64Func);
    void generateBinary(const ir::BinaryOp& BinaryInstr, X64Function* X64Func);
    void generateDiv(const ir::BinaryOp& BinaryInstr, X64Function* X64Func);
    void generateRem(const ir::BinaryOp& BinaryInstr, X64Function* X64Func);
    
    // Operand conversion helpers
    X64Operand* convertOperand(const ir::Value* Val, X64Context& Ctx);
    X64Register* convertRegister(const ir::Reg& Reg, X64Context& Ctx);
    X64Int* convertInteger(const ir::Int& IntVal, X64Context& Ctx);
    
    // Phase 2: Replace pseudo-registers with stack allocations
    void allocateStackSlots();
    void allocateStackSlotsForFunction(X64Function* Func);
    void replacePseudoRegistersInInstruction(X64Instruction* Inst, X64Context& Ctx);
    X64Operand* getOrAllocateStackSlot(unsigned pseudoID, X64Context& Ctx);
    
    // Phase 3: Fix instructions and insert prologue/epilogue
    void fixupInstructions();
    void fixupInstructionsForFunction(X64Function* Func);
    void replaceInstructionInFunction(X64Function* Func, X64Instruction* oldInst, 
                                     const std::vector<X64Instruction*>& newInstructions);
    void insertAllocationInstruction(X64Function* Func);
    
    // Assembly generation
    std::string emitAssembly();
    std::string emitFunction(const X64Function& Func);
    std::string emitInstruction(const X64Instruction& Inst);
    
    // Assembly formatting helpers
    std::string formatLabel(StringRef Name);
    std::string formatDirective(StringRef Directive);
    std::string formatComment(StringRef Comment);
};

}
}
}