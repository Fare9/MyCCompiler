#pragma once

#include "mycc/IR/SimpleIR.hpp"
#include "llvm/ADT/StringRef.h"

#include <sstream>
#include <string>

namespace mycc {
namespace codegen {
namespace x64 {

class X64CodeGenerator {
    std::ostringstream Output;
    
public:
    X64CodeGenerator() = default;
    
    // Generate x64 assembly from IR Program
    void generateAssembly(const ir::Program& IRProg);
    
    // Get the generated assembly as string
    std::string getAssembly() const { return Output.str(); }
    
private:
    // Generate assembly for a single function
    void generateFunction(const ir::Function& IRFunc);
    
    // Generate assembly for a single instruction
    void generateInstruction(const ir::Instruction& Inst);
    
    // Helper methods for specific instructions
    void generateMov(const ir::Mov& MovInst);
    void generateRet(const ir::Ret& RetInst);
    
    // Helper methods for operands
    std::string getOperandString(const ir::Value* Val);
    std::string getRegisterName(const ir::Reg& Reg);
    std::string getIntegerValue(const ir::Int& IntVal);
    
    // Assembly output helpers
    void emitLabel(StringRef Label);
    void emitInstruction(StringRef Opcode, StringRef Operands = "");
    void emitComment(StringRef Comment);
    void emitDirective(StringRef Directive);
};

}
}
}