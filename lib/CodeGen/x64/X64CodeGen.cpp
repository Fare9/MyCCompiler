#include "mycc/CodeGen/x64/X64CodeGen.hpp"
#include "llvm/Support/Casting.h"

using namespace mycc;
using namespace mycc::codegen::x64;

void X64CodeGenerator::generateAssembly(const ir::Program& IRProg) {
    // Emit standard x64 assembly header
    emitDirective(".intel_syntax noprefix");
    emitDirective(".text");
    Output << "\n";
    
    // Generate assembly for each function
    for (const ir::Function* IRFunc : IRProg) {
        generateFunction(*IRFunc);
        Output << "\n";
    }

#ifdef __linux__
    emitDirective(".section .note.GNU-stack,\"\",@progbits");
#endif
}

void X64CodeGenerator::generateFunction(const ir::Function& IRFunc) {
    // Emit function label
    emitDirective(".globl " + IRFunc.get_name().str());
    emitLabel(IRFunc.get_name());
    emitInstruction("push", "rbp");
    emitInstruction("mov", "rbp, rsp");

    // Generate code for each instruction
    for (const ir::Instruction* Inst : IRFunc) {
        generateInstruction(*Inst);
    }
}

void X64CodeGenerator::generateInstruction(const ir::Instruction& Inst) {
    StringRef opcode = Inst.getOpcodeName();
    
    if (opcode == "mov") {
        generateMov(dynamic_cast<const ir::Mov&>(Inst));
    } else if (opcode == "ret") {
        generateRet(dynamic_cast<const ir::Ret&>(Inst));
    }
    // Add more instruction types as needed
}

void X64CodeGenerator::generateMov(const ir::Mov& MovInst) {
    std::string src = getOperandString(MovInst.getSrc());
    std::string dst = getOperandString(MovInst.getDst());
    
    emitInstruction("mov", dst + ", " + src);
}

void X64CodeGenerator::generateRet(const ir::Ret& RetInst) {
    if (!RetInst.isVoidReturn()) {
        // Move return value to RAX
        std::string retVal = getOperandString(RetInst.getReturnValue());
        emitInstruction("mov", "rax, " + retVal);
    }
    
    // Standard function epilogue and return
    emitInstruction("mov", "rsp, rbp");
    emitInstruction("pop", "rbp");
    emitInstruction("ret");
}

std::string X64CodeGenerator::getOperandString(const ir::Value* Val) {
    if (auto* IntVal = dynamic_cast<const ir::Int*>(Val)) {
        return getIntegerValue(*IntVal);
    } else if (auto* RegVal = dynamic_cast<const ir::Reg*>(Val)) {
        return getRegisterName(*RegVal);
    }
    
    return "unknown_operand";
}

std::string X64CodeGenerator::getRegisterName(const ir::Reg& Reg) {
    // For now, map virtual registers to physical registers
    // In a real compiler, this would be done by register allocation
    switch (Reg.getID() % 8) {
        case 0: return "rax";
        case 1: return "rbx";
        case 2: return "rcx";
        case 3: return "rdx";
        case 4: return "rsi";
        case 5: return "rdi";
        case 6: return "r8";
        case 7: return "r9";
        default: return "rax"; // fallback
    }
}

std::string X64CodeGenerator::getIntegerValue(const ir::Int& IntVal) {
    return std::to_string(IntVal.getValue().getSExtValue());
}

void X64CodeGenerator::emitLabel(StringRef Label) {
    Output << Label.str() << ":\n";
}

void X64CodeGenerator::emitInstruction(StringRef Opcode, StringRef Operands) {
    Output << "    " << Opcode.str();
    if (!Operands.empty()) {
        Output << " " << Operands.str();
    }
    Output << "\n";
}

void X64CodeGenerator::emitComment(StringRef Comment) {
    Output << "    # " << Comment.str() << "\n";
}

void X64CodeGenerator::emitDirective(StringRef Directive) {
    Output << Directive.str() << "\n";
}