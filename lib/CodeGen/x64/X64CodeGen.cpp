#include "mycc/CodeGen/x64/X64CodeGen.hpp"
#include "mycc/IR/SimpleIR.hpp"
#include "mycc/CodeGen/x64/x64AST.hpp"

#include <sstream>
#include <vector>

namespace mycc {
namespace codegen {
namespace x64 {

// ===== Main X64CodeGenerator Implementation =====

void X64CodeGenerator::generateX64AST(const ir::Program& IRProg) {
    // Phase 1: Generate X64AST from IR with pseudo-registers
    generateProgram(IRProg);
    
    // Phase 2: Replace pseudo-registers with stack allocations
    allocateStackSlots();
    
    // Phase 3: Fix instructions and insert prologue/epilogue
    fixupInstructions();
}

std::string X64CodeGenerator::generateAssembly() {
    if (!Program) {
        return "";
    }
    
    // Phase 5: Generate final assembly
    return emitAssembly();
}

// ===== Phase 1: IR to X64AST Generation =====

void X64CodeGenerator::generateProgram(const ir::Program& IRProg) {
    Program = std::make_unique<X64Program>();

    for (const auto* IRFunc : IRProg) {
        auto* X64Func = new X64Function(IRFunc->get_name());
        Program->add_function(X64Func);
        generateFunction(*IRFunc, X64Func);
    }
}

void X64CodeGenerator::generateFunction(const ir::Function& IRFunc, X64Function* X64Func) {
    for (const auto * Instr : IRFunc) {
        generateInstruction(*Instr, X64Func);
    }
}

void X64CodeGenerator::generateInstruction(const ir::Instruction& Inst, X64Function* X64Func) {
    if (const auto * mov = dynamic_cast<const ir::Mov*>(&Inst)) {
        generateMov(*mov, X64Func);
    } else if (const auto * ret = dynamic_cast<const ir::Ret*>(&Inst)) {
        generateRet(*ret, X64Func);
    } else if (const auto * unary = dynamic_cast<const ir::UnaryOp*>(&Inst)) {
        generateUnary(*unary, X64Func);
    } else if (const auto * binary = dynamic_cast<const ir::BinaryOp*>(&Inst)) {
        switch (binary->getKind()) {
            case ir::BinaryOp::Add:
            case ir::BinaryOp::Sub:
            case ir::BinaryOp::Mul:
            case ir::BinaryOp::And:
            case ir::BinaryOp::Or:
            case ir::BinaryOp::Xor:
            case ir::BinaryOp::Sal:
            case ir::BinaryOp::Sar:
                generateBinary(*binary, X64Func);
                break;
            case ir::BinaryOp::Div:
                generateDiv(*binary, X64Func);
                break;
            case ir::BinaryOp::Rem:
                generateRem(*binary, X64Func);
                break;
        }
    }
}

void X64CodeGenerator::generateMov(const ir::Mov& MovInst, X64Function* X64Func) {
    auto & Ctx = X64Func->getContext();
    X64Operand * Src = convertOperand(MovInst.getSrc(), Ctx);
    X64Operand * Dst = convertOperand(MovInst.getDst(), Ctx);
    auto Mov = Ctx.createMov(Src, Dst);
    X64Func->add_instruction(Mov);
}

void X64CodeGenerator::generateRet(const ir::Ret& RetInst, X64Function* X64Func) {
    auto &Ctx = X64Func->getContext();
    if (!RetInst.isVoidReturn()) {
        X64Operand * RetValue = convertOperand(RetInst.getReturnValue(), Ctx);
        X64Operand * RetReg = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, PhysicalRegister::Size::DWORD);
        auto Mov = Ctx.createMov(RetValue, RetReg);
        X64Func->add_instruction(Mov);
    }
    X64Func->add_instruction(Ctx.createRet());
}

void X64CodeGenerator::generateUnary(const ir::UnaryOp& UnaryInst, X64Function* X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand * tempRegister = convertOperand(UnaryInst.getDestination(), Ctx);
    X64Operand * operand = convertOperand(UnaryInst.getSource(), Ctx);
    X64Unary::X64UnaryKind Kind;
    switch (UnaryInst.getKind()) {
        case ir::UnaryOp::UnaryOpKind::Neg:
            Kind = X64Unary::X64UnaryKind::Neg;
            break;
        case ir::UnaryOp::UnaryOpKind::Complement:
            Kind = X64Unary::X64UnaryKind::Complement;
            break;
    }
    // First a Move from Operand to TempRegister
    X64Func->add_instruction(Ctx.createMov(operand, tempRegister));
    // Second, we generate the unary operator
    X64Func->add_instruction(Ctx.createUnary(Kind, tempRegister));
}

void X64CodeGenerator::generateBinary(const ir::BinaryOp& BinaryInstr, X64Function* X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand * dst = convertOperand(BinaryInstr.getDestination(), Ctx);
    X64Operand * src1 = convertOperand(BinaryInstr.getLeft(), Ctx);
    X64Operand * src2 = convertOperand(BinaryInstr.getRight(), Ctx);
    X64Binary::X64BinaryKind Kind;
    switch (BinaryInstr.getKind()) {
        case ir::BinaryOp::Add:
            Kind = X64Binary::X64BinaryKind::Add;
            break;
        case ir::BinaryOp::Sub:
            Kind = X64Binary::X64BinaryKind::Sub;
            break;
        case ir::BinaryOp::Mul:
            Kind = X64Binary::X64BinaryKind::Mult;
            break;
        case ir::BinaryOp::And:
            Kind = X64Binary::X64BinaryKind::And;
            break;
        case ir::BinaryOp::Or:
            Kind = X64Binary::X64BinaryKind::Or;
            break;
        case ir::BinaryOp::Xor:
            Kind = X64Binary::X64BinaryKind::Xor;
            break;
        case ir::BinaryOp::Sal:
            Kind = X64Binary::X64BinaryKind::Sal;
            break;
        case ir::BinaryOp::Sar:
            Kind = X64Binary::X64BinaryKind::Sar;
            break;
    }
    X64Func->add_instruction(Ctx.createMov(src1, dst));
    X64Func->add_instruction(Ctx.createBinary(Kind, src2, dst));
}

void X64CodeGenerator::generateDiv(const ir::BinaryOp& BinaryInstr, X64Function* X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand * dst = convertOperand(BinaryInstr.getDestination(), Ctx);
    X64Operand * src1 = convertOperand(BinaryInstr.getLeft(), Ctx);
    X64Operand * src2 = convertOperand(BinaryInstr.getRight(), Ctx);
    X64Operand * eax = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, PhysicalRegister::Size::DWORD);
    X64Func->add_instruction(Ctx.createMov(src1, eax));
    X64Func->add_instruction(Ctx.createCdq());
    X64Func->add_instruction(Ctx.createIDiv(src2));
    X64Func->add_instruction(Ctx.createMov(eax, dst));
}

void X64CodeGenerator::generateRem(const ir::BinaryOp& BinaryInstr, X64Function* X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand * dst = convertOperand(BinaryInstr.getDestination(), Ctx);
    X64Operand * src1 = convertOperand(BinaryInstr.getLeft(), Ctx);
    X64Operand * src2 = convertOperand(BinaryInstr.getRight(), Ctx);
    X64Operand * eax = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, PhysicalRegister::Size::DWORD);
    X64Operand * edx = Ctx.getPhysReg(PhysicalRegister::PhysReg::RDX, PhysicalRegister::Size::DWORD);
    X64Func->add_instruction(Ctx.createMov(src1, eax));
    X64Func->add_instruction(Ctx.createCdq());
    X64Func->add_instruction(Ctx.createIDiv(src2));
    X64Func->add_instruction(Ctx.createMov(edx, dst));
}

// ===== Operand Conversion Helpers =====

X64Operand* X64CodeGenerator::convertOperand(const ir::Value* Val, X64Context& Ctx) {
    if (const auto * Imm = dynamic_cast<const ir::Int*>(Val))
        return convertInteger(*Imm, Ctx);
    if (const auto * Reg = dynamic_cast<const ir::Reg*>(Val))
        return convertRegister(*Reg, Ctx);
    return nullptr;
}

X64Register* X64CodeGenerator::convertRegister(const ir::Reg& Reg, X64Context& Ctx) {
    return Ctx.getPseudoReg(Reg.getID());
}

X64Int* X64CodeGenerator::convertInteger(const ir::Int& IntVal, X64Context& Ctx) {
    return Ctx.createInt(IntVal.getValue());
}

// ===== Phase 2: Stack Allocation =====

void X64CodeGenerator::allocateStackSlots() {
    if (!Program) return;
    
    for (auto& Func : *Program) {
        allocateStackSlotsForFunction(Func);
    }
}

void X64CodeGenerator::allocateStackSlotsForFunction(X64Function* Func) {
    auto& Ctx = Func->getContext();
    
    // Traverse instructions and replace pseudo-registers with stack allocations
    for (auto& Inst : *Func) {
        replacePseudoRegistersInInstruction(Inst, Ctx);
    }
}

void X64CodeGenerator::replacePseudoRegistersInInstruction(X64Instruction* Inst, X64Context& Ctx) {
    // Handle different instruction types
    if (auto* mov = dynamic_cast<X64Mov*>(Inst)) {
        // Replace source operand if it's a pseudo register
        X64Operand* src = mov->getSrc();
        if (auto* pseudoReg = dynamic_cast<PseudoRegister*>(src)) {
            X64Operand* stackSlot = getOrAllocateStackSlot(pseudoReg->getID(), Ctx);
            mov->setSrc(stackSlot);
        }
        
        // Replace destination operand if it's a pseudo register
        X64Operand* dst = mov->getDst();
        if (auto* pseudoReg = dynamic_cast<PseudoRegister*>(dst)) {
            X64Operand* stackSlot = getOrAllocateStackSlot(pseudoReg->getID(), Ctx);
            mov->setDst(stackSlot);
        }
    }
    else if (auto* unary = dynamic_cast<X64Unary*>(Inst)) {
        // Replace operand if it's a pseudo register
        X64Operand* op = unary->getOperand();
        if (auto* pseudoReg = dynamic_cast<PseudoRegister*>(op)) {
            X64Operand* stackSlot = getOrAllocateStackSlot(pseudoReg->getID(), Ctx);
            unary->setOperand(stackSlot);
        }
    }
    else if (auto * binary = dynamic_cast<X64Binary*>(Inst)) {
        X64Operand * src = binary->getSrc();
        if (auto* pseudoReg = dynamic_cast<PseudoRegister*>(src)) {
            X64Operand* stackSlot = getOrAllocateStackSlot(pseudoReg->getID(), Ctx);
            binary->setSrc(stackSlot);
        }
        X64Operand* dst = binary->getDst();
        if (auto* pseudoReg = dynamic_cast<PseudoRegister*>(dst)) {
            X64Operand* stackSlot = getOrAllocateStackSlot(pseudoReg->getID(), Ctx);
            binary->setDst(stackSlot);
        }
    }
    else if (auto * div = dynamic_cast<X64IDiv*>(Inst)) {
        X64Operand* op = div->getOperand();
        if (auto* pseudoReg = dynamic_cast<PseudoRegister*>(op)) {
            X64Operand* stackSlot = getOrAllocateStackSlot(pseudoReg->getID(), Ctx);
            div->setOperand(stackSlot);
        }
    }
    // X64Ret has no operands to replace
}

X64Operand* X64CodeGenerator::getOrAllocateStackSlot(unsigned pseudoID, X64Context& Ctx) {
    // Check if already allocated to memory
    if (Ctx.isAllocatedToMemory(pseudoID)) {
        return Ctx.getAllocatedMemory(pseudoID);
    }
    
    // Allocate new stack slot
    Ctx.allocateMemory(pseudoID, X64Stack::DWORD);
    return Ctx.getAllocatedMemory(pseudoID);
}

// ===== Phase 3: Instruction Fixup =====

void X64CodeGenerator::fixupInstructions() {
    if (!Program) return;
    
    for (auto& Func : *Program) {
        fixupInstructionsForFunction(Func);
        insertAllocationInstruction(Func);
    }
}

void X64CodeGenerator::fixupInstructionsForFunction(X64Function* Func) {
    auto & Ctx = Func->getContext();
    
    // We need to collect instructions to replace since we can't modify while iterating
    std::vector<std::pair<X64Instruction*, std::vector<X64Instruction*>>> replacements;
    
    for (auto * Inst : *Func) {
        if (auto* mov = dynamic_cast<X64Mov*>(Inst)) {
            // Check for MEM to MEM move (both operands are X64Stack)
            X64Operand* src = mov->getSrc();
            X64Operand* dst = mov->getDst();
            auto *srcStack = dynamic_cast<X64Stack*>(src);
            auto *dstStack = dynamic_cast<X64Stack*>(dst);
            
            // If both source and destination are memory, we need to fix this
            if (srcStack != nullptr && dstStack != nullptr) {
                // Use R10D as intermediate register
                auto *R10D = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, PhysicalRegister::Size::DWORD);
                
                // Create two new instructions:
                // 1. MOV R10D, [src_memory]
                // 2. MOV [dst_memory], R10D
                std::vector<X64Instruction*> newInstructions;
                newInstructions.push_back(Ctx.createMov(srcStack, R10D));
                newInstructions.push_back(Ctx.createMov(R10D, dstStack));
                
                // Mark this instruction for replacement
                replacements.emplace_back(mov, std::move(newInstructions));
            }
        }
        else if (auto * binOp = dynamic_cast<X64Binary*>(Inst)) {
            // Check for MEM to MEM move (both operands are X64Stack)
            X64Operand* src = binOp->getSrc();
            X64Operand* dst = binOp->getDst();
            auto *srcStack = dynamic_cast<X64Stack*>(src);
            auto *dstStack = dynamic_cast<X64Stack*>(dst);

            // If both source and destination are memory, we need to fix this
            if (srcStack != nullptr && dstStack != nullptr) {
                // Use R10D as intermediate register
                auto *R10D = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, PhysicalRegister::Size::DWORD);
                auto *CL = Ctx.getPhysReg(PhysicalRegister::PhysReg::RCX, PhysicalRegister::Size::BYTE);
                PhysicalRegister * srcRegister;
                // Create two new instructions:
                // 1. MOV R10D, [src_memory]
                // 2. BinOp [dst_memory], R10D
                std::vector<X64Instruction*> newInstructions;
                // SAL/SAR shift count must be in CL register
                if (binOp->getKind() == X64Binary::Sal || binOp->getKind() == X64Binary::Sar) {
                    auto *srcStackByte = Ctx.getAllocatedStack(srcStack, X64Stack::Size::BYTE);
                    newInstructions.push_back(Ctx.createMov(srcStackByte, CL));
                    srcRegister = CL;
                } else {
                    newInstructions.push_back(Ctx.createMov(srcStack, R10D));
                    srcRegister = R10D;
                }
                // the destination for a Mul cannot be a memory address, so in case we have
                // a memory address, replace it for a real register.
                if (binOp->getKind() == X64Binary::Mult) {
                    auto *R11D = Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, PhysicalRegister::Size::DWORD);
                    newInstructions.push_back(Ctx.createMov(dst, R11D));
                    newInstructions.push_back(Ctx.createBinary(binOp->getKind(), srcRegister, R11D));
                    newInstructions.push_back(Ctx.createMov(R11D, dstStack));
                }
                else {
                    newInstructions.push_back(Ctx.createBinary(binOp->getKind(), srcRegister, dstStack));
                }
                // Mark this instruction for replacement
                replacements.emplace_back(binOp, std::move(newInstructions));
            }
            // Same as before, but in this case, we can have that source is an immediate value
            // and destination is a memory address
            else if (dstStack != nullptr && binOp->getKind() == X64Binary::Mult) {
                std::vector<X64Instruction*> newInstructions;
                auto *R11D = Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, PhysicalRegister::Size::DWORD);
                newInstructions.push_back(Ctx.createMov(dst, R11D));
                newInstructions.push_back(Ctx.createBinary(binOp->getKind(), src, R11D));
                newInstructions.push_back(Ctx.createMov(R11D, dstStack));
                replacements.emplace_back(binOp, std::move(newInstructions));
            }
            // Handle SAL/SAR where shift count is not immediate and not in CL
            else if ((binOp->getKind() == X64Binary::Sal || binOp->getKind() == X64Binary::Sar)) {
                auto* srcImm = dynamic_cast<X64Int*>(src);
                // If source is not immediate, it must go into CL register
                if (srcImm == nullptr) {
                    std::vector<X64Instruction*> newInstructions;
                    auto *CL = Ctx.getPhysReg(PhysicalRegister::PhysReg::RCX, PhysicalRegister::Size::BYTE);
                    newInstructions.push_back(Ctx.createMov(src, CL));
                    newInstructions.push_back(Ctx.createBinary(binOp->getKind(), CL, dst));
                    replacements.emplace_back(binOp, std::move(newInstructions));
                }
            }
        }
        else if (auto * div = dynamic_cast<X64IDiv*>(Inst)) {
            X64Operand* op = div->getOperand();
            auto* Imm = dynamic_cast<X64Int*>(op);

            if (Imm != nullptr) {
                // Use R10D as intermediate register
                auto *R10D = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, PhysicalRegister::Size::DWORD);

                // Create two new instructions
                // 1. Mov R10D, Int
                // 2. IDIV R10D
                std::vector<X64Instruction*> newInstructions;
                newInstructions.push_back(Ctx.createMov(Imm, R10D));
                newInstructions.push_back(Ctx.createIDiv(R10D));

                // Mark this instruction for replacement
                replacements.emplace_back(div, std::move(newInstructions));
            }
        }
    }
    
    // Apply the replacements
    for (const auto& [oldInst, newInstructions] : replacements) {
        replaceInstructionInFunction(Func, oldInst, newInstructions);
    }
}

void X64CodeGenerator::replaceInstructionInFunction(X64Function* Func, X64Instruction* oldInst, 
                                                    const std::vector<X64Instruction*>& newInstructions) {
    // Find the old instruction in the function's instruction list
    auto& instrs = Func->getInstructions();  // Assuming X64Function has this method
    
    for (auto it = instrs.begin(); it != instrs.end(); ++it) {
        if (*it == oldInst) {
            // Remove the old instruction
            it = instrs.erase(it);
            
            // Insert new instructions at the same position
            for (auto* newInst : newInstructions) {
                it = instrs.insert(it, newInst);
                ++it;
            }
            break;
        }
    }
}


void X64CodeGenerator::insertAllocationInstruction(X64Function* Func) {
    auto& Ctx = Func->getContext();
    int stackSize = Ctx.getStackOffset(); // Stack grows downward

    if (stackSize == 0) return;

    llvm::APSInt stackSizeAPSint(llvm::APInt(32, abs(stackSize)));
    auto * stackSizeImm = Ctx.createInt(stackSizeAPSint);
    auto * allocateInsn = Ctx.createAllocation(stackSizeImm);
    Func->getInstructions().push_front(allocateInsn);
}

// ===== Phase 5: Assembly Generation =====

std::string X64CodeGenerator::emitAssembly() {
    // TODO: Generate final assembly string
    std::ostringstream asm_output;
    
    // Emit assembly header/directives
    asm_output << formatDirective(".intel_syntax noprefix") << "\n";
    asm_output << formatDirective(".text") << "\n\n";
    
    // Emit each function
    for (const auto& Func : *Program) {
        asm_output << emitFunction(*Func) << "\n";
    }

#ifdef __linux__
    asm_output << formatDirective(".section .note.GNU-stack,\"\",@progbits");
#endif

    return asm_output.str();
}

std::string X64CodeGenerator::emitFunction(const X64Function& Func) {
    // TODO: Emit single function assembly
    std::ostringstream func_output;
    
    // Function label
    func_output << formatDirective(".global " + Func.get_name().str() + "\n");
    func_output << formatLabel(Func.get_name()) << "\n";

    func_output << "    push rbp" << "\n";
    func_output << "    mov rbp, rsp" << "\n";
    bool contains_ret = false;
    // Function instructions
    for (const auto& Inst : Func) {
        if (dynamic_cast<const X64Ret*>(Inst)) {
            func_output << "    mov rsp, rbp" << "\n";
            func_output << "    pop rbp" << "\n";
            contains_ret = true;
        }

        func_output << "    " << emitInstruction(*Inst) << "\n";
    }

    if (!contains_ret) {
        func_output << "    mov rsp, rbp" << "\n";
        func_output << "    pop rbp" << "\n";
        func_output << "    ret\n";
    }
    
    return func_output.str();
}

std::string X64CodeGenerator::emitInstruction(const X64Instruction& Inst) {
    return Inst.to_string();
}

// ===== Assembly Formatting Helpers =====

std::string X64CodeGenerator::formatLabel(StringRef Name) {
    return std::string(Name) + ":";
}

std::string X64CodeGenerator::formatDirective(StringRef Directive) {
    return std::string(Directive);
}

std::string X64CodeGenerator::formatComment(StringRef Comment) {
    return "# " + std::string(Comment);
}

}
}
}