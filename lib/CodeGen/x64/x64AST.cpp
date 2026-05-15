#include "mycc/CodeGen/x64/x64AST.hpp"

#include <cassert>
#include <string>

namespace mycc::codegen::x64 {

// ============================================================
// Operand to_string implementations
// ============================================================

std::string X64Int::to_string() const {
    return std::to_string(Value.getSExtValue());
}

std::string PhysicalRegister::to_string() const {
    std::string baseName;
    switch (reg) {
        case RAX: baseName = "ax"; break;
        case RBX: baseName = "bx"; break;
        case RCX: baseName = "cx"; break;
        case RDX: baseName = "dx"; break;
        case RSI: baseName = "si"; break;
        case RDI: baseName = "di"; break;
        case RSP: baseName = "sp"; break;
        case RBP: baseName = "bp"; break;
        case R8:  baseName = "8";  break;
        case R9:  baseName = "9";  break;
        case R10: baseName = "10"; break;
        case R11: baseName = "11"; break;
        case R12: baseName = "12"; break;
        case R13: baseName = "13"; break;
        case R14: baseName = "14"; break;
        case R15: baseName = "15"; break;
    }

    switch (regSize) {
        case X64Type::Byte:
            if (reg >= R8) return "r" + baseName + "b";
            return (reg <= RDX) ? baseName.substr(0, 1) + "l" : baseName.substr(0, 2) + "l";
        case X64Type::Word:
            if (reg >= R8) return "r" + baseName + "w";
            return baseName;
        case X64Type::LongWord:
            if (reg >= R8) return "r" + baseName + "d";
            return "e" + baseName;
        case X64Type::QuadWord:
            if (reg >= R8) return "r" + baseName;
            return "r" + baseName;
    }
    return "";
}

std::string PseudoRegister::to_string() const {
    return "%r" + std::to_string(ID);
}

std::string X64Stack::to_string() const {
    std::string ins = getPtrDirective(AccessSize);
    ins += " ";
    ins += "[" + StackReg->to_string();

    int64_t offsetValue = Offset.getSExtValue();
    if (offsetValue != 0) {
        if (offsetValue > 0) {
            ins += "+" + std::to_string(offsetValue);
        } else {
            ins += std::to_string(offsetValue);
        }
    }
    ins += "]";
    return ins;
}

std::string X64Data::to_string() const {
    return std::string(getPtrDirective(Size)) + " [rip + " + Name + "]";
}

// ============================================================
// Instruction to_string implementations
// ============================================================

std::string X64Label::to_string() const {
    std::string prefix;
#ifdef __linux__
    prefix = ".L";
#endif
    return prefix + Name.str();
}

std::string X64Mov::to_string() const {
    return "mov " + Dst->to_string() + ", " + Src->to_string();
}

std::string X64Movsx::to_string() const {
    const std::string mnemonic =
        (TypeFrom == X64Type::LongWord && TypeTo == X64Type::QuadWord)
            ? "movsxd" : "movsx";
    return mnemonic + " " + Dst->to_string() + ", " + Src->to_string();
}

std::string X64Cmp::to_string() const {
    return "cmp " + Left->to_string() + ", " + Right->to_string();
}

std::string X64Jmp::to_string() const {
    return "jmp " + Target->to_string();
}

std::string X64JmpCC::to_string() const {
    std::string ins;
    switch (Condition) {
        case X64ConditionTypeE::E:  ins = "je";  break;
        case X64ConditionTypeE::NE: ins = "jne"; break;
        case X64ConditionTypeE::G:  ins = "jg";  break;
        case X64ConditionTypeE::GE: ins = "jge"; break;
        case X64ConditionTypeE::L:  ins = "jl";  break;
        case X64ConditionTypeE::LE: ins = "jle"; break;
    }
    return ins + " " + Target->to_string();
}

std::string X64SetCC::to_string() const {
    std::string ins;
    switch (Condition) {
        case X64ConditionTypeE::E:  ins = "sete";  break;
        case X64ConditionTypeE::NE: ins = "setne"; break;
        case X64ConditionTypeE::G:  ins = "setg";  break;
        case X64ConditionTypeE::GE: ins = "setge"; break;
        case X64ConditionTypeE::L:  ins = "setl";  break;
        case X64ConditionTypeE::LE: ins = "setle"; break;
    }
    return ins + " " + Op->to_string();
}

std::string X64Unary::to_string() const {
    std::string opcode;
    switch (Kind) {
        case Neg:        opcode = "neg"; break;
        case Complement: opcode = "not"; break;
        default:         opcode = "";
    }
    return opcode + " " + Op->to_string();
}

std::string X64Binary::to_string() const {
    std::string opcode;
    switch (Kind) {
        case Add:  opcode = "add";  break;
        case Sub:  opcode = "sub";  break;
        case Mult: opcode = "imul"; break;
        case And:  opcode = "and";  break;
        case Or:   opcode = "or";   break;
        case Xor:  opcode = "xor";  break;
        case Sal:  opcode = "sal";  break;
        case Sar:  opcode = "sar";  break;
        default: break;
    }
    return opcode + " " + dst->to_string() + ", " + src->to_string();
}

std::string X64IDiv::to_string() const {
    return "idiv " + Op->to_string();
}

std::string X64Cdq::to_string() const {
    return Size == X64Type::QuadWord ? "cqo" : "cdq";
}

std::string X64Ret::to_string() const {
    return "ret";
}

std::string X64Allocate::to_string() const {
    return "sub " + allocationRegister->to_string() + ", " + Offset->to_string();
}

std::string X64Deallocate::to_string() const {
    return "add " + deAllocationRegister->to_string() + ", " + Offset->to_string();
}

std::string X64Push::to_string() const {
    return "push " + pushOp->to_string();
}

std::string X64Call::to_string() const {
    return "call " + functionName;
}

// ============================================================
// X64Context
// ============================================================

PseudoRegister *X64Context::getPseudoReg(unsigned ID, X64Type size) {
    auto it = PseudoRegs.find(ID);
    if (it != PseudoRegs.end()) {
        return it->second;
    }

    auto *reg = new PseudoRegister(ID, size);
    Operands.emplace_back(reg);
    PseudoRegs[ID] = reg;
    return reg;
}

PhysicalRegister *X64Context::getPhysReg(PhysicalRegister::PhysReg physReg, X64Type size) {
    auto *reg = new PhysicalRegister(physReg, size);
    Operands.emplace_back(reg);
    return reg;
}

X64Int *X64Context::createInt(llvm::APSInt value) {
    auto *intVal = new X64Int(std::move(value));
    Operands.emplace_back(intVal);
    return intVal;
}

X64Stack *X64Context::createStack(llvm::APSInt offset, X64Register *baseReg, X64Type size) {
    auto *stackVal = new X64Stack(std::move(offset), baseReg, size);
    Operands.emplace_back(stackVal);
    return stackVal;
}

X64Data *X64Context::createData(StringRef Name, X64Type size) {
    auto *x64Data = new X64Data(Name, size);
    Operands.emplace_back(x64Data);
    return x64Data;
}

void X64Context::allocateReg(unsigned pseudoID, PhysicalRegister::PhysReg physReg) {
    RegAllocation[pseudoID] = physReg;
}

PhysicalRegister *X64Context::getAllocatedReg(unsigned pseudoID) {
    auto it = RegAllocation.find(pseudoID);
    if (it != RegAllocation.end()) {
        return getPhysReg(it->second);
    }
    return nullptr;
}

bool X64Context::isAllocated(unsigned pseudoID) const {
    return RegAllocation.find(pseudoID) != RegAllocation.end();
}

void X64Context::allocateMemory(unsigned pseudoID, X64Type size) {
    StackOffset -= static_cast<int>(getSizeInBytes(size));
    PhysicalRegister *rbp = getPhysReg(PhysicalRegister::RBP);
    X64Stack *stackSlot = createStack(llvm::APSInt(llvm::APInt(64, StackOffset)), rbp, size);
    MemoryAlloc[pseudoID] = stackSlot;
}

void X64Context::allocateMemory(unsigned pseudoID, X64Stack *stackSlot) {
    MemoryAlloc[pseudoID] = stackSlot;
}

X64Stack *X64Context::getAllocatedMemory(unsigned pseudoID) {
    auto it = MemoryAlloc.find(pseudoID);
    if (it != MemoryAlloc.end()) {
        return it->second;
    }
    return nullptr;
}

bool X64Context::isAllocatedToMemory(unsigned pseudoID) const {
    return MemoryAlloc.find(pseudoID) != MemoryAlloc.end();
}

X64Context::AllocationType X64Context::getAllocationType(unsigned pseudoID) const {
    if (isAllocated(pseudoID)) return Register;
    if (isAllocatedToMemory(pseudoID)) return Memory;
    return None;
}

X64Operand *X64Context::getOperandForPseudo(unsigned pseudoID) {
    if (isAllocated(pseudoID)) {
        return getAllocatedReg(pseudoID);
    }
    if (isAllocatedToMemory(pseudoID)) {
        return getAllocatedMemory(pseudoID);
    }
    return getPseudoReg(pseudoID);
}

X64Stack *X64Context::allocateStack(X64Type size) {
    StackOffset -= static_cast<int>(getSizeInBytes(size));
    PhysicalRegister *rbp = getPhysReg(PhysicalRegister::RBP);
    return createStack(llvm::APSInt(llvm::APInt(64, StackOffset)), rbp, size);
}

X64Stack *X64Context::getAllocatedStack(X64Stack *stack_access, X64Type size) {
    assert(stack_access != nullptr && "Stack access provided must not be nullptr.");
    PhysicalRegister *rbp = getPhysReg(PhysicalRegister::RBP);
    return createStack(llvm::APSInt(stack_access->getOffset()), rbp, size);
}

X64Label *X64Context::getOrCreateLabel(StringRef Name) {
    std::string NameStr{Name.str()};
    if (existingLabels.contains(NameStr))
        return existingLabels[NameStr];
    auto *label = new X64Label(Name);
    Instructions.emplace_back(label);
    existingLabels[NameStr] = label;
    return label;
}

X64Cmp *X64Context::createCmp(X64Operand *Left, X64Operand *Right, X64Type type) {
    auto *cmp = new X64Cmp(Left, Right, type);
    Instructions.emplace_back(cmp);
    return cmp;
}

X64Jmp *X64Context::createJmp(X64Label *Label) {
    auto *jmp = new X64Jmp(Label);
    Instructions.emplace_back(jmp);
    return jmp;
}

X64JmpCC *X64Context::createJCC(X64ConditionTypeE Condition, X64Label *Label) {
    auto *jmp = new X64JmpCC(Condition, Label);
    Instructions.emplace_back(jmp);
    return jmp;
}

X64SetCC *X64Context::createSetCC(X64ConditionTypeE Condition, X64Operand *Op) {
    auto *set = new X64SetCC(Condition, Op);
    Instructions.emplace_back(set);
    return set;
}

X64Mov *X64Context::createMov(X64Operand *src, X64Operand *dst) {
    auto *inst = new X64Mov(src, dst);
    Instructions.emplace_back(inst);
    return inst;
}

X64Movsx *X64Context::createMovsx(X64Operand *src, X64Operand *dst, X64Type TypeFrom, X64Type TypeTo) {
    auto *inst = new X64Movsx(src, dst, TypeFrom, TypeTo);
    Instructions.emplace_back(inst);
    return inst;
}

X64Unary *X64Context::createUnary(X64Unary::X64UnaryKind kind, X64Operand *op, X64Type type) {
    auto *inst = new X64Unary(kind, op, type);
    Instructions.emplace_back(inst);
    return inst;
}

X64Binary *X64Context::createBinary(X64Binary::X64BinaryKind kind, X64Operand *Src, X64Operand *Dst, X64Type type) {
    auto *inst = new X64Binary(kind, Src, Dst, type);
    Instructions.emplace_back(inst);
    return inst;
}

X64IDiv *X64Context::createIDiv(X64Operand *op, X64Type type) {
    auto *inst = new X64IDiv(op, type);
    Instructions.emplace_back(inst);
    return inst;
}

X64Cdq *X64Context::createCdq(X64Type type) {
    auto *inst = new X64Cdq(type);
    Instructions.emplace_back(inst);
    return inst;
}

X64Ret *X64Context::createRet() {
    auto *inst = new X64Ret();
    Instructions.emplace_back(inst);
    return inst;
}

X64Allocate *X64Context::createAllocation(X64Operand *Offset) {
    auto *RSP = getPhysReg(PhysicalRegister::PhysReg::RSP, X64Type::QuadWord);
    auto *inst = new X64Allocate(RSP, Offset);
    Instructions.emplace_back(inst);
    return inst;
}

X64Deallocate *X64Context::createDeallocation(X64Operand *Offset) {
    auto *RSP = getPhysReg(PhysicalRegister::PhysReg::RSP, X64Type::QuadWord);
    auto *inst = new X64Deallocate(RSP, Offset);
    Instructions.emplace_back(inst);
    return inst;
}

X64Push *X64Context::createPush(X64Operand *Op) {
    auto *inst = new X64Push(Op);
    Instructions.emplace_back(inst);
    return inst;
}

X64Call *X64Context::createCall(const std::string &functionName) {
    auto *inst = new X64Call(functionName);
    Instructions.emplace_back(inst);
    return inst;
}

// ============================================================
// X64Program
// ============================================================

X64Program::~X64Program() {
    for (auto &F: Funcs) {
        delete F;
    }
    for (auto &S: StaticVars) {
        delete S;
    }
}

} // namespace mycc::codegen::x64
