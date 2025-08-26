#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/APSInt.h"

#include <deque>
#include <vector>
#include <unordered_map>
#include <sstream>

namespace mycc {
namespace codegen {
namespace x64 {

class X64Program;
class X64Function;
class X64Instruction;
class X64Register;
class X64Operand;

using X64Functions = std::vector<X64Function*>;
using X64Instructions = std::deque<X64Instruction*>;

class X64Operand {
public:
    [[nodiscard]] virtual std::string to_string() const = 0;
};

class X64Int : public X64Operand {
    llvm::APSInt Value;
public:
    explicit X64Int(llvm::APSInt Value) : Value(std::move(Value)) {
    }

    llvm::APSInt& getValue() {
        return Value;
    }

    [[nodiscard]] const llvm::APSInt& getValue() const {
        return Value;
    }

    [[nodiscard]] std::string to_string() const override {
        return std::to_string(Value.getSExtValue());
    }
};

class X64Register : public X64Operand {
public:
    enum Type { Pseudo, Physical };
    [[nodiscard]] virtual Type getType() const = 0;
};

class PseudoRegister : public X64Register {
    unsigned ID; // Same as the IR Register
public:
    explicit PseudoRegister(unsigned id) : ID(id) {
    }

    [[nodiscard]] Type getType() const override {
        return Pseudo;
    }
    
    [[nodiscard]] unsigned getID() const {
        return ID;
    }

    [[nodiscard]] std::string to_string() const override {
        return "%r" + std::to_string(ID);
    }
};

class PhysicalRegister : public X64Register {
public:
    enum PhysReg { 
        RAX, RBX, RCX, RDX, 
        RSI, RDI, RSP, RBP,
        R8, R9, R10, R11, 
        R12, R13, R14, R15 
    };
    
    enum Size { BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8 };
    
private:
    PhysReg reg;
    Size regSize;
    
public:
    explicit PhysicalRegister(PhysReg r, Size s = QWORD) : reg(r), regSize(s) {}
    
    [[nodiscard]] Type getType() const override {
        return Physical;
    }
    
    void setReg(PhysReg r) {
        reg = r;
    }
    
    void setSize(Size s) {
        regSize = s;
    }
    
    [[nodiscard]] PhysReg getReg() const {
        return reg; 
    }
    
    [[nodiscard]] Size getSize() const {
        return regSize;
    }
    
    [[nodiscard]] std::string to_string() const override {
        std::string baseName;
        switch(reg) {
            case RAX: baseName = "ax"; break;
            case RBX: baseName = "bx"; break;
            case RCX: baseName = "cx"; break;
            case RDX: baseName = "dx"; break;
            case RSI: baseName = "si"; break;
            case RDI: baseName = "di"; break;
            case RSP: baseName = "sp"; break;
            case RBP: baseName = "bp"; break;
            case R8:  baseName = "8"; break;
            case R9:  baseName = "9"; break;
            case R10: baseName = "10"; break;
            case R11: baseName = "11"; break;
            case R12: baseName = "12"; break;
            case R13: baseName = "13"; break;
            case R14: baseName = "14"; break;
            case R15: baseName = "15"; break;
        }
        
        switch(regSize) {
            case BYTE:
                if (reg >= R8) return "r" + baseName + "b";
                return (reg <= RDX) ? baseName.substr(0,1) + "l" : baseName.substr(0,2) + "l";
            case WORD:
                if (reg >= R8) return "r" + baseName + "w";
                return baseName;
            case DWORD:
                if (reg >= R8) return "r" + baseName + "d";
                return "e" + baseName;
            case QWORD:
                if (reg >= R8) return "r" + baseName;
                return "r" + baseName;
        }
        return "";
    }
};

class X64Stack : public X64Operand {
public:
    enum Size { BYTE = 1, WORD = 2, DWORD = 4, QWORD = 8 };
    
private:
    llvm::APSInt Offset;
    X64Register * StackReg;
    Size AccessSize;
    
public:
    X64Stack() = default;

    X64Stack(llvm::APSInt Offset, X64Register * StackReg, Size AccessSize = QWORD)
        : Offset(std::move(Offset)), StackReg(StackReg), AccessSize(AccessSize) {
    }
    
    void setOffset(llvm::APSInt O) {
        Offset = std::move(O);
    }
    
    void setStackReg(X64Register * R) {
        StackReg = R;
    }
    
    void setSize(Size S) {
        AccessSize = S;
    }
    
    llvm::APSInt& getOffset() {
        return Offset;
    }
    
    [[nodiscard]] const llvm::APSInt& getOffset() const {
        return Offset;
    }
    
    X64Register* getStackReg() {
        return StackReg;
    }
    
    [[nodiscard]] const X64Register* getStackReg() const {
        return StackReg;
    }
    
    [[nodiscard]] Size getSize() const {
        return AccessSize;
    }
    
    [[nodiscard]] std::string to_string() const override {
        std::string ins;

        switch (AccessSize) {
            case QWORD:
                ins = "qword ptr ";
                break;
            case DWORD:
                ins = "dword ptr ";
                break;
            case WORD:
                ins = "word ptr ";
                break;
            case BYTE:
                ins = "byte ptr ";
                break;
        }
        ins += "[" + StackReg->to_string();
        
        int64_t offsetValue = Offset.getSExtValue();
        if (offsetValue != 0) {
            if (offsetValue > 0) {
                ins += "+" + std::to_string(offsetValue);
            } else {
                ins += std::to_string(offsetValue); // Already includes minus sign
            }
        }
        ins += "]";
        return ins;
    }
};

class X64Instruction {
public:
    virtual std::string to_string() const = 0;
};

class X64Mov : public X64Instruction {
    X64Operand * Src{};
    X64Operand * Dst{};
public:
    X64Mov() = default;

    X64Mov(X64Operand * Src, X64Operand * Dst)
        : Src(Src), Dst(Dst) {
    }

    void setSrc(X64Operand * S) {
        Src = S;
    }

    void setDst(X64Operand * D) {
        Dst = D;
    }

    X64Operand * getSrc() {
        return Src;
    }

    [[nodiscard]] const X64Operand * getSrc() const {
        return Src;
    }

    X64Operand * getDst() {
        return Dst;
    }

    [[nodiscard]] const X64Operand * getDst() const {
        return Dst;
    }

    [[nodiscard]] std::string to_string() const override {
        return "mov " + Dst->to_string() + ", " + Src->to_string();
    }
};

class X64Unary : public X64Instruction {
public:
    enum X64UnaryKind {
        Neg,
        Complement
    };
private:
    X64UnaryKind Kind;
    X64Operand * Op;
public:
    X64Unary() = default;

    X64Unary(X64UnaryKind Kind, X64Operand * Op) : Kind(Kind), Op(Op) {
    }

    void setKind(X64UnaryKind K) {
        Kind = K;
    }

    void setOperand(X64Operand * O) {
        Op = O;
    }

    [[nodiscard]] X64UnaryKind getKind() const {
        return Kind;
    }

    X64Operand * getOperand() {
        return Op;
    }

    [[nodiscard]] const X64Operand * getOperand() const {
        return Op;
    }

    [[nodiscard]] std::string to_string() const override {
        std::string opcode;
        switch (Kind) {
            case Neg:
                opcode = "neg";
                break;
            case Complement:
                opcode = "not";
                break;
            default:
                opcode = "";
        }
        return opcode + " " + Op->to_string();
    }
};

class X64Ret : public X64Instruction {
public:
    X64Ret() = default;

    [[nodiscard]] std::string to_string() const override {
        return "ret";
    }
};

class X64Allocate : public X64Instruction {
    X64Operand * allocationRegister;
    X64Operand * Offset;
public:
    X64Allocate() = default;

    X64Allocate(X64Operand * allocationRegister, X64Operand * Offset)
        : allocationRegister(allocationRegister), Offset(Offset) {
    }

    void setAllocationRegister(X64Operand * Reg) {
        allocationRegister = Reg;
    }

    void setOffset(X64Operand * O) {
        Offset = O;
    }

    [[nodiscard]] X64Operand * getAllocationRegister() const {
        return allocationRegister;
    }

    [[nodiscard]] X64Operand * getOffset() const {
        return Offset;
    }

    [[nodiscard]] std::string to_string() const override {
        return "sub " + allocationRegister->to_string() + ", " + Offset->to_string();
    }
};

class X64Context {
private:
    // Memory management - owns all operands and instructions
    std::vector<std::unique_ptr<X64Operand>> Operands;
    std::vector<std::unique_ptr<X64Instruction>> Instructions;
    
    // Register management
    std::unordered_map<unsigned, PseudoRegister*> PseudoRegs;           // ID -> PseudoReg
    std::unordered_map<PhysicalRegister::PhysReg, PhysicalRegister*> PhysRegs; // PhysReg -> PhysicalReg
    std::unordered_map<unsigned, PhysicalRegister::PhysReg> RegAllocation; // PseudoID -> PhysReg
    std::unordered_map<unsigned, X64Stack*> MemoryAlloc; // PseudoID -> X64Stack
    
    // Stack management
    int StackOffset = 0;
    
public:
    X64Context() = default;
    ~X64Context() = default;
    
    // Non-copyable, non-movable
    X64Context(const X64Context&) = delete;
    X64Context& operator=(const X64Context&) = delete;
    
    // Factory methods for operands
    PseudoRegister* getPseudoReg(unsigned ID) {
        auto it = PseudoRegs.find(ID);
        if (it != PseudoRegs.end()) {
            return it->second;
        }
        
        auto* reg = new PseudoRegister(ID);
        Operands.emplace_back(reg);
        PseudoRegs[ID] = reg;
        return reg;
    }
    
    PhysicalRegister* getPhysReg(PhysicalRegister::PhysReg physReg, PhysicalRegister::Size size = PhysicalRegister::QWORD) {
        auto it = PhysRegs.find(physReg);
        if (it != PhysRegs.end()) {
            // Update size if different
            it->second->setSize(size);
            return it->second;
        }
        
        auto* reg = new PhysicalRegister(physReg, size);
        Operands.emplace_back(reg);
        PhysRegs[physReg] = reg;
        return reg;
    }
    
    X64Int* createInt(llvm::APSInt value) {
        auto* intVal = new X64Int(std::move(value));
        Operands.emplace_back(intVal);
        return intVal;
    }
    
    X64Stack* createStack(llvm::APSInt offset, X64Register* baseReg, X64Stack::Size size = X64Stack::QWORD) {
        auto* stackVal = new X64Stack(std::move(offset), baseReg, size);
        Operands.emplace_back(stackVal);
        return stackVal;
    }
    
    // Register allocation methods
    void allocateReg(unsigned pseudoID, PhysicalRegister::PhysReg physReg) {
        RegAllocation[pseudoID] = physReg;
    }
    
    PhysicalRegister* getAllocatedReg(unsigned pseudoID) {
        auto it = RegAllocation.find(pseudoID);
        if (it != RegAllocation.end()) {
            return getPhysReg(it->second);
        }
        return nullptr; // Not allocated yet
    }
    
    bool isAllocated(unsigned pseudoID) const {
        return RegAllocation.find(pseudoID) != RegAllocation.end();
    }
    
    // Memory allocation methods for pseudo registers
    void allocateMemory(unsigned pseudoID, X64Stack::Size size = X64Stack::QWORD) {
        StackOffset -= size;
        PhysicalRegister* rbp = getPhysReg(PhysicalRegister::RBP);
        X64Stack* stackSlot = createStack(llvm::APSInt(llvm::APInt(64, StackOffset)), rbp, size);
        MemoryAlloc[pseudoID] = stackSlot;
    }
    
    void allocateMemory(unsigned pseudoID, X64Stack* stackSlot) {
        MemoryAlloc[pseudoID] = stackSlot;
    }
    
    X64Stack* getAllocatedMemory(unsigned pseudoID) {
        auto it = MemoryAlloc.find(pseudoID);
        if (it != MemoryAlloc.end()) {
            return it->second;
        }
        return nullptr; // Not allocated to memory
    }
    
    bool isAllocatedToMemory(unsigned pseudoID) const {
        return MemoryAlloc.find(pseudoID) != MemoryAlloc.end();
    }
    
    // Check allocation status
    enum AllocationType { None, Register, Memory };
    AllocationType getAllocationType(unsigned pseudoID) const {
        if (isAllocated(pseudoID)) return Register;
        if (isAllocatedToMemory(pseudoID)) return Memory;
        return None;
    }
    
    // Get the actual operand for a pseudo register (register or memory)
    X64Operand* getOperandForPseudo(unsigned pseudoID) {
        if (isAllocated(pseudoID)) {
            return getAllocatedReg(pseudoID);
        }
        if (isAllocatedToMemory(pseudoID)) {
            return getAllocatedMemory(pseudoID);
        }
        return getPseudoReg(pseudoID); // Still pseudo
    }
    
    // Stack management
    X64Stack* allocateStack(X64Stack::Size size = X64Stack::QWORD) {
        StackOffset -= size;
        PhysicalRegister* rbp = getPhysReg(PhysicalRegister::RBP);
        return createStack(llvm::APSInt(llvm::APInt(64, StackOffset)), rbp, size);
    }
    
    int getStackOffset() const {
        return StackOffset;
    }
    
    // Factory methods for instructions
    X64Mov* createMov(X64Operand* src, X64Operand* dst) {
        auto* inst = new X64Mov(src, dst);
        Instructions.emplace_back(inst);
        return inst;
    }
    
    X64Unary* createUnary(X64Unary::X64UnaryKind kind, X64Operand* op) {
        auto* inst = new X64Unary(kind, op);
        Instructions.emplace_back(inst);
        return inst;
    }
    
    X64Ret* createRet() {
        auto* inst = new X64Ret();
        Instructions.emplace_back(inst);
        return inst;
    }

    X64Allocate* createAllocation(X64Operand * Offset) {
        auto * RSP = getPhysReg(PhysicalRegister::PhysReg::RSP, PhysicalRegister::Size::QWORD);
        auto * inst = new X64Allocate(RSP, Offset);
        Instructions.emplace_back(inst);
        return inst;
    }
};

class X64Function {
    X64Instructions Instrs;
    StringRef FuncName;
    std::unique_ptr<X64Context> Ctx;
    
public:
    X64Function() : Ctx(std::make_unique<X64Context>()) {}
    explicit X64Function(StringRef Name) : FuncName(Name), Ctx(std::make_unique<X64Context>()) {
    }

    X64Function(X64Instructions & Instrs, StringRef Name) :
        Instrs(std::move(Instrs)), FuncName(Name), Ctx(std::make_unique<X64Context>()) {
    }

    X64Context& getContext() { 
        return *Ctx; 
    }
    
    [[nodiscard]] StringRef get_name() const {
        return FuncName;
    }

    [[nodiscard]] size_t size() const {
        return Instrs.size();
    }

    [[nodiscard]] bool empty() const {
        return Instrs.empty();
    }

    void add_instruction(X64Instruction * I) {
        Instrs.push_back(I);
    }
    
    X64Instructions& getInstructions() {
        return Instrs;
    }
    
    [[nodiscard]] const X64Instructions& getInstructions() const {
        return Instrs;
    }

    X64Instructions::iterator begin() {
        return Instrs.begin();
    }

    X64Instructions::iterator end() {
        return Instrs.end();
    }

    [[nodiscard]] X64Instructions::const_iterator begin() const {
        return Instrs.begin();
    }

    [[nodiscard]] X64Instructions::const_iterator end() const {
        return Instrs.end();
    }
};

class X64Program {
    X64Functions Funcs;
    StringRef Name;
public:
    X64Program() = default;
    explicit X64Program(StringRef Name) : Name(Name) {
    }

    X64Program(X64Functions & Funcs, StringRef Name) :
        Funcs(std::move(Funcs)), Name(Name) {
    }

    ~X64Program() {
        for (auto & F : Funcs) {
            delete F;
        }
    }

    [[nodiscard]] StringRef get_name() const {
        return Name;
    }

    [[nodiscard]] size_t size() const {
        return Funcs.size();
    }

    [[nodiscard]] bool empty() const {
        return Funcs.empty();
    }

    void add_function(X64Function * F) {
        Funcs.push_back(F);
    }

    X64Functions::iterator begin() {
        return Funcs.begin();
    }

    X64Functions::iterator end() {
        return Funcs.end();
    }

    [[nodiscard]] X64Functions::const_iterator begin() const {
        return Funcs.begin();
    }

    [[nodiscard]] X64Functions::const_iterator end() const {
        return Funcs.end();
    }
};

}
}
}