#include "mycc/CodeGen/x64/X64CodeGen.hpp"

#include "mycc/IR/SimpleIR.hpp"
#include "mycc/CodeGen/x64/x64AST.hpp"

#include <array>
#include <sstream>
#include <vector>

using namespace mycc::codegen::x64;

namespace {
    /**
     * Check for an integer if it is a large value or not in sign extended.
     * @param imm integer to check.
     * @return `true` if the value is larger than 32 bits, `false` otherwise.
     */
    bool isLargeImmediate(const X64Int *imm) {
        const int64_t val = imm->getValue().getSExtValue();
        return val < INT32_MIN || val > INT32_MAX;
    }

    /**
     * Check for type of memory operands, these are those that belong
     * to the stack, or to data.
     *
     * @param op operand to check.
     * @return `true` if operand is a memory operand, `false` otherwise.
     */
    bool isMemoryOperand(X64Operand *op) {
        return dynamic_cast<X64Stack *>(op) != nullptr
            || dynamic_cast<X64Data *>(op) != nullptr;
    }

    /**
     * Helper method to obtain a X64Type from an IR type.
     *
     * @param t IR type to convert.
     * @return X64Type equivalent to the IR one..
     */
    X64Type getTypeFromIR(mycc::ir::Type *t) {
        if (llvm::isa<mycc::ir::IntType>(t)) {
            auto *it = llvm::cast<mycc::ir::IntType>(t);
            switch (it->getBitWidth()) {
                case 8:
                    return X64Type::Byte;
                case 16:
                    return X64Type::Word;
                case 32:
                    return X64Type::LongWord;
                case 64:
                    return X64Type::QuadWord;
                default:
                    return X64Type::Byte;
            }
        }
        // We shouldn't reach here
        return X64Type::Byte;
    }
};

// ===== Main X64CodeGenerator Implementation =====

void X64CodeGenerator::generateX64AST(const ir::Program &IRProg) {
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

void X64CodeGenerator::generateProgram(const ir::Program &IRProg) {
    Program = std::make_unique<X64Program>();

    for (const auto *IRStaticVar: IRProg.getStaticVars()) {
        auto *staticVar = new X64StaticVar(IRStaticVar->getName(), IRStaticVar->isGlobal(),
                                           IRStaticVar->getInitialValue(), getTypeFromIR(IRStaticVar->getType()));
        Program->add_static_var(staticVar);
        SymTab[IRStaticVar->getName()] = ObjEntry{
            getTypeFromIR(IRStaticVar->getType()),
            /*IsStatic=*/true
        };
    }

    for (const auto *IRFunc: IRProg) {
        const bool defined = !IRFunc->empty();
        SymTab[IRFunc->get_name()] = FunEntry{defined};
        if (!defined) {
            continue;
        }

        auto *X64Func = new X64Function(IRFunc->get_name(), IRFunc->isGlobal());
        Program->add_function(X64Func);
        generateFunction(*IRFunc, X64Func);
    }
}

void X64CodeGenerator::generateFunction(const ir::Function &IRFunc, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();

    const std::array paramRegEnums = {
        PhysicalRegister::PhysReg::RDI, PhysicalRegister::PhysReg::RSI,
        PhysicalRegister::PhysReg::RDX, PhysicalRegister::PhysReg::RCX,
        PhysicalRegister::PhysReg::R8,  PhysicalRegister::PhysReg::R9,
    };

    const auto &params = IRFunc.getArgs();

    // Handle first 6 parameters (passed in registers), sized by each param's actual type.
    for (size_t i = 0; i < params.size() && i < 6; i++) {
        X64Type paramType = getTypeFromIR(params[i]->getType());
        X64Operand *paramSrc = Ctx.getPhysReg(paramRegEnums[i], paramType);
        X64Operand *paramDst = convertOperand(params[i], Ctx);
        X64Func->add_instruction(Ctx.createMov(paramSrc, paramDst));
    }

    // Handle parameters 7+ (passed on the stack).
    // Stack layout: [rbp] = old rbp, [rbp+8] = return address, [rbp+16] = 7th param, ...
    PhysicalRegister *rbp = Ctx.getPhysReg(PhysicalRegister::PhysReg::RBP, X64Type::QuadWord);
    for (size_t i = 6; i < params.size(); i++) {
        int64_t stackOffset = 16 + (i - 6) * 8;
        X64Type paramType = getTypeFromIR(params[i]->getType());
        X64Stack *paramSrc = Ctx.createStack(llvm::APSInt(llvm::APInt(64, stackOffset)), rbp, paramType);
        X64Operand *paramDst = convertOperand(params[i], Ctx);
        X64Func->add_instruction(Ctx.createMov(paramSrc, paramDst));
    }

    // Generate the rest of the function instructions
    for (const auto *Instr: IRFunc) {
        generateInstruction(*Instr, X64Func);
    }
}

void X64CodeGenerator::generateInstruction(const ir::Instruction &Inst, X64Function *X64Func) {
    if (const auto *mov = dynamic_cast<const ir::Mov *>(&Inst)) {
        generateMov(*mov, X64Func);
    } else if (const auto *copy = dynamic_cast<const ir::Copy *>(&Inst)) {
        generateCopy(*copy, X64Func);
    } else if (const auto *ret = dynamic_cast<const ir::Ret *>(&Inst)) {
        generateRet(*ret, X64Func);
    } else if (const auto *unary = dynamic_cast<const ir::UnaryOp *>(&Inst)) {
        generateUnary(*unary, X64Func);
    } else if (const auto *binary = dynamic_cast<const ir::BinaryOp *>(&Inst)) {
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
            case ir::BinaryOp::none:
                break;
        }
    } else if (const auto *label = dynamic_cast<const ir::Label *>(&Inst)) {
        generateLabel(*label, X64Func);
    } else if (const auto *Jump = dynamic_cast<const ir::Jump *>(&Inst)) {
        generateJump(*Jump, X64Func);
    } else if (const auto *JNZ = dynamic_cast<const ir::JumpIfNotZero *>(&Inst)) {
        generateJumpIfNotZero(*JNZ, X64Func);
    } else if (const auto *JZ = dynamic_cast<const ir::JumpIfZero *>(&Inst)) {
        generateJumpIfZero(*JZ, X64Func);
    } else if (const auto *Comp = dynamic_cast<const ir::ICmpOp *>(&Inst)) {
        generateComp(*Comp, X64Func);
    } else if (const auto *Invoke = dynamic_cast<const ir::Invoke *>(&Inst)) {
        generateCall(*Invoke, X64Func);
    } else if (const auto *SignExt = dynamic_cast<const ir::SignExtend*>(&Inst)) {
        generateSignExtend(*SignExt, X64Func);
    } else if (const auto *Trunc = dynamic_cast<const ir::Truncate*>(&Inst)) {
        generateTruncate(*Trunc, X64Func);
    }
}

void X64CodeGenerator::generateLabel(const ir::Label &LabelInst, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    auto *X64Label = Ctx.getOrCreateLabel(LabelInst.get_identifier());
    X64Func->add_instruction(X64Label);
}

void X64CodeGenerator::generateJump(const ir::Jump &JumpInst, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    // Simply generate/retrieve label where to jump, and add the jump instruction
    // the label is not added to the instruction list yet, it will be added
    // once it is found in the code.
    auto *X64Label = Ctx.getOrCreateLabel(JumpInst.getDst()->get_identifier());
    auto *X64Jump = Ctx.createJmp(X64Label);
    X64Func->add_instruction(X64Jump);
}

void X64CodeGenerator::generateJumpIfNotZero(const ir::JumpIfNotZero &JumpIfNZInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    // Generate/Retrieve label where to jump
    auto *X64Label = Ctx.getOrCreateLabel(JumpIfNZInstr.getDst()->get_identifier());
    // Take and generate the condition, as well as a zero value to compare with
    auto *condition = convertOperand(JumpIfNZInstr.getCondition(), Ctx);
    X64Int *zeroValue = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 0)));
    auto *Cmp = Ctx.createCmp(condition, zeroValue);
    X64Func->add_instruction(Cmp);
    // generate a conditional jump of if not equal
    auto *X64JNZ = Ctx.createJCC(X64ConditionTypeE::NE, X64Label);
    X64Func->add_instruction(X64JNZ);
}

void X64CodeGenerator::generateJumpIfZero(const ir::JumpIfZero &JumpIfZInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    // Generate/Retrieve label where to jump
    auto *X64Label = Ctx.getOrCreateLabel(JumpIfZInstr.getDst()->get_identifier());
    // Take and generate the condition, as well as a zero value to compare with
    auto *condition = convertOperand(JumpIfZInstr.getCondition(), Ctx);
    X64Int *zeroValue = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 0)));
    auto *Cmp = Ctx.createCmp(condition, zeroValue);
    X64Func->add_instruction(Cmp);
    // generate a conditional jump of if not equal
    auto *X64JZ = Ctx.createJCC(X64ConditionTypeE::E, X64Label);
    X64Func->add_instruction(X64JZ);
}

void X64CodeGenerator::generateComp(const ir::ICmpOp &CompInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    // Get the operands for the comparison and the destination where
    // the comparison will be stored
    X64Operand *Left = convertOperand(CompInstr.getLeft(), Ctx);
    X64Operand *Right = convertOperand(CompInstr.getRight(), Ctx);
    X64Operand *Dst = convertOperand(CompInstr.getDestination(), Ctx);
    const X64Type type = getTypeFromIR(
        dynamic_cast<const ir::Operand *>(CompInstr.getLeft())->getType());
    // Because we will use setXX to set the value of the register
    // we need to zero the register, so we use a `MOV` instruction
    // with zero, to set the whole value of the register.
    X64Int *zeroValue = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 0)));
    X64ConditionTypeE condition_type_e{};
    switch (CompInstr.getKind()) {
        case ir::ICmpOp::lt:
            condition_type_e = X64ConditionTypeE::L;
            break;
        case ir::ICmpOp::le:
            condition_type_e = X64ConditionTypeE::LE;
            break;
        case ir::ICmpOp::gt:
            condition_type_e = X64ConditionTypeE::G;
            break;
        case ir::ICmpOp::ge:
            condition_type_e = X64ConditionTypeE::GE;
            break;
        case ir::ICmpOp::eq:
            condition_type_e = X64ConditionTypeE::E;
            break;
        case ir::ICmpOp::neq:
            condition_type_e = X64ConditionTypeE::NE;
            break;
        case ir::ICmpOp::none:
            break;
    }
    // comparison between values
    auto *Comp = Ctx.createCmp(Left, Right, type);
    X64Func->add_instruction(Comp);
    // initialization of destination register
    auto *Mov = Ctx.createMov(zeroValue, Dst);
    X64Func->add_instruction(Mov);
    // set the approppriate RFLAG
    auto *SetCC = Ctx.createSetCC(condition_type_e, Dst);
    X64Func->add_instruction(SetCC);
}

void X64CodeGenerator::generateMov(const ir::Mov &MovInst, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *Src = convertOperand(MovInst.getSrc(), Ctx);
    X64Operand *Dst = convertOperand(MovInst.getDst(), Ctx);
    auto Mov = Ctx.createMov(Src, Dst);
    X64Func->add_instruction(Mov);
}

void X64CodeGenerator::generateCopy(const ir::Copy &CopyInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *Src = convertOperand(CopyInstr.getSrc(), Ctx);
    X64Operand *Dst = convertOperand(CopyInstr.getDst(), Ctx);
    auto Mov = Ctx.createMov(Src, Dst);
    X64Func->add_instruction(Mov);
}

void X64CodeGenerator::generateRet(const ir::Ret &RetInst, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    if (!RetInst.isVoidReturn()) {
        X64Operand *RetValue = convertOperand(RetInst.getReturnValue(), Ctx);
        X64Type retType = X64Type::LongWord;
        if (const auto *op = dynamic_cast<const ir::Operand *>(RetInst.getReturnValue()))
            retType = getTypeFromIR(op->getType());
        X64Operand *RetReg = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, retType);
        X64Func->add_instruction(Ctx.createMov(RetValue, RetReg));
    }
    X64Func->add_instruction(Ctx.createRet());
}

void X64CodeGenerator::generateUnary(const ir::UnaryOp &UnaryInst, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *tempRegister = convertOperand(UnaryInst.getDestination(), Ctx);
    X64Operand *operand = convertOperand(UnaryInst.getSource(), Ctx);
    const X64Type type = getTypeFromIR(UnaryInst.getDestination()->getType());
    X64Unary::X64UnaryKind Kind = X64Unary::None;
    switch (UnaryInst.getKind()) {
        case ir::UnaryOp::UnaryOpKind::Neg:
            Kind = X64Unary::X64UnaryKind::Neg;
            break;
        case ir::UnaryOp::UnaryOpKind::Complement:
            Kind = X64Unary::X64UnaryKind::Complement;
            break;
        case ir::UnaryOp::UnaryOpKind::Not:
            // not instruction from SimpleIR does not
            // generate a Unary instruction like we had before
            // it generates a comparison with zero
            // !x is equals to x == 0
            X64Int *zeroValue = Ctx.createInt(llvm::APSInt(llvm::APInt(32, 0)));
            X64Instruction *compInstruction = Ctx.createCmp(operand, zeroValue, type);
            X64Instruction *initDst = Ctx.createMov(zeroValue, tempRegister);
            X64Instruction *setCC = Ctx.createSetCC(X64ConditionTypeE::E, tempRegister);
            X64Func->add_instructions(compInstruction, initDst, setCC);
            return;
    }
    // First a Move from Operand to TempRegister
    X64Func->add_instruction(Ctx.createMov(operand, tempRegister));
    // Second, we generate the unary operator
    X64Func->add_instruction(Ctx.createUnary(Kind, tempRegister, type));
}

void X64CodeGenerator::generateBinary(const ir::BinaryOp &BinaryInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *dst = convertOperand(BinaryInstr.getDestination(), Ctx);
    X64Operand *src1 = convertOperand(BinaryInstr.getLeft(), Ctx);
    X64Operand *src2 = convertOperand(BinaryInstr.getRight(), Ctx);
    const X64Type type = getTypeFromIR(BinaryInstr.getDestination()->getType());
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
        default:
            break;
    }
    X64Func->add_instruction(Ctx.createMov(src1, dst));
    X64Func->add_instruction(Ctx.createBinary(Kind, src2, dst, type));
}

void X64CodeGenerator::generateDiv(const ir::BinaryOp &BinaryInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *dst = convertOperand(BinaryInstr.getDestination(), Ctx);
    X64Operand *src1 = convertOperand(BinaryInstr.getLeft(), Ctx);
    X64Operand *src2 = convertOperand(BinaryInstr.getRight(), Ctx);
    const X64Type type = getTypeFromIR(BinaryInstr.getDestination()->getType());
    X64Operand *eax = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, type);
    X64Func->add_instruction(Ctx.createMov(src1, eax));
    X64Func->add_instruction(Ctx.createCdq(type));
    X64Func->add_instruction(Ctx.createIDiv(src2, type));
    X64Func->add_instruction(Ctx.createMov(eax, dst));
}

void X64CodeGenerator::generateRem(const ir::BinaryOp &BinaryInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *dst = convertOperand(BinaryInstr.getDestination(), Ctx);
    X64Operand *src1 = convertOperand(BinaryInstr.getLeft(), Ctx);
    X64Operand *src2 = convertOperand(BinaryInstr.getRight(), Ctx);
    const X64Type type = getTypeFromIR(BinaryInstr.getDestination()->getType());
    X64Operand *eax = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, type);
    X64Operand *edx = Ctx.getPhysReg(PhysicalRegister::PhysReg::RDX, type);
    X64Func->add_instruction(Ctx.createMov(src1, eax));
    X64Func->add_instruction(Ctx.createCdq(type));
    X64Func->add_instruction(Ctx.createIDiv(src2, type));
    X64Func->add_instruction(Ctx.createMov(edx, dst));
}

void X64CodeGenerator::generateSignExtend(const ir::SignExtend& SIgnExtend, X64Function* X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *dst = convertOperand(SIgnExtend.getResult(), Ctx);
    X64Operand *src = convertOperand(SIgnExtend.getSource(), Ctx);
    const X64Type typeFrom = getTypeFromIR(SIgnExtend.getSource()->getType());
    const X64Type typeTo = getTypeFromIR(SIgnExtend.getResult()->getType());
    X64Func->add_instruction(Ctx.createMovsx(src, dst, typeFrom, typeTo));
}

void X64CodeGenerator::generateTruncate(const ir::Truncate& TruncInst, X64Function* X64Func) {
    auto &Ctx = X64Func->getContext();
    X64Operand *dst = convertOperand(TruncInst.getResult(), Ctx);
    X64Operand *src = convertOperand(TruncInst.getSource(), Ctx);
    const X64Type type = getTypeFromIR(TruncInst.getResult()->getType());
    X64Func->add_instruction(Ctx.createMov(src, dst));
}


void X64CodeGenerator::generateCall(const ir::Invoke &InvokeInstr, X64Function *X64Func) {
    auto &Ctx = X64Func->getContext();

    const std::array argRegs = {
        PhysicalRegister::PhysReg::RDI,
        PhysicalRegister::PhysReg::RSI,
        PhysicalRegister::PhysReg::RDX,
        PhysicalRegister::PhysReg::RCX,
        PhysicalRegister::PhysReg::R8,
        PhysicalRegister::PhysReg::R9,
    };

    // In X86-64 the stack must be aligned to 16 bytes, we need to calculate
    // how many arguments will go to the stack, and see if the stack will be
    // aligned to 16 bytes. To know that we will see if the number of those
    // operands is even or odd.

    int stack_padding = 0;
    size_t rest_of_operands = 0;
    if (InvokeInstr.getNumOperands() > 6) {
        rest_of_operands = InvokeInstr.getNumOperands() - 6;
        if (rest_of_operands % 2 != 0) {
            stack_padding = 8;
        }
    }

    if (stack_padding > 0) {
        llvm::APSInt stackPaddingAPSInt(llvm::APInt(32, stack_padding));
        X64Func->add_instruction(Ctx.createAllocation(Ctx.createInt(stackPaddingAPSInt)));
    }

    // Move first 6 arguments to registers, sized by the argument's IR type
    for (size_t i = 0, e = InvokeInstr.getNumOperands() - rest_of_operands; i < e; i++) {
        auto *arg = InvokeInstr.getOperand(i);
        X64Operand *param = convertOperand(arg, Ctx);
        const X64Type argType = getTypeFromIR(
            dynamic_cast<const ir::Operand *>(arg)->getType());
        X64Func->add_instruction(Ctx.createMov(param, Ctx.getPhysReg(argRegs[i], argType)));
    }

    // Push remaining arguments onto stack (in reverse order: last arg first)
    for (int i = rest_of_operands - 1; i >= 0; i--) {
        auto *arg = InvokeInstr.getOperand(6 + i);
        X64Operand *param = convertOperand(arg, Ctx);

        // Check if the operand is a physical register or immediate value
        if (dynamic_cast<X64Int *>(param)) {
            // Push directly
            X64Func->add_instruction(Ctx.createPush(param));
        } else if (dynamic_cast<PhysicalRegister *>(param)) {
            auto *physical_reg = dynamic_cast<PhysicalRegister *>(param);
            if (physical_reg->getSize() != X64Type::QuadWord) {
                auto *Reg_64bit = Ctx.getPhysReg(physical_reg->getReg(), X64Type::QuadWord);
                X64Func->add_instruction(Ctx.createMov(physical_reg, Reg_64bit));
                X64Func->add_instruction(Ctx.createPush(Reg_64bit));
            } else {
                // Already 64-bit, push directly
                X64Func->add_instruction(Ctx.createPush(physical_reg));
            }
        } else {
            // Memory or pseudo: load into arg-type-sized RAX, then push the 64-bit version.
            // Using the arg's own type avoids a "mov eax, qword ptr [...]" mismatch when
            // the argument is a long (QuadWord) stored in a qword stack slot.
            const X64Type argType = getTypeFromIR(
                dynamic_cast<const ir::Operand *>(arg)->getType());
            auto *axLoad = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, argType);
            X64Func->add_instruction(Ctx.createMov(param, axLoad));
            auto *RAX = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, X64Type::QuadWord);
            X64Func->add_instruction(Ctx.createPush(RAX));
        }
    }

    // Make the function call.
    // Always use @PLT: this avoids GAS Intel-syntax keyword conflicts (e.g. a
    // function named "not", "and", or "or" would otherwise be parsed as a
    // bitwise operator rather than a call target).  The linker relaxes
    // unnecessary PLT stubs for locally-defined functions at link time.
    std::string callTarget = InvokeInstr.getCalledFunction().str() + "@PLT";
    X64Func->add_instruction(Ctx.createCall(callTarget));

    // Deallocate stack space (arguments + padding)
    int total_stack_bytes = rest_of_operands * 8 + stack_padding;
    if (total_stack_bytes > 0) {
        llvm::APSInt stackBytesAPSInt(llvm::APInt(32, total_stack_bytes));
        X64Func->add_instruction(Ctx.createDeallocation(Ctx.createInt(stackBytesAPSInt)));
    }

    // If the function returns a value, move RAX to the result register
    if (InvokeInstr.hasResult()) {
        X64Operand *result = convertOperand(InvokeInstr.getResult(), Ctx);
        const X64Type retType = getTypeFromIR(InvokeInstr.getResult()->getType());
        X64Operand *rax = Ctx.getPhysReg(PhysicalRegister::PhysReg::RAX, retType);
        X64Func->add_instruction(Ctx.createMov(rax, result));
    }
}

// ===== Operand Conversion Helpers =====

X64Operand *X64CodeGenerator::convertOperand(const ir::Value *Val, X64Context &Ctx) {
    if (const auto *Imm = dynamic_cast<const ir::Constant *>(Val))
        return convertConstant(*Imm, Ctx);
    if (const auto *Reg = dynamic_cast<const ir::Reg *>(Val))
        return convertRegister(*Reg, Ctx);
    if (const auto *Var = dynamic_cast<const ir::VarOp *>(Val))
        return convertVariable(*Var, Ctx);
    if (const auto *Param = dynamic_cast<const ir::ParameterOp *>(Val))
        return convertParameter(*Param, Ctx);
    if (const auto *StaticVar = dynamic_cast<const ir::StaticVarOp *>(Val))
        return convertStaticVar(*StaticVar, Ctx);
    return nullptr;
}

X64Register *X64CodeGenerator::convertRegister(const ir::Reg &Reg, X64Context &Ctx) {
    return Ctx.getPseudoReg(Reg.getID(), getTypeFromIR(Reg.getType()));
}

X64Int *X64CodeGenerator::convertConstant(const ir::Constant &ConstVal, X64Context &Ctx) {
    return Ctx.createInt(llvm::APSInt::get(ConstVal.getRawValue()));
}

X64Register *X64CodeGenerator::convertVariable(const ir::VarOp &Var, X64Context &Ctx) {
    unsigned pseudoID = std::hash<std::string>{}(Var.getName());
    return Ctx.getPseudoReg(pseudoID, getTypeFromIR(Var.getType()));
}

X64Register *X64CodeGenerator::convertParameter(const ir::ParameterOp &Var, X64Context &Ctx) {
    unsigned pseudoID = std::hash<std::string>{}(Var.getName());
    return Ctx.getPseudoReg(pseudoID, getTypeFromIR(Var.getType()));
}

X64Data *X64CodeGenerator::convertStaticVar(const ir::StaticVarOp &Var, X64Context &Ctx) {
    return Ctx.createData(Var.getName(), getTypeFromIR(Var.getType()));
}

// ===== Phase 2: Stack Allocation =====

void X64CodeGenerator::allocateStackSlots() {
    if (!Program) return;

    for (auto &Func: *Program) {
        allocateStackSlotsForFunction(Func);
    }
}

void X64CodeGenerator::allocateStackSlotsForFunction(X64Function *Func) {
    auto &Ctx = Func->getContext();

    // Traverse instructions and replace pseudo-registers with stack allocations
    for (auto &Inst: *Func) {
        replacePseudoRegistersInInstruction(Inst, Ctx);
    }
}

void X64CodeGenerator::replacePseudoRegistersInInstruction(X64Instruction *Inst, X64Context &Ctx) {
    // Handle different instruction types
    if (auto *mov = dynamic_cast<X64Mov *>(Inst)) {
        // Replace source operand if it's a pseudo register
        X64Operand *src = mov->getSrc();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(src)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            mov->setSrc(stackSlot);
        }

        // Replace destination operand if it's a pseudo register
        X64Operand *dst = mov->getDst();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(dst)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            mov->setDst(stackSlot);
        }
    } else if (auto *unary = dynamic_cast<X64Unary *>(Inst)) {
        // Replace operand if it's a pseudo register
        X64Operand *op = unary->getOperand();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(op)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            unary->setOperand(stackSlot);
        }
    } else if (auto *binary = dynamic_cast<X64Binary *>(Inst)) {
        X64Operand *src = binary->getSrc();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(src)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            binary->setSrc(stackSlot);
        }
        X64Operand *dst = binary->getDst();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(dst)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            binary->setDst(stackSlot);
        }
    } else if (auto *div = dynamic_cast<X64IDiv *>(Inst)) {
        X64Operand *op = div->getOperand();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(op)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            div->setOperand(stackSlot);
        }
    } else if (auto *cmp = dynamic_cast<X64Cmp *>(Inst)) {
        X64Operand *left = cmp->getLeft();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(left)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            cmp->setLeft(stackSlot);
        }
        X64Operand *right = cmp->getRight();
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(right)) {
            X64Operand *stackSlot = getOrAllocateStackSlot(pseudoReg, Ctx);
            cmp->setRight(stackSlot);
        }
    } else if (auto *movsx = dynamic_cast<X64Movsx *>(Inst)) {
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(movsx->getSrc())) {
            movsx->setSrc(getOrAllocateStackSlot(pseudoReg, Ctx));
        }
        if (auto *pseudoReg = dynamic_cast<PseudoRegister *>(movsx->getDst())) {
            movsx->setDst(getOrAllocateStackSlot(pseudoReg, Ctx));
        }
    }
    // X64Ret has no operands to replace
}

X64Operand *X64CodeGenerator::getOrAllocateStackSlot(PseudoRegister *pseudo, X64Context &Ctx) {
    const unsigned pseudoID = pseudo->getID();
    if (Ctx.isAllocatedToMemory(pseudoID)) {
        return Ctx.getAllocatedMemory(pseudoID);
    }
    Ctx.allocateMemory(pseudoID, pseudo->getSize());
    return Ctx.getAllocatedMemory(pseudoID);
}

// ===== Phase 3: Instruction Fixup =====

void X64CodeGenerator::fixupInstructions() {
    if (!Program) return;

    for (auto &Func: *Program) {
        fixupInstructionsForFunction(Func);
        fixupLargeImmediates(Func);
        insertAllocationInstruction(Func);
    }
}

void X64CodeGenerator::fixupLargeImmediates(X64Function *Func) {
    auto &Ctx = Func->getContext();
    std::vector<std::pair<X64Instruction *, std::vector<X64Instruction *>>> replacements;

    for (auto *Inst: *Func) {
        if (auto *mov = dynamic_cast<X64Mov *>(Inst)) {
            auto *srcImm = dynamic_cast<X64Int *>(mov->getSrc());
            if (srcImm && isLargeImmediate(srcImm) && isMemoryOperand(mov->getDst())) {
                // mov [mem], imm64  →  mov r10, imm64  +  mov [mem], r10{d}
                // Load into full 64-bit r10; store using dst-sized alias to match slot width.
                auto *R10load = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, X64Type::QuadWord);
                X64Type dstType = X64Type::QuadWord;
                if (auto *s = dynamic_cast<X64Stack *>(mov->getDst()))
                    dstType = s->getSize();
                else if (auto *d = dynamic_cast<X64Data *>(mov->getDst()))
                    dstType = d->getSize();
                auto *R10store = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, dstType);
                replacements.emplace_back(mov, std::vector<X64Instruction *>{
                    Ctx.createMov(srcImm, R10load),
                    Ctx.createMov(R10store, mov->getDst()),
                });
            }
        } else if (auto *binary = dynamic_cast<X64Binary *>(Inst)) {
            auto *srcImm = dynamic_cast<X64Int *>(binary->getSrc());
            if (srcImm && isLargeImmediate(srcImm)) {
                // op dst, imm64  →  mov r10, imm64  +  op dst, r10
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, X64Type::QuadWord);
                replacements.emplace_back(binary, std::vector<X64Instruction *>{
                    Ctx.createMov(srcImm, R10),
                    Ctx.createBinary(binary->getKind(), R10, binary->getDst(), binary->getType()),
                });
            }
        } else if (auto *cmp = dynamic_cast<X64Cmp *>(Inst)) {
            auto *rightImm = dynamic_cast<X64Int *>(cmp->getRight());
            if (rightImm && isLargeImmediate(rightImm)) {
                // cmp left, imm64  →  mov scratch, imm64  +  cmp left, scratch
                // If left is already R11 (placed there by fixupInstructionsForFunction),
                // use R10 instead to avoid cmp R11, R11.
                auto *leftReg = dynamic_cast<PhysicalRegister *>(cmp->getLeft());
                const bool leftIsR11 = leftReg &&
                    leftReg->getReg() == PhysicalRegister::PhysReg::R11;
                auto *scratch = leftIsR11
                    ? Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, X64Type::QuadWord)
                    : Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, X64Type::QuadWord);
                replacements.emplace_back(cmp, std::vector<X64Instruction *>{
                    Ctx.createMov(rightImm, scratch),
                    Ctx.createCmp(cmp->getLeft(), scratch, cmp->getSize()),
                });
            }
        } else if (auto *push = dynamic_cast<X64Push *>(Inst)) {
            auto *opImm = dynamic_cast<X64Int *>(push->getPushOp());
            if (opImm && isLargeImmediate(opImm)) {
                // push imm64  →  mov r10, imm64  +  push r10
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, X64Type::QuadWord);
                replacements.emplace_back(push, std::vector<X64Instruction *>{
                    Ctx.createMov(opImm, R10),
                    Ctx.createPush(R10),
                });
            }
        }
    }

    for (const auto &[oldInst, newInsts]: replacements)
        replaceInstructionInFunction(Func, oldInst, newInsts);
}

void X64CodeGenerator::fixupInstructionsForFunction(X64Function *Func) {
    auto &Ctx = Func->getContext();

    // We need to collect instructions to replace since we can't modify while iterating
    std::vector<std::pair<X64Instruction *, std::vector<X64Instruction *> > > replacements;

    for (auto *Inst: *Func) {
        if (auto *mov = dynamic_cast<X64Mov *>(Inst)) {
            // Check for MEM to MEM move (both operands are memory: X64Stack or X64Data)
            X64Operand *src = mov->getSrc();
            X64Operand *dst = mov->getDst();
            bool srcIsMem = isMemoryOperand(src);
            bool dstIsMem = isMemoryOperand(dst);

            // If both source and destination are memory, we need to fix this
            if (srcIsMem && dstIsMem) {
                // Size R10 separately for load (src width) and store (dst width).
                // This handles truncation (qword→dword) and widening (dword→qword) correctly.
                X64Type srcType = X64Type::LongWord;
                if (auto *s = dynamic_cast<X64Stack *>(src))
                    srcType = s->getSize();
                else if (auto *d = dynamic_cast<X64Data *>(src))
                    srcType = d->getSize();
                X64Type dstType = X64Type::LongWord;
                if (auto *s = dynamic_cast<X64Stack *>(dst))
                    dstType = s->getSize();
                else if (auto *d = dynamic_cast<X64Data *>(dst))
                    dstType = d->getSize();
                auto *R10load  = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, srcType);
                auto *R10store = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, dstType);

                // Create two new instructions:
                // 1. MOV R10, [src_memory]   (sized by source)
                // 2. MOV [dst_memory], R10   (sized by destination)
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMov(src, R10load));
                newInstructions.push_back(Ctx.createMov(R10store, dst));

                // Mark this instruction for replacement
                replacements.emplace_back(mov, std::move(newInstructions));
            }
        } else if (auto *binOp = dynamic_cast<X64Binary *>(Inst)) {
            // Check for MEM to MEM operation (both operands are memory: X64Stack or X64Data)
            X64Operand *src = binOp->getSrc();
            X64Operand *dst = binOp->getDst();
            auto *srcStack = dynamic_cast<X64Stack *>(src);
            bool srcIsMem = isMemoryOperand(src);
            bool dstIsMem = isMemoryOperand(dst);
            const X64Type binType = binOp->getType();

            // If both source and destination are memory, we need to fix this
            if (srcIsMem && dstIsMem) {
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, binType);
                auto *CL  = Ctx.getPhysReg(PhysicalRegister::PhysReg::RCX, X64Type::Byte);
                PhysicalRegister *srcRegister;
                // Create two new instructions:
                // 1. MOV R10, [src_memory]
                // 2. BinOp [dst_memory], R10
                std::vector<X64Instruction *> newInstructions;
                // SAL/SAR shift count must be in CL register
                if (binOp->getKind() == X64Binary::Sal || binOp->getKind() == X64Binary::Sar) {
                    // For X64Stack we can get byte-sized access, for X64Data just move to CL
                    if (srcStack) {
                        auto *srcStackByte = Ctx.getAllocatedStack(srcStack, X64Type::Byte);
                        newInstructions.push_back(Ctx.createMov(srcStackByte, CL));
                    } else {
                        newInstructions.push_back(Ctx.createMov(src, CL));
                    }
                    srcRegister = CL;
                } else {
                    newInstructions.push_back(Ctx.createMov(src, R10));
                    srcRegister = R10;
                }
                // the destination for a Mul cannot be a memory address, so in case we have
                // a memory address, replace it for a real register.
                if (binOp->getKind() == X64Binary::Mult) {
                    auto *R11 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, binType);
                    newInstructions.push_back(Ctx.createMov(dst, R11));
                    newInstructions.push_back(Ctx.createBinary(binOp->getKind(), srcRegister, R11));
                    newInstructions.push_back(Ctx.createMov(R11, dst));
                } else {
                    newInstructions.push_back(Ctx.createBinary(binOp->getKind(), srcRegister, dst));
                }
                // Mark this instruction for replacement
                replacements.emplace_back(binOp, std::move(newInstructions));
            }
            // Same as before, but in this case, we can have that source is an immediate value
            // and destination is a memory address
            else if (dstIsMem && binOp->getKind() == X64Binary::Mult) {
                std::vector<X64Instruction *> newInstructions;
                auto *R11 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, binType);
                newInstructions.push_back(Ctx.createMov(dst, R11));
                newInstructions.push_back(Ctx.createBinary(binOp->getKind(), src, R11));
                newInstructions.push_back(Ctx.createMov(R11, dst));
                replacements.emplace_back(binOp, std::move(newInstructions));
            }
            // Handle SAL/SAR where shift count is not immediate and not in CL
            else if ((binOp->getKind() == X64Binary::Sal || binOp->getKind() == X64Binary::Sar)) {
                auto *srcImm = dynamic_cast<X64Int *>(src);
                // If source is not immediate, it must go into CL register
                if (srcImm == nullptr) {
                    std::vector<X64Instruction *> newInstructions;
                    auto *CL = Ctx.getPhysReg(PhysicalRegister::PhysReg::RCX, X64Type::Byte);
                    newInstructions.push_back(Ctx.createMov(src, CL));
                    newInstructions.push_back(Ctx.createBinary(binOp->getKind(), CL, dst));
                    replacements.emplace_back(binOp, std::move(newInstructions));
                }
            }
        } else if (auto *div = dynamic_cast<X64IDiv *>(Inst)) {
            X64Operand *op = div->getOperand();
            auto *Imm = dynamic_cast<X64Int *>(op);

            if (Imm != nullptr) {
                const X64Type divType = div->getSize();
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, divType);

                // Create two new instructions
                // 1. Mov R10, Int
                // 2. IDIV R10
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMov(Imm, R10));
                newInstructions.push_back(Ctx.createIDiv(R10, divType));

                // Mark this instruction for replacement
                replacements.emplace_back(div, std::move(newInstructions));
            }
        } else if (auto *cmp = dynamic_cast<X64Cmp *>(Inst)) {
            X64Operand *left = cmp->getLeft();
            X64Operand *right = cmp->getRight();
            auto *leftImm = dynamic_cast<X64Int *>(left);
            auto *rightImm = dynamic_cast<X64Int *>(right);
            bool leftIsMem = isMemoryOperand(left);
            bool rightIsMem = isMemoryOperand(right);
            // Check if left is X64Data (RIP-relative memory)
            auto *leftData = dynamic_cast<X64Data *>(left);
            const X64Type cmpType = cmp->getSize();

            // in a CMP instruction both operands cannot be memory operands
            if (leftIsMem && rightIsMem) {
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, cmpType);
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMov(left, R10));
                newInstructions.push_back(Ctx.createCmp(R10, right, cmpType));

                replacements.emplace_back(cmp, std::move(newInstructions));
            }
            // in a CMP instruction, the left side cannot be a number, because
            // the result of the operation should be stored there, even if CMP
            // does not store anything, same rule applies
            else if (leftImm != nullptr) {
                auto *R11 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, cmpType);
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMov(leftImm, R11));
                newInstructions.push_back(Ctx.createCmp(R11, right, cmpType));

                replacements.emplace_back(cmp, std::move(newInstructions));
            }
            // cmp [X64Data], imm has ambiguous size - move data to register first
            else if (leftData != nullptr && rightImm != nullptr) {
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, cmpType);
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMov(leftData, R10));
                newInstructions.push_back(Ctx.createCmp(R10, rightImm, cmpType));

                replacements.emplace_back(cmp, std::move(newInstructions));
            }
        } else if (auto *movsx = dynamic_cast<X64Movsx *>(Inst)) {
            // movsx constraints: src cannot be immediate; dst must be a register
            X64Operand *src = movsx->getSrc();
            X64Operand *dst = movsx->getDst();
            auto *srcImm = dynamic_cast<X64Int *>(src);
            bool dstIsMem = isMemoryOperand(dst);
            const X64Type typeFrom = movsx->getTypeFrom();
            const X64Type typeTo   = movsx->getTypeTo();

            if (srcImm != nullptr && dstIsMem) {
                // Movsx(Imm, Mem) → Mov(typeFrom, Imm, R10) + Movsx(R10, R11) + Mov(typeTo, R11, Mem)
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, typeFrom);
                auto *R11 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, typeTo);
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMov(srcImm, R10));
                newInstructions.push_back(Ctx.createMovsx(R10, R11, typeFrom, typeTo));
                newInstructions.push_back(Ctx.createMov(R11, dst));
                replacements.emplace_back(movsx, std::move(newInstructions));
            } else if (srcImm != nullptr) {
                // Movsx(Imm, Reg) → Mov(typeFrom, Imm, R10) + Movsx(R10, dst)
                auto *R10 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R10, typeFrom);
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMov(srcImm, R10));
                newInstructions.push_back(Ctx.createMovsx(R10, dst, typeFrom, typeTo));
                replacements.emplace_back(movsx, std::move(newInstructions));
            } else if (dstIsMem) {
                // Movsx(Src, Mem) → Movsx(Src, R11) + Mov(typeTo, R11, Mem)
                auto *R11 = Ctx.getPhysReg(PhysicalRegister::PhysReg::R11, typeTo);
                std::vector<X64Instruction *> newInstructions;
                newInstructions.push_back(Ctx.createMovsx(src, R11, typeFrom, typeTo));
                newInstructions.push_back(Ctx.createMov(R11, dst));
                replacements.emplace_back(movsx, std::move(newInstructions));
            }
        } else if (auto *setCC = dynamic_cast<X64SetCC *>(Inst)) {
            auto it = std::ranges::find_if(Func->getInstructions(), [&](X64Instruction *I) {
                return I == setCC;
            });
            auto *prevMov = dynamic_cast<X64Mov *>(*(--it));
            auto *movOperator = prevMov->getDst();
            if (auto *mem = dynamic_cast<X64Stack *>(movOperator)) {
                auto *byteMem = Ctx.getAllocatedStack(mem, X64Type::Byte);
                setCC->setOperand(byteMem);
            } else if (auto *reg = dynamic_cast<PhysicalRegister *>(movOperator)) {
                auto *byteReg = Ctx.getPhysReg(reg->getReg(), X64Type::Byte);
                setCC->setOperand(byteReg);
            }
        }
    }

    // Apply the replacements
    for (const auto &[oldInst, newInstructions]: replacements) {
        replaceInstructionInFunction(Func, oldInst, newInstructions);
    }
}

void X64CodeGenerator::replaceInstructionInFunction(X64Function *Func, X64Instruction *oldInst,
                                                    const std::vector<X64Instruction *> &newInstructions) {
    // Find the old instruction in the function's instruction list
    auto &instrs = Func->getInstructions(); // Assuming X64Function has this method

    for (auto it = instrs.begin(); it != instrs.end(); ++it) {
        if (*it == oldInst) {
            // Remove the old instruction
            it = instrs.erase(it);

            // Insert new instructions at the same position
            for (auto *newInst: newInstructions) {
                it = instrs.insert(it, newInst);
                ++it;
            }
            break;
        }
    }
}

void X64CodeGenerator::insertAllocationInstruction(X64Function *Func) {
    auto &Ctx = Func->getContext();
    int stackSize = Ctx.getStackOffset(); // Stack grows downward

    if (stackSize == 0) return;

    // Round up to the next multiple of 16 bytes for alignment
    int roundedStackSize = ((abs(stackSize) + 15) / 16) * 16;

    llvm::APSInt stackSizeAPSint(llvm::APInt(32, roundedStackSize));
    auto *stackSizeImm = Ctx.createInt(stackSizeAPSint);
    auto *allocateInsn = Ctx.createAllocation(stackSizeImm);
    Func->getInstructions().push_front(allocateInsn);
}

// ===== Phase 5: Assembly Generation =====

std::string X64CodeGenerator::emitAssembly() {
    std::ostringstream asm_output;

    // Emit assembly header/directives
    asm_output << formatDirective(".intel_syntax noprefix") << "\n";

    // Emit static variables
    for (auto it = Program->staticVars_begin(); it != Program->staticVars_end(); ++it) {
        asm_output << emitStaticVar(**it) << "\n";
    }

    // Emit each function
    for (const auto &Func: *Program) {
        asm_output << emitFunction(*Func) << "\n";
    }

#ifdef __linux__
    asm_output << formatDirective(".section .note.GNU-stack,\"\",@progbits");
#endif

    return asm_output.str();
}

std::string X64CodeGenerator::emitStaticVar(const X64StaticVar &StaticVar) {
    std::ostringstream var_output;

    if (StaticVar.isGlobal()) {
        var_output << formatDirective(".globl " + StaticVar.getName().str()) << "\n";
    }

    const std::string alignStr = std::to_string(getSizeInBytes(StaticVar.getType()));
    if (StaticVar.getInitValue() != 0) {
        // Non-zero init: use .data section
        var_output << formatDirective(".data") << "\n";
        var_output << formatDirective(".align " + alignStr) << "\n";
        var_output << StaticVar.getName().str() << ":\n";
        if (StaticVar.getType() == X64Type::LongWord)
            var_output << formatDirective(".long " + std::to_string(StaticVar.getInitValue())) << "\n";
        else if (StaticVar.getType() == X64Type::QuadWord)
            var_output << formatDirective(".quad " + std::to_string(StaticVar.getInitValue())) << "\n";
    } else {
        // Zero init: use .bss section
        var_output << formatDirective(".bss") << "\n";
        var_output << formatDirective(".align " + alignStr) << "\n";
        var_output << StaticVar.getName().str() << ":\n";
        if (StaticVar.getType() == X64Type::LongWord)
            var_output << formatDirective(".zero 4") << "\n";
        else if (StaticVar.getType() == X64Type::QuadWord)
            var_output << formatDirective(".zero 8") << "\n";
    }

    return var_output.str();
}

std::string X64CodeGenerator::emitFunction(const X64Function &Func) {
    std::ostringstream func_output;

    // Text section directive
    func_output << formatDirective(".text") << "\n";

    // Global directive only if function is global
    if (Func.isGlobal()) {
        func_output << formatDirective(".globl " + Func.get_name().str()) << "\n";
    }

    // Function label
    func_output << formatLabel(Func.get_name()) << "\n";

    func_output << "    push rbp" << "\n";
    func_output << "    mov rbp, rsp" << "\n";
    bool contains_ret = false;
    // Function instructions
    for (const auto &Inst: Func) {
        if (dynamic_cast<const X64Ret *>(Inst)) {
            func_output << "    mov rsp, rbp" << "\n";
            func_output << "    pop rbp" << "\n";
            contains_ret = true;
        }

        if (dynamic_cast<X64Label *>(Inst)) {
            func_output << emitInstruction(*Inst) << ":" << "\n";
        } else {
            func_output << "    " << emitInstruction(*Inst) << "\n";
        }
    }

    if (!contains_ret) {
        func_output << "    mov rsp, rbp" << "\n";
        func_output << "    pop rbp" << "\n";
        func_output << "    ret\n";
    }

    return func_output.str();
}

std::string X64CodeGenerator::emitInstruction(const X64Instruction &Inst) {
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
