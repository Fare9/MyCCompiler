#include "mycc/IR/SimpleIR.hpp"

#include "llvm/Support/Casting.h"

#include <cassert>
#include <string>

namespace mycc::ir {

// ============================================================
// Type to_string implementations
// ============================================================

std::string IntType::to_string() const {
    return "i" + std::to_string(BitWidth);
}

std::string VoidType::to_string() const { return "void"; }

std::string FunctionType::to_string() const {
    std::string s = ReturnTy->to_string() + " (";
    for (size_t i = 0; i < ParamTys.size(); ++i) {
        if (i > 0) s += ", ";
        s += ParamTys[i]->to_string();
    }
    return s + ")";
}

// ============================================================
// Operand to_string implementations
// ============================================================

std::string Constant::to_string() const {
    return std::to_string(Val);
}

std::string Reg::to_string() const {
    return "%r" + std::to_string(RegID);
}

std::string StaticVarOp::to_string() const { return "@" + Identifier; }

std::string VarOp::to_string() const { return "%" + Name; }

std::string ParameterOp::to_string() const { return "%" + Name; }

// ============================================================
// Instruction to_string implementations
// ============================================================

std::string Instruction::to_string() const {
    std::string result = getOpcodeName().str();
    if (getNumOperands() > 0) {
        result += " ";
        for (size_t i = 0; i < getNumOperands(); ++i) {
            if (i > 0) result += ", ";
            result += getOperand(i)->to_string();
        }
    }
    return result;
}

std::string Label::to_string() const { return label_identifier; }

std::string Copy::to_string() const {
    return "copy " + getDst()->to_string() + ", " + getSrc()->to_string();
}

std::string Mov::to_string() const {
    return "mov " + getDst()->to_string() + ", " + getSrc()->to_string();
}

std::string Ret::to_string() const {
    return isVoidReturn() ? "ret" : "ret " + getReturnValue()->to_string();
}

std::string Jump::to_string() const {
    return "jmp " + getDst()->to_string();
}

std::string JumpIfZero::to_string() const {
    return "jz " + getCondition()->to_string() + ", " + getDst()->to_string();
}

std::string JumpIfNotZero::to_string() const {
    return "jnz " + getCondition()->to_string() + ", " + getDst()->to_string();
}

StringRef UnaryOp::getOpcodeName() const {
    if (Kind == Neg) return "neg";
    if (Kind == Complement) return "complement";
    return "not";
}

std::string UnaryOp::to_string() const {
    return dst->to_string() + " = " + getOpcodeName().str() + " " +
           dst->getType()->to_string() + " " + getSource()->to_string();
}

StringRef BinaryOp::getOpcodeName() const {
    switch (Kind) {
        case Add: return "add"; case Sub: return "sub"; case Mul: return "mul";
        case Div: return "div"; case Rem: return "rem"; case And: return "and";
        case Or:  return "or";  case Xor: return "xor"; case Sal: return "sal";
        case Sar: return "sar"; default: return "";
    }
}

std::string BinaryOp::to_string() const {
    return dst->to_string() + " = " + getOpcodeName().str() + " " +
           dst->getType()->to_string() + " " +
           getLeft()->to_string() + ", " + getRight()->to_string();
}

StringRef ICmpOp::getOpcodeName() const {
    switch (Kind) {
        case lt: return "lt"; case le: return "le"; case gt: return "gt";
        case ge: return "ge"; case eq: return "eq"; case neq: return "neq";
        default: return "";
    }
}

std::string ICmpOp::to_string() const {
    const std::string ty = dynamic_cast<Operand *>(getOperand(0))->getType()->to_string();
    return Dst->to_string() + " = icmp " + getOpcodeName().str() + " " +
           ty + " " + getLeft()->to_string() + ", " + getRight()->to_string();
}

std::string Invoke::to_string() const {
    std::string s;
    if (hasResult()) s = getResult()->to_string() + " = ";
    s += "call " + (hasResult() ? getResult()->getType()->to_string() + " " : "void ");
    s += getCalledFunction().str() + "(";
    for (size_t i = 0; i < getNumOperands(); ++i) {
        if (i > 0) s += ", ";
        const auto *op = dynamic_cast<Operand *>(getOperand(i));
        s += op->getType()->to_string() + " " + op->to_string();
    }
    return s + ")";
}

std::string SignExtend::to_string() const {
    return Result->to_string() + " = sext " +
           getSource()->getType()->to_string() + " " + getSource()->to_string() +
           " to " + Result->getType()->to_string();
}

std::string Truncate::to_string() const {
    return Result->to_string() + " = trunc " +
           getSource()->getType()->to_string() + " " + getSource()->to_string() +
           " to " + Result->getType()->to_string();
}

// ============================================================
// StaticVariable
// ============================================================

std::string StaticVariable::to_string() const {
    std::string result = "@" + Identifier + " = ";
    if (!Global) result += "internal ";
    result += "global " + Ty->to_string() + " " + std::to_string(InitValue);
    return result;
}

// ============================================================
// Function
// ============================================================

std::string Function::to_string() const {
    std::string global_str = global ? ".global" : "";
    std::string result = "define " + global_str + " " + get_name().str() + "(";
    if (FuncTy) result = "define " + global_str + " " + FuncTy->getReturnType()->to_string() +
                         " " + get_name().str() + "(";
    for (auto *arg: args) result += arg->to_string() + ",";
    if (!args.empty()) result.pop_back();
    result += ") {\n";
    for (const Instruction *inst: Instructions) {
        if (const auto *label = dynamic_cast<const Label *>(inst))
            result += label->to_string() + ":\n";
        else
            result += "  " + inst->to_string() + "\n";
    }
    return result + "}\n";
}

// ============================================================
// Program
// ============================================================

Program::Program() : Ctx(std::make_unique<Context>()) {}

Program::Program(const StringRef Name) : Name(Name), Ctx(std::make_unique<Context>()) {}

Program::~Program() {
    for (const auto &F: Funcs) delete F;
    for (const auto &SV: StaticVars) delete SV;
}

std::string Program::to_string() const {
    std::string result = "; Program: " + get_name().str() + "\n\n";
    for (const StaticVariable *sv: StaticVars) result += sv->to_string() + "\n";
    if (!StaticVars.empty()) result += "\n";
    for (const Function *func: Funcs) result += func->to_string() + "\n";
    return result;
}

// ============================================================
// Context
// ============================================================

IntType* Context::getIntType(unsigned bitWidth) {
    auto &ty = IntTypes[bitWidth];
    if (!ty) ty = std::make_unique<IntType>(bitWidth);
    return ty.get();
}

VoidType* Context::getVoidTy() {
    if (!VoidTy) VoidTy = std::make_unique<VoidType>();
    return VoidTy.get();
}

FunctionType* Context::createFunctionType(Type* ret, std::vector<Type*> params) {
    auto ft = std::make_unique<FunctionType>(ret, std::move(params));
    FunctionType* ptr = ft.get();
    FuncTypes.push_back(std::move(ft));
    return ptr;
}

Label *Context::getOrCreateLabel(const std::string &name, bool isUserDefined) {
    if (isUserDefined && Labels.contains(name))
        return Labels[name];
    std::string labelName = isUserDefined ? name : (name + "_" + std::to_string(LabelNextID++));
    auto *label = new Label(labelName);
    Values.emplace_back(label);
    Labels[name] = label;
    return label;
}

Jump *Context::createJump(Label *label) {
    auto *jump = new Jump(label);
    Values.emplace_back(jump);
    return jump;
}

JumpIfZero *Context::createJZ(Value *cond, Label *label) {
    auto *jz = new JumpIfZero(cond, label);
    Values.emplace_back(jz);
    return jz;
}

JumpIfNotZero *Context::createJNZ(Value *cond, Label *label) {
    auto *jnz = new JumpIfNotZero(cond, label);
    Values.emplace_back(jnz);
    return jnz;
}

Constant *Context::createConstant(Type* ty, const int64_t value) {
    assert(ty->getKind() == Type::TK_Int && "createConstant requires IntType");
    const auto* intTy = llvm::cast<IntType>(ty);
    if (intTy->getBitWidth() <= 32) {
        const auto key = static_cast<int32_t>(value);
        auto it = all_int_constants.find(key);
        if (it != all_int_constants.end()) return it->second;
        auto *c = new Constant(ty, value);
        Values.emplace_back(c);
        all_int_constants[key] = c;
        return c;
    } else {
        auto it = all_long_constants.find(value);
        if (it != all_long_constants.end()) return it->second;
        auto *c = new Constant(ty, value);
        Values.emplace_back(c);
        all_long_constants[value] = c;
        return c;
    }
}

std::vector<Constant *> Context::getAllConstants() const {
    std::vector<Constant *> result;
    for (const auto &[_, c]: all_int_constants)  result.emplace_back(c);
    for (const auto &[_, c]: all_long_constants) result.emplace_back(c);
    return result;
}

Reg *Context::createReg(Type* ty) {
    auto *RegVal = new Reg(NextRegID++, ty);
    Values.emplace_back(RegVal);
    return RegVal;
}

VarOp *Context::getOrCreateVar(const StringRef Name, Type* ty) {
    if (Variables.contains(Name)) return Variables[Name];
    auto *newVar = new VarOp(Name, ty);
    Values.emplace_back(newVar);
    Variables[Name] = newVar;
    return newVar;
}

StaticVarOp *Context::getOrCreateStaticVar(const StringRef Name, ir::Type* ty) {
    if (StaticVars.contains(Name)) return StaticVars[Name];
    auto *newVar = new StaticVarOp(Name, ty);
    Values.emplace_back(newVar);
    StaticVars[Name] = newVar;
    return newVar;
}

Copy *Context::createCopy(Value *src, Value *dst) {
    auto *copy = new Copy(src, dst);
    Values.emplace_back(copy);
    return copy;
}

Mov *Context::createMov(Value *src, Value *dst) {
    auto *MovInst = new Mov(src, dst);
    Values.emplace_back(MovInst);
    return MovInst;
}

Ret *Context::createRet(Value *retval) {
    Ret *RetInst = retval ? new Ret(retval) : new Ret();
    Values.emplace_back(RetInst);
    return RetInst;
}

UnaryOp *Context::createUnaryOp(Operand *src, UnaryOp::UnaryOpKind kind) {
    Reg *dst = createReg(src->getType());
    auto *UnaryInst = new UnaryOp(dst, src, kind);
    Values.emplace_back(UnaryInst);
    return UnaryInst;
}

BinaryOp *Context::createBinaryOp(Operand *left, Operand *right, BinaryOp::BinaryOpKind kind) {
    Reg *dst = createReg(left->getType());
    auto *BinaryInst = new BinaryOp(dst, left, right, kind);
    Values.emplace_back(BinaryInst);
    return BinaryInst;
}

ICmpOp *Context::createICmpOp(Operand *left, Operand *right, ICmpOp::CmpOpKind kind, Type* resultTy) {
    Reg *dst = createReg(resultTy);
    auto *cmp = new ICmpOp(dst, left, right, kind);
    Values.emplace_back(cmp);
    return cmp;
}

Invoke *Context::createInvoke(const StringRef CalledFunction, const std::vector<Operand *> &operands, Type* resultTy) {
    Reg *dst = createReg(resultTy);
    auto *invoke = new Invoke(CalledFunction, operands, dst);
    Values.emplace_back(invoke);
    return invoke;
}

SignExtend *Context::createSignExtend(Operand *src, Type* toType) {
    Reg *dst = createReg(toType);
    auto *signExtend = new SignExtend(src, dst);
    Values.emplace_back(signExtend);
    return signExtend;
}

Truncate *Context::createTruncate(Operand *src, Type* toType) {
    Reg *dst = createReg(toType);
    auto *truncate = new Truncate(src, dst);
    Values.emplace_back(truncate);
    return truncate;
}

} // namespace mycc::ir
