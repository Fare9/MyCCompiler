#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"

#include <cassert>
#include <deque>
#include <memory>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace mycc::ir {

// ============================================================
// IR Type System
// ============================================================

/**
 * @brief Base class for all IR types. Simpler than C AST types —
 * no qualifiers, no typedefs, just machine-level representation.
 * Mirrors LLVM's llvm::Type approach.
 */
class Type {
public:
    enum TypeKind { TK_Int, TK_Void, TK_Function };

protected:
    explicit Type(TypeKind K) : Kind(K) {}

public:
    virtual ~Type() = default;

    [[nodiscard]] TypeKind getKind() const { return Kind; }
    [[nodiscard]] virtual std::string to_string() const = 0;

private:
    TypeKind Kind;
};

/**
 * @brief Integer type with explicit bit width (i8, i16, i32, i64).
 * Signedness is encoded in instructions (sext vs zext), not the type.
 */
class IntType : public Type {
    unsigned BitWidth;

public:
    explicit IntType(unsigned bitWidth) : Type(TK_Int), BitWidth(bitWidth) {}

    [[nodiscard]] unsigned getBitWidth() const { return BitWidth; }

    [[nodiscard]] std::string to_string() const override {
        return "i" + std::to_string(BitWidth);
    }

    static bool classof(const Type *T) { return T->getKind() == TK_Int; }
};

/**
 * @brief Void type, used as return type of void functions.
 */
class VoidType : public Type {
public:
    VoidType() : Type(TK_Void) {}
    [[nodiscard]] std::string to_string() const override { return "void"; }

    static bool classof(const Type *T) { return T->getKind() == TK_Void; }
};

/**
 * @brief Function type: return type + parameter types.
 * Used to describe the signature of ir::Function nodes.
 */
class FunctionType : public Type {
    Type* ReturnTy;
    std::vector<Type*> ParamTys;

public:
    FunctionType(Type* ret, std::vector<Type*> params)
        : Type(TK_Function), ReturnTy(ret), ParamTys(std::move(params)) {}

    [[nodiscard]] Type* getReturnType() const { return ReturnTy; }
    [[nodiscard]] const std::vector<Type*>& getParamTypes() const { return ParamTys; }

    [[nodiscard]] std::string to_string() const override {
        std::string s = ReturnTy->to_string() + " (";
        for (size_t i = 0; i < ParamTys.size(); ++i) {
            if (i > 0) s += ", ";
            s += ParamTys[i]->to_string();
        }
        return s + ")";
    }

    static bool classof(const Type *T) { return T->getKind() == TK_Function; }
};

// ============================================================
// Forward declarations
// ============================================================

class Context;
class Program;
class Function;
class StaticVariable;
class Value;
class Instruction;
class Operand;
class ParameterOp;

using FuncList = std::vector<Function *>;
using StaticVarList = std::vector<StaticVariable *>;
using InstList = std::vector<Instruction *>;
using Args = std::vector<ParameterOp *>;

// ============================================================
// Value hierarchy
// ============================================================

/**
 * @brief Base class for all IR values. Every instruction result,
 * constant, variable reference, etc. is a Value.
 */
class Value {
public:
    virtual ~Value() = default;
    [[nodiscard]] virtual std::string to_string() const = 0;
};

/**
 * @brief Instruction base class. Owns its operand list.
 */
class Instruction : public Value {
    std::vector<Value *> operands;

public:
    Instruction() = default;
    explicit Instruction(std::vector<Value *> operands) : operands(std::move(operands)) {}
    ~Instruction() override = default;

    [[nodiscard]] size_t getNumOperands() const { return operands.size(); }
    [[nodiscard]] Value *getOperand(size_t i) const { return operands[i]; }
    void setOperand(size_t i, Value *val) { operands[i] = val; }
    void addOperand(Value *val) { operands.push_back(val); }
    void clearOperands() { operands.clear(); }

    std::vector<Value *>::iterator operand_begin() { return operands.begin(); }
    std::vector<Value *>::iterator operand_end() { return operands.end(); }
    [[nodiscard]] std::vector<Value *>::const_iterator operand_begin() const { return operands.begin(); }
    [[nodiscard]] std::vector<Value *>::const_iterator operand_end() const { return operands.end(); }

    [[nodiscard]] virtual StringRef getOpcodeName() const = 0;

    [[nodiscard]] std::string to_string() const override {
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
};

/**
 * @brief Operand base class. Every operand carries an IR type.
 */
class Operand : public Value {
public:
    Operand() = default;
    ~Operand() override = default;
    [[nodiscard]] virtual ir::Type* getType() const = 0;
};

// ============================================================
// Operand subclasses
// ============================================================

class Constant : public Operand {
    ir::Type* Ty;
    int64_t Val;

public:
    Constant(ir::Type* ty, int64_t value) : Ty(ty), Val(value) {}

    [[nodiscard]] ir::Type* getType() const override { return Ty; }
    [[nodiscard]] int32_t getIntValue() const { return static_cast<int32_t>(Val); }
    [[nodiscard]] int64_t getLongValue() const { return Val; }
    [[nodiscard]] int64_t getRawValue() const { return Val; }

    [[nodiscard]] std::string to_string() const override {
        return std::to_string(Val);
    }
};

class Reg : public Operand {
    unsigned RegID;
    ir::Type* Ty;

public:
    explicit Reg(unsigned ID, ir::Type* Ty) : RegID(ID), Ty(Ty) {}

    [[nodiscard]] unsigned getID() const { return RegID; }
    [[nodiscard]] ir::Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override {
        return "%r" + std::to_string(RegID);
    }
};

class StaticVarOp : public Operand {
    std::string Identifier;
    ir::Type* Ty;

public:
    StaticVarOp(StringRef Identifier, ir::Type* Ty) : Identifier(Identifier), Ty(Ty) {}

    [[nodiscard]] std::string getName() const { return Identifier; }
    [[nodiscard]] ir::Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override { return "@" + Identifier; }
};

class VarOp : public Operand {
    std::string Name;
    Type* Ty;

public:
    explicit VarOp(const StringRef Name, Type* Ty) : Name(Name), Ty(Ty) {}

    [[nodiscard]] std::string getName() const { return Name; }
    [[nodiscard]] Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override { return "%" + Name; }
};

class ParameterOp : public Operand {
    std::string Name;
    Type* Ty;

public:
    explicit ParameterOp(StringRef Name, ir::Type* Ty) : Name(Name), Ty(Ty) {}

    [[nodiscard]] std::string getName() const { return Name; }
    [[nodiscard]] Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override { return "%" + Name; }
};

// ============================================================
// Instructions
// ============================================================

class Label : public Instruction {
    std::string label_identifier;

public:
    explicit Label(std::string identifier) : label_identifier(std::move(identifier)) {}

    [[nodiscard]] const std::string &get_identifier() const { return label_identifier; }
    [[nodiscard]] StringRef getOpcodeName() const override { return label_identifier; }
    [[nodiscard]] std::string to_string() const override { return label_identifier; }
};

class Copy : public Instruction {
public:
    explicit Copy(Value *src, Value *dst) {
        addOperand(src);
        addOperand(dst);
    }

    void setSrc(Value *src) { setOperand(0, src); }
    void setDst(Value *dst) { setOperand(1, dst); }
    [[nodiscard]] Value *getSrc() const { return getOperand(0); }
    [[nodiscard]] Value *getDst() const { return getOperand(1); }
    [[nodiscard]] StringRef getOpcodeName() const override { return "copy"; }
    [[nodiscard]] std::string to_string() const override {
        return "copy " + getDst()->to_string() + ", " + getSrc()->to_string();
    }
};

class Mov : public Instruction {
public:
    explicit Mov(Value *src, Value *dst) {
        addOperand(src);
        addOperand(dst);
    }

    void setSrc(Value *src) { setOperand(0, src); }
    void setDst(Value *dst) { setOperand(1, dst); }
    [[nodiscard]] Value *getSrc() const { return getOperand(0); }
    [[nodiscard]] Value *getDst() const { return getOperand(1); }
    [[nodiscard]] StringRef getOpcodeName() const override { return "mov"; }
    [[nodiscard]] std::string to_string() const override {
        return "mov " + getDst()->to_string() + ", " + getSrc()->to_string();
    }
};

class Ret : public Instruction {
public:
    explicit Ret(Value *retval) { addOperand(retval); }
    Ret() = default;

    void setReturnValue(Value *retval) {
        if (getNumOperands() == 0) addOperand(retval);
        else setOperand(0, retval);
    }

    [[nodiscard]] Value *getReturnValue() const { return getOperand(0); }
    [[nodiscard]] bool isVoidReturn() const { return getNumOperands() == 0; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "ret"; }
    [[nodiscard]] std::string to_string() const override {
        return isVoidReturn() ? "ret" : "ret " + getReturnValue()->to_string();
    }
};

class Jump : public Instruction {
    Label *destination;

public:
    explicit Jump(Label *dst) : destination(dst) {}

    void setDst(Label *dst) { destination = dst; }
    [[nodiscard]] Label *getDst() const { return destination; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "jmp"; }
    [[nodiscard]] std::string to_string() const override {
        return "jmp " + getDst()->to_string();
    }
};

class JumpIfZero : public Instruction {
    Label *destination;

public:
    explicit JumpIfZero(Value *condition, Label *dst) : destination(dst) {
        addOperand(condition);
    }

    void setCondition(Value *condition) { setOperand(0, condition); }
    void setDst(Label *dst) { destination = dst; }
    [[nodiscard]] Value *getCondition() const { return getOperand(0); }
    [[nodiscard]] Label *getDst() const { return destination; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "jz"; }
    [[nodiscard]] std::string to_string() const override {
        return "jz " + getCondition()->to_string() + ", " + getDst()->to_string();
    }
};

class JumpIfNotZero : public Instruction {
    Label *destination;

public:
    explicit JumpIfNotZero(Value *condition, Label *dst) : destination(dst) {
        addOperand(condition);
    }

    void setCondition(Value *condition) { setOperand(0, condition); }
    void setDst(Label *dst) { destination = dst; }
    [[nodiscard]] Value *getCondition() const { return getOperand(0); }
    [[nodiscard]] Label *getDst() const { return destination; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "jnz"; }
    [[nodiscard]] std::string to_string() const override {
        return "jnz " + getCondition()->to_string() + ", " + getDst()->to_string();
    }
};

class UnaryOp : public Instruction {
public:
    enum UnaryOpKind { Neg, Complement, Not };

private:
    Reg *dst;
    UnaryOpKind Kind;

public:
    UnaryOp(Reg *dst, Operand *src, UnaryOpKind Kind) : dst(dst), Kind(Kind) {
        addOperand(src);
    }

    void setDestination(Reg *reg) { dst = reg; }
    void setSource(Operand *src) {
        if (getNumOperands() == 0) addOperand(src); else setOperand(0, src);
    }
    void setKind(UnaryOpKind K) { Kind = K; }
    [[nodiscard]] Reg *getDestination() const { return dst; }
    [[nodiscard]] Value *getSource() const { return getOperand(0); }
    [[nodiscard]] UnaryOpKind getKind() const { return Kind; }

    [[nodiscard]] StringRef getOpcodeName() const override {
        if (Kind == Neg) return "neg";
        if (Kind == Complement) return "complement";
        return "not";
    }

    [[nodiscard]] std::string to_string() const override {
        return dst->to_string() + " = " + getOpcodeName().str() + " " +
               dst->getType()->to_string() + " " + getSource()->to_string();
    }
};

class BinaryOp : public Instruction {
public:
    enum BinaryOpKind { Add, Sub, Mul, Div, Rem, And, Or, Xor, Sal, Sar, none };

private:
    Reg *dst;
    BinaryOpKind Kind;

public:
    BinaryOp(Reg *dst, Operand *left, Operand *right, BinaryOpKind Kind)
        : dst(dst), Kind(Kind) {
        addOperand(left);
        addOperand(right);
    }

    void setDestination(Reg *reg) { dst = reg; }
    void setLeft(Operand *left) {
        if (getNumOperands() == 0) addOperand(left); else setOperand(0, left);
    }
    void setRight(Operand *right) {
        if (getNumOperands() == 1) addOperand(right); else setOperand(1, right);
    }
    void setKind(BinaryOpKind K) { Kind = K; }
    [[nodiscard]] Reg *getDestination() const { return dst; }
    [[nodiscard]] Value *getLeft() const { return getOperand(0); }
    [[nodiscard]] Value *getRight() const { return getOperand(1); }
    [[nodiscard]] BinaryOpKind getKind() const { return Kind; }

    [[nodiscard]] StringRef getOpcodeName() const override {
        switch (Kind) {
            case Add: return "add"; case Sub: return "sub"; case Mul: return "mul";
            case Div: return "div"; case Rem: return "rem"; case And: return "and";
            case Or:  return "or";  case Xor: return "xor"; case Sal: return "sal";
            case Sar: return "sar"; default: return "";
        }
    }

    [[nodiscard]] std::string to_string() const override {
        return dst->to_string() + " = " + getOpcodeName().str() + " " +
               dst->getType()->to_string() + " " +
               getLeft()->to_string() + ", " + getRight()->to_string();
    }
};

class ICmpOp : public Instruction {
public:
    enum CmpOpKind { lt, le, gt, ge, eq, neq, none };

private:
    Reg *Dst;
    CmpOpKind Kind;

public:
    ICmpOp(Reg *Dst, Operand *left, Operand *right, CmpOpKind K) : Dst(Dst), Kind(K) {
        addOperand(left);
        addOperand(right);
    }

    void setDestination(Reg *reg) { Dst = reg; }
    void setLeft(Operand *left) {
        if (getNumOperands() == 0) addOperand(left); else setOperand(0, left);
    }
    void setRight(Operand *right) {
        if (getNumOperands() == 1) addOperand(right); else setOperand(1, right);
    }
    void setKind(const CmpOpKind K) { Kind = K; }
    [[nodiscard]] Reg *getDestination() const { return Dst; }
    [[nodiscard]] Value *getLeft() const { return getOperand(0); }
    [[nodiscard]] Value *getRight() const { return getOperand(1); }
    [[nodiscard]] CmpOpKind getKind() const { return Kind; }

    [[nodiscard]] StringRef getOpcodeName() const override {
        switch (Kind) {
            case lt: return "lt"; case le: return "le"; case gt: return "gt";
            case ge: return "ge"; case eq: return "eq"; case neq: return "neq";
            default: return "";
        }
    }

    [[nodiscard]] std::string to_string() const override {
        // Use the left operand's type — both sides must be the same type for a comparison
        const std::string ty = dynamic_cast<Operand *>(getOperand(0))->getType()->to_string();
        return Dst->to_string() + " = icmp " + getOpcodeName().str() + " " +
               ty + " " + getLeft()->to_string() + ", " + getRight()->to_string();
    }
};

class Invoke : public Instruction {
    StringRef CalledFunction;
    Reg *result = nullptr;

public:
    Invoke() = default;
    explicit Invoke(const StringRef CalledFunction) : CalledFunction(CalledFunction) {}

    Invoke(const StringRef CalledFunction, const std::vector<Operand *> &operands, Reg *result)
        : CalledFunction(CalledFunction), result(result) {
        for (Operand *operand: operands) addOperand(operand);
    }

    [[nodiscard]] StringRef getCalledFunction() const { return CalledFunction; }
    [[nodiscard]] Reg *getResult() const { return result; }
    [[nodiscard]] bool hasResult() const { return result != nullptr; }
    void setResult(Reg *res) { result = res; }
    void setCalledFunction(StringRef funcName) { CalledFunction = funcName; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "call"; }

    [[nodiscard]] std::string to_string() const override {
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
};

class SignExtend : public Instruction {
    Reg *Result = nullptr;

public:
    SignExtend() = default;
    SignExtend(Operand *Src, Reg *Result) : Result(Result) { addOperand(Src); }

    [[nodiscard]] Operand *getSource() const { return dynamic_cast<Operand *>(getOperand(0)); }
    [[nodiscard]] Reg *getResult() const { return Result; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "sext"; }
    [[nodiscard]] std::string to_string() const override {
        return Result->to_string() + " = sext " +
               getSource()->getType()->to_string() + " " + getSource()->to_string() +
               " to " + Result->getType()->to_string();
    }
};

class Truncate : public Instruction {
    Reg *Result = nullptr;

public:
    Truncate() = default;
    Truncate(Operand *source, Reg *Result) : Result(Result) { addOperand(source); }

    [[nodiscard]] Operand *getSource() const { return dynamic_cast<Operand *>(getOperand(0)); }
    [[nodiscard]] Reg *getResult() const { return Result; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "trunc"; }
    [[nodiscard]] std::string to_string() const override {
        return Result->to_string() + " = trunc " +
               getSource()->getType()->to_string() + " " + getSource()->to_string() +
               " to " + Result->getType()->to_string();
    }
};

// ============================================================
// StaticVariable — no AST dependency, stores ir::Type* + int64_t
// ============================================================

class StaticVariable {
    std::string Identifier;
    bool Global;
    int64_t InitValue;
    Type* Ty;

public:
    StaticVariable(StringRef Identifier, bool Global, Type* ty, int64_t initValue)
        : Identifier(Identifier), Global(Global), InitValue(initValue), Ty(ty) {}

    [[nodiscard]] std::string getName() const { return Identifier; }
    [[nodiscard]] bool isGlobal() const { return Global; }
    [[nodiscard]] int64_t getInitialValue() const { return InitValue; }
    [[nodiscard]] Type* getType() const { return Ty; }

    [[nodiscard]] std::string to_string() const {
        std::string result = "@" + Identifier + " = ";
        if (!Global) result += "internal ";
        result += "global " + Ty->to_string() + " " + std::to_string(InitValue);
        return result;
    }
};

// ============================================================
// Function
// ============================================================

class Function {
    InstList Instructions;
    StringRef Name;
    Args args;
    bool global = true;
    FunctionType* FuncTy = nullptr;

public:
    Function() = default;

    Function(InstList &Instructions, StringRef Name, Args &args, bool global,
             FunctionType* funcTy = nullptr)
        : Instructions(std::move(Instructions)), Name(Name), args(std::move(args)),
          global(global), FuncTy(funcTy) {}

    [[nodiscard]] StringRef get_name() const { return Name; }
    [[nodiscard]] size_t size() const { return Instructions.size(); }
    [[nodiscard]] bool isGlobal() const { return global; }
    [[nodiscard]] bool empty() const { return Instructions.empty(); }
    [[nodiscard]] FunctionType* getFunctionType() const { return FuncTy; }
    void setFunctionType(FunctionType* ty) { FuncTy = ty; }

    void add_instruction(Instruction *I) { Instructions.push_back(I); }

    [[nodiscard]] const Args &getArgs() const { return args; }
    Args &getArgs() { return args; }

    InstList::iterator begin() { return Instructions.begin(); }
    InstList::iterator end() { return Instructions.end(); }
    [[nodiscard]] InstList::const_iterator begin() const { return Instructions.begin(); }
    [[nodiscard]] InstList::const_iterator end() const { return Instructions.end(); }

    [[nodiscard]] std::string to_string() const {
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
};

// ============================================================
// Program
// ============================================================

class Program {
    FuncList Funcs;
    StaticVarList StaticVars;
    StringRef Name;
    std::unique_ptr<Context> Ctx;

public:
    Program() : Ctx(std::make_unique<Context>()) {}
    explicit Program(const StringRef Name) : Name(Name), Ctx(std::make_unique<Context>()) {}

    ~Program() {
        for (const auto &F: Funcs) delete F;
        for (const auto &SV: StaticVars) delete SV;
    }

    [[nodiscard]] Context &getContext() const { return *Ctx; }
    [[nodiscard]] StringRef get_name() const { return Name; }
    [[nodiscard]] size_t size() const { return Funcs.size(); }
    [[nodiscard]] bool empty() const { return Funcs.empty(); }

    void add_function(Function *F) { Funcs.push_back(F); }
    void add_static_variable(StaticVariable *SV) { StaticVars.push_back(SV); }

    FuncList::iterator begin() { return Funcs.begin(); }
    FuncList::iterator end() { return Funcs.end(); }
    [[nodiscard]] FuncList::const_iterator begin() const { return Funcs.begin(); }
    [[nodiscard]] FuncList::const_iterator end() const { return Funcs.end(); }
    StaticVarList::iterator static_vars_begin() { return StaticVars.begin(); }
    StaticVarList::iterator static_vars_end() { return StaticVars.end(); }
    [[nodiscard]] StaticVarList::const_iterator static_vars_begin() const { return StaticVars.begin(); }
    [[nodiscard]] StaticVarList::const_iterator static_vars_end() const { return StaticVars.end(); }
    [[nodiscard]] const StaticVarList &getStaticVars() const { return StaticVars; }

    [[nodiscard]] std::string to_string() const {
        std::string result = "; Program: " + get_name().str() + "\n\n";
        for (const StaticVariable *sv: StaticVars) result += sv->to_string() + "\n";
        if (!StaticVars.empty()) result += "\n";
        for (const Function *func: Funcs) result += func->to_string() + "\n";
        return result;
    }
};

// ============================================================
// Context — owns all IR values and types
// ============================================================

class Context {
    // --- Type storage (interned) ---
    std::unordered_map<unsigned, std::unique_ptr<IntType>> IntTypes;
    std::unique_ptr<VoidType> VoidTy;
    std::deque<std::unique_ptr<FunctionType>> FuncTypes;

    // --- Operand/instruction storage ---
    StringMap<VarOp *> Variables;
    StringMap<StaticVarOp *> StaticVars;
    std::unordered_map<std::string, Label *> Labels;
    std::unordered_map<int32_t, Constant *> all_int_constants;
    std::unordered_map<int64_t, Constant *> all_long_constants;
    std::deque<std::unique_ptr<Value>> Values;
    unsigned NextRegID = 0;
    unsigned LabelNextID = 0;

public:
    Context() = default;
    ~Context() = default;
    Context(const Context &) = delete;
    Context &operator=(const Context &) = delete;

    // ---- Type factory methods ----

    IntType* getIntType(unsigned bitWidth) {
        auto &ty = IntTypes[bitWidth];
        if (!ty) ty = std::make_unique<IntType>(bitWidth);
        return ty.get();
    }

    IntType* getInt8Ty()  { return getIntType(8);  }
    IntType* getInt16Ty() { return getIntType(16); }
    IntType* getInt32Ty() { return getIntType(32); }
    IntType* getInt64Ty() { return getIntType(64); }

    VoidType* getVoidTy() {
        if (!VoidTy) VoidTy = std::make_unique<VoidType>();
        return VoidTy.get();
    }

    FunctionType* createFunctionType(Type* ret, std::vector<Type*> params) {
        auto ft = std::make_unique<FunctionType>(ret, std::move(params));
        FunctionType* ptr = ft.get();
        FuncTypes.push_back(std::move(ft));
        return ptr;
    }

    // ---- Value factory methods ----

    Label *getOrCreateLabel(const std::string &name, bool isUserDefined = false) {
        if (isUserDefined && Labels.contains(name))
            return Labels[name];
        std::string labelName = isUserDefined ? name : (name + "_" + std::to_string(LabelNextID++));
        auto *label = new Label(labelName);
        Values.emplace_back(label);
        Labels[name] = label;
        return label;
    }

    Jump *createJump(Label *label) {
        auto *jump = new Jump(label);
        Values.emplace_back(jump);
        return jump;
    }

    JumpIfZero *createJZ(Value *cond, Label *label) {
        auto *jz = new JumpIfZero(cond, label);
        Values.emplace_back(jz);
        return jz;
    }

    JumpIfNotZero *createJNZ(Value *cond, Label *label) {
        auto *jnz = new JumpIfNotZero(cond, label);
        Values.emplace_back(jnz);
        return jnz;
    }

    Constant *createConstant(Type* ty, const int64_t value) {
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

    std::vector<Constant *> getAllConstants() const {
        std::vector<Constant *> result;
        for (const auto &[_, c]: all_int_constants)  result.emplace_back(c);
        for (const auto &[_, c]: all_long_constants) result.emplace_back(c);
        return result;
    }

    Reg *createReg(Type* ty) {
        auto *RegVal = new Reg(NextRegID++, ty);
        Values.emplace_back(RegVal);
        return RegVal;
    }

    VarOp *getOrCreateVar(const StringRef Name, Type* ty) {
        if (Variables.contains(Name)) return Variables[Name];
        auto *newVar = new VarOp(Name, ty);
        Values.emplace_back(newVar);
        Variables[Name] = newVar;
        return newVar;
    }

    StaticVarOp *getOrCreateStaticVar(const StringRef Name, ir::Type* ty) {
        if (StaticVars.contains(Name)) return StaticVars[Name];
        auto *newVar = new StaticVarOp(Name, ty);
        Values.emplace_back(newVar);
        StaticVars[Name] = newVar;
        return newVar;
    }

    Copy *createCopy(Value *src, Value *dst) {
        auto *copy = new Copy(src, dst);
        Values.emplace_back(copy);
        return copy;
    }

    Mov *createMov(Value *src, Value *dst) {
        auto *MovInst = new Mov(src, dst);
        Values.emplace_back(MovInst);
        return MovInst;
    }

    Ret *createRet(Value *retval = nullptr) {
        Ret *RetInst = retval ? new Ret(retval) : new Ret();
        Values.emplace_back(RetInst);
        return RetInst;
    }

    UnaryOp *createUnaryOp(Operand *src, UnaryOp::UnaryOpKind kind) {
        Reg *dst = createReg(src->getType());
        auto *UnaryInst = new UnaryOp(dst, src, kind);
        Values.emplace_back(UnaryInst);
        return UnaryInst;
    }

    BinaryOp *createBinaryOp(Operand *left, Operand *right, BinaryOp::BinaryOpKind kind) {
        Reg *dst = createReg(left->getType());
        auto *BinaryInst = new BinaryOp(dst, left, right, kind);
        Values.emplace_back(BinaryInst);
        return BinaryInst;
    }

    ICmpOp *createICmpOp(Operand *left, Operand *right, ICmpOp::CmpOpKind kind, Type* resultTy) {
        Reg *dst = createReg(resultTy);
        auto *cmp = new ICmpOp(dst, left, right, kind);
        Values.emplace_back(cmp);
        return cmp;
    }

    Invoke *createInvoke(const StringRef CalledFunction, const std::vector<Operand *> &operands, Type* resultTy) {
        Reg *dst = createReg(resultTy);
        auto *invoke = new Invoke(CalledFunction, operands, dst);
        Values.emplace_back(invoke);
        return invoke;
    }

    SignExtend *createSignExtend(Operand *src, Type* toType) {
        Reg *dst = createReg(toType);
        auto *signExtend = new SignExtend(src, dst);
        Values.emplace_back(signExtend);
        return signExtend;
    }

    Truncate *createTruncate(Operand *src, Type* toType) {
        Reg *dst = createReg(toType);
        auto *truncate = new Truncate(src, dst);
        Values.emplace_back(truncate);
        return truncate;
    }
};

} // namespace mycc::ir
