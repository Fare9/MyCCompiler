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

    [[nodiscard]] std::string to_string() const override;

    static bool classof(const Type *T) { return T->getKind() == TK_Int; }
};

/**
 * @brief Void type, used as return type of void functions.
 */
class VoidType : public Type {
public:
    VoidType() : Type(TK_Void) {}
    [[nodiscard]] std::string to_string() const override;

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

    [[nodiscard]] std::string to_string() const override;

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

    [[nodiscard]] std::string to_string() const override;
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

    [[nodiscard]] std::string to_string() const override;
};

class Reg : public Operand {
    unsigned RegID;
    ir::Type* Ty;

public:
    explicit Reg(unsigned ID, ir::Type* Ty) : RegID(ID), Ty(Ty) {}

    [[nodiscard]] unsigned getID() const { return RegID; }
    [[nodiscard]] ir::Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override;
};

class StaticVarOp : public Operand {
    std::string Identifier;
    ir::Type* Ty;

public:
    StaticVarOp(StringRef Identifier, ir::Type* Ty) : Identifier(Identifier), Ty(Ty) {}

    [[nodiscard]] std::string getName() const { return Identifier; }
    [[nodiscard]] ir::Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override;
};

class VarOp : public Operand {
    std::string Name;
    Type* Ty;

public:
    explicit VarOp(const StringRef Name, Type* Ty) : Name(Name), Ty(Ty) {}

    [[nodiscard]] std::string getName() const { return Name; }
    [[nodiscard]] Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override;
};

class ParameterOp : public Operand {
    std::string Name;
    Type* Ty;

public:
    explicit ParameterOp(StringRef Name, ir::Type* Ty) : Name(Name), Ty(Ty) {}

    [[nodiscard]] std::string getName() const { return Name; }
    [[nodiscard]] Type* getType() const override { return Ty; }

    [[nodiscard]] std::string to_string() const override;
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
    [[nodiscard]] std::string to_string() const override;
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
    [[nodiscard]] std::string to_string() const override;
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
    [[nodiscard]] std::string to_string() const override;
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
    [[nodiscard]] std::string to_string() const override;
};

class Jump : public Instruction {
    Label *destination;

public:
    explicit Jump(Label *dst) : destination(dst) {}

    void setDst(Label *dst) { destination = dst; }
    [[nodiscard]] Label *getDst() const { return destination; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "jmp"; }
    [[nodiscard]] std::string to_string() const override;
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
    [[nodiscard]] std::string to_string() const override;
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
    [[nodiscard]] std::string to_string() const override;
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

    [[nodiscard]] StringRef getOpcodeName() const override;
    [[nodiscard]] std::string to_string() const override;
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

    [[nodiscard]] StringRef getOpcodeName() const override;
    [[nodiscard]] std::string to_string() const override;
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

    [[nodiscard]] StringRef getOpcodeName() const override;
    [[nodiscard]] std::string to_string() const override;
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

    [[nodiscard]] std::string to_string() const override;
};

class SignExtend : public Instruction {
    Reg *Result = nullptr;

public:
    SignExtend() = default;
    SignExtend(Operand *Src, Reg *Result) : Result(Result) { addOperand(Src); }

    [[nodiscard]] Operand *getSource() const { return dynamic_cast<Operand *>(getOperand(0)); }
    [[nodiscard]] Reg *getResult() const { return Result; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "sext"; }
    [[nodiscard]] std::string to_string() const override;
};

class Truncate : public Instruction {
    Reg *Result = nullptr;

public:
    Truncate() = default;
    Truncate(Operand *source, Reg *Result) : Result(Result) { addOperand(source); }

    [[nodiscard]] Operand *getSource() const { return dynamic_cast<Operand *>(getOperand(0)); }
    [[nodiscard]] Reg *getResult() const { return Result; }
    [[nodiscard]] StringRef getOpcodeName() const override { return "trunc"; }
    [[nodiscard]] std::string to_string() const override;
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

    [[nodiscard]] std::string to_string() const;
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

    [[nodiscard]] std::string to_string() const;
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
    Program();
    explicit Program(const StringRef Name);

    ~Program();

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

    [[nodiscard]] std::string to_string() const;
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

    IntType* getIntType(unsigned bitWidth);

    IntType* getInt8Ty()  { return getIntType(8);  }
    IntType* getInt16Ty() { return getIntType(16); }
    IntType* getInt32Ty() { return getIntType(32); }
    IntType* getInt64Ty() { return getIntType(64); }

    VoidType* getVoidTy();

    FunctionType* createFunctionType(Type* ret, std::vector<Type*> params);

    // ---- Value factory methods ----

    Label *getOrCreateLabel(const std::string &name, bool isUserDefined = false);
    Jump *createJump(Label *label);
    JumpIfZero *createJZ(Value *cond, Label *label);
    JumpIfNotZero *createJNZ(Value *cond, Label *label);
    Constant *createConstant(Type* ty, int64_t value);
    std::vector<Constant *> getAllConstants() const;
    Reg *createReg(Type* ty);
    VarOp *getOrCreateVar(StringRef Name, Type* ty);
    StaticVarOp *getOrCreateStaticVar(StringRef Name, ir::Type* ty);
    Copy *createCopy(Value *src, Value *dst);
    Mov *createMov(Value *src, Value *dst);
    Ret *createRet(Value *retval = nullptr);
    UnaryOp *createUnaryOp(Operand *src, UnaryOp::UnaryOpKind kind);
    BinaryOp *createBinaryOp(Operand *left, Operand *right, BinaryOp::BinaryOpKind kind);
    ICmpOp *createICmpOp(Operand *left, Operand *right, ICmpOp::CmpOpKind kind, Type* resultTy);
    Invoke *createInvoke(StringRef CalledFunction, const std::vector<Operand *> &operands, Type* resultTy);
    SignExtend *createSignExtend(Operand *src, Type* toType);
    Truncate *createTruncate(Operand *src, Type* toType);
};

} // namespace mycc::ir
