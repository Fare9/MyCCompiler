#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"

#include <vector>
#include <memory>
#include <string>

namespace mycc {
namespace ir {

class Context;
class Program;
class Function;
class Value;
class Instruction;
class Operand;

using FuncList = std::vector<Function*>;

using InstList = std::vector<Instruction*>;

class Value {
public:
    virtual ~Value() = default;
    [[nodiscard]] virtual std::string to_string() const = 0;
};

class Instruction : public Value {
    std::vector<Value*> operands;
public:
    Instruction() = default;
    explicit Instruction(std::vector<Value*> operands) : operands(std::move(operands)) {}
    
    virtual ~Instruction() = default;
    
    // Operand access
    [[nodiscard]] size_t getNumOperands() const { return operands.size(); }
    [[nodiscard]] Value* getOperand(size_t i) const { return operands[i]; }
    void setOperand(size_t i, Value* val) { operands[i] = val; }
    
    // Operand modification
    void addOperand(Value* val) { operands.push_back(val); }
    void clearOperands() { operands.clear(); }
    
    // Iterators
    std::vector<Value*>::iterator operand_begin() { return operands.begin(); }
    std::vector<Value*>::iterator operand_end() { return operands.end(); }
    [[nodiscard]] std::vector<Value*>::const_iterator operand_begin() const { return operands.begin(); }
    [[nodiscard]] std::vector<Value*>::const_iterator operand_end() const { return operands.end(); }
    
    // Pure virtual for instruction-specific behavior
    [[nodiscard]] virtual StringRef getOpcodeName() const = 0;
    
    // Default implementation for instructions
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

class Operand : public Value {
public:
    Operand() = default;
    ~Operand() override = default;
};

class Int : public Operand {
    llvm::APSInt Value;
public:
    explicit Int(llvm::APSInt Value) : Value(std::move(Value)) {
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

class Reg : public Operand {
    unsigned RegID;

public:
    explicit Reg(unsigned ID) : RegID(ID) {}
    
    [[nodiscard]] unsigned getID() const { return RegID; }
    
    [[nodiscard]] std::string to_string() const override {
        return "%r" + std::to_string(RegID);
    }
};

class Mov : public Instruction {
public:
    explicit Mov(Value * src, Value *dst) {
        addOperand(src);
        addOperand(dst);
    }

    void setSrc(Value * src) {
        assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
        setOperand(0, src);
    }

    void setDst(Value * dst) {
        assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
        setOperand(1, dst);
    }

    [[nodiscard]] Value * getSrc() const {
        assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
        return getOperand(0);
    }

    [[nodiscard]] Value * getDst() const {
        assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
        return getOperand(1);
    }

    [[nodiscard]] StringRef getOpcodeName() const override {
        return "mov";
    }
    
    [[nodiscard]] std::string to_string() const override {
        return "mov " + getDst()->to_string() + ", " + getSrc()->to_string();
    }
};

class Ret : public Instruction {
public:
    // Return with value
    explicit Ret(Value * retval) {
        addOperand(retval);
    }

    // Return void (no operands)
    Ret() = default;

    void setReturnValue(Value * retval) {
        if (getNumOperands() == 0) {
            addOperand(retval);
        } else {
            assert(getNumOperands() >= 1 && "Num of operands must be at least 1");
            setOperand(0, retval);
        }
    }

    [[nodiscard]] Value * getReturnValue() const {
        assert(getNumOperands() >= 1 && "No return value for void return");
        return getOperand(0);
    }

    [[nodiscard]] bool isVoidReturn() const {
        return getNumOperands() == 0;
    }

    [[nodiscard]] StringRef getOpcodeName() const override {
        return "ret";
    }
    
    [[nodiscard]] std::string to_string() const override {
        if (isVoidReturn()) {
            return "ret";
        } else {
            return "ret " + getReturnValue()->to_string();
        }
    }
};

class UnaryOp : public Instruction {
public:
    enum UnaryOpKind {
        Neg,
        Complement
    };
private:
    Reg * dst;
    UnaryOpKind Kind;
public:
    UnaryOp(Reg *dst, Operand *src, UnaryOpKind Kind) :
        dst(dst), Kind(Kind) {
        addOperand(src);
    }

    UnaryOp() = default;

    void setDestination(Reg * reg) {
        this->dst = reg;
    }

    void setSource(Operand * src) {
        if (getNumOperands() == 0)
            addOperand(src);
        else
            setOperand(0, src);
    }

    void setKind(UnaryOpKind K) {
        this->Kind = K;
    }

    Reg * getDestination() {
        return dst;
    }

    [[nodiscard]] const Reg * getDestination() const {
        return dst;
    }

    [[nodiscard]] Value * getSource() const {
        assert(getNumOperands() > 0 && "No operands to retrieve source.");
        return getOperand(0);
    }

    [[nodiscard]] UnaryOpKind getKind() const { return Kind; }

    [[nodiscard]] StringRef getOpcodeName() const override {
        if (Kind == Neg)
            return "neg";
        else if (Kind == Complement)
            return "complement";
        return "";
    }

    [[nodiscard]] std::string to_string() const override {
        return dst->to_string() + " = " + getOpcodeName().str() + " " + getSource()->to_string();
    }
};

class Function {
    InstList Instructions;
    StringRef Name;

public:

    Function() = default;

    Function(InstList & Instructions, StringRef Name) :
            Instructions(std::move(Instructions)), Name(Name) {
    }

    [[nodiscard]] StringRef get_name() const {
        return Name;
    }

    [[nodiscard]] size_t size() const {
        return Instructions.size();
    }

    [[nodiscard]] bool empty() const {
        return Instructions.empty();
    }

    void add_instruction(Instruction * I) {
        Instructions.push_back(I);
    }

    InstList::iterator begin() {
        return Instructions.begin();
    }

    InstList::iterator end() {
        return Instructions.end();
    }

    [[nodiscard]] InstList::const_iterator begin() const {
        return Instructions.begin();
    }

    [[nodiscard]] InstList::const_iterator end() const {
        return Instructions.end();
    }
    
    [[nodiscard]] std::string to_string() const {
        std::string result = "define " + get_name().str() + "() {\n";
        for (const Instruction* inst : Instructions) {
            result += "  " + inst->to_string() + "\n";
        }
        result += "}\n";
        return result;
    }
};

class Program {
    FuncList Funcs;
    StringRef Name;
    std::unique_ptr<Context> Ctx;

public:
    Program() : Ctx(std::make_unique<Context>()) {}
    explicit Program(StringRef Name) : Name(Name), Ctx(std::make_unique<Context>()) {}

    Program(FuncList & Funcs, StringRef Name) :
            Funcs(std::move(Funcs)), Name(Name), Ctx(std::make_unique<Context>()) {
    }

    ~Program() {
        for (auto & F : Funcs) {
            delete F;
        }
    }

    Context& getContext() { return *Ctx; }

    [[nodiscard]] StringRef get_name() const {
        return Name;
    }

    [[nodiscard]] size_t size() const {
        return Funcs.size();
    }

    [[nodiscard]] bool empty() const {
        return Funcs.empty();
    }

    void add_function(Function * F) {
        Funcs.push_back(F);
    }

    FuncList::iterator begin() {
        return Funcs.begin();
    }

    FuncList::iterator end() {
        return Funcs.end();
    }

    [[nodiscard]] FuncList::const_iterator begin() const {
        return Funcs.begin();
    }

    [[nodiscard]] FuncList::const_iterator end() const {
        return Funcs.end();
    }
    
    [[nodiscard]] std::string to_string() const {
        std::string result = "; Program: " + get_name().str() + "\n\n";
        for (const Function* func : Funcs) {
            result += func->to_string() + "\n";
        }
        return result;
    }
};


class Context {
    std::vector<std::unique_ptr<Value>> Values;
    unsigned NextRegID = 0;
    
public:
    Context() = default;
    ~Context() = default;
    
    // Non-copyable, non-movable for simplicity
    Context(const Context&) = delete;
    Context& operator=(const Context&) = delete;
    
    // Factory methods for creating Values
    Int* createInt(llvm::APSInt Value) {
        auto* IntVal = new Int(std::move(Value));
        Values.emplace_back(IntVal);
        return IntVal;
    }
    
    Reg* createReg() {
        auto* RegVal = new Reg(NextRegID++);
        Values.emplace_back(RegVal);
        return RegVal;
    }
    
    Mov* createMov(Value* src, Value* dst) {
        auto* MovInst = new Mov(src, dst);
        Values.emplace_back(MovInst);
        return MovInst;
    }
    
    Ret* createRet(Value* retval = nullptr) {
        Ret* RetInst = retval ? new Ret(retval) : new Ret();
        Values.emplace_back(RetInst);
        return RetInst;
    }
    
    UnaryOp* createUnaryOp(Operand* src, UnaryOp::UnaryOpKind kind) {
        Reg* dst = createReg();  // Generate temporal register for destination
        auto* UnaryInst = new UnaryOp(dst, src, kind);
        Values.emplace_back(UnaryInst);
        return UnaryInst;
    }

    // All Values are automatically cleaned up when Context is destroyed
};

}
}