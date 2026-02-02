#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringMap.h"

#include <utility>
#include <vector>
#include <memory>
#include <ranges>
#include <string>
#include <unordered_map>
#include <iostream>

#include "SimpleIR.hpp"


namespace mycc::ir {
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

    /**
     * @brief Base class from most of the other classes in the IR.
     * In this way, it is easier to manipulate those classes. Also
     * thanks to that, @ref Instruction class can have a vector
     * of operands where all of them are pointers to values.
     */
    class Value {
    public:
        virtual ~Value() = default;

        [[nodiscard]] virtual std::string to_string() const = 0;
    };

    /**
     * @brief Instruction class, and as everything in the IR derives from
     * Value. The Instruction class contains a vector with all the operands
     * from an instruction. Every instruction is responsible on how these
     * operands are inserted in the instruction, and they can also have their
     * own functions to retrieve these operands.
     */
    class Instruction : public Value {
        std::vector<Value *> operands;

    public:
        Instruction() = default;

        explicit Instruction(std::vector<Value *> operands) : operands(std::move(operands)) {
        }

        ~Instruction() override = default;

        // Operand access
        [[nodiscard]] size_t getNumOperands() const { return operands.size(); }
        [[nodiscard]] Value *getOperand(size_t i) const { return operands[i]; }
        void setOperand(size_t i, Value *val) { operands[i] = val; }

        // Operand modification
        void addOperand(Value *val) { operands.push_back(val); }
        void clearOperands() { operands.clear(); }

        // Iterators
        std::vector<Value *>::iterator operand_begin() { return operands.begin(); }
        std::vector<Value *>::iterator operand_end() { return operands.end(); }
        [[nodiscard]] std::vector<Value *>::const_iterator operand_begin() const { return operands.begin(); }
        [[nodiscard]] std::vector<Value *>::const_iterator operand_end() const { return operands.end(); }

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

        llvm::APSInt &getValue() {
            return Value;
        }

        [[nodiscard]] const llvm::APSInt &getValue() const {
            return Value;
        }

        [[nodiscard]] std::string to_string() const override {
            return std::to_string(Value.getSExtValue());
        }
    };

    class Reg : public Operand {
        unsigned RegID;

    public:
        explicit Reg(const unsigned ID) : RegID(ID) {
        }

        [[nodiscard]] unsigned getID() const { return RegID; }

        [[nodiscard]] std::string to_string() const override {
            return "%r" + std::to_string(RegID);
        }
    };

    class StaticVarOp : public Operand {
        std::string Identifier;

    public:
        StaticVarOp(const StringRef Identifier) : Identifier(Identifier) {
        }

        [[nodiscard]] std::string getName() const { return Identifier; }

        [[nodiscard]] std::string to_string() const override {
            return "@" + Identifier;
        }
    };

    class VarOp : public Operand {
        std::string Name;

    public:
        explicit VarOp(const StringRef Name) : Name(Name) {
        }

        [[nodiscard]] std::string getName() const { return Name; }

        [[nodiscard]] std::string to_string() const override {
            return "%" + Name;
        }
    };

    class ParameterOp : public Operand {
        std::string Name;

    public:
        explicit ParameterOp(const StringRef Name) : Name(Name) {
        }

        [[nodiscard]] std::string getName() const { return Name; }

        [[nodiscard]] std::string to_string() const override {
            return "%" + Name;
        }
    };

    class Label : public Instruction {
        std::string label_identifier;

    public:
        explicit Label(std::string identifier) : label_identifier(std::move(identifier)) {
        }

        [[nodiscard]] const std::string &get_identifier() const { return label_identifier; }

        [[nodiscard]] StringRef getOpcodeName() const override {
            return label_identifier;
        }

        [[nodiscard]] std::string to_string() const override {
            return label_identifier;
        }
    };

    class Copy : public Instruction {
    public:
        explicit Copy(Value *src, Value *dst) {
            addOperand(src);
            addOperand(dst);
        }

        void setSrc(Value *src) {
            assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
            setOperand(0, src);
        }

        void setDst(Value *dst) {
            assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
            setOperand(1, dst);
        }

        [[nodiscard]] Value *getSrc() const {
            assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
            return getOperand(0);
        }

        [[nodiscard]] Value *getDst() const {
            assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
            return getOperand(1);
        }

        [[nodiscard]] StringRef getOpcodeName() const override {
            return "copy";
        }

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

        void setSrc(Value *src) {
            assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
            setOperand(0, src);
        }

        void setDst(Value *dst) {
            assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
            setOperand(1, dst);
        }

        [[nodiscard]] Value *getSrc() const {
            assert(getNumOperands() >= 2 && "Num of operands must be at least 2");
            return getOperand(0);
        }

        [[nodiscard]] Value *getDst() const {
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
        explicit Ret(Value *retval) {
            addOperand(retval);
        }

        // Return void (no operands)
        Ret() = default;

        void setReturnValue(Value *retval) {
            if (getNumOperands() == 0) {
                addOperand(retval);
            } else {
                assert(getNumOperands() >= 1 && "Num of operands must be at least 1");
                setOperand(0, retval);
            }
        }

        [[nodiscard]] Value *getReturnValue() const {
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

    class Jump : public Instruction {
        Label *destination;

    public:
        explicit Jump(Label *dst) : destination(dst) {
        }

        void setDst(Label *dst) {
            destination = dst;
        }

        [[nodiscard]] Label *getDst() const {
            return destination;
        }

        [[nodiscard]] StringRef getOpcodeName() const override {
            return "jmp";
        }

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

        void setCondition(Value *condition) {
            assert(getNumOperands() >= 1 && "Num of operands must be at least 1");
            setOperand(0, condition);
        }

        void setDst(Label *dst) {
            destination = dst;
        }

        [[nodiscard]] Value *getCondition() const {
            assert(getNumOperands() >= 1 && "No condition operand");
            return getOperand(0);
        }

        [[nodiscard]] Label *getDst() const {
            return destination;
        }

        [[nodiscard]] StringRef getOpcodeName() const override {
            return "jz";
        }

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

        void setCondition(Value *condition) {
            assert(getNumOperands() >= 1 && "Num of operands must be at least 1");
            setOperand(0, condition);
        }

        void setDst(Label *dst) {
            destination = dst;
        }

        [[nodiscard]] Value *getCondition() const {
            assert(getNumOperands() >= 1 && "No condition operand");
            return getOperand(0);
        }

        [[nodiscard]] Label *getDst() const {
            return destination;
        }

        [[nodiscard]] StringRef getOpcodeName() const override {
            return "jnz";
        }

        [[nodiscard]] std::string to_string() const override {
            return "jnz " + getCondition()->to_string() + ", " + getDst()->to_string();
        }
    };

    class UnaryOp : public Instruction {
    public:
        enum UnaryOpKind {
            Neg,
            Complement,
            Not,
        };

    private:
        Reg *dst;
        UnaryOpKind Kind;

    public:
        UnaryOp(Reg *dst, Operand *src, const UnaryOpKind Kind) : dst(dst), Kind(Kind) {
            addOperand(src);
        }

        void setDestination(Reg *reg) {
            this->dst = reg;
        }

        void setSource(Operand *src) {
            if (getNumOperands() == 0)
                addOperand(src);
            else
                setOperand(0, src);
        }

        void setKind(UnaryOpKind K) {
            this->Kind = K;
        }

        Reg *getDestination() {
            return dst;
        }

        [[nodiscard]] const Reg *getDestination() const {
            return dst;
        }

        [[nodiscard]] Value *getSource() const {
            assert(getNumOperands() > 0 && "No operands to retrieve source.");
            return getOperand(0);
        }

        [[nodiscard]] UnaryOpKind getKind() const { return Kind; }

        [[nodiscard]] StringRef getOpcodeName() const override {
            if (Kind == Neg)
                return "neg";
            if (Kind == Complement)
                return "complement";
            if (Kind == Not)
                return "not";
            return "";
        }

        [[nodiscard]] std::string to_string() const override {
            return dst->to_string() + " = " + getOpcodeName().str() + " " + getSource()->to_string();
        }
    };

    class BinaryOp : public Instruction {
    public:
        enum BinaryOpKind {
            // Chapter 3
            Add,
            Sub,
            Mul,
            Div,
            Rem,
            And,
            Or,
            Xor,
            Sal,
            Sar,
            none,
        };

    private:
        Reg *dst;
        BinaryOpKind Kind;

    public:
        BinaryOp(Reg *dst, Operand *left, Operand *right, BinaryOpKind Kind)
            : dst(dst), Kind(Kind) {
            addOperand(left);
            addOperand(right);
        }

        void setDestination(Reg *reg) {
            dst = reg;
        }

        void setLeft(Operand *left) {
            if (getNumOperands() == 0) {
                addOperand(left);
            } else {
                setOperand(0, left);
            }
        }

        void setRight(Operand *right) {
            assert(getNumOperands() >= 1 && "There must be at least one operand provided before right operand.");
            if (getNumOperands() == 1) {
                addOperand(right);
            } else {
                setOperand(1, right);
            }
        }

        void setKind(BinaryOpKind K) {
            Kind = K;
        }

        [[nodiscard]] Reg *getDestination() const {
            return dst;
        }

        [[nodiscard]] Value *getLeft() const {
            assert(getNumOperands() > 0 && "No operands to retrieve.");
            return getOperand(0);
        }

        [[nodiscard]] Value *getRight() const {
            assert(getNumOperands() > 1 && "No operands to retrieve.");
            return getOperand(1);
        }

        [[nodiscard]] BinaryOpKind getKind() const { return Kind; }

        [[nodiscard]] StringRef getOpcodeName() const override {
            switch (Kind) {
                case Add:
                    return "add";
                case Sub:
                    return "sub";
                case Mul:
                    return "mul";
                case Div:
                    return "div";
                case Rem:
                    return "rem";
                case And:
                    return "and";
                case Or:
                    return "or";
                case Xor:
                    return "xor";
                case Sal:
                    return "sal";
                case Sar:
                    return "sar";
                default:
                    return "";
            }
        }

        [[nodiscard]] std::string to_string() const override {
            return dst->to_string() + " = " + getOpcodeName().str() + " " +
                   getLeft()->to_string() + ", " + getRight()->to_string();
        }
    };

    class ICmpOp : public Instruction {
    public:
        enum CmpOpKind {
            lt,
            le,
            gt,
            ge,
            eq,
            neq,
            none,
        };

    private:
        Reg *Dst;
        CmpOpKind Kind;

    public:
        ICmpOp(Reg *Dst, Operand *left, Operand *right, CmpOpKind K) : Dst(Dst), Kind(K) {
            addOperand(left);
            addOperand(right);
        }

        void setDestination(Reg *reg) {
            Dst = reg;
        }

        void setLeft(Operand *left) {
            if (getNumOperands() == 0) {
                addOperand(left);
            } else {
                setOperand(0, left);
            }
        }

        void setRight(Operand *right) {
            assert(getNumOperands() >= 1 && "There must be at least one operand provided before right operand.");
            if (getNumOperands() == 1) {
                addOperand(right);
            } else {
                setOperand(1, right);
            }
        }

        void setKind(CmpOpKind K) {
            Kind = K;
        }

        [[nodiscard]] Reg *getDestination() const {
            return Dst;
        }

        [[nodiscard]] Value *getLeft() const {
            assert(getNumOperands() > 0 && "No operands to retrieve.");
            return getOperand(0);
        }

        [[nodiscard]] Value *getRight() const {
            assert(getNumOperands() > 1 && "No operands to retrieve.");
            return getOperand(1);
        }

        [[nodiscard]] CmpOpKind getKind() const { return Kind; }

        [[nodiscard]] StringRef getOpcodeName() const override {
            switch (Kind) {
                case lt:
                    return "lt";
                case le:
                    return "le";
                case gt:
                    return "gt";
                case ge:
                    return "ge";
                case eq:
                    return "eq";
                case neq:
                    return "neq";
                default:
                    return "";
            }
        }

        [[nodiscard]] std::string to_string() const override {
            return Dst->to_string() + " = icmp " + getOpcodeName().str() + " " +
                   getLeft()->to_string() + ", " + getRight()->to_string();
        }
    };

    class Invoke : public Instruction {
        StringRef CalledFunction;
        Reg *result = nullptr;

    public:
        Invoke() = default;

        explicit Invoke(const StringRef CalledFunction) : CalledFunction(CalledFunction) {
        }

        Invoke(const StringRef CalledFunction, const std::vector<Operand *> &operands) : CalledFunction(
            CalledFunction) {
            for (Operand *operand: operands) {
                addOperand(operand);
            }
        }

        Invoke(const StringRef CalledFunction, const std::vector<Operand *> &operands,
               Reg *result) : CalledFunction(CalledFunction), result(result) {
            for (Operand *operand: operands) {
                addOperand(operand);
            }
        }

        [[nodiscard]] StringRef getCalledFunction() const {
            return CalledFunction;
        }

        [[nodiscard]] Reg *getResult() const {
            return result;
        }

        [[nodiscard]] bool hasResult() const {
            return result != nullptr;
        }

        void setResult(Reg *res) {
            result = res;
        }

        void setCalledFunction(StringRef funcName) {
            CalledFunction = funcName;
        }

        [[nodiscard]] StringRef getOpcodeName() const override {
            return "call";
        }

        [[nodiscard]] std::string to_string() const override {
            std::string result_str;
            if (hasResult()) {
                result_str = getResult()->to_string() + " = ";
            }
            result_str += "call " + getCalledFunction().str() + "(";
            for (size_t i = 0; i < getNumOperands(); ++i) {
                if (i > 0) result_str += ", ";
                result_str += getOperand(i)->to_string();
            }
            result_str += ")";
            return result_str;
        }
    };

    class StaticVariable {
        std::string Identifier;
        bool Global;
        int InitialValue;

    public:
        StaticVariable(const StringRef Identifier, const bool Global, const int InitialValue = 0)
            : Identifier(Identifier), Global(Global), InitialValue(InitialValue) {
        }

        [[nodiscard]] std::string getName() const { return Identifier; }

        [[nodiscard]] bool isGlobal() const { return Global; }

        [[nodiscard]] int getInitialValue() const { return InitialValue; }

        [[nodiscard]] std::string to_string() const {
            // Format: @name = [internal] global i32 <value>
            std::string result = "@" + Identifier + " = ";
            if (!Global) {
                result += "internal ";
            }
            result += "global i32 " + std::to_string(InitialValue);
            return result;
        }
    };

    class Function {
        InstList Instructions;
        StringRef Name;
        Args args;
        bool global = true;

    public:
        Function() = default;

        Function(InstList &Instructions, StringRef Name) : Instructions(std::move(Instructions)), Name(Name) {
        }

        Function(InstList &Instructions, StringRef Name, Args &args) : Instructions(std::move(Instructions)),
                                                                       Name(Name), args(std::move(args)) {
        }

        Function(InstList &Instructions, StringRef Name, Args &args, bool global) : Instructions(
                std::move(Instructions)),
            Name(Name), args(std::move(args)), global(global) {
        }

        [[nodiscard]] StringRef get_name() const {
            return Name;
        }

        [[nodiscard]] size_t size() const {
            return Instructions.size();
        }

        [[nodiscard]] bool isGlobal() const {
            return global;
        }

        [[nodiscard]] bool empty() const {
            return Instructions.empty();
        }

        void add_instruction(Instruction *I) {
            Instructions.push_back(I);
        }

        Args &getArgs() {
            return args;
        }

        [[nodiscard]] const Args &getArgs() const {
            return args;
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
            std::string result = "define " + get_name().str() + "(";
            for (auto *arg: args) {
                result += arg->to_string() + ",";
            }
            if (!args.empty()) {
                result.pop_back(); // Remove trailing comma
            }
            result += ") {\n";
            for (const Instruction *inst: Instructions) {
                if (const auto *label = dynamic_cast<const Label *>(inst)) {
                    result += label->to_string() + ":\n";
                } else {
                    result += "  " + inst->to_string() + "\n";
                }
            }
            result += "}\n";
            return result;
        }
    };

    class Program {
        FuncList Funcs;
        StaticVarList StaticVars;
        StringRef Name;
        std::unique_ptr<Context> Ctx;

    public:
        Program() : Ctx(std::make_unique<Context>()) {
        }

        explicit Program(const StringRef Name) : Name(Name), Ctx(std::make_unique<Context>()) {
        }

        Program(FuncList &Funcs, StringRef Name) : Funcs(std::move(Funcs)), Name(Name),
                                                   Ctx(std::make_unique<Context>()) {
        }

        ~Program() {
            for (const auto &F: Funcs) {
                delete F;
            }
            for (const auto &SV: StaticVars) {
                delete SV;
            }
        }

        [[nodiscard]] Context &getContext() const { return *Ctx; }

        [[nodiscard]] StringRef get_name() const {
            return Name;
        }

        [[nodiscard]] size_t size() const {
            return Funcs.size();
        }

        [[nodiscard]] bool empty() const {
            return Funcs.empty();
        }

        void add_function(Function *F) {
            Funcs.push_back(F);
        }

        void add_static_variable(StaticVariable *SV) {
            StaticVars.push_back(SV);
        }

        // Function iterators
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

        // Static variable iterators
        StaticVarList::iterator static_vars_begin() {
            return StaticVars.begin();
        }

        StaticVarList::iterator static_vars_end() {
            return StaticVars.end();
        }

        [[nodiscard]] StaticVarList::const_iterator static_vars_begin() const {
            return StaticVars.begin();
        }

        [[nodiscard]] StaticVarList::const_iterator static_vars_end() const {
            return StaticVars.end();
        }

        [[nodiscard]] const StaticVarList &getStaticVars() const {
            return StaticVars;
        }

        [[nodiscard]] std::string to_string() const {
            std::string result = "; Program: " + get_name().str() + "\n\n";
            // Print static variables first (like LLVM IR does)
            for (const StaticVariable *sv: StaticVars) {
                result += sv->to_string() + "\n";
            }
            if (!StaticVars.empty()) {
                result += "\n";
            }
            for (const Function *func: Funcs) {
                result += func->to_string() + "\n";
            }
            return result;
        }
    };

    class Context {
        StringMap<VarOp *> Variables;
        StringMap<StaticVarOp *> StaticVars;
        std::unordered_map<std::string, Label *> Labels;
        std::unordered_map<std::int64_t, Int *> all_integer_values;
        std::vector<std::unique_ptr<Value> > Values;
        unsigned NextRegID = 0;
        unsigned LabelNextID = 0;

    public:
        Context() = default;

        ~Context() = default;

        // Non-copyable, non-movable for simplicity
        Context(const Context &) = delete;

        Context &operator=(const Context &) = delete;

        Label *getOrCreateLabel(const std::string &name, bool isUserDefined = false) {
            if (isUserDefined && Labels.contains(name))
                return Labels[name];
            // Only append unique ID for auto-generated labels
            // User-defined labels are already verified to be unique by semantic analysis
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

        // Factory methods for creating Values
        Int *createInt(llvm::APSInt Value) {
            const std::int64_t s_ext = Value.getSExtValue();
            if (all_integer_values.contains(s_ext))
                return all_integer_values[s_ext];
            auto *IntVal = new Int(std::move(Value));
            Values.emplace_back(IntVal);
            all_integer_values[s_ext] = IntVal;
            return IntVal;
        }

        std::vector<Int *> getAllIntegerConstants() const {
            std::vector<Int *> result;
            for (const auto &[Keys, Value]: all_integer_values) {
                result.emplace_back(Value);
            }
            return result;
        }

        Reg *createReg() {
            auto *RegVal = new Reg(NextRegID++);
            Values.emplace_back(RegVal);
            return RegVal;
        }

        VarOp *getOrCreateVar(const StringRef Name) {
            if (Variables.contains(Name)) {
                return Variables[Name];
            }
            auto *newVar = new VarOp(Name);
            Values.emplace_back(newVar);
            Variables[Name] = newVar;
            return newVar;
        }

        StaticVarOp *getOrCreateStaticVar(const StringRef Name) {
            if (StaticVars.contains(Name)) {
                return StaticVars[Name];
            }
            auto *newVar = new StaticVarOp(Name);
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

        UnaryOp *createUnaryOp(Operand *src, const UnaryOp::UnaryOpKind kind) {
            Reg *dst = createReg(); // Generate temporal register for destination
            auto *UnaryInst = new UnaryOp(dst, src, kind);
            Values.emplace_back(UnaryInst);
            return UnaryInst;
        }

        BinaryOp *createBinaryOp(Operand *left, Operand *right, const BinaryOp::BinaryOpKind kind) {
            Reg *dst = createReg(); // Generate temporal register for destination
            auto *BinaryInst = new BinaryOp(dst, left, right, kind);
            Values.emplace_back(BinaryInst);
            return BinaryInst;
        }

        ICmpOp *createICmpOp(Operand *left, Operand *right, const ICmpOp::CmpOpKind kind) {
            Reg *dst = createReg();
            auto *cmp = new ICmpOp(dst, left, right, kind);
            Values.emplace_back(cmp);
            return cmp;
        }

        Invoke *createInvoke(StringRef CalledFunction, const std::vector<Operand *> &operands) {
            Reg *dst = createReg();
            auto invoke = new Invoke(CalledFunction, operands, dst);
            Values.emplace_back(invoke);
            return invoke;
        }

        // All Values are automatically cleaned up when Context is destroyed
    };
}
