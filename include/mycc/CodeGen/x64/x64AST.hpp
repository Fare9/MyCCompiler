#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/APSInt.h"

#include <deque>
#include <utility>
#include <vector>
#include <unordered_map>
#include <sstream>

#include "x64AST.hpp"


namespace mycc::codegen::x64 {
    enum class X64Type : uint8_t {
        Byte = 1, // i8   - al, bl, ...     - byte ptr
        Word = 2, // i16  - ax, bx, ...     - word ptr
        LongWord = 4, // i32  - eax, ebx, ...   - dword ptr
        QuadWord = 8, // i64  - rax, rbx, ...   - qword ptr
    };

    inline unsigned getSizeInBytes(X64Type type) {
        return static_cast<unsigned>(type);
    }

    inline const char *getPtrDirective(X64Type T) {
        switch (T) {
            case X64Type::Byte: return "byte ptr";
            case X64Type::Word: return "word ptr";
            case X64Type::LongWord: return "dword ptr";
            case X64Type::QuadWord: return "qword ptr";
        }
        return "";
    }

    inline const char *getAsmSuffix(X64Type T) {
        switch (T) {
            case X64Type::Byte: return "b";
            case X64Type::Word: return "w";
            case X64Type::LongWord: return "l";
            case X64Type::QuadWord: return "q";
        }
        return "";
    }

    enum class X64ConditionTypeE {
        E, // Equal
        NE, // NotEqual
        G, // Greater
        GE, // GreaterEqual
        L, // Lower
        LE, // LowerEqual
    };

    class X64Program;
    class X64Function;
    class X64StaticVar;
    class X64Instruction;
    class X64Register;
    class X64Operand;

    using X64StaticVars = std::vector<X64StaticVar *>;
    using X64Functions = std::vector<X64Function *>;
    using X64Instructions = std::deque<X64Instruction *>;

    class X64Operand {
    public:
        virtual ~X64Operand() = default;

        [[nodiscard]] virtual std::string to_string() const = 0;
    };

    class X64Int : public X64Operand {
        llvm::APSInt Value;

    public:
        explicit X64Int(llvm::APSInt Value) : Value(std::move(Value)) {
        }

        llvm::APSInt &getValue() {
            return Value;
        }

        [[nodiscard]] const llvm::APSInt &getValue() const {
            return Value;
        }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Register : public X64Operand {
    public:
        enum Type { Pseudo, Physical };

        [[nodiscard]] virtual Type getType() const = 0;
    };

    class PseudoRegister : public X64Register {
        unsigned ID;
        X64Type Size{X64Type::LongWord};

    public:
        explicit PseudoRegister(unsigned id, X64Type size = X64Type::LongWord)
            : ID(id), Size(size) {
        }

        [[nodiscard]] Type getType() const override {
            return Pseudo;
        }

        [[nodiscard]] unsigned getID() const {
            return ID;
        }

        [[nodiscard]] X64Type getSize() const {
            return Size;
        }

        [[nodiscard]] std::string to_string() const override;
    };

    class PhysicalRegister : public X64Register {
    public:
        enum PhysReg {
            RAX, RBX, RCX, RDX,
            RSI, RDI, RSP, RBP,
            R8, R9, R10, R11,
            R12, R13, R14, R15
        };

    private:
        PhysReg reg;
        X64Type regSize;

    public:
        explicit PhysicalRegister(PhysReg r, X64Type s = X64Type::QuadWord) : reg(r), regSize(s) {
        }

        [[nodiscard]] Type getType() const override {
            return Physical;
        }

        void setReg(PhysReg r) {
            reg = r;
        }

        void setSize(X64Type s) {
            regSize = s;
        }

        [[nodiscard]] PhysReg getReg() const {
            return reg;
        }

        [[nodiscard]] X64Type getSize() const {
            return regSize;
        }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Stack : public X64Operand {
        llvm::APSInt Offset;
        X64Register *StackReg = nullptr;
        X64Type AccessSize;

    public:
        X64Stack() : AccessSize(X64Type::QuadWord) {
        }

        X64Stack(llvm::APSInt Offset, X64Register *StackReg, X64Type AccessSize = X64Type::QuadWord)
            : Offset(std::move(Offset)), StackReg(StackReg), AccessSize(AccessSize) {
        }

        void setOffset(llvm::APSInt O) {
            Offset = std::move(O);
        }

        void setStackReg(X64Register *R) {
            StackReg = R;
        }

        void setSize(X64Type S) {
            AccessSize = S;
        }

        llvm::APSInt &getOffset() {
            return Offset;
        }

        [[nodiscard]] const llvm::APSInt &getOffset() const {
            return Offset;
        }

        X64Register *getStackReg() {
            return StackReg;
        }

        [[nodiscard]] const X64Register *getStackReg() const {
            return StackReg;
        }

        [[nodiscard]] X64Type getSize() const {
            return AccessSize;
        }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Data : public X64Operand {
        std::string Name;
        X64Type Size{X64Type::LongWord};

    public:
        X64Data() : Size(X64Type::LongWord) {}

        X64Data(StringRef Name, X64Type size = X64Type::LongWord)
            : Name(Name), Size(size) {
        }

        [[nodiscard]] StringRef getName() const {
            return Name;
        }

        [[nodiscard]] X64Type getSize() const {
            return Size;
        }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Instruction {
    public:
        virtual ~X64Instruction() = default;

        virtual std::string to_string() const = 0;
    };

    class X64Label : public X64Instruction {
        StringRef Name;

    public:
        X64Label() = default;

        explicit X64Label(StringRef Name) : Name(Name) {
        }

        [[nodiscard]] StringRef getName() const {
            return Name;
        }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Mov : public X64Instruction {
        X64Operand *Src{};
        X64Operand *Dst{};
        X64Type AccessSize{X64Type::QuadWord};

    public:
        X64Mov() = default;

        X64Mov(X64Operand *Src, X64Operand *Dst)
            : Src(Src), Dst(Dst) {
        }

        X64Mov(X64Operand *Src, X64Operand *Dst, X64Type AccessSize)
            : Src(Src), Dst(Dst), AccessSize(AccessSize) {
        }

        void setSrc(X64Operand *S) { Src = S; }
        void setDst(X64Operand *D) { Dst = D; }
        X64Operand *getSrc() { return Src; }
        [[nodiscard]] const X64Operand *getSrc() const { return Src; }
        X64Operand *getDst() { return Dst; }
        [[nodiscard]] const X64Operand *getDst() const { return Dst; }
        [[nodiscard]] X64Type getAccessSize() const { return AccessSize; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Movsx : public X64Instruction {
        X64Operand *Src{};
        X64Operand *Dst{};
        X64Type TypeFrom{X64Type::LongWord};
        X64Type TypeTo{X64Type::QuadWord};
    public:
        X64Movsx() = default;

        X64Movsx(X64Operand *Src, X64Operand *Dst)
            : Src(Src), Dst(Dst) {
        }

        X64Movsx(X64Operand *Src, X64Operand *Dst, X64Type TypeFrom, X64Type TypeTo)
            : Src(Src), Dst(Dst), TypeFrom(TypeFrom), TypeTo(TypeTo) {
        }

        void setSrc(X64Operand *S) { Src = S; }
        void setDst(X64Operand *D) { Dst = D; }
        X64Operand *getSrc() { return Src; }
        [[nodiscard]] const X64Operand *getSrc() const { return Src; }
        X64Operand *getDst() { return Dst; }
        [[nodiscard]] const X64Operand *getDst() const { return Dst; }
        [[nodiscard]] X64Type getTypeFrom() const { return TypeFrom; }
        [[nodiscard]] X64Type getTypeTo() const { return TypeTo; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Cmp : public X64Instruction {
        X64Operand *Left{};
        X64Operand *Right{};
        X64Type Size{X64Type::QuadWord};

    public:
        X64Cmp() = default;

        X64Cmp(X64Operand *Left, X64Operand *Right)
            : Left(Left), Right(Right) {
        }

        X64Cmp(X64Operand *Left, X64Operand *Right, X64Type Size)
            : Left(Left), Right(Right), Size(Size) {
        }

        [[nodiscard]] X64Operand *getLeft() const { return Left; }
        [[nodiscard]] X64Operand *getRight() const { return Right; }
        void setLeft(X64Operand *L) { Left = L; }
        void setRight(X64Operand *R) { Right = R; }
        [[nodiscard]] X64Type getSize() const { return Size; }
        void setSize(X64Type S) { Size = S; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Jmp : public X64Instruction {
        X64Label *Target{};

    public:
        X64Jmp() = default;

        X64Jmp(X64Label *Target) : Target(Target) {
        }

        [[nodiscard]] X64Label *getTarget() const { return Target; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64JmpCC : public X64Instruction {
        X64ConditionTypeE Condition;
        X64Label *Target;

    public:
        X64JmpCC() = default;

        X64JmpCC(X64ConditionTypeE Condition, X64Label *Target)
            : Condition(Condition), Target(Target) {
        }

        [[nodiscard]] X64ConditionTypeE getCondition() const { return Condition; }
        [[nodiscard]] X64Label *getTarget() const { return Target; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64SetCC : public X64Instruction {
        X64ConditionTypeE Condition;
        X64Operand *Op;

    public:
        X64SetCC() = default;

        X64SetCC(X64ConditionTypeE Condition, X64Operand *Op)
            : Condition(Condition), Op(Op) {
        }

        [[nodiscard]] X64Operand *getOperand() const { return Op; }
        [[nodiscard]] X64ConditionTypeE getCondition() const { return Condition; }
        void setOperand(X64Operand *O) { Op = O; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Unary : public X64Instruction {
    public:
        enum X64UnaryKind {
            Neg,
            Complement,
            None
        };

    private:
        X64UnaryKind Kind{None};
        X64Operand *Op{};
        X64Type Type{X64Type::QuadWord};

    public:
        X64Unary() = default;

        X64Unary(X64UnaryKind Kind, X64Operand *Op) : Kind(Kind), Op(Op) {
        }

        X64Unary(X64UnaryKind Kind, X64Operand *Op, X64Type Type) : Kind(Kind), Op(Op), Type(Type) {
        }

        void setKind(X64UnaryKind K) { Kind = K; }
        void setOperand(X64Operand *O) { Op = O; }
        void setType(X64Type T) { Type = T; }
        [[nodiscard]] X64UnaryKind getKind() const { return Kind; }
        X64Operand *getOperand() { return Op; }
        [[nodiscard]] const X64Operand *getOperand() const { return Op; }
        [[nodiscard]] X64Type getType() const { return Type; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Binary : public X64Instruction {
    public:
        enum X64BinaryKind {
            Add,
            Sub,
            Mult,
            And,
            Or,
            Xor,
            Sal,
            Sar,
            None
        };

    private:
        X64BinaryKind Kind{None};
        X64Operand *src{};
        X64Operand *dst{};
        X64Type Type{X64Type::LongWord};

    public:
        X64Binary() = default;

        X64Binary(X64BinaryKind Kind, X64Operand *src, X64Operand *dst)
            : Kind(Kind), src(src), dst(dst) {
        }

        X64Binary(X64BinaryKind Kind, X64Operand *src, X64Operand *dst, X64Type Type)
            : Kind(Kind), src(src), dst(dst), Type(Type) {
        }

        void setKind(X64BinaryKind K) { Kind = K; }
        void setSrc(X64Operand *S) { src = S; }
        void setDst(X64Operand *D) { dst = D; }
        [[nodiscard]] X64BinaryKind getKind() const { return Kind; }
        [[nodiscard]] X64Operand *getSrc() const { return src; }
        [[nodiscard]] X64Operand *getDst() const { return dst; }
        [[nodiscard]] X64Type getType() const { return Type; }
        void setType(X64Type T) { Type = T; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64IDiv : public X64Instruction {
        X64Operand *Op;
        X64Type Size{X64Type::QuadWord};

    public:
        explicit X64IDiv(X64Operand *Op) : Op(Op) {
        }

        X64IDiv(X64Operand *Op, X64Type Size) : Op(Op), Size(Size) {
        }

        void setOperand(X64Operand *O) { Op = O; }
        [[nodiscard]] X64Operand *getOperand() const { return Op; }
        [[nodiscard]] X64Type getSize() const { return Size; }
        void setSize(X64Type S) { Size = S; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Cdq : public X64Instruction {
        X64Type Size{X64Type::LongWord};

    public:
        X64Cdq() = default;

        explicit X64Cdq(X64Type Size) : Size(Size) {
        }

        [[nodiscard]] X64Type getSize() const { return Size; }
        void setSize(X64Type S) { Size = S; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Ret : public X64Instruction {
    public:
        X64Ret() = default;

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Allocate : public X64Instruction {
        X64Operand *allocationRegister;
        X64Operand *Offset;

    public:
        X64Allocate() = default;

        X64Allocate(X64Operand *allocationRegister, X64Operand *Offset)
            : allocationRegister(allocationRegister), Offset(Offset) {
        }

        void setAllocationRegister(X64Operand *Reg) { allocationRegister = Reg; }
        void setOffset(X64Operand *O) { Offset = O; }
        [[nodiscard]] X64Operand *getAllocationRegister() const { return allocationRegister; }
        [[nodiscard]] X64Operand *getOffset() const { return Offset; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Deallocate : public X64Instruction {
        X64Operand *deAllocationRegister;
        X64Operand *Offset;

    public:
        X64Deallocate() = default;

        X64Deallocate(X64Operand *deAllocationRegister,
                      X64Operand *Offset) : deAllocationRegister(deAllocationRegister), Offset(Offset) {
        }

        void setDeAllocationRegister(X64Operand *Reg) { deAllocationRegister = Reg; }
        void setOffset(X64Operand *O) { Offset = O; }
        [[nodiscard]] X64Operand *getDeAllocationRegister() const { return deAllocationRegister; }
        [[nodiscard]] X64Operand *getOffset() const { return Offset; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Push : public X64Instruction {
        X64Operand *pushOp;

    public:
        X64Push() = default;

        X64Push(X64Operand *pushOp) : pushOp(pushOp) {
        }

        void setPushOp(X64Operand *Op) { pushOp = Op; }
        [[nodiscard]] X64Operand *getPushOp() const { return pushOp; }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Call : public X64Instruction {
        std::string functionName;

    public:
        X64Call() = default;

        X64Call(std::string functionName) : functionName(std::move(functionName)) {
        }

        void setFunctionName(const std::string &functionName) {
            this->functionName = functionName;
        }

        [[nodiscard]] StringRef getFunctionName() const {
            return functionName;
        }

        [[nodiscard]] std::string to_string() const override;
    };

    class X64Context {
        // Memory management - owns all operands and instructions
        std::vector<std::unique_ptr<X64Operand> > Operands;
        std::vector<std::unique_ptr<X64Instruction> > Instructions;

        std::unordered_map<std::string, X64Label *> existingLabels;

        // Register management
        std::unordered_map<unsigned, PseudoRegister *> PseudoRegs;
        std::unordered_map<PhysicalRegister::PhysReg, PhysicalRegister *> PhysRegs;
        std::unordered_map<unsigned, PhysicalRegister::PhysReg> RegAllocation;
        std::unordered_map<unsigned, X64Stack *> MemoryAlloc;

        // Stack management
        int StackOffset = 0;

    public:
        X64Context() = default;
        ~X64Context() = default;

        X64Context(const X64Context &) = delete;
        X64Context &operator=(const X64Context &) = delete;

        // Factory methods for operands
        PseudoRegister *getPseudoReg(unsigned ID, X64Type size = X64Type::LongWord);
        PhysicalRegister *getPhysReg(PhysicalRegister::PhysReg physReg,
                                     X64Type size = X64Type::QuadWord);
        X64Int *createInt(llvm::APSInt value);
        X64Stack *createStack(llvm::APSInt offset, X64Register *baseReg, X64Type size = X64Type::QuadWord);
        X64Data *createData(StringRef Name, X64Type size = X64Type::LongWord);

        // Register allocation methods
        void allocateReg(unsigned pseudoID, PhysicalRegister::PhysReg physReg);
        PhysicalRegister *getAllocatedReg(unsigned pseudoID);
        bool isAllocated(unsigned pseudoID) const;

        // Memory allocation methods for pseudo registers
        void allocateMemory(unsigned pseudoID, X64Type size = X64Type::QuadWord);
        void allocateMemory(unsigned pseudoID, X64Stack *stackSlot);
        X64Stack *getAllocatedMemory(unsigned pseudoID);
        bool isAllocatedToMemory(unsigned pseudoID) const;

        // Check allocation status
        enum AllocationType { None, Register, Memory };
        AllocationType getAllocationType(unsigned pseudoID) const;

        // Get the actual operand for a pseudo register (register or memory)
        X64Operand *getOperandForPseudo(unsigned pseudoID);

        // Stack management
        X64Stack *allocateStack(X64Type size = X64Type::QuadWord);

        // @brief Given an already generated stack access, generate another one
        // with any other size.
        X64Stack *getAllocatedStack(X64Stack *stack_access, X64Type size = X64Type::QuadWord);

        int getStackOffset() const { return StackOffset; }

        // Factory methods for instructions
        X64Label *getOrCreateLabel(StringRef Name);
        X64Cmp *createCmp(X64Operand *Left, X64Operand *Right,
                          X64Type type = X64Type::LongWord);
        X64Jmp *createJmp(X64Label *Label);
        X64JmpCC *createJCC(X64ConditionTypeE Condition, X64Label *Label);
        X64SetCC *createSetCC(X64ConditionTypeE Condition, X64Operand *Op);
        X64Mov *createMov(X64Operand *src, X64Operand *dst);
        X64Movsx *createMovsx(X64Operand *src, X64Operand *dst, X64Type TypeFrom, X64Type TypeTo);
        X64Unary *createUnary(X64Unary::X64UnaryKind kind, X64Operand *op,
                              X64Type type = X64Type::LongWord);
        X64Binary *createBinary(X64Binary::X64BinaryKind kind, X64Operand *Src, X64Operand *Dst,
                                X64Type type = X64Type::LongWord);
        X64IDiv *createIDiv(X64Operand *op, X64Type type = X64Type::LongWord);
        X64Cdq *createCdq(X64Type type = X64Type::LongWord);
        X64Ret *createRet();
        X64Allocate *createAllocation(X64Operand *Offset);
        X64Deallocate *createDeallocation(X64Operand *Offset);
        X64Push *createPush(X64Operand *Op);
        X64Call *createCall(const std::string &functionName);
    };

    class X64StaticVar {
        std::string Name;
        bool global;
        int64_t init_value;
        int alignment;
        X64Type type;

    public:
        explicit X64StaticVar(StringRef Name) : Name(Name), global(true), init_value(0) {
        }

        X64StaticVar(StringRef Name, bool global) : Name(Name), global(global), init_value(0) {
        }

        X64StaticVar(StringRef Name, bool global, int64_t init_value, X64Type type) : Name(Name), global(global),
                                                                        init_value(init_value), type(type) {
        }

        X64StaticVar(StringRef Name, bool global, int64_t init_value, int alignment) : Name(Name), global(global),
            init_value(init_value), alignment(alignment) {
        }

        [[nodiscard]] StringRef getName() const { return Name; }
        [[nodiscard]] bool isGlobal() const { return global; }
        [[nodiscard]] int64_t getInitValue() const { return init_value; }
        [[nodiscard]] int getAlignment() const { return alignment; }
        [[nodiscard]] X64Type getType() const { return type; }
    };

    class X64Function {
        X64Instructions Instrs;
        StringRef FuncName;
        bool global = true;
        std::unique_ptr<X64Context> Ctx;

    public:
        X64Function() : Ctx(std::make_unique<X64Context>()) {
        }

        explicit X64Function(StringRef Name) : FuncName(Name), Ctx(std::make_unique<X64Context>()) {
        }

        X64Function(StringRef Name, bool global) : FuncName(Name), global(global), Ctx(std::make_unique<X64Context>()) {
        }

        X64Function(X64Instructions &Instrs, StringRef Name) : Instrs(std::move(Instrs)), FuncName(Name), global(true),
                                                               Ctx(std::make_unique<X64Context>()) {
        }

        X64Function(X64Instructions &Instrs, StringRef Name, bool global) : Instrs(std::move(Instrs)), FuncName(Name),
                                                                            global(global),
                                                                            Ctx(std::make_unique<X64Context>()) {
        }

        X64Context &getContext() { return *Ctx; }

        [[nodiscard]] StringRef get_name() const { return FuncName; }
        [[nodiscard]] bool isGlobal() const { return global; }
        [[nodiscard]] size_t size() const { return Instrs.size(); }
        [[nodiscard]] bool empty() const { return Instrs.empty(); }

        void add_instruction(X64Instruction *I) { Instrs.push_back(I); }

        void add_instructions(X64Instruction *I, X64Instruction *I2) {
            Instrs.push_back(I);
            Instrs.push_back(I2);
        }

        template<typename... Ts>
        void add_instructions(X64Instruction *I, X64Instruction *I2, Ts... Is) {
            Instrs.push_back(I);
            add_instructions(I2, Is...);
        }

        X64Instructions &getInstructions() { return Instrs; }
        [[nodiscard]] const X64Instructions &getInstructions() const { return Instrs; }

        X64Instructions::iterator begin() { return Instrs.begin(); }
        X64Instructions::iterator end() { return Instrs.end(); }
        [[nodiscard]] X64Instructions::const_iterator begin() const { return Instrs.begin(); }
        [[nodiscard]] X64Instructions::const_iterator end() const { return Instrs.end(); }
    };

    class X64Program {
        X64Functions Funcs;
        X64StaticVars StaticVars;
        StringRef Name;

    public:
        X64Program() = default;

        explicit X64Program(StringRef Name) : Name(Name) {
        }

        X64Program(X64Functions &Funcs, StringRef Name) : Funcs(std::move(Funcs)), Name(Name) {
        }

        X64Program(X64Functions &Funcs, X64StaticVars &StaticVars, StringRef Name) : Funcs(std::move(Funcs)),
            StaticVars(std::move(StaticVars)), Name(Name) {
        }

        ~X64Program();

        [[nodiscard]] StringRef get_name() const { return Name; }
        [[nodiscard]] size_t size() const { return Funcs.size(); }
        [[nodiscard]] bool empty() const { return Funcs.empty(); }

        void add_function(X64Function *F) { Funcs.push_back(F); }
        void add_static_var(X64StaticVar *S) { StaticVars.push_back(S); }

        X64Functions::iterator begin() { return Funcs.begin(); }
        X64Functions::iterator end() { return Funcs.end(); }
        [[nodiscard]] X64Functions::const_iterator begin() const { return Funcs.begin(); }
        [[nodiscard]] X64Functions::const_iterator end() const { return Funcs.end(); }

        X64StaticVars::iterator staticVars_begin() { return StaticVars.begin(); }
        X64StaticVars::iterator staticVars_end() { return StaticVars.end(); }
        [[nodiscard]] X64StaticVars::const_iterator staticVars_begin() const { return StaticVars.begin(); }
        [[nodiscard]] X64StaticVars::const_iterator staticVars_end() const { return StaticVars.end(); }
    };
}
