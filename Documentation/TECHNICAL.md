# Technical Documentation

## Compiler Architecture Overview

MyCCompiler implements a traditional multi-pass compiler architecture based on
the book "Writing a C Compiler" by Nora Sandler. The pipeline transforms C
source code through several intermediate representations before producing either
x86-64 assembly or LLVM IR.

```
Source (.c)
    │
    ▼
┌──────────┐
│  Lexer   │  Tokenises characters into a stream of Token objects.
└──────────┘
    │  token stream
    ▼
┌──────────┐
│  Parser  │  Recursive-descent + Pratt precedence. Calls Sema action functions.
└──────────┘
    │  AST (Program / FunctionDeclaration / Statement / Expr)
    ▼
┌──────────┐
│   Sema   │  Validates semantics, resolves names, assigns types, generates labels.
└──────────┘
    │  annotated AST + GlobalSymbolTable
    ▼
┌──────────────┐     ┌────────────────┐
│  IRGenerator │     │ LLVMIRGenerator│  (alternative backend, --llvm flag)
└──────────────┘     └────────────────┘
    │  SimpleIR (ir::Program)
    ▼
┌──────────────┐
│ X64CodeGen   │  Three-phase: translate → register alloc → fixup → emit
└──────────────┘
    │  Intel-syntax assembly (.s)
```

---

## Frontend Components

### Lexical Analyzer (`include/mycc/Lexer/`, `lib/Lexer/`)

The `Lexer` converts raw source bytes into a flat sequence of `Token` objects.

- Source locations use `llvm::SMLoc` and are attached to every token.
- String content is handled via `llvm::StringRef` (zero-copy view into the
  source buffer).
- Token categories are defined in `Basic/TokenKinds.def` using an X-macro,
  which both generates the `TokenKind` enum and the textual name lookup.
- Recognised token classes: identifiers, keywords, integer literals (decimal,
  octal, hex, with optional `L`/`l` suffix), all C operators, and delimiters.

### Parser (`include/mycc/Parser/`, `lib/Parser/`)

The parser implements recursive descent with Pratt-style precedence climbing
for expressions. It does not build the AST directly — instead it calls *action
methods* on the `Sema` object, which validates and constructs each node.

Supported grammar productions:

**Declarations**
- Variable declarations with optional initialiser and optional storage class
  (`static`, `extern`)
- Function declarations and definitions with typed parameters
- File-scope (global) and block-scope declarations

**Expressions** (in precedence order, lowest to highest)
- Ternary conditional (`?:`)
- Logical or/and (`||`, `&&`)
- Bitwise or/xor/and (`|`, `^`, `&`)
- Equality/relational (`==`, `!=`, `<`, `<=`, `>`, `>=`)
- Shifts (`<<`, `>>`)
- Additive / multiplicative (`+`, `-`, `*`, `/`, `%`)
- Unary (`-`, `~`, `!`)
- Prefix increment/decrement (`++x`, `--x`)
- Postfix increment/decrement (`x++`, `x--`)
- Explicit cast (`(type) expr`)
- Function call (`f(args...)`)
- Primary (integer literal, identifier, parenthesised expression)

**Statements**
- Return, expression, null (`;`)
- Compound (`{ ... }`)
- If / if-else
- While, do-while, for (init may be a declaration or expression)
- Break, continue
- Goto, labeled statement
- Switch with case and default labels

### Abstract Syntax Tree (`include/mycc/AST/`)

All AST node classes are declared in `AST.hpp`. Every node family uses an
integer discriminator for LLVM-style RTTI (`classof` / `llvm::cast<>`).

#### Type Hierarchy

```
Type  (TK_Builtin, TK_Pointer, TK_Function)
├── BuiltinType   (Bool, Char, Short, Int, Long, Void)
│     · integerRank() returns conversion rank for implicit promotion
│     · isIntegerType() / isVoid() convenience predicates
├── PointerType   (TK_Pointer — declared but not yet implemented)
└── FunctionType  (return type + param types, equality, to_string caching)
```

#### Expression Hierarchy

```
Expr  (ExprKind enum)
├── IntegerLiteral   (APSInt, 32-bit)
├── LongLiteral      (APSInt, 64-bit)
├── IntInit          (compile-time int32 for static initialisers)
├── LongInit         (compile-time int64 for static initialisers)
├── Var              (identifier reference)
├── UnaryOperator    (Complement, Negate, Not)
├── BinaryOperator   (arithmetic, bitwise, relational, logical)
├── AssignmentOperator
├── PrefixOperator   (PreIncrement, PreDecrement)
├── PostfixOperator  (PostIncrement, PostDecrement)
├── ConditionalExpr  (ternary)
├── FunctionCallExpr
└── CastExpr
```

`IntInit` and `LongInit` are special nodes that carry a compile-time constant
already sized to the target type. They are created by `Sema::actOnStaticInit`
for static variable initialisers so that IRGen can emit them as constants
without re-evaluating.

#### Statement Hierarchy

```
Statement  (StmtKind enum)
├── ReturnStatement
├── ExpressionStatement
├── NullStatement
├── CompoundStatement   (holds BlockItems — variant of Stmt/VarDecl/FuncDecl)
├── IfStatement         (condition + then + optional else)
├── WhileStatement      (condition + body + compiler-assigned label)
├── DoWhileStatement    (body + condition + label)
├── ForStatement        (ForInit variant + optional condition + optional post + body + label)
├── BreakStatement      (compiler-assigned label set by Sema)
├── ContinueStatement   (compiler-assigned label set by Sema)
├── LabelStatement      (user label for goto)
├── GotoStatement
├── CaseStatement       (value + compiler-assigned label)
├── DefaultStatement    (compiler-assigned label)
└── SwitchStatement     (condition + body + break label)
```

#### Declaration Nodes

```
VarDeclaration
  · Var*                 — name (original source name, not renamed)
  · Expr*                — optional initialiser
  · optional<StorageClass>
  · Type*
  · optional<string>     — unique global name for static locals ("func.var")

FunctionDeclaration
  · StringRef Name
  · ArgsList             — vector of Var*
  · BlockItems body      — empty if declaration-only
  · FunctionType*
  · optional<StorageClass>
  · bool IsDefinition

Program
  · DeclarationList      — variant of FunctionDeclaration* / VarDeclaration*
```

#### Memory Management — `ASTContext`

`ASTContext` owns all AST nodes via `std::deque<std::unique_ptr<T>>` (one
deque per node category). `std::deque` guarantees pointer stability: addresses
never move on append, so raw pointers stored elsewhere remain valid.

Factory methods (`createStatement<T>`, `createExpression<T>`, etc.) follow a
uniform pattern:

```cpp
template<typename T, typename... Args>
T* createExpression(Args&&... args) {
    auto node = std::make_unique<T>(std::forward<Args>(args)...);
    T* ptr = node.get();
    Expressions.emplace_back(std::move(node));
    return ptr;
}
```

Canonical built-in types (`Int`, `Long`, `Void`) are pre-created and returned
by `getIntTy()`, `getLongTy()`, `getVoidTy()` to avoid duplicates.

---

## Semantic Analysis (`include/mycc/Sema/`)

### Overview

The `Sema` class is the semantic analysis engine. The parser calls `actOn*`
methods instead of constructing nodes directly, so `Sema` intercepts every
declaration and expression.

Major responsibilities:
- Name resolution and uniqueness checking
- Linkage computation for `static` / `extern` declarations
- Type annotation on every `Expr` node
- Compiler-assigned label generation for loops, switch, break/continue
- Goto label validation (all targets must be defined in the same function)

### Scope and Symbol Table

The `Scope` class is a linked list of symbol tables:

```
CurrentScope  →  parent  →  ...  →  file-scope  →  nullptr
```

Each `Scope` holds a `StringMap<SymbolEntry>`. `SymbolEntry` stores:

```
SymbolEntry {
    variant<FunctionDeclaration*, VarDeclaration*>  decl;
    variant<FunAttr, StaticAttr, LocalAttr>         attrs;
}
```

Type-safe lookup methods (`lookupForVar`, `lookupForFunction`,
`lookupEntry`) traverse the chain and check `std::holds_alternative` before
extracting a value, so variables and functions with the same name in the same
scope chain are correctly distinguished.

### Global Symbol Table

`Sema` maintains two separate symbol-table roots:

| Table | Purpose |
|-------|---------|
| `CurrentScope` chain | Scope-aware name visibility during parsing |
| `GlobalSymbolTable` | Flat table of all names that IRGen needs to emit globally |

The `GlobalSymbolTable` collects:
- All file-scope functions and variables (under their original names)
- All block-scope `static` variables (under the unique name `functionName.varName`)

IRGen receives a const reference to `GlobalSymbolTable` and uses it to:
1. Emit `ir::StaticVariable` entries for all static-duration symbols.
2. Distinguish local stack variables from static globals when lowering `Var`
   expressions.

### Linkage Computation

```
Linkage computeLinkage(optional<StorageClass> sc, ScopeType scope):

  File scope:
    SC_Static  → Linkage::Static   (internal linkage)
    otherwise  → Linkage::External

  Block scope:
    SC_Extern  → Linkage::External
    otherwise  → Linkage::None
```

### Identifier Attributes

| Attribute | Used For |
|-----------|---------|
| `FunAttr{defined, global}` | Function declarations |
| `StaticAttr{init, value, global}` | Global vars, static locals |
| `LocalAttr{}` | Automatic local variables |

`InitialValue` tracks three states for static-duration variables:
- `Tentative` — no initialiser, will be zero-initialised
- `Initial` — explicit constant initialiser
- `NoInitializer` — `extern` declaration; defined in another translation unit

### Sub-Analyses

Semantic analysis is decomposed into focused passes within `Sema/Analyses/`:

| Class | Responsibility |
|-------|---------------|
| `LabelGenerator` | Generates unique compiler labels for loops, cases, switch breaks |
| `LoopLabelAssigner` | Walks `FunctionDeclaration` and attaches compiler labels to loops |
| `GotoLabelValidator` | Tracks user labels and goto targets; reports undeclared targets |
| `TypeExpressionInference` | Walks expression trees and annotates each `Expr` with its type |
| `BreakableContext` | Tracks whether break/continue are inside a loop or switch |

`EnterDeclScope` is an RAII helper that calls `enterScope()` on construction
and `exitScope()` on destruction, ensuring correct cleanup in all parser paths.

### Type System and Implicit Conversions

`TypeExpressionInference` applies integer promotions and usual arithmetic
conversions according to the C standard:

- Unary operators on integer types retain the operand's type.
- Binary operators promote operands to the common type (higher rank wins).
- Assignments coerce the RHS to the LHS type, inserting `CastExpr` nodes.
- Explicit casts (`(type)expr`) are represented as `CastExpr`.

Coercion is performed by `Sema::coerce(Loc, expr, targetType)`, which wraps
the expression in a `CastExpr` if the types differ.

---

## Intermediate Representation (`include/mycc/IR/SimpleIR.hpp`)

SimpleIR is a flat, register-based IR with explicit control-flow labels. It
lives entirely in a single header.

### Type System

```
ir::Type  (TK_Int, TK_Void, TK_Function)
├── IntType   (bit width: 8, 16, 32, 64)
├── VoidType
└── FunctionType  (return type + param types)
```

Signedness is NOT encoded in the type (matching LLVM's convention). Signed vs.
unsigned is expressed in the instruction (`sext` vs `zext`, signed vs
unsigned div/rem — not yet fully implemented).

### Value Hierarchy

```
Value
├── Instruction
│   ├── Copy          (dst = src — named variable copy)
│   ├── Mov           (generic move)
│   ├── Ret           (void or value return)
│   ├── Jump          (unconditional branch)
│   ├── JumpIfZero    (conditional branch on zero)
│   ├── JumpIfNotZero (conditional branch on non-zero)
│   ├── Label         (branch target)
│   ├── UnaryOp       (Neg, Complement, Not)
│   ├── BinaryOp      (Add, Sub, Mul, Div, Rem, And, Or, Xor, Sal, Sar)
│   ├── ICmpOp        (lt, le, gt, ge, eq, neq)
│   ├── Invoke        (function call, optional result register)
│   ├── SignExtend    (sext i32 to i64)
│   └── Truncate      (trunc i64 to i32)
└── Operand
    ├── Constant      (int64_t with associated IntType)
    ├── Reg           (anonymous temporary, unique numeric ID)
    ├── VarOp         (named local variable)
    ├── StaticVarOp   (named static/global variable)
    └── ParameterOp   (named function parameter)
```

### Context and Memory Ownership

`ir::Context` owns all IR values via `std::deque<std::unique_ptr<Value>>`.
It also interns types and constants:
- `IntType` instances are interned by bit-width (one per width).
- `Constant` values are interned by `int32_t` key (for ≤32-bit) or `int64_t`
  key (for 64-bit), avoiding duplicate constant nodes.
- Registers receive monotonically increasing IDs from `NextRegID`.

Factory methods (`createReg`, `createCopy`, `createBinaryOp`, etc.) allocate,
store, and return a raw pointer. Callers do not own the returned pointer.

---

## IR Generation (`include/mycc/CodeGen/IRGen.hpp`)

`IRGenerator` lowers the annotated AST to SimpleIR.

### Variable Renaming (SSA-style)

At IR-generation time, each local variable declaration receives a unique name:

```
original name: "foo"   →   IR name: "foo_0", "foo_1", ...
```

A `VariableRenameStack` (a `StringMap<vector<string>>`) tracks the current
IR name for each original name. On scope entry, new declarations push a new
name. On scope exit, they pop it, restoring the visible name for any
outer declaration with the same original name.

This correctly handles shadowing:

```c
int x = 1;         // IR: "x_0"
{
    int x = 2;     // IR: "x_1"  — shadows x_0
}
// Back to "x_0" here
```

Static variables and extern references skip the rename stack and use their
global names directly.

### Static Variable Emission

`convertSymbolsToTacky(symbols)` iterates the `GlobalSymbolTable`:
- For each entry with `StaticAttr` and `init != NoInitializer`, emits an
  `ir::StaticVariable` with the appropriate initial value (zero for `Tentative`).
- `NoInitializer` entries (extern declarations) are skipped.

### Expression Lowering

Each expression type has a dedicated generation method:

| AST node | IR output |
|----------|-----------|
| `IntegerLiteral` / `LongLiteral` | `Constant` |
| `IntInit` / `LongInit` | `Constant` (compile-time static init) |
| `Var` | `VarOp` (local) or `StaticVarOp` (static/global) |
| `UnaryOperator` | `UnaryOp` instruction |
| `BinaryOperator` | `BinaryOp` or `ICmpOp`; logical `&&`/`||` use short-circuit jumps |
| `AssignmentOperator` | `Copy` instruction; result is the stored value |
| `PrefixOperator` | Read → `BinaryOp` (add/sub 1) → `Copy` → return new value |
| `PostfixOperator` | Read → save old value → `BinaryOp` → `Copy` → return old value |
| `ConditionalExpr` | `JumpIfZero` / `Jump` + label structure |
| `FunctionCallExpr` | `Invoke` instruction |
| `CastExpr` | `SignExtend` or `Truncate` depending on direction |

---

## LLVM IR Backend (`include/mycc/CodeGen/llvm/LLVMIRGen.hpp`)

`LLVMIRGenerator` is an alternative backend that lowers the AST directly to
LLVM IR, bypassing SimpleIR entirely. Activated by the `--llvm` flag.

It uses `llvm::IRBuilder<>` to emit instructions into an `llvm::Module`.

Key implementation details:
- Local variables are allocated with `alloca` in the function entry block
  (classic SSA construction without phi nodes).
- Global variables are emitted as `llvm::GlobalVariable` with appropriate
  linkage (`ExternalLinkage` or `InternalLinkage`).
- Short-circuit evaluation for `&&` and `||` is handled by dedicated helper
  methods (`generateLogicalAnd`, `generateLogicalOr`) that emit conditional
  branches into separate basic blocks.
- `BreakBlocks` and `ContinueBlocks` are stacks of `llvm::BasicBlock*`
  maintained as loops are entered and exited (LIFO, supporting nested loops).
- `LabelBlocks` maps user label names to `llvm::BasicBlock*` for goto support.
  (**Note:** goto/label generation is not yet connected in the LLVM backend —
  see `ToDo/IMPROVEMENTS.md` §12.)

The generated module is verified with `llvm::verifyModule` and can be printed
as textual LLVM IR or compiled further with the LLVM toolchain.

---

## x86-64 Backend (`include/mycc/CodeGen/x64/`)

### X64AST — Target-Specific IR Layer

Before emitting text assembly, the code generator builds an `X64AST` — a
model of x86-64 constructs that is still independent of text formatting.

**Operands:**
- `X64Int` — immediate integer value
- `PseudoRegister` — virtual register (resolved to stack slot)
- `PhysicalRegister` — actual hardware register (`%rax`, `%rcx`, etc.) with
  explicit size (BYTE / DWORD / QWORD)
- `X64Stack` — RBP-relative memory reference (`-8(%rbp)`)

**Instructions:**
- `X64Mov`, `X64Binary` (two-operand arithmetic), `X64Unary`
- `X64Cmp`, `X64JmpCC` (conditional jump on flag)
- `X64IDiv` (uses implicit RAX/RDX)
- `X64Label`, `X64Ret`, `X64Call`

### Code Generation Phases

#### Phase 1: SimpleIR → X64AST

IR instructions are translated one-to-one where possible. Key mappings:
- `ir::Constant` → `X64Int`
- `ir::Reg` and `ir::VarOp` → `PseudoRegister` (by name hash)
- `ir::StaticVarOp` → RIP-relative memory reference
- `ir::BinaryOp` → `X64Binary` with appropriate opcode
- `ir::ICmpOp` → `X64Cmp` + `X64JmpCC`

#### Phase 2: Pseudo-Register Allocation

Every `PseudoRegister` is assigned a unique stack slot. Slots are allocated
downward from `%rbp` in 8-byte units. The total frame size is computed and
used in the function prologue (`sub $N, %rsp`).

This is a simple *spill-everything* strategy — no physical register assignment
beyond what the instruction fixup phase requires.

#### Phase 3: Instruction Fixup

x86-64 architectural constraints are enforced:
- **Memory-to-memory moves:** insert an intermediate `%rax` move.
- **`idiv`:** divisor must be in a register; quotient in `%rax`, remainder in
  `%rdx` (RAX sign-extended to RDX:RAX via `cdq`/`cqo`).
- **Shift amount:** must be in `%cl`; emit a move to `%cl` before the shift.
- **`imul` with memory destination:** not allowed on x86-64; insert a
  temporary register.

#### Phase 4: Assembly Emission

Final Intel-syntax assembly is written to a `.s` file:
- Function prologue: `push %rbp; mov %rsp, %rbp; sub $N, %rsp`
- Function epilogue: `mov %rbp, %rsp; pop %rbp; ret`
- Platform directives: `.globl` (Linux), `.globl` + `.text` (macOS)
- Labels: function-local labels use `_` prefix convention where required

---

## Diagnostic Infrastructure (`include/mycc/Basic/Diagnostic.hpp`)

`DiagnosticsEngine` wraps LLVM's `SourceMgr` to attach source locations to
error messages.

Diagnostic identifiers and message strings are defined in `Diagnostic.def`
using an X-macro:

```
DIAG(err_undeclared_var, Error, "use of undeclared variable '{}'")
```

The `report` method uses `{fmt}` to format the message string:

```cpp
template <typename... Args>
void report(SMLoc Loc, unsigned DiagID, Args&&... Arguments) {
    std::string Msg = fmt::vformat(getDiagnosticText(DiagID),
                                   fmt::make_format_args(Arguments...));
    SrcMgr.PrintMessage(Loc, getDiagnosticKind(DiagID), Msg);
    NumErrors += (Kind == SourceMgr::DK_Error);
}
```

This provides precise source-location error messages pointing into the original
`.c` file.

---

## Build System

CMake is used with per-component static libraries:

| Library | Contents |
|---------|---------|
| `libBasic.a` | TokenKinds, Diagnostic |
| `libLexer.a` | Lexer, Token |
| `libParser.a` | Parser |
| `libAST.a` | ASTPrinter |
| `libSema.a` | Sema, Scope, analyses |
| `libCodeGen.a` | IRGen, X64CodeGen, LLVMIRGen |

The final `mycc` executable links all libraries and `main.cpp`.

LLVM is found via `find_package(LLVM)` and components are linked selectively
(`LLVMCore`, `LLVMSupport`, `LLVMIRReader`, etc.).

---

## Command-Line Interface

```
mycc [options] <input.c>

Options:
  --lex       Run lexer only (no output file)
  --parse     Run through parser (no output file)
  --validate  Run through semantic analysis (no output file)
  --tacky     Generate SimpleIR (no output file unless --print)
  --codegen   Generate assembly without assembling to object file
  --llvm      Use the LLVM backend; produces <input>.ll
  --print     Print output to stdout instead of writing a file
  (no flag)   Full compilation; produces <input>.s
```

---

## Implementation Patterns

### LLVM-Style RTTI

All AST and IR class hierarchies use discriminated enums and `classof`:

```cpp
static bool classof(const Expr *E) {
    return E->getKind() == Ek_UnaryOperator;
}
```

This allows `llvm::cast<>`, `llvm::dyn_cast<>`, and `llvm::isa<>` throughout
the codebase without paying the overhead of `dynamic_cast`.

### X-Macro `.def` Files

`TokenKinds.def` and `Diagnostic.def` use the X-macro pattern. Including the
`.def` file with a different `#define` of the macro generates different code
from the same source of truth, keeping token names and diagnostic messages DRY.

### RAII Scope Guards

`EnterDeclScope` enters a new lexical scope on construction and exits it on
destruction, ensuring `exitScope()` is always called even when early returns or
error paths are taken.

### Arena Allocation via `std::deque`

Both `ASTContext` and `ir::Context` use `std::deque<std::unique_ptr<T>>` as
allocation arenas. `std::deque` is chosen over `std::vector` specifically
because it does not invalidate existing pointers on append — critical since raw
pointers to nodes are stored extensively throughout the compiler.
