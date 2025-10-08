# Technical Documentation

## Compiler Architecture Overview

MyCCompiler implements a traditional multi-pass compiler architecture with four distinct phases: lexical analysis, syntactic analysis, semantic analysis, and code generation. The design emphasizes modularity and maintainability through clear separation of concerns between compilation phases.

## Frontend Components

### Lexical Analyzer

The lexical analyzer (`Lexer`) transforms raw source text into a sequence of tokens. The implementation utilizes LLVM's `StringRef` for efficient string handling and `SMLoc` for precise source location tracking. Token categories include:

- **Identifiers**: Variable and function names
- **Keywords**: C language reserved words (`int`, `return`, `if`, etc.)
- **Operators**: Arithmetic, logical, and assignment operators
- **Literals**: Integer constants
- **Delimiters**: Parentheses, braces, semicolons

### Parser

The parser implements a recursive descent parsing strategy to construct an Abstract Syntax Tree (AST) from the token stream. The grammar supports:

- **Declarations**: Variable declarations with optional initialization
- **Expressions**: Binary operations, unary operations, assignments, and variable references
- **Statements**: Expression statements, return statements, and null statements
- **Functions**: Function definitions with statement blocks

### Abstract Syntax Tree

The AST implementation provides a hierarchical representation of the program structure with the following node types:

#### Expression Nodes
- `IntegerLiteral`: Represents integer constants using LLVM's `APSInt` for arbitrary precision
- `Var`: Variable references containing identifier names
- `UnaryOperator`: Unary operations (negation, complement, logical not)
- `BinaryOperator`: Binary operations including arithmetic, comparison, and logical operations
- `AssignmentOperator`: Assignment expressions with left-hand and right-hand operands

#### Statement Nodes
- `ReturnStatement`: Function return statements with optional return values
- `ExpressionStatement`: Expression evaluations
- `NullStatement`: Empty statements

#### Declaration Nodes
- `Declaration`: Variable declarations with optional initializer expressions

#### Program Structure
- `Function`: Function definitions containing statement sequences
- `Program`: Top-level container for function definitions

### Semantic Analysis

The semantic analyzer performs scope resolution and type checking. The `Scope` class maintains symbol tables for variable declarations, while the `Sema` component validates semantic correctness including:

- Variable declaration and usage validation
- Scope-based name resolution
- Type compatibility checking

## Intermediate Representation

### SimpleIR Design

The compiler employs a custom intermediate representation (SimpleIR) that abstracts away source-level constructs while maintaining semantic information necessary for code generation. The IR operates on a value-based system with the following components:

#### Value Hierarchy
- **`Value`**: Base class for all IR values with string representation capability
- **`Operand`**: Base class for values that can be used as instruction operands
- **`Instruction`**: Base class for computational operations, inheriting from `Value`

#### Operand Types
- **`Int`**: Integer constants represented using LLVM's `APSInt`
- **`Reg`**: Temporary registers with unique numeric identifiers
- **`VarOp`**: Named variables for user-declared identifiers

#### Instruction Types

**Data Movement**
- `Copy`: Copies values between operands (variables and temporaries)
- `Mov`: Generic move operations

**Arithmetic Operations**
- `UnaryOp`: Unary operations (negation, bitwise complement, logical not)
- `BinaryOp`: Binary arithmetic (add, subtract, multiply, divide, modulo)
- `ICmpOp`: Integer comparison operations (less than, equal, etc.)

**Control Flow**
- `Label`: Jump targets for control flow
- `Jump`: Unconditional jumps to labels
- `JumpIfZero`: Conditional jumps when operand equals zero
- `JumpIfNotZero`: Conditional jumps when operand is non-zero
- `Ret`: Function return with optional return value

#### Context Management

The `Context` class manages IR value creation and lifetime through factory methods, ensuring proper memory management and value uniqueness. It maintains:

- **Variable registry**: Maps variable names to `VarOp` instances
- **Constant pool**: Deduplicates integer constants
- **Register allocation**: Assigns unique identifiers to temporary registers
- **Memory management**: Automatic cleanup of all created values

### IR Generation Process

The `IRGenerator` transforms AST nodes into SimpleIR through visitor-pattern traversal:

1. **Expression Translation**: Converts AST expressions into IR values, creating temporary registers for intermediate results
2. **Statement Processing**: Transforms control flow constructs into appropriate IR instruction sequences
3. **Short-Circuit Evaluation**: Implements lazy evaluation for logical operators using conditional jumps

## Backend: x86-64 Code Generation

### X64AST Intermediate Layer

The x86-64 backend introduces an intermediate representation (X64AST) that models target-specific constructs while maintaining abstraction from actual assembly syntax.

#### Operand Modeling
- **`X64Int`**: Immediate integer values
- **`PseudoRegister`**: Virtual registers with numeric identifiers
- **`PhysicalRegister`**: Actual x86-64 registers with size specifications
- **`X64Stack`**: Stack memory locations with offset calculations

#### Instruction Modeling
- **`X64Mov`**: Data movement between operands
- **`X64Binary`**: Two-operand arithmetic operations
- **`X64Unary`**: Single-operand operations
- **`X64Cmp`**: Comparison operations setting flags
- **`X64JmpCC`**: Conditional jumps based on flag states
- **`X64IDiv`**: Division operations with implicit register usage

### Code Generation Pipeline

The x86-64 code generator implements a multi-phase approach:

#### Phase 1: IR to X64AST Translation
Converts SimpleIR instructions to X64AST nodes while performing operand conversion:
- Integer constants map directly to `X64Int`
- IR registers become `PseudoRegister` instances
- Variables (VarOp) convert to pseudo registers using name hashing for consistency

#### Phase 2: Register Allocation
Implements a simple stack-based allocation strategy:
- All pseudo registers receive stack slot allocations
- Stack slots are allocated with appropriate size (DWORD for integers)
- Memory layout uses RBP-relative addressing with negative offsets

#### Phase 3: Instruction Fixup
Addresses x86-64 architectural constraints:
- **Memory-to-memory operations**: Inserts intermediate register moves
- **Division operations**: Ensures proper RAX/RDX register usage
- **Shift operations**: Enforces CL register requirement for variable shifts
- **Multiplication constraints**: Handles memory operand restrictions

#### Phase 4: Assembly Generation
Produces Intel syntax assembly with proper formatting:
- Function prologue/epilogue generation
- Label resolution and formatting
- Platform-specific directive handling (Linux/macOS compatibility)

### Variable Handling Strategy

Variables undergo a carefully designed transformation pipeline:

1. **AST Level**: Variables represented as `Var` nodes with string names
2. **IR Level**: Converted to `VarOp` operands maintaining name information
3. **X64 Level**: Transformed to pseudo registers using deterministic name hashing
4. **Final Assembly**: Allocated to stack memory locations

This approach ensures:
- **Consistency**: Same variable names map to identical storage locations
- **Scope Respect**: Variable lifetime matches C semantics
- **Memory Safety**: Stack allocation provides automatic cleanup

### Optimization Considerations

The current implementation prioritizes correctness over optimization, implementing:
- **Naive register allocation**: All values reside in memory
- **Conservative instruction selection**: Explicit intermediate moves for safety
- **Minimal peephole optimization**: Basic instruction combining only

Future optimization opportunities include:
- Register pressure analysis for physical register allocation
- Dead code elimination in IR
- Constant folding and propagation
- Common subexpression elimination

## Implementation Patterns

### Memory Management
The compiler employs RAII principles with smart pointers for automatic memory management. Each compilation phase owns its output data structures, ensuring clear ownership semantics.

### Error Handling
Diagnostic reporting utilizes LLVM's diagnostic infrastructure for consistent error messaging with source location information.

### Extensibility
The modular design facilitates feature additions through:
- AST node hierarchy extension
- IR instruction set expansion
- Target architecture abstraction

This architecture provides a solid foundation for implementing additional C language features while maintaining code clarity and correctness.