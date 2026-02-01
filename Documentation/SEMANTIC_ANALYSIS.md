# Semantic Analysis: Scope, Linkage, and Storage Duration

This document describes the semantic analysis phase for variable and function declarations, focusing on scope management, linkage resolution, and storage duration as implemented in Chapter 10.

## Overview

The C language has three orthogonal concepts for declarations:

1. **Scope** - Where a name is visible in source code
2. **Linkage** - Whether declarations in different scopes refer to the same entity
3. **Storage Duration** - Lifetime of the object (static vs automatic)

These concepts interact based on where a declaration appears and what storage class specifier is used.

## Scope

Scope determines the visibility of identifiers. Our implementation uses a hierarchical scope chain.

### Scope Types

| Scope Type | Description |
|------------|-------------|
| File Scope | Declarations outside any function body |
| Block Scope | Declarations inside a function body or compound statement |
| Function Prototype Scope | Parameter names in function declarations (not definitions) |

### Implementation

The `Scope` class maintains:
- A `StringMap<SymbolEntry>` for symbol lookup
- A pointer to the parent scope
- A list of declared identifiers

```cpp
class Scope {
    Scope *parentScope;
    StringMap<SymbolEntry> Symbols;
    std::vector<std::string> DeclaredIdentifiers;
};
```

Name lookup traverses from the current scope upward through parent scopes:

```cpp
VarDeclaration *Scope::lookupForVar(StringRef Name) {
    Scope *S = this;
    while (S) {
        auto I = S->Symbols.find(Name);
        if (I != S->Symbols.end()) {
            if (std::holds_alternative<VarDeclaration *>(I->second.decl)) {
                return std::get<VarDeclaration *>(I->second.decl);
            }
        }
        S = S->getParentScope();
    }
    return nullptr;
}
```

## Linkage

Linkage determines whether multiple declarations of the same identifier refer to the same entity.

### Linkage Types

| Linkage | Description |
|---------|-------------|
| External | Identifier refers to the same entity across translation units |
| Internal | Identifier refers to the same entity within one translation unit |
| None | Each declaration creates a distinct entity |

### Linkage Rules

The linkage of an identifier depends on its scope and storage class:

| Scope | Storage Class | Linkage |
|-------|---------------|---------|
| File | (none) | External |
| File | `extern` | External |
| File | `static` | Internal |
| Block | (none) | None |
| Block | `extern` | External |
| Block | `static` | None (but static storage duration) |

### Implementation

Linkage is computed by `Sema::computeLinkage`:

```cpp
Linkage Sema::computeLinkage(std::optional<StorageClass> sc, ScopeType scope) {
    if (scope == ScopeType::Global) {
        if (sc == StorageClass::SC_Static)
            return Linkage::Static;  // internal linkage
        return Linkage::External;
    } else {
        if (sc == StorageClass::SC_Extern)
            return Linkage::External;
        return Linkage::None;
    }
}
```

## Symbol Attributes

Each symbol entry contains attributes that track semantic properties:

### Attribute Types

```cpp
struct FunAttr {
    bool defined;   // true if function body has been seen
    bool global;    // true = external linkage, false = internal
};

struct StaticAttr {
    InitialValue init;              // Tentative, Initial, or NoInitializer
    std::optional<int64_t> value;   // compile-time value if known
    bool global;                    // true = external, false = internal
};

struct LocalAttr {};  // automatic storage, no linkage
```

### InitialValue States

For variables with static storage duration:

| State | Meaning |
|-------|---------|
| `Initial` | Has explicit initializer with known value |
| `Tentative` | No initializer, will be zero-initialized |
| `NoInitializer` | `extern` without initializer, defined elsewhere |

## File-Scope Variable Declarations

File-scope variables are processed by `actOnGlobalVarDeclaration`.

### Algorithm

```
1. Determine initial_value:
   - If initializer is constant integer: Initial(value)
   - If no initializer and extern: NoInitializer
   - If no initializer and not extern: Tentative
   - If initializer is non-constant: ERROR

2. Determine linkage:
   - global = (storage_class != static)

3. Check for prior block-scope extern with external linkage:
   - If this is static and prior extern exists: CONFLICT

4. Check symbol table for existing declaration:
   - If function with same name: ERROR (redeclaration as different kind)
   - If variable with conflicting linkage: ERROR
   - If variable with conflicting definition: ERROR
   - Otherwise: merge attributes

5. Add/update symbol table entry
```

### Merging Declarations

When multiple declarations of the same file-scope variable exist:

```cpp
if (storageClass == StorageClass::SC_Extern) {
    // Inherit linkage from previous declaration
    global = oldAttrs->global;
} else if (oldAttrs->global != global) {
    // static follows non-static or vice versa
    fail("Conflicting variable linkage");
}

// Merge initialization
if (oldAttrs->init == InitialValue::Initial) {
    if (newInit == InitialValue::Initial) {
        fail("Multiple definitions");
    }
    // Keep old initializer
} else if (oldAttrs->init == InitialValue::Tentative) {
    if (newInit == InitialValue::Initial) {
        // New definition takes precedence
    }
    // Both tentative: remains tentative
}
```

## Block-Scope Variable Declarations

Block-scope variables are processed by `actOnVarDeclaration`.

### Storage Class Semantics

**extern at block scope:**
- No initializer allowed
- References file-scope variable with same name (if exists)
- Otherwise creates new identifier with external linkage
- Name is only visible within the block

**static at block scope:**
- Must have constant initializer (or defaults to 0)
- Has static storage duration (persists across calls)
- Has no linkage (distinct from any file-scope static)

**No storage class:**
- Automatic storage duration
- No linkage
- Initialized each time block is entered

### Block-Scope Extern and Linkage Tracking

A block-scope `extern` declaration can establish external linkage for an identifier even when no file-scope declaration exists:

```c
int main(void) {
    {
        extern int x;  // x now has external linkage
    }
    return 0;
}
static int x = 10;  // ERROR: conflicts with external linkage above
```

This is tracked separately from scope visibility using `BlockScopeExternLinkage`:

```cpp
std::set<std::string> BlockScopeExternLinkage;

// When processing block-scope extern with no prior declaration:
if (globalEntry == nullptr) {
    BlockScopeExternLinkage.insert(Name.str());
}

// When processing file-scope static:
if (!global && BlockScopeExternLinkage.contains(Name.str())) {
    fail("Conflicting linkage");
}
```

## Function Declarations

Functions are processed by `actOnFunctionDeclaration`.

### Key Rules

1. Functions always have static storage duration
2. File-scope functions have external linkage by default
3. `static` gives internal linkage
4. Block-scope function declarations cannot be `static`
5. Function declarations must have consistent parameter counts
6. `static` cannot follow a non-static declaration

### Conflict Detection

```cpp
// Block-scope static function is illegal
if (isBlockScope && storageClass == StorageClass::SC_Static) {
    fail("static function at block scope");
}

// Check for variable/function name conflict
if (globalScope->lookupForVar(Name) != nullptr) {
    fail("variable redeclared as function");
}

// Check linkage consistency
if (existing && oldIsGlobal && newIsStatic) {
    fail("static follows non-static");
}
```

## Declaration Processing Flow

### Variable Declaration (Block Scope)

```
actOnVarDeclaration(name, storageClass)
    |
    +-- Check current scope for conflicts
    |
    +-- Create VarDeclaration
    |
    +-- If extern:
    |       +-- Check current scope for local/static conflict
    |       +-- Look up in global scope
    |       +-- If not found: add to BlockScopeExternLinkage
    |       +-- Add to current scope (visibility only)
    |
    +-- If static:
    |       +-- Add to current scope with StaticAttr
    |
    +-- Otherwise:
            +-- Add to current scope with LocalAttr

actOnVarDeclarationInit(decl, initExpr)
    |
    +-- If extern: no initializer allowed
    |
    +-- If static: must be constant expression
    |
    +-- Set expression on declaration
```

### Variable Declaration (File Scope)

```
actOnGlobalVarDeclaration(name, initExpr, storageClass)
    |
    +-- Validate initializer (must be constant)
    |
    +-- Determine linkage (global flag)
    |
    +-- Check BlockScopeExternLinkage for conflicts
    |
    +-- Look up existing declaration
    |       |
    |       +-- If function: ERROR
    |       +-- If variable: merge attributes
    |
    +-- Add/update symbol table
```

## Error Conditions

| Error | Condition |
|-------|-----------|
| Non-constant initializer | File-scope or static local with non-constant init |
| Conflicting linkage | static follows extern or vice versa |
| Multiple definitions | Two initializers for same file-scope variable |
| Function redeclared as variable | Name previously declared as function |
| Variable redeclared as function | Name previously declared as variable |
| Extern with initializer | Block-scope extern has initializer |
| Static block-scope function | Function declaration with static at block scope |

## Future Improvements

The current implementation mixes scope and symbol table concerns in the `Scope` class. A cleaner architecture would separate:

1. **Scope Chain** - Pure visibility/name lookup
2. **Symbol Table** - Global registry of all identifiers with linkage and their attributes

This would eliminate workarounds like `BlockScopeExternLinkage` and simplify the code for handling complex linkage scenarios.