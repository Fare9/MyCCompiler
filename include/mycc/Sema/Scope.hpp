#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "mycc/AST/AST.hpp"
#include "llvm/ADT/StringMap.h"
#include <vector>
#include <string>
#include <variant>

namespace mycc {
    class VarDeclaration;
    class FunctionDeclaration;

    using symbol_t = std::variant<FunctionDeclaration *, VarDeclaration *>;

    /// Linkage determines whether an identifier refers to the same entity
    /// across different scopes or translation units.
    ///
    /// In C, linkage is determined by:
    /// - Where the declaration appears (file scope vs block scope)
    /// - What storage-class specifier is used (static, extern, or none)
    ///
    /// Examples:
    ///   int x;              // file scope, no specifier -> External
    ///   static int y;       // file scope, static -> Internal (we call it Static)
    ///   extern int z;       // file scope, extern -> External
    ///   void f() { int a; } // block scope, no specifier -> None
    ///   void f() { extern int b; } // block scope, extern -> External
    enum class Linkage {
        External,   // Visible across translation units (e.g., global functions/vars)
        Static,     // Visible only within this translation unit (file-scope static)
        None        // No linkage - local variables with automatic storage
    };

    /// ScopeType indicates where a declaration appears in the source code.
    ///
    /// This affects:
    /// - Default linkage (Global declarations have External linkage by default)
    /// - Storage duration (Global -> static duration, Local -> automatic duration)
    /// - Which storage-class specifiers are valid
    ///
    /// Note: "file scope" in C terminology corresponds to Global here.
    enum class ScopeType {
        Global,     // File scope - outside any function body
        Local,      // Block scope - inside a function body (includes parameters)
    };

    /// InitialValue tracks the initialization state of variables with static
    /// storage duration. This matters for code generation and linkage resolution.
    ///
    /// C has special rules for "tentative definitions" at file scope:
    ///   int x;        // Tentative - becomes zero-initialized if no other definition
    ///   int y = 5;    // Initial - has explicit value
    ///   extern int z; // NoInitializer - declaration only, defined elsewhere
    enum class InitialValue {
        Tentative,      // No initializer, no extern -> will be zero-initialized
        Initial,        // Has explicit initializer (e.g., `int x = 5;`)
        NoInitializer,  // extern without initializer -> defined in another TU
    };

    //==========================================================================
    // Identifier Attributes
    //==========================================================================
    // These structs store semantic information about symbols in the symbol table.
    // The variant `IdentifierAttrs` holds one of these based on the symbol kind.
    // We can extract these structures mostly from the book Chapter 10 page 229.
    //
    // The split into three attribute types reflects C's storage and linkage model:
    // - FunAttr:    Functions (always have static storage duration)
    // - StaticAttr: Variables with static storage duration (globals, static locals)
    // - LocalAttr:  Variables with automatic storage duration (regular locals)
    //==========================================================================

    /// FunAttr - Attributes for function declarations/definitions.
    ///
    /// Functions in C always have static storage duration and either external
    /// or internal linkage. We track whether we've seen the definition (body)
    /// to detect multiple definitions of the same function.
    struct FunAttr {
        bool defined;   // true if we've seen a function body (definition)
        bool global;    // true = external linkage, false = internal (static)
    };

    /// StaticAttr - Attributes for variables with static storage duration.
    ///
    /// This includes:
    /// - File-scope variables (global variables)
    /// - Block-scope variables declared `static`
    /// - Block-scope variables declared `extern` (refer to a global)
    ///
    /// These variables live for the entire program execution and are stored
    /// in the data/bss segment, not on the stack.
    struct StaticAttr {
        InitialValue init;              // How was it initialized?
        std::optional<int64_t> value;   // Compile-time value if init == Initial
        bool global;                    // true = external linkage, false = internal
    };

    /// LocalAttr - Attributes for variables with automatic storage duration.
    ///
    /// Regular local variables and function parameters. These live on the stack
    /// and have no linkage (each declaration creates a distinct object).
    /// No extra attributes needed - the declaration itself has all the info.
    struct LocalAttr {};

    /// IdentifierAttrs - Variant holding the appropriate attribute type for a symbol.
    /// Use std::get_if or the SymbolEntry helper methods to access the specific type.
    using IdentifierAttrs = std::variant<FunAttr, StaticAttr, LocalAttr>;

    struct SymbolEntry {
        std::variant<FunctionDeclaration *, VarDeclaration *> decl;
        IdentifierAttrs attrs;

        // Constructor for functions
        SymbolEntry(FunctionDeclaration *func, Linkage linkage, ScopeType scope)
            : decl(func) {
            bool global = (linkage == Linkage::External);
            bool defined = func->hasBody();
            attrs = FunAttr{defined, global};
        }

        // Constructor for variables
        SymbolEntry(VarDeclaration *var, Linkage linkage, ScopeType scope)
            : decl(var) {
            bool global = (linkage == Linkage::External);

            // Check if it has static storage duration
            bool hasStaticStorage = (scope == ScopeType::Global) ||
                                    (var->getStorageClass() == StorageClass::SC_Static) ||
                                    (var->getStorageClass() == StorageClass::SC_Extern);

            if (hasStaticStorage) {
                InitialValue init;
                std::optional<int64_t> value = std::nullopt;

                if (var->getExpr() != nullptr) {
                    init = InitialValue::Initial;
                    // TODO: evaluate constant expression to get actual value
                } else if (var->getStorageClass() == StorageClass::SC_Extern) {
                    init = InitialValue::NoInitializer;
                } else {
                    init = InitialValue::Tentative;
                }

                attrs = StaticAttr{init, value, global};
            } else {
                attrs = LocalAttr{};
            }
        }

        // Helper methods to access attributes

        [[nodiscard]] bool isFunction() const {
            return std::holds_alternative<FunctionDeclaration *>(decl);
        }

        [[nodiscard]] bool isFunAttr() const {
            return std::holds_alternative<FunAttr>(attrs);
        }

        [[nodiscard]] bool isStaticAttr() const {
            return std::holds_alternative<StaticAttr>(attrs);
        }

        [[nodiscard]] bool isLocalAttr() const {
            return std::holds_alternative<LocalAttr>(attrs);
        }

        [[nodiscard]] const FunAttr* getFunAttr() const {
            return std::get_if<FunAttr>(&attrs);
        }

        [[nodiscard]] const StaticAttr* getStaticAttr() const {
            return std::get_if<StaticAttr>(&attrs);
        }

        // Convenience methods matching old interface
        [[nodiscard]] bool isGlobal() const {
            if (auto *fa = getFunAttr()) return fa->global;
            if (auto *sa = getStaticAttr()) return sa->global;
            return false;  // LocalAttr is never global
        }

        [[nodiscard]] bool hasStaticStorage() const {
            return isStaticAttr();
        }
    };

    class Scope {
        Scope *parentScope;
        StringMap<SymbolEntry> Symbols;

        // Track variables declared in this scope (original names)
        std::vector<std::string> DeclaredIdentifiers;

    public:
        Scope(Scope *parentScope = nullptr) : parentScope(parentScope) {
        }

        bool insert(VarDeclaration *declaration, Linkage linkage, ScopeType scope);

        bool insert(StringRef key, VarDeclaration *declaration, Linkage linkage, ScopeType scope);

        bool insert(FunctionDeclaration *funcDeclaration, Linkage linkage, ScopeType scope);

        bool insert(StringRef key, FunctionDeclaration *funcDeclaration, Linkage linkage, ScopeType scope);

        // Lookup through scope chain (current scope + parent scopes)
        VarDeclaration *lookupForVar(StringRef Name);

        FunctionDeclaration *lookupForFunction(StringRef Name);

        // Lookup the SymbolEntry for a name (returns nullptr if not found)
        [[nodiscard]] const SymbolEntry *lookupEntry(StringRef Name) const;

        // Check if a symbol exists in the current scope only (not parent scopes)
        [[nodiscard]] bool hasSymbolInCurrentScope(StringRef Name) const;

        // Check if a symbol has a linkage conflict with an existing symbol
        [[nodiscard]] bool hasLinkageConflict(StringRef Name, std::optional<StorageClass> newStorageClass) const;

        // Update the attributes of an existing symbol entry (for file scope variable merging)
        bool updateSymbolEntry(StringRef Name, const StaticAttr &newAttrs);

        void addDeclaredIdentifier(StringRef originalName);

        [[nodiscard]] const std::vector<std::string> &getDeclaredIdentifiers() const { return DeclaredIdentifiers; }

        [[nodiscard]] Scope *getParentScope() const { return parentScope; }

        [[nodiscard]] const StringMap<SymbolEntry> &getSymbols() const { return Symbols; }
    };
}
