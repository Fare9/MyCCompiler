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
    /// Not included:
    /// - Variables declared inside a function.
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

        /// @brief Construct a SymbolEntry for a function declaration.
        /// @param func pointer to the function declaration AST node.
        /// @param linkage linkage kind (External or Static).
        /// @param scope scope type where the declaration appears.
        SymbolEntry(FunctionDeclaration *func, Linkage linkage, ScopeType scope)
            : decl(func) {
            const bool global = (linkage == Linkage::External);
            const bool defined = func->hasBody();
            attrs = FunAttr{defined, global};
        }

        /// @brief Construct a SymbolEntry for a variable declaration.
        /// Determines whether the variable has static or automatic storage
        /// duration and sets the appropriate attributes.
        /// @param var pointer to the variable declaration AST node.
        /// @param linkage linkage kind (External, Static, or None).
        /// @param scope scope type where the declaration appears.
        SymbolEntry(VarDeclaration *var, Linkage linkage, ScopeType scope)
            : decl(var) {
            bool global = (linkage == Linkage::External);

            // Check if it has static storage duration
            // - Declared globally
            // - or Declared static (anywhere)
            // - or Declared extern (anywhere)
            bool hasStaticStorage = (scope == ScopeType::Global) ||
                                    (var->getStorageClass() == StorageClass::SC_Static) ||
                                    (var->getStorageClass() == StorageClass::SC_Extern);

            if (hasStaticStorage) {
                InitialValue init;
                const std::optional<int64_t> value = std::nullopt;

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

        /// @brief Check whether this entry holds a function declaration.
        /// @return `true` if the entry is a function, `false` otherwise.
        [[nodiscard]] bool isFunction() const {
            return std::holds_alternative<FunctionDeclaration *>(decl);
        }

        /// @brief Check whether the attributes are of function kind.
        /// @return `true` if this entry has FunAttr, `false` otherwise.
        [[nodiscard]] bool isFunAttr() const {
            return std::holds_alternative<FunAttr>(attrs);
        }

        /// @brief Check whether the attributes are of static-storage kind.
        /// @return `true` if this entry has StaticAttr, `false` otherwise.
        [[nodiscard]] bool isStaticAttr() const {
            return std::holds_alternative<StaticAttr>(attrs);
        }

        /// @brief Check whether the attributes are of local (automatic) kind.
        /// @return `true` if this entry has LocalAttr, `false` otherwise.
        [[nodiscard]] bool isLocalAttr() const {
            return std::holds_alternative<LocalAttr>(attrs);
        }

        /// @brief Get the function attributes, if present.
        /// @return pointer to FunAttr, or nullptr if this is not a function entry.
        [[nodiscard]] const FunAttr* getFunAttr() const {
            return std::get_if<FunAttr>(&attrs);
        }

        /// @brief Get the static-storage attributes, if present.
        /// @return pointer to StaticAttr, or nullptr if not a static-storage entry.
        [[nodiscard]] const StaticAttr* getStaticAttr() const {
            return std::get_if<StaticAttr>(&attrs);
        }

        /// @brief Check whether this symbol has external linkage.
        /// @return `true` if the symbol is globally visible, `false` otherwise.
        [[nodiscard]] bool isGlobal() const {
            if (auto *fa = getFunAttr()) return fa->global;
            if (auto *sa = getStaticAttr()) return sa->global;
            return false;  // LocalAttr is never global
        }

        /// @brief Check whether this symbol has static storage duration.
        /// @return `true` if the symbol lives for the entire program execution.
        [[nodiscard]] bool hasStaticStorage() const {
            return isStaticAttr();
        }
    };

    /// @brief Scope represents a lexical scope in the program and holds a symbol
    /// table mapping names to their declarations and attributes. Scopes form a
    /// chain via parent pointers to support nested lookup.
    class Scope {
        /// @brief Pointer to the enclosing scope, or nullptr for the global scope.
        Scope *parentScope;
        /// @brief Symbol table mapping identifier names to their entries.
        StringMap<SymbolEntry> Symbols;
        /// @brief Original names of identifiers declared in this scope (before renaming).
        std::vector<std::string> DeclaredIdentifiers;

    public:
        /// @brief Construct a new Scope.
        /// @param parentScope pointer to the enclosing scope, or nullptr for the global scope.
        explicit Scope(Scope *parentScope = nullptr) : parentScope(parentScope) {
        }

        /// @brief Insert a variable declaration using the variable's own name as key.
        /// @param declaration pointer to the variable declaration AST node.
        /// @param linkage linkage kind for the variable.
        /// @param scope scope type where the declaration appears.
        /// @return `true` if insertion succeeded, `false` if the name already exists.
        bool insert(VarDeclaration *declaration, Linkage linkage, ScopeType scope);

        /// @brief Insert a variable declaration under a specific key.
        /// @param key the name to use as the symbol table key.
        /// @param declaration pointer to the variable declaration AST node.
        /// @param linkage linkage kind for the variable.
        /// @param scope scope type where the declaration appears.
        /// @return `true` if insertion succeeded, `false` if the key already exists.
        bool insert(StringRef key, VarDeclaration *declaration, Linkage linkage, ScopeType scope);

        /// @brief Insert a function declaration using the function's own name as key.
        /// @param funcDeclaration pointer to the function declaration AST node.
        /// @param linkage linkage kind for the function.
        /// @param scope scope type where the declaration appears.
        /// @return `true` if insertion succeeded, `false` if the name already exists.
        bool insert(FunctionDeclaration *funcDeclaration, Linkage linkage, ScopeType scope);

        /// @brief Insert a function declaration under a specific key.
        /// @param key the name to use as the symbol table key.
        /// @param funcDeclaration pointer to the function declaration AST node.
        /// @param linkage linkage kind for the function.
        /// @param scope scope type where the declaration appears.
        /// @return `true` if insertion succeeded, `false` if the key already exists.
        bool insert(StringRef key, FunctionDeclaration *funcDeclaration, Linkage linkage, ScopeType scope);

        /// @brief Look up a variable by name through the scope chain (current + parents).
        /// @param Name the identifier name to search for.
        /// @return pointer to the VarDeclaration if found, nullptr otherwise.
        VarDeclaration *lookupForVar(StringRef Name);

        /// @brief Look up a function by name through the scope chain (current + parents).
        /// @param Name the identifier name to search for.
        /// @return pointer to the FunctionDeclaration if found, nullptr otherwise.
        FunctionDeclaration *lookupForFunction(StringRef Name);

        /// @brief Look up a SymbolEntry by name through the scope chain.
        /// @param Name the identifier name to search for.
        /// @return pointer to the SymbolEntry if found, nullptr otherwise.
        [[nodiscard]] const SymbolEntry *lookupEntry(StringRef Name) const;

        /// @brief Check if a symbol exists in the current scope only (not parent scopes).
        /// @param Name the identifier name to check.
        /// @return `true` if the name is declared in this scope, `false` otherwise.
        [[nodiscard]] bool hasSymbolInCurrentScope(StringRef Name) const;

        /// @brief Check if inserting a symbol with the given storage class would
        /// create a linkage conflict with an existing symbol.
        /// @param Name the identifier name to check.
        /// @param newStorageClass the storage class of the new declaration.
        /// @return `true` if there is a conflict, `false` otherwise.
        [[nodiscard]] bool hasLinkageConflict(StringRef Name, std::optional<StorageClass> newStorageClass) const;

        /// @brief Update the attributes of an existing symbol entry (used for
        /// merging file-scope variable redeclarations).
        /// @param Name the identifier name to update.
        /// @param newAttrs the new static attributes to apply.
        /// @return `true` if the entry was found and updated, `false` otherwise.
        bool updateSymbolEntry(StringRef Name, const StaticAttr &newAttrs);

        /// @brief Record an identifier as declared in this scope.
        /// @param originalName the original name of the identifier.
        void addDeclaredIdentifier(StringRef originalName);

        /// @brief Get the list of identifiers declared in this scope.
        /// @return const reference to the vector of declared identifier names.
        [[nodiscard]] const std::vector<std::string> &getDeclaredIdentifiers() const { return DeclaredIdentifiers; }

        /// @brief Get the parent (enclosing) scope.
        /// @return pointer to the parent scope, or nullptr if this is the global scope.
        [[nodiscard]] Scope *getParentScope() const { return parentScope; }

        /// @brief Get the symbol table for this scope.
        /// @return const reference to the symbol map.
        [[nodiscard]] const StringMap<SymbolEntry> &getSymbols() const { return Symbols; }
    };
}
