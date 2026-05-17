#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/Support/Casting.h"

#include <string>
#include <vector>

namespace mycc {

    /// @brief Storage class specifiers that can appear on declarations
    /// (`static` or `extern`).
    enum class StorageClass {
        SC_Static,
        SC_Extern,
    };

    /// @brief Base class for all types in the AST. Uses a discriminator kind
    /// to support LLVM-style RTTI (`classof`).
    class Type {
    public:
        enum TypeKind {
            TK_Builtin,
            TK_Pointer,
            TK_Function,
        };

    private:
        const TypeKind Kind;

    protected:
        explicit Type(TypeKind Kind) : Kind(Kind) {
        }

    public:
        virtual ~Type() = default;

        [[nodiscard]] TypeKind getKind() const { return Kind; }

        virtual std::string to_string() = 0;

        [[nodiscard]] virtual bool equal(const Type &other) const {
            return Kind == other.Kind;
        }
    };

    /// @brief Represents a built-in primitive type (e.g. `int`, `void`).
    class BuiltinType : public Type {
    public:
        enum BuiltinKind {
            Bool,
            Char,
            Short,
            Int,
            UInt,
            Long,
            ULong,
            Void,
            // For the moment, we can include in the future more
        };

    private:
        BuiltinKind BuiltinK;

    public:
        explicit BuiltinType(BuiltinKind K)
            : Type(TK_Builtin), BuiltinK(K) {
        }

        [[nodiscard]] BuiltinKind getBuiltinKind() const { return BuiltinK; }

        /// Returns the integer conversion rank for a builtin kind.
        /// Returns -1 for non-integer types (e.g. Void) so callers can detect errors.
        static int integerRank(BuiltinType::BuiltinKind K);

        [[nodiscard]] bool isIntegerType() const { return integerRank(BuiltinK) >= 0; }
        [[nodiscard]] bool isVoid() const { return BuiltinK == Void; }

        std::string to_string() override;

        [[nodiscard]] bool equal(const Type &other) const override;

        static bool classof(const Type *T) {
            return T->getKind() == TK_Builtin;
        }
    };

    /// @brief Represents a Function type, it contains return types, and argument types.
    class FunctionType : public Type {
        std::string funcTypeStr;
        Type *retType;
        std::vector<Type*> argType;

    public:
        FunctionType(Type *retType, std::vector<Type*> argType) : Type(TK_Function),
            retType(retType), argType(std::move(argType)) {
        }

        ~FunctionType() override = default;

        /// @return reference to the return type
        [[nodiscard]] Type *getReturnType() const {
            return retType;
        }

        /// @return reference to the vector with the argument types
        [[nodiscard]] const std::vector<Type*> &getArgTypes() const {
            return argType;
        }

        std::string to_string() override;

        [[nodiscard]] bool equal(const Type &other) const override;

        static bool classof(const Type *T) {
            return T->getKind() == TK_Function;
        }
    };

} // namespace mycc
