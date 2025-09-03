#pragma once

#include "mycc/Basic/TokenKinds.hpp"
#include "mycc/Basic/LLVM.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/SMLoc.h"

namespace mycc {

class Lexer;

class Token {
    friend class Lexer;
    // pointer to token
    const char * Ptr;
    // length of the token
    size_t Length;
    // actual type of the token
    tok::TokenKind Kind;
    // a string version of the token
    std::string string_version;


public:
    /// @return TokenKind value for the current token
    [[nodiscard]] tok::TokenKind getKind() const {
        return Kind;
    }

    /// @brief Set the current Kind of the token
    /// @param K new Kind for the token
    void setKind(tok::TokenKind K) {
        Kind = K;
    }

    /// @brief Check if the current TokenKind is the provided one
    /// @param K the kind to check
    /// @return `true` if token is the provided one, `false` otherwise
    [[nodiscard]] bool is(tok::TokenKind K) const {
        return Kind == K;
    }

    /// @brief Negative version of previous function, check if token is not the given one
    /// @param K the kind to check it is not
    /// @return `true` if token is not the provided one, `false` otherwise
    [[nodiscard]] bool isNot(tok::TokenKind K) const {
        return Kind != K;
    }

    /// @brief checker function to check between two types, used to expand it with a templatized function
    /// @param K1 first Kind to check
    /// @param K2 second Kind to check
    /// @return `true` if it's any of the provided types
    [[nodiscard]] bool isOneOf(tok::TokenKind K1, tok::TokenKind K2) const
    {
        return is(K1) || is(K2);
    }

    template<typename... Ts>
    bool isOneOf(tok::TokenKind K1, tok::TokenKind K2, Ts... Ks) const
    {
        return is(K1) || isOneOf(K2, Ks...);
    }

    [[nodiscard]] const char * getName() const
    {
        return tok::getTokenName(Kind);
    }

    [[nodiscard]] size_t getLength() const
    {
        return Length;
    }

    [[nodiscard]] SMLoc getLocation() const
    {
        return SMLoc::getFromPointer(Ptr);
    }

    StringRef getIdentifier()
    {
        assert(is(tok::identifier) && "Cannot get identifier of non-identifier");
        return {Ptr, Length};
    }

    StringRef getLiteralData()
    {
        assert(is(tok::integer_literal) && "Cannot get literal data of non-literal");
        return {Ptr, Length};
    }

    std::string to_string() {
        string_version = "[Tok_";
        string_version += std::string(tok::getTokenName(Kind));
        if (!is(tok::eof)) {
            string_version += ", '";
            string_version += std::string(Ptr, Length);
            string_version += "'";
        }
        string_version += "]";
        return string_version;
    }


};

}