
#pragma once

#include "mycc/Basic/Diagnostic.hpp"
#include "mycc/Lexer/Token.hpp"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

namespace mycc {

class KeywordFilter {
    /// @brief Map of strings and keywords
    llvm::StringMap<tok::TokenKind> HashTable;
    /// @brief Create an entry in HashTable with the name of the keyword and the token type
    /// @param Keyword name of keyword
    /// @param TokenCode token of keyword
    void addKeyword(StringRef Keyword, tok::TokenKind TokenCode);
public:
    /// @brief Add all the keywords to the hash table
    void addKeywords();

    /// @brief Get a keyword by the name or return a default token
    /// @param Name name of the keyword
    /// @param DefaultTokenCode default token to return
    /// @return requested keyword or default token
    tok::TokenKind getKeyword(StringRef Name, tok::TokenKind DefaultTokenCode = tok::unknown)
    {
        auto Result = HashTable.find(Name);
        if (Result != HashTable.end())
            return Result->second;
        return DefaultTokenCode;
    }
};

class Lexer {
    /// @brief Source manager from LLVM
    SourceMgr &SrcMgr;
    /// @brief Management of diagnostic errors
    DiagnosticsEngine &Diags;

    /// @brief Current pointer in the file for parsing
    const char *CurPtr;
    /// @brief Buffer of text
    StringRef CurBuf;

    /// CurrBuffer - buffer index we're
    /// lexing from as managed by SourceMgr object
    unsigned CurBuffer = 0;

    /// Filter for managing the keywords of the language
    KeywordFilter Keywords;
public:
    /// @brief A lexer is a class that manages a buffer with tokens, this buffer with source code will be traversed parsing tokens
    /// @param SrcMgr manager for the file
    /// @param Diags error diagnostic object
    explicit Lexer(SourceMgr &SrcMgr, DiagnosticsEngine &Diags) : SrcMgr(SrcMgr), Diags(Diags) {
        // ID of the main file buffer
        CurBuffer = SrcMgr.getMainFileID();
        // current buffer with file to parse
        CurBuf = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
        // pointer to the buffer
        CurPtr = CurBuf.begin();
        // initialize the keywords
        Keywords.addKeywords();
    }

    [[nodiscard]] DiagnosticsEngine &getDiagnostics() const {
        return Diags;
    }

    /// @brief Reset some of the variables from the Lexer in order to be able to
    ///     tokenize again from the beginning. This is mostly used with debugging
    ///     purposes.
    void reset() {
        CurBuffer = SrcMgr.getMainFileID();
        CurBuf = SrcMgr.getMemoryBuffer(CurBuffer)->getBuffer();
        CurPtr = CurBuf.begin();
    }

    /// @brief Parse the buffer and always return the next token found, it is a recursive descent parser
    /// @param Result
    void next(Token &Result);

    /// @brief Peek at the next token without consuming it
    /// @param Result token to store the peeked result
    void peek(Token &Result);

    /// Get source code buffer.
    [[nodiscard]] StringRef getBuffer() const
    {
        return CurBuf;
    }

private:
    /// @brief Check that next token is an identifier and return an ident token.
    /// @param Result
    void identifier(Token &Result);
    /// @brief Check next token is a number and in that case return it.
    /// @param Result
    void number(Token &Result);

    /// @brief check comment tokens, and skip them.
    void comment();

    /// @brief get location in code from the current token
    /// @return
    [[nodiscard]] SMLoc getLoc() const
    {
        return SMLoc::getFromPointer(CurPtr);
    }

    /// @brief Create a token object given a type and a string
    /// @param Result
    /// @param TokEnd end of the string with the current token
    /// @param Kind type of token
    void formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind);
};

}