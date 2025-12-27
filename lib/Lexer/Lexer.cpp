#include "mycc/Lexer/Lexer.hpp"

using namespace mycc;

void KeywordFilter::addKeyword(StringRef Keyword, tok::TokenKind TokenCode)
{
    HashTable.insert(std::make_pair(Keyword, TokenCode));
}

void KeywordFilter::addKeywords()
{
#define KEYWORD(NAME, FLAGS) addKeyword(StringRef(#NAME), tok::kw_##NAME);
#include "mycc/Basic/TokenKinds.def"
}

/// @brief Namespace with utilities to check characters
namespace charinfo
{
    /**
     * @brief Check if character is an ASCII char.
     *
     * @param Ch character to test
     * @return `true` if character is ascii, `false` other case
     */

    /// @brief Check if character is inside of the ASCII characterset
    /// @param Ch character to check
    /// @return boolean indicating if is ASCII
    LLVM_READNONE inline bool isASCII(char Ch)
    {
        return static_cast<unsigned char>(Ch) <= 127;
    }

    /// @brief Check if we are moving to a new or beginning of a line
    /// @param Ch character to check
    /// @return true in case of vertical whitespace
    LLVM_READNONE inline bool isVerticalWhitespace(char Ch)
    {
        return isASCII(Ch) && (Ch == '\r' || Ch == '\n');
    }

    /// @brief Check if character is a horizontal white space
    /// @param Ch character to check
    /// @return true in case is a horizontal white space
    LLVM_READNONE inline bool isHorizontalWhitespace(char Ch)
    {
        return isASCII(Ch) && (Ch == ' ' || Ch == '\t' || Ch == '\f' || Ch == '\v');
    }

    /// @brief Is in general a whitespace? these are mostly bypassed
    /// @param Ch character to check
    /// @return true in case some kind of whitespace
    LLVM_READNONE inline bool isWhitespace(char Ch)
    {
        return isHorizontalWhitespace(Ch) || isVerticalWhitespace(Ch);
    }

    /// @brief Check of character to know if it's a character digit
    /// @param Ch
    /// @return
    LLVM_READNONE inline bool isDigit(char Ch)
    {
        return isASCII(Ch) && Ch >= '0' && Ch <= '9';
    }

    /// @brief Check if current character is a hex digit, for that it must
    /// be a digit or a value between 'A' and 'F'
    /// @param Ch
    /// @return
    LLVM_READNONE inline bool isHexDigit(char Ch)
    {
        return isASCII(Ch) && (isDigit(Ch) || (Ch >= 'A' && Ch <= 'F'));
    }

    /// @brief Check if current character is a possible identifier, for
    /// that, this value must be an under line, or a letter.
    /// @param Ch
    /// @return
    LLVM_READNONE inline bool isIdentifierHead(char Ch)
    {
        return isASCII(Ch) && ((Ch == '_' || (Ch >= 'A' && Ch <= 'Z')) || (Ch >= 'a' && Ch <= 'z'));
    }

    /// @brief The characters from an identifier except from the first
    /// can be also numbers.
    /// @param Ch
    /// @return
    LLVM_READNONE inline bool isIdentifierBody(char Ch)
    {
        return isIdentifierHead(Ch) || isDigit(Ch);
    }

} //! namespace charinfo

void Lexer::peek(Token &Result) {
    // Save current position
    const char *SavedCurPtr = CurPtr;

    // Get next token
    next(Result);

    // Restore position
    CurPtr = SavedCurPtr;
}

void Lexer::next(Token &Result) {
    // move current pointer while is not a white space or the buffer is not empty
    while (*CurPtr && charinfo::isWhitespace(*CurPtr)) {
        ++CurPtr;
    }

    // if there are not more tokens, set current as eof and finish
    if (!*CurPtr) {
        Result.setKind(tok::eof);
        return;
    }

    // We apply a recursive descendent parser, we need to check
    // for the different tokens.

    // check for a possible identifier
    if (charinfo::isIdentifierHead(*CurPtr)) {
        // parse the identifier
        identifier(Result);
        return;
    }
    // parse possible digit number
    if (charinfo::isDigit(*CurPtr)) {
        number(Result);
        return;
    }
    // check for other possible tokens
    switch (*CurPtr) {
            // These tokens are just one character, we can create a macro
            // that will expand to create a case for each one.
#define CASE(ch, tok)                       \
    case ch:                                \
        formToken(Result, CurPtr + 1, tok); \
        break
        CASE(',', tok::comma);   // , character
        CASE('.', tok::period);  // . character
        CASE(':', tok::colon);   // : character
        CASE(';', tok::semi);    // ; character (end of code line)
        CASE('(', tok::l_paren); // beginning of parenthesis
        CASE(')', tok::r_paren); // end of parenthesis
        CASE('{', tok::l_brace);
        CASE('}', tok::r_brace);
        CASE('[', tok::l_square); // [ character
        CASE(']', tok::r_square); // ] character
        CASE('~', tok::tilde);    // ~ character
        CASE('?', tok::interrogation);
#undef CASE
        case '+':
            if (*(CurPtr + 1) == '+')
                formToken(Result, CurPtr+2, tok::increment);
            else if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compoundadd);
            else
                formToken(Result, CurPtr+1, tok::plus);
            break;
        case '-':
            if (*(CurPtr + 1) == '-')
                formToken(Result, CurPtr+2, tok::decrement);
            else if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compoundsub);
            else
                formToken(Result, CurPtr+1, tok::minus);
            break;
        case '*':
            if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compoundmul);
            else
                formToken(Result, CurPtr+1, tok::star);
            break;
        case '%':
            if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compoundrem);
            else
                formToken(Result, CurPtr+1, tok::percent);
            break;
        case '=':
            if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::equalequal);
            else
                formToken(Result, CurPtr+1, tok::equal);
            break;
        case '!':
            if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::exclaimequal);
            else
                formToken(Result, CurPtr+1, tok::exclaim);
            break;
        case '<':
            if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::lessequal);
            else if (*(CurPtr + 1) == '<') {
                if (*(CurPtr + 2) == '=')
                    formToken(Result, CurPtr+3, tok::compoundshl);
                else
                    formToken(Result, CurPtr+2, tok::lessless);
            }
            else
                formToken(Result, CurPtr+1, tok::less);
            break;
        case '>':
            if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::greaterequal);
            else if (*(CurPtr + 1) == '>') {
                if (*(CurPtr + 2) == '=')
                    formToken(Result, CurPtr+3, tok::compoundshr);
                else
                    formToken(Result, CurPtr+2, tok::greatergreater);
            }
            else
                formToken(Result, CurPtr+1, tok::greater);
            break;
        case '&':
            if (*(CurPtr + 1) == '&')
                formToken(Result, CurPtr+2, tok::ampamp);
            else if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compoundand);
            else
                formToken(Result, CurPtr+1, tok::amp);
            break;
        case '|':
            if (*(CurPtr + 1) == '|')
                formToken(Result, CurPtr+2, tok::pipepipe);
            else if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compoundor);
            else
                formToken(Result, CurPtr+1, tok::pipe);
            break;
        case '/':
            // long comment
            if (*(CurPtr + 1) == '*') {
                comment();
                // after the comment we need to return a Token
                next(Result);
            }
            // one line comment
            else if (*(CurPtr + 1) == '/') {
                while (*CurPtr && !charinfo::isVerticalWhitespace(*CurPtr)) {
                    ++CurPtr;
                }
                // after the comment we need to return a Token
                next(Result);
            }
            else if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compounddiv);
            // slash token
            else {
                formToken(Result, CurPtr + 1, tok::slash);
            }
            break;
        case '^':
            if (*(CurPtr + 1) == '=')
                formToken(Result, CurPtr+2, tok::compoundxor);
            else
                formToken(Result, CurPtr+1, tok::caret);
            break;
        case '#':
            // for the moment skip it until we have some code
            // to manage pragmas and includes...
            while (*CurPtr && !charinfo::isVerticalWhitespace(*CurPtr)) {
                ++CurPtr;
            }
            next(Result);
            break;
        default:
            Result.setKind(tok::unknown);
            StringRef character{CurPtr, 1};
            CurPtr = CurPtr + 1;
            Diags.report(getLoc(), diag::err_unexpected_token, character.str());
    }
}

void Lexer::identifier(Token &Result) {
    const char *Start = CurPtr;
    const char *End = CurPtr + 1;
    while (charinfo::isIdentifierBody(*End))
        ++End;
    // Now we can create a StringRef with the token
    StringRef Name(Start, End - Start);
    // Create a token, as an identifier and a keyword
    // has the same form, we need to check if the Token
    // is a keyword (in which case, it will be created as
    // an identifier).
    formToken(Result, End, Keywords.getKeyword(Name, tok::identifier));
}

void Lexer::number(Token &Result) {
    const char *End = CurPtr + 1;
    tok::TokenKind Kind = tok::unknown;

    while (*End) {
        // we only support normal integers
        // for now
        if (!charinfo::isDigit(*End))
            break;
        ++End;
    }

    // Check for invalid suffix (like letters after numbers)
    // this would violate C's lexical rules (identifiers can't start
    // with digits, numbers can't contain letters except in scientific notation).
    if (*End && charinfo::isIdentifierHead(*End)) {
        // Error: invalid suffix on integer literal
        // You might want to advance End to consume the invalid suffix
        // to help with error recovery
        const char *SuffixStart = End;
        while (*End && charinfo::isIdentifierBody(*End)) {
            ++End;
        }

        Diags.report(getLoc(),
                     diag::err_invalid_suffix_integer,
                     StringRef(SuffixStart, End - SuffixStart).str());

        Kind = tok::unknown;
    } else {
        Kind = tok::integer_literal;
    }

    formToken(Result, End, Kind);
}

void Lexer::comment() {
    const char *End = CurPtr + 2;
    unsigned Level = 1;
    while (*End && Level) {
        // each time we find /* we enter a level
        // more of comment
        if (*End == '/' && *(End + 1) == '*') {
            End += 2;
            Level++;
        }
        // comments end with */
        else if (*End == '*' && *(End + 1) == '/') {
            End += 2;
            Level--;
        }
        else
            ++End;
    }

    CurPtr = End;
    // do not generate any token
}

void Lexer::formToken(Token &Result, const char *TokEnd, tok::TokenKind Kind) {
    size_t TokLen = TokEnd - CurPtr;
    Result.Ptr = CurPtr;
    Result.Length = TokLen;
    Result.Kind = Kind;
    CurPtr = TokEnd;
}
