#include <mycc/Lexer/NumericLiteralParser.hpp>

namespace {
    LLVM_READNONE inline bool isDigit(char Ch) {
        return Ch >= '0' && Ch <= '9';
    }

    LLVM_READNONE inline bool isHexDigit(char Ch) {
        return isDigit(Ch) || (Ch >= 'a' && Ch <= 'f') || (Ch >= 'A' && Ch <= 'F');
    }

    LLVM_READNONE inline bool isOctalDigit(char Ch) {
        return Ch >= '0' && Ch <= '7';
    }

    inline bool isDigitForBase(char Ch, unsigned base) {
        if (base == 16) return isHexDigit(Ch);
        if (base == 8)  return isOctalDigit(Ch);
        return isDigit(Ch);
    }
}

mycc::NumericLiteralParser::NumericLiteralParser(llvm::StringRef Literal) {
    isCorrect_ = true;

    const char *begin = Literal.data();
    const char *end   = begin + Literal.size();

    // Determine base from prefix
    unsigned base = 10;
    const char *digitsBegin = begin;
    if (Literal.size() >= 2 && *begin == '0') {
        if (*(begin + 1) == 'x' || *(begin + 1) == 'X') {
            base = 16;
            digitsBegin = begin + 2;
        } else if (*(begin + 1) != '\0') {
            base = 8;
            digitsBegin = begin + 1;
        }
    }

    // Scan digit sequence
    const char *digitsEnd = digitsBegin;
    while (digitsEnd < end && isDigitForBase(*digitsEnd, base))
        ++digitsEnd;

    // Detect unsupported float literals
    if (digitsEnd < end && (*digitsEnd == '.' ||
                            *digitsEnd == 'e' || *digitsEnd == 'E' ||
                            *digitsEnd == 'p' || *digitsEnd == 'P')) {
        isFloat_   = true;
        isCorrect_ = false;
        return;
    }

    // Validate we got at least one digit
    if (digitsBegin == digitsEnd) {
        isCorrect_ = false;
        return;
    }

    // Parse suffix: [u|U][l|L] or [l|L][u|U]
    const char *p = digitsEnd;
    bool seenU = false;
    bool seenL = false;

    while (p < end && isCorrect_) {
        if (*p == 'u' || *p == 'U') {
            if (seenU) { isCorrect_ = false; break; }
            isSigned_ = false;
            seenU = true;
            ++p;
        } else if (*p == 'l' || *p == 'L') {
            if (seenL) { isCorrect_ = false; break; }
            char first = *p++;
            if (p < end && *p == first) {
                // ll / LL — not yet supported
                isCorrect_ = false;
                break;
            }
            isLong_ = true;
            seenL = true;
        } else {
            isCorrect_ = false;
        }
    }

    if (!isCorrect_)
        return;

    // Parse into 65 bits — the extra bit lets us detect values that exceed
    // UINT64_MAX without silent wrapping.
    llvm::StringRef digits(digitsBegin, digitsEnd - digitsBegin);
    llvm::APInt wide(65, digits, static_cast<uint8_t>(base));

    if (wide.getActiveBits() > 64) {
        isOverflow_ = true;
        isCorrect_  = false;
        return;
    }

    constantValue = wide.trunc(64);
}
