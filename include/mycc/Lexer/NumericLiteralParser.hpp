//
// Created by eduardo on 17/05/2026.
//

#pragma once

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>
#include <mycc/Basic/LLVM.hpp>

namespace mycc {

class NumericLiteralParser {
private:
    llvm::APInt constantValue;
    bool isSigned_   = true;
    bool isLong_     = false;
    bool isFloat_    = false;
    bool isCorrect_  = false;
    bool isOverflow_ = false;
public:
    NumericLiteralParser(StringRef Literal);

    [[nodiscard]] bool isCorrect()  const { return isCorrect_; }
    [[nodiscard]] bool isSigned()   const { return isSigned_; }
    [[nodiscard]] bool isLong()     const { return isLong_; }
    [[nodiscard]] bool isFloat()    const { return isFloat_; }
    [[nodiscard]] bool isOverflow() const { return isOverflow_; }

    // Returns the value truncated to 64 bits. Only valid when isCorrect() is true.
    [[nodiscard]] const llvm::APInt &getValue() const { return constantValue; }
};

} // namespace mycc