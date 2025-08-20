
#pragma once

#include "mycc/Basic/LLVM.hpp"
#include "llvm/Support/SourceMgr.h"

namespace mycc {

class ASTContext {
    llvm::SourceMgr &SrcMgr;
    StringRef FileName;
public:
    ASTContext(llvm::SourceMgr &SrcMgr, StringRef FileName)
        : SrcMgr(SrcMgr), FileName(FileName) {
    }

    StringRef getFileName() const {
        return FileName;
    }

    llvm::SourceMgr & getSourceMgr() {
        return SrcMgr;
    }

    const llvm::SourceMgr & getSourceMgr() const {
        return SrcMgr;
    }
};

}