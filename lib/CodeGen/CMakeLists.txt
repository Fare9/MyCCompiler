set(LLVM_LINK_COMPONENTS support)

add_library(CodeGen
    IRGen.cpp
    x64/X64CodeGen.cpp
)
