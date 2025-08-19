set(LLVM_LINK_COMPONENTS support)

add_library(Lexer Lexer.cpp)
target_link_libraries(Lexer Basic)