set(LLVM_LINK_COMPONENTS support)

add_library(Parser Parser.cpp)
target_link_libraries(Parser Sema)