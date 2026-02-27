set(LLVM_LINK_COMPONENTS support)

add_library(Sema LoopLabelAssigner.cpp LabelGenerator.cpp)
target_link_libraries(Sema Lexer Basic)