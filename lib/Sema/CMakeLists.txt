set(LLVM_LINK_COMPONENTS support)

add_library(Sema Scope.cpp Sema.cpp
        Analyses/LoopLabelAssigner.cpp
        Analyses/LabelGenerator.cpp)
target_link_libraries(Sema Lexer Basic)