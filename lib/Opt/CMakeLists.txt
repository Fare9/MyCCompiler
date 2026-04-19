add_library(Optimizer
        LLVMOptimizer.cpp
)

llvm_map_components_to_libnames(opt_llvm_libs
        passes
        support
)

target_link_libraries(Optimizer ${opt_llvm_libs})
target_include_directories(Optimizer PUBLIC
        ${CMAKE_SOURCE_DIR}/include
        ${LLVM_INCLUDE_DIRS}
)