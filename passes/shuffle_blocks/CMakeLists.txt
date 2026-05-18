cmake_minimum_required(VERSION 3.13.4)
project(ShuffleBlocksPass)

set(CMAKE_CXX_STANDARD 20)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# MODULE produces a .so that can only be dlopen-ed, not linked against.
# Plugins must NOT link against LLVM — the host process already has it loaded.
add_library(ShuffleBlocksPass MODULE
    ShuffleBlocksPass.cpp
)

target_include_directories(ShuffleBlocksPass PRIVATE
    ${LLVM_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Match LLVM's own build settings — it is typically compiled without RTTI.
target_compile_options(ShuffleBlocksPass PRIVATE -fno-rtti)
