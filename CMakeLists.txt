cmake_minimum_required(VERSION 3.13.4)
project(MyCCompiler)

set(CMAKE_CXX_STANDARD 20)

find_package(LLVM REQUIRED CONFIG)
find_package(fmt REQUIRED)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

include_directories(include)

add_subdirectory(lib)

add_executable(mycc main.cpp)

target_link_libraries(mycc
        LLVM
        fmt::fmt
        Basic
        Lexer
        Sema
        Parser
        ASTPrinter
        CodeGen)
