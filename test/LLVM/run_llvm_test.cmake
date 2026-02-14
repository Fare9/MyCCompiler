# CMake script to run a single LLVM IR test
# Usage: cmake -P run_llvm_test.cmake -DTEST_FILE=<.c file> -DEXPECTED=<exit code> -DMYCC=<compiler path>

# Step 1: Compile .c to .ll with mycc
execute_process(
    COMMAND "${MYCC}" --llvm "${TEST_FILE}"
    RESULT_VARIABLE mycc_result
    OUTPUT_VARIABLE mycc_output
    ERROR_VARIABLE mycc_error
)
if(NOT mycc_result EQUAL 0)
    message(FATAL_ERROR "mycc failed:\n${mycc_error}")
endif()

# Derive .ll path from .c path
string(REGEX REPLACE "\\.c$" ".ll" LL_FILE "${TEST_FILE}")

# Step 2: Compile .ll to binary with clang
string(REGEX REPLACE "\\.c$" ".out" BIN_FILE "${TEST_FILE}")
execute_process(
    COMMAND clang "${LL_FILE}" -o "${BIN_FILE}"
    RESULT_VARIABLE clang_result
    ERROR_VARIABLE clang_error
)
if(NOT clang_result EQUAL 0)
    file(REMOVE "${LL_FILE}")
    message(FATAL_ERROR "clang failed:\n${clang_error}")
endif()

# Step 3: Run the binary and check exit code
execute_process(
    COMMAND "${BIN_FILE}"
    RESULT_VARIABLE actual_exit
)

# Cleanup
file(REMOVE "${LL_FILE}" "${BIN_FILE}")

# Step 4: Compare
if(NOT actual_exit EQUAL "${EXPECTED}")
    message(FATAL_ERROR "Exit code mismatch: expected ${EXPECTED}, got ${actual_exit}")
endif()
