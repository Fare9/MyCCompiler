# LLVM IR integration tests
# Each .c file must have a first-line comment: // Expected exit code: <N>

set(LLVM_TEST_RUNNER "${CMAKE_CURRENT_SOURCE_DIR}/run_llvm_test.cmake")

# Find all test .c files recursively
file(GLOB_RECURSE LLVM_TEST_FILES "${CMAKE_CURRENT_SOURCE_DIR}/*/*.c")

foreach(TEST_FILE ${LLVM_TEST_FILES})
    # Read the first line to extract expected exit code
    file(STRINGS "${TEST_FILE}" FIRST_LINE LIMIT_COUNT 1)
    string(REGEX MATCH "Expected exit code: ([0-9]+)" _match "${FIRST_LINE}")

    if(NOT _match)
        message(WARNING "Skipping ${TEST_FILE}: no expected exit code found")
        continue()
    endif()

    set(EXPECTED_CODE "${CMAKE_MATCH_1}")

    # Build a nice test name from the relative path (e.g. "llvm/return/constant")
    file(RELATIVE_PATH REL_PATH "${CMAKE_CURRENT_SOURCE_DIR}" "${TEST_FILE}")
    string(REGEX REPLACE "\\.c$" "" TEST_NAME "${REL_PATH}")
    string(REPLACE "/" "_" TEST_NAME "${TEST_NAME}")
    set(TEST_NAME "llvm_${TEST_NAME}")

    add_test(
        NAME "${TEST_NAME}"
        COMMAND "${CMAKE_COMMAND}"
            -DTEST_FILE=${TEST_FILE}
            -DEXPECTED=${EXPECTED_CODE}
            -DMYCC=$<TARGET_FILE:mycc>
            -P "${LLVM_TEST_RUNNER}"
    )
endforeach()
