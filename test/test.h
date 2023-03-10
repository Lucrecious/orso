#ifndef TEST_H_
#define TEST_H_

#include <stdio.h>

#include "minunit.h"

#include "def.h"
#include "virtual_machine.h"
#include "interpreter.h"
#include "error_codes.h"

char test_buffer[1024];
i32 buffer_length;

char test_error_buffer[1024];
i32 error_buffer_length;
OrsoErrorType error_type;
i32 error_line;

void write_to_test_buffer(const char* chars) {
    buffer_length += sprintf(test_buffer + buffer_length, "%s", chars);
}

void write_to_test_error_buffer(OrsoErrorType type, i32 line, const char* message) {
    error_buffer_length += sprintf(test_error_buffer + error_buffer_length, "%s", message);
    error_type = type;
    error_line = line;
}

void clear_test_buffer() {
    buffer_length = 0;
    test_buffer[0] = '\0';
}

void clear_test_error_buffer() {
    error_buffer_length = 0;
    test_error_buffer[0] = '\0';
    error_type = -1;
    error_line = -1;
}

#define INTERPRETER_TEST(NAME, SOURCE, EXPECTED) MU_TEST(NAME) { \
    OrsoInterpreter test_interpreter; \
    clear_test_buffer(); \
    orso_interpreter_init(&test_interpreter, write_to_test_buffer, NULL); \
    orso_interpreter_run(&test_interpreter, SOURCE); \
    orso_interpreter_free(&test_interpreter); \
    MU_ASSERT_STRING_EQ(EXPECTED, test_buffer); \
}

#define INTERPRETER_ERROR_TEST(NAME, SOURCE, ERROR_TYPE, LINE, MESSAGE) MU_TEST(NAME) { \
    OrsoInterpreter test_interpreter; \
    clear_test_error_buffer(); \
    orso_interpreter_init(&test_interpreter, NULL, write_to_test_error_buffer); \
    orso_interpreter_run(&test_interpreter, SOURCE); \
    orso_interpreter_free(&test_interpreter); \
    MU_ASSERT_STRING_EQ(MESSAGE, test_error_buffer); \
    MU_ASSERT_INT_EQ(ERROR_TYPE, error_type); \
    MU_ASSERT_INT_EQ(LINE, error_line) ;\
}

#endif
