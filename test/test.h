#ifndef TEST_H_
#define TEST_H_

#include <stdio.h>

#include "minunit.h"

#include "def.h"
#include "virtual_machine.h"
#include "interpreter.h"
#include "error.h"

char test_buffer[1024];
i32 buffer_length;

char test_error_buffer[1024];
i32 error_buffer_length;
OrsoError error;

void clear_test_buffer(void) {
    buffer_length = 0;
    test_buffer[0] = '\0';
}

void clear_test_error_buffer(void) {
    error_buffer_length = 0;
    test_error_buffer[0] = '\0';
}

void write_to_test_buffer(const char* chars) {
    buffer_length += snprintf(test_buffer + buffer_length, 1024 - buffer_length, "%s", chars);
}

void write_to_test_error_buffer(OrsoError error) {
    error_buffer_length += snprintf(test_error_buffer + error_buffer_length, 1024 - buffer_length, "%s", error.message);
}

#define INTERPRETER_TEST(NAME, SOURCE, EXPECTED) MU_TEST(NAME) { \
    clear_test_buffer(); \
    OrsoVM vm; \
    orso_vm_init(&vm, write_to_test_buffer); \
    orso_run_source(&vm, SOURCE, NULL); \
    orso_vm_free(&vm); \
    MU_ASSERT_STRING_EQ(EXPECTED, test_buffer); \
}

#define INTERPRETER_ERROR_TEST(NAME, SOURCE, ERROR_TYPE, LINE, MESSAGE) MU_TEST(NAME) { \
    clear_test_error_buffer(); \
    OrsoVM vm; \
    orso_vm_init(&vm, NULL); \
    orso_run_source(&vm, SOURCE, write_to_test_error_buffer); \
    orso_vm_free(&vm); \
    MU_ASSERT_STRING_EQ(MESSAGE, test_error_buffer); \
}
    // MU_ASSERT_INT_EQ(ERROR_TYPE, error_type);
    // MU_ASSERT_INT_EQ(LINE, error_line) ;

#endif
