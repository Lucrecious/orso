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

void write_to_test_buffer(const char* chars) {
    buffer_length += sprintf(test_buffer + buffer_length, "%s", chars);
}

void clear_test_buffer() {
    buffer_length = 0;
    test_buffer[0] = '\0';
}

OrsoInterpreter test_interpreter;

#define INTERPRETER_TEST(NAME, SOURCE, EXPECTED) MU_TEST(NAME) { \
    clear_test_buffer(); \
    orso_interpreter_init(&test_interpreter, write_to_test_buffer, NULL); \
\
    orso_interpreter_run(&test_interpreter, SOURCE); \
\
    orso_interpreter_free(&test_interpreter); \
\
    MU_ASSERT_STRING_EQ(EXPECTED, test_buffer); \
}

#endif