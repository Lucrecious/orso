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
}

OrsoInterpreter test_interpreter;

#define INTERPRETER_STARTUP() do { \
    orso_interpreter_init(&test_interpreter, write_to_test_buffer, NULL); \
} while(false)

#define INTERPRETER_RUN(SOURCE) orso_interpreter_run(&test_interpreter, SOURCE)

#define INTERPRETER_TEARDOWN()  do { \
    orso_interpreter_free(&test_interpreter); \
    clear_test_buffer(); \
} while(false)

MU_TEST(declaration_i32_explicit) {
    INTERPRETER_STARTUP();

    INTERPRETER_RUN("x: i64 = 1; print_expr x;");

    MU_ASSERT_STRING_EQ("x => 1", test_buffer);

    INTERPRETER_TEARDOWN();
}

MU_TEST(declaration_i32_implicit) {
    INTERPRETER_STARTUP();

    INTERPRETER_RUN("x := 1; print_expr x;");

    MU_ASSERT_STRING_EQ("x => 1", test_buffer);

    INTERPRETER_TEARDOWN();
}

MU_TEST_SUITE(all_tests) {
    MU_RUN_TEST(declaration_i32_explicit);
    MU_RUN_TEST(declaration_i32_implicit);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
