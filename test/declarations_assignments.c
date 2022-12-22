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
    test_buffer[0] = '\0'; \
    orso_interpreter_init(&test_interpreter, write_to_test_buffer, NULL); \
} while(false)

#define INTERPRETER_RUN(SOURCE) orso_interpreter_run(&test_interpreter, SOURCE)

#define INTERPRETER_TEARDOWN()  do { \
    orso_interpreter_free(&test_interpreter); \
    clear_test_buffer(); \
} while(false)

#define INTERPRETER_CHECK(EXPECTED) MU_ASSERT_STRING_EQ(EXPECTED, test_buffer)

#define INTERPRETER_TEST(NAME, SOURCE, EXPECTED) MU_TEST(NAME) { \
    INTERPRETER_STARTUP(); \
    INTERPRETER_RUN(SOURCE); \
    INTERPRETER_CHECK(EXPECTED); \
    INTERPRETER_TEARDOWN(); \
}

INTERPRETER_TEST(declaration_i64_default,
    "x: i64; print_expr x;",
    "x (i64) => 0")

INTERPRETER_TEST(declaration_i64_implicit,
    "x := 3000000000; print_expr x;",
    "x (i64) => 3000000000")

INTERPRETER_TEST(declaration_i64_explicit,
    "x: i64 = 1; print_expr x;",
    "x (i64) => 1")

INTERPRETER_TEST(declaration_i32_default,
    "x: i32; print_expr x;",
    "x (i32) => 0")

INTERPRETER_TEST(declaration_i32_implicit,
    "x := 1; print_expr x;",
    "x (i32) => 1")

INTERPRETER_TEST(declaration_i32_explicit,
    "x: i32 = 42; print_expr x;",
    "x (i32) => 42")

INTERPRETER_TEST(declaration_i32_explicit_bool,
    "x: i32 = true; print_expr x;",
    "x (i32) => 1")

INTERPRETER_TEST(declaration_f64_default,
    "x: f64; print_expr x;",
    "x (f64) => 0")

INTERPRETER_TEST(declaration_f64_implicit,
    "x := 0.5; print_expr x;",
    "x (f64) => 0.5")

INTERPRETER_TEST(declaration_f64_explicit,
    "x: f64 = 420.69; print_expr x;",
    "x (f64) => 420.69")

INTERPRETER_TEST(declaration_symbol_default,
    "x: symbol; print_expr x;",
    "x (symbol) => ''")

INTERPRETER_TEST(declaration_symbol_implicit,
    "x := 'foo'; print_expr x;",
    "x (symbol) => 'foo'")

INTERPRETER_TEST(declaration_symbol_explicit,
    "x: symbol = 'bar'; print_expr x;",
    "x (symbol) => 'bar'")

INTERPRETER_TEST(declaration_string_default,
    "x: string; print_expr x;",
    "x (string) => ")

INTERPRETER_TEST(declaration_string_implicit,
    "x := \"foo\"; print_expr x;",
    "x (string) => foo")

INTERPRETER_TEST(declaration_string_explicit,
    "x: string = \"bar\"; print_expr x;",
    "x (string) => bar")

INTERPRETER_TEST(declaration_void_default,
    "x: void; print_expr x;",
    "x (void) => null")

INTERPRETER_TEST(declaration_void_implicit,
    "x := null; print_expr x;",
    "x (void) => null")

INTERPRETER_TEST(declaration_void_explicit,
    "x: void = null; print_expr x;",
    "x (void) => null")

INTERPRETER_TEST(declaration_bool_default,
    "x: bool; print_expr x;",
    "x (bool) => false")

INTERPRETER_TEST(declaration_bool_implicit,
    "x := true; print_expr x;",
    "x (bool) => true")

INTERPRETER_TEST(declaration_bool_explicit,
    "x: bool = false; print_expr x;",
    "x (bool) => false")

MU_TEST_SUITE(all_tests) {
    MU_RUN_TEST(declaration_i64_default);
    MU_RUN_TEST(declaration_i64_implicit);
    MU_RUN_TEST(declaration_i64_explicit);
    MU_RUN_TEST(declaration_i32_default);
    MU_RUN_TEST(declaration_i32_implicit);
    MU_RUN_TEST(declaration_i32_explicit);
    MU_RUN_TEST(declaration_i32_explicit_bool);
    MU_RUN_TEST(declaration_f64_default);
    MU_RUN_TEST(declaration_f64_implicit);
    MU_RUN_TEST(declaration_f64_explicit);
    MU_RUN_TEST(declaration_symbol_default);
    MU_RUN_TEST(declaration_symbol_implicit);
    MU_RUN_TEST(declaration_symbol_explicit);
    MU_RUN_TEST(declaration_string_default);
    MU_RUN_TEST(declaration_string_implicit);
    MU_RUN_TEST(declaration_string_explicit);
    MU_RUN_TEST(declaration_void_default);
    MU_RUN_TEST(declaration_void_implicit);
    MU_RUN_TEST(declaration_void_explicit);
    MU_RUN_TEST(declaration_bool_default);
    MU_RUN_TEST(declaration_bool_implicit);
    MU_RUN_TEST(declaration_bool_explicit);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
