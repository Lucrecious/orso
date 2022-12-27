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

#define INTERPRETER_STARTUP() do { \
    clear_test_buffer(); \
    orso_interpreter_init(&test_interpreter, write_to_test_buffer, NULL); \
} while(false)

#define INTERPRETER_RUN(SOURCE) orso_interpreter_run(&test_interpreter, SOURCE)

#define INTERPRETER_TEARDOWN()  do { \
    orso_interpreter_free(&test_interpreter); \
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
    "x (i64) => 0\n")

INTERPRETER_TEST(declaration_i64_implicit,
    "x := 3000000000; print_expr x;",
    "x (i64) => 3000000000\n")

INTERPRETER_TEST(declaration_i64_explicit,
    "x: i64 = 1; print_expr x;",
    "x (i64) => 1\n")

INTERPRETER_TEST(assignment_i64,
    "x: i64 = 42; print_expr x; x = 420; print_expr x;",
    "x (i64) => 42\nx (i64) => 420\n");


INTERPRETER_TEST(declaration_i32_default,
    "x: i32; print_expr x;",
    "x (i32) => 0\n")

INTERPRETER_TEST(declaration_i32_implicit,
    "x := 1; print_expr x;",
    "x (i32) => 1\n")

INTERPRETER_TEST(declaration_i32_explicit,
    "x: i32 = 42; print_expr x;",
    "x (i32) => 42\n")

INTERPRETER_TEST(assignment_i32,
    "y := 69; print_expr y; y = 690; print_expr y;",
    "y (i32) => 69\ny (i32) => 690\n")

INTERPRETER_TEST(declaration_i32_explicit_bool,
    "x: i32 = true; print_expr x;",
    "x (i32) => 1\n")


INTERPRETER_TEST(declaration_f64_default,
    "x: f64; print_expr x;",
    "x (f64) => 0\n")

INTERPRETER_TEST(declaration_f64_implicit,
    "x := 0.5; print_expr x;",
    "x (f64) => 0.5\n")

INTERPRETER_TEST(declaration_f64_explicit,
    "x: f64 = 420.69; print_expr x;",
    "x (f64) => 420.69\n")

INTERPRETER_TEST(assignment_f64,
    "x := 420.69; print_expr x; x = 69.42; print_expr x;",
    "x (f64) => 420.69\nx (f64) => 69.42\n")


INTERPRETER_TEST(declaration_symbol_default,
    "x: symbol; print_expr x;",
    "x (symbol) => ''\n")

INTERPRETER_TEST(declaration_symbol_implicit,
    "x := 'foo'; print_expr x;",
    "x (symbol) => 'foo'\n")

INTERPRETER_TEST(declaration_symbol_explicit,
    "x: symbol = 'bar'; print_expr x;",
    "x (symbol) => 'bar'\n")

INTERPRETER_TEST(assignment_symbol,
    "x := 'foo'; print_expr x; x = 'bar'; print_expr x;",
    "x (symbol) => 'foo'\nx (symbol) => 'bar'\n")


INTERPRETER_TEST(declaration_string_default,
    "x: string; print_expr x;",
    "x (string) => \n")

INTERPRETER_TEST(declaration_string_implicit,
    "x := \"foo\"; print_expr x;",
    "x (string) => foo\n")

INTERPRETER_TEST(declaration_string_explicit,
    "x: string = \"bar\"; print_expr x;",
    "x (string) => bar\n")

INTERPRETER_TEST(assignment_string,
    "x := \"foo\"; print_expr x; x = \"bar\"; print_expr x;",
    "x (string) => foo\nx (string) => bar\n")


INTERPRETER_TEST(declaration_void_default,
    "x: void; print_expr x;",
    "x (void) => null\n")

INTERPRETER_TEST(declaration_void_implicit,
    "x := null; print_expr x;",
    "x (void) => null\n")

INTERPRETER_TEST(declaration_void_explicit,
    "x: void = null; print_expr x;",
    "x (void) => null\n")


INTERPRETER_TEST(declaration_bool_default,
    "x: bool; print_expr x;",
    "x (bool) => false\n")

INTERPRETER_TEST(declaration_bool_implicit,
    "x := true; print_expr x;",
    "x (bool) => true\n")

INTERPRETER_TEST(declaration_bool_explicit,
    "x: bool = false; print_expr x;",
    "x (bool) => false\n")

INTERPRETER_TEST(assignment_bool,
    "x := false; print_expr x; x = true; print_expr x;",
    "x (bool) => false\nx (bool) => true\n")


INTERPRETER_TEST(declaration_union_explicit_default_void,
    "x: bool | void; print_expr x;",
    "x (bool|void) => null\n")

INTERPRETER_TEST(declaration_union_explicit,
    "x: bool | void = true; print_expr x;",
    "x (bool|void) => true\n")

INTERPRETER_TEST(declaration_union_explicit_no_void,
    "x: bool | string = \"true\"; print_expr x;",
    "x (bool|string) => true\n")

INTERPRETER_TEST(assignment_union_stack_data,
    "x: bool | i32 = true; print_expr x; x = 32; print_expr x;",
    "x (bool|i32) => true\nx (bool|i32) => 32\n");

INTERPRETER_TEST(assignment_union_object_data,
    "x: string | symbol = 'foo'; print_expr x; x = \"bar\"; print_expr x;",
    "x (string|symbol) => 'foo'\nx (string|symbol) => bar\n");

INTERPRETER_TEST(assignment_union_object_stack_mix_data,
    "x: bool | symbol = 'foo'; print_expr x; x = false; print_expr x;",
    "x (bool|symbol) => 'foo'\nx (bool|symbol) => false\n");


MU_TEST_SUITE(all_tests) {
    MU_RUN_TEST(declaration_i64_default);
    MU_RUN_TEST(declaration_i64_implicit);
    MU_RUN_TEST(declaration_i64_explicit);
    MU_RUN_TEST(assignment_i64);

    MU_RUN_TEST(declaration_i32_default);
    MU_RUN_TEST(declaration_i32_implicit);
    MU_RUN_TEST(declaration_i32_explicit);
    MU_RUN_TEST(assignment_i32);
    MU_RUN_TEST(declaration_i32_explicit_bool);

    MU_RUN_TEST(declaration_f64_default);
    MU_RUN_TEST(declaration_f64_implicit);
    MU_RUN_TEST(declaration_f64_explicit);
    MU_RUN_TEST(assignment_f64);

    MU_RUN_TEST(declaration_symbol_default);
    MU_RUN_TEST(declaration_symbol_implicit);
    MU_RUN_TEST(declaration_symbol_explicit);
    MU_RUN_TEST(assignment_symbol);

    MU_RUN_TEST(declaration_string_default);
    MU_RUN_TEST(declaration_string_implicit);
    MU_RUN_TEST(declaration_string_explicit);
    MU_RUN_TEST(assignment_string);

    MU_RUN_TEST(declaration_void_default);
    MU_RUN_TEST(declaration_void_implicit);
    MU_RUN_TEST(declaration_void_explicit);

    MU_RUN_TEST(declaration_bool_default);
    MU_RUN_TEST(declaration_bool_implicit);
    MU_RUN_TEST(declaration_bool_explicit);
    MU_RUN_TEST(assignment_bool);

    MU_RUN_TEST(declaration_union_explicit_default_void);
    MU_RUN_TEST(declaration_union_explicit);
    MU_RUN_TEST(declaration_union_explicit_no_void);
    MU_RUN_TEST(assignment_union_stack_data);
    MU_RUN_TEST(assignment_union_object_data);
    MU_RUN_TEST(assignment_union_object_stack_mix_data);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
