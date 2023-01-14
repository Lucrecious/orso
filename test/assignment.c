#include "test.h"

#define TEST_MAIN

INTERPRETER_TEST(assignment_i64,
    "x: i64 = 42; print_expr x; x = 420; print_expr x;",
    "x (i64) => 42\nx (i64) => 420\n");

INTERPRETER_TEST(assignment_i32,
    "y := 69; print_expr y; y = 690; print_expr y;",
    "y (i32) => 69\ny (i32) => 690\n")

INTERPRETER_TEST(assignment_f64,
    "x := 420.69; print_expr x; x = 69.42; print_expr x;",
    "x (f64) => 420.69\nx (f64) => 69.42\n")

INTERPRETER_TEST(assignment_symbol,
    "x := 'foo'; print_expr x; x = 'bar'; print_expr x;",
    "x (symbol) => 'foo'\nx (symbol) => 'bar'\n")

INTERPRETER_TEST(assignment_string,
    "x := \"foo\"; print_expr x; x = \"bar\"; print_expr x;",
    "x (string) => foo\nx (string) => bar\n")

INTERPRETER_TEST(assignment_void,
    "x := null; print_expr x; x = null; print_expr x;",
    "x (void) => null\nx (void) => null\n")

INTERPRETER_TEST(assignment_bool,
    "x := false; print_expr x; x = true; print_expr x;",
    "x (bool) => false\nx (bool) => true\n")

INTERPRETER_TEST(assignment_variable_to_variable,
    "x := 1; y := 2; x = y; print_expr x; print_expr y;",
    "x (i32) => 2\ny (i32) => 2\n")


INTERPRETER_TEST(assignment_union_stack_data,
    "x: bool | i32 = true; print_expr x; x = 32; print_expr x;",
    "x (bool|i32) => true\nx (bool|i32) => 32\n");

INTERPRETER_TEST(assignment_union_object_data,
    "x: string | symbol = 'foo'; print_expr x; x = \"bar\"; print_expr x;",
    "x (string|symbol) => 'foo'\nx (string|symbol) => bar\n");

INTERPRETER_TEST(assignment_union_object_stack_mix_data,
    "x: bool | symbol = 'foo'; print_expr x; x = false; print_expr x;",
    "x (bool|symbol) => 'foo'\nx (bool|symbol) => false\n");

INTERPRETER_TEST(assignment_union_to_single,
    "foo: symbol; bar: symbol|void = 'foobar'; foo = bar; print_expr foo;",
    "foo (symbol) => 'foobar'\n");

INTERPRETER_TEST(assignment_union_to_single_after_change,
    "foo: symbol; bar: symbol|void; print_expr bar; bar = 'foobar'; foo = bar; print_expr foo;",
    "bar (symbol|void) => null\nfoo (symbol) => 'foobar'");

INTERPRETER_TEST(assignment_union_to_union,
    "foo: bool|void = false; bar: bool|void = true; foo = bar; print_expr foo;",
    "foo (bool|void) => true\n");



MU_TEST_SUITE(tests) {
    MU_RUN_TEST(assignment_i64);
    MU_RUN_TEST(assignment_i32);
    MU_RUN_TEST(assignment_f64);
    MU_RUN_TEST(assignment_symbol);
    MU_RUN_TEST(assignment_string);
    MU_RUN_TEST(assignment_void);
    MU_RUN_TEST(assignment_bool);
    MU_RUN_TEST(assignment_variable_to_variable);
    MU_RUN_TEST(assignment_union_stack_data);
    MU_RUN_TEST(assignment_union_object_data);
    MU_RUN_TEST(assignment_union_object_stack_mix_data);
    MU_RUN_TEST(assignment_union_to_single);
    MU_RUN_TEST(assignment_union_to_single_after_change);
    MU_RUN_TEST(assignment_union_to_union);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}