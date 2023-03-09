#include "test.h"

INTERPRETER_TEST(assign_i64,
    "x: i64 = 42; print_expr x; x = 420; print_expr x;",
    "x (i64) => 42\nx (i64) => 420\n")

INTERPRETER_TEST(assign_i32,
    "y := 69; print_expr y; y = 690; print_expr y;",
    "y (i32) => 69\ny (i32) => 690\n")

INTERPRETER_TEST(assign_f64,
    "x := 420.69; print_expr x; x = 69.42; print_expr x;",
    "x (f64) => 420.69\nx (f64) => 69.42\n")

INTERPRETER_TEST(assign_symbol,
    "x := 'foo'; print_expr x; x = 'bar'; print_expr x;",
    "x (symbol) => 'foo'\nx (symbol) => 'bar'\n")

INTERPRETER_TEST(assign_string,
    "x := \"foo\"; print_expr x; x = \"bar\"; print_expr x;",
    "x (string) => foo\nx (string) => bar\n")

INTERPRETER_TEST(assign_void,
    "x := null; print_expr x; x = null; print_expr x;",
    "x (void) => null\nx (void) => null\n")

INTERPRETER_TEST(assign_bool,
    "x := false; print_expr x; x = true; print_expr x;",
    "x (bool) => false\nx (bool) => true\n")

INTERPRETER_TEST(assign_variable_to_variable,
    "x := 1; y := 2; x = y; print_expr x; print_expr y;",
    "x (i32) => 2\ny (i32) => 2\n")


INTERPRETER_TEST(assign_union_stack_data,
    "x: bool|i32 = true; print_expr x; x = 32; print_expr x;",
    "x (bool|i32) => true\nx (bool|i32) => 32\n")

INTERPRETER_TEST(assign_union_object_data,
    "x: string|symbol = 'foo'; print_expr (x); x = \"bar\"; print_expr (x);",
    "(x) (symbol) => 'foo'\n(x) (string) => bar\n")

INTERPRETER_TEST(assign_union_object_stack_mix_data,
    "x: bool|symbol = 'foo'; print_expr (x); x = false; print_expr (x);",
    "(x) (symbol) => 'foo'\n(x) (bool) => false\n")

INTERPRETER_TEST(assign_union_to_single,
    "foo: symbol; bar: symbol|void = 'foobar'; foo = bar; print_expr foo;",
    "foo (symbol) => 'foobar'\n")

INTERPRETER_TEST(assign_union_to_single_after_change,
    "foo: symbol; bar: symbol|void; print_expr bar; bar = 'foobar'; foo = bar; print_expr foo;",
    "bar (symbol|void) => null\nfoo (symbol) => 'foobar'\n")

INTERPRETER_TEST(assign_union_to_union,
    "foo: bool|void = false; bar: bool|void = true; foo = bar; print_expr foo;",
    "foo (bool|void) => true\n")

INTERPRETER_TEST(assign_inside_expressions,
    "foo: i32|void = null; bar := 42; foobar := (foo = 1) + (bar = foo); print_expr foobar; print_expr bar;",
    "foobar (i32) => 2\nbar (i32) => 1\n")

INTERPRETER_TEST(assign_inside_expressions2,
    "foo := 42; bar: i32|void = null; print_expr bar = 1; print_expr foo = bar + foo; print_expr foo;",
    "bar = 1 (i32|void) => 1\nfoo = bar + foo (i32) => 43\nfoo (i32) => 43\n")

INTERPRETER_TEST(assign_inside_expressions3,
    "foo: symbol|i32 = 'hello'; bar: i32|void = null; baz := 0; print_expr baz = bar = foo = 10;",
    "baz = bar = foo = 10 (i32) => 10\n")


INTERPRETER_ERROR_TEST(undefined_variable,
    "x = 0;",
    ORSO_ERROR_COMPILE, 0, "Variable does not exist.")

INTERPRETER_ERROR_TEST(assign_type_mismatch,
    "x := 0; x = 'foo';",
    ORSO_ERROR_COMPILE, 0, "Expression needs explicit cast to store in variable.")

INTERPRETER_ERROR_TEST(assign_type_mismatch_union,
    "x: string|bool = true; x = 42;",
    ORSO_ERROR_COMPILE, 0, "Expression needs explicit cast to store in variable.")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(assign_i64);
    MU_RUN_TEST(assign_i32);
    MU_RUN_TEST(assign_f64);
    MU_RUN_TEST(assign_symbol);
    MU_RUN_TEST(assign_string);
    MU_RUN_TEST(assign_void);
    MU_RUN_TEST(assign_bool);
    MU_RUN_TEST(assign_variable_to_variable);
    MU_RUN_TEST(assign_union_stack_data);
    MU_RUN_TEST(assign_union_object_data);
    MU_RUN_TEST(assign_union_object_stack_mix_data);
    MU_RUN_TEST(assign_union_to_union);
    MU_RUN_TEST(assign_union_to_single);
    MU_RUN_TEST(assign_union_to_single_after_change);
    MU_RUN_TEST(assign_inside_expressions);
    MU_RUN_TEST(assign_inside_expressions2);
    MU_RUN_TEST(assign_inside_expressions3);

    MU_RUN_TEST(undefined_variable);
    MU_RUN_TEST(assign_type_mismatch);
    MU_RUN_TEST(assign_type_mismatch_union);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
