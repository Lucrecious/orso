#include "test.h"

INTERPRETER_TEST(constant_add_i32s,
    "print_expr 1 + 1;",
    "1 + 1 (i32) => 2\n")

INTERPRETER_TEST(constant_add_f64s,
    "print_expr 1.0 + 1.0;",
    "1.0 + 1.0 (f64) => 2.0\n")

INTERPRETER_TEST(constant_add_i32_and_f64,
    "print_expr 1 + 1.0;",
    "1 + 1.0 (f64) => 2.0\n")

INTERPRETER_TEST(constant_add_strings,
    "print_expr \"hello \" + \"world\";",
    "\"hello \" + \"world\" (string) => hello world\n")

INTERPRETER_TEST(constant_add_bools,
    "print_expr true + true;",
    "true + true (i32) => 2\n")


INTERPRETER_TEST(constant_add_bool_and_i32,
    "print_expr true + 10;",
    "true + 10 (i32) => 11\n")

INTERPRETER_TEST(constant_sub_i32s,
    "print_expr 2 - 1;",
    "2 - 1 (i32) => 1\n")

INTERPRETER_TEST(constant_sub_f64s,
    "print_expr 5.5 - 1.7;",
    "5.5 - 1.7 (f64) => 3.8\n")

INTERPRETER_TEST(constant_sub_i32_and_f64,
    "print_expr 40 - 1.5;",
    "40 - 1.5 (f64) => 38.5\n")

INTERPRETER_TEST(constant_sub_bools,
    "print_expr true - true;",
    "true - true (i32) => 0\n")

INTERPRETER_TEST(constant_sub_bool_and_i32,
    "print_expr true - 10;",
    "true - 10 (i32) => -9\n")


INTERPRETER_TEST(constant_mul_i32s,
    "print_expr 2 * 3;",
    "2 * 3 (i32) => 6\n")

INTERPRETER_TEST(constant_mul_f64s,
    "print_expr 5.5 * 2.0;",
    "5.5 * 2.0 (f64) => 11.0\n")

INTERPRETER_TEST(constant_mul_i32_and_f64,
    "print_expr 40 * 1.5;",
    "40 * 1.5 (f64) => 60.0\n")

INTERPRETER_TEST(constant_mul_bools,
    "print_expr true * true;",
    "true * true (i32) => 1\n")

INTERPRETER_TEST(constant_mul_bool_and_i32,
    "print_expr true * 10;",
    "true * 10 (i32) => 10\n")


INTERPRETER_TEST(constant_div_i32s,
    "print_expr 9 / 3;",
    "9 / 3 (i32) => 3\n")

INTERPRETER_TEST(constant_div_f64s,
    "print_expr 5.5 / 0.5;",
    "5.5 / 0.5 (f64) => 11.0\n")

INTERPRETER_TEST(constant_div_i32_and_f64,
    "print_expr 40 / 0.5;",
    "40 / 0.5 (f64) => 80.0\n")

INTERPRETER_TEST(constant_div_bools,
    "print_expr true / true;",
    "true / true (i32) => 1\n")

INTERPRETER_TEST(constant_div_bool_and_i32,
    "print_expr true / 10;",
    "true / 10 (i32) => 0\n")


INTERPRETER_TEST(constant_negate_i32,
    "print_expr -3;",
    "-3 (i32) => -3\n")

INTERPRETER_TEST(constant_negate_f64,
    "print_expr -0.5;",
    "-0.5 (f64) => -0.5\n")

INTERPRETER_TEST(constant_negate_bool,
    "print_expr -true;",
    "-true (i32) => -1\n")


INTERPRETER_TEST(variable_add,
    "foo := 60; bar := 9; print_expr foo + bar;",
    "foo + bar (i32) => 69\n")

INTERPRETER_TEST(variable_add_unions,
    "foo: i32|void = 60; bar: i32|void = 9; print_expr foo + bar;",
    "foo + bar (i32) => 69\n")

INTERPRETER_TEST(variable_add_unions_after_assign,
    "foo: i32|void = null; bar: i32|void = 9; print_expr (foo = 60) + bar;",
    "(foo = 60) + bar (i32) => 69\n")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(constant_add_i32s);
    MU_RUN_TEST(constant_add_f64s);
    MU_RUN_TEST(constant_add_i32_and_f64);
    MU_RUN_TEST(constant_add_strings);
    MU_RUN_TEST(constant_add_bools);
    MU_RUN_TEST(constant_add_bool_and_i32);

    MU_RUN_TEST(constant_sub_i32s);
    MU_RUN_TEST(constant_sub_f64s);
    MU_RUN_TEST(constant_sub_i32_and_f64);
    MU_RUN_TEST(constant_sub_bools);
    MU_RUN_TEST(constant_sub_bool_and_i32);

    MU_RUN_TEST(constant_mul_i32s);
    MU_RUN_TEST(constant_mul_f64s);
    MU_RUN_TEST(constant_mul_i32_and_f64);
    MU_RUN_TEST(constant_mul_bools);
    MU_RUN_TEST(constant_mul_bool_and_i32);

    MU_RUN_TEST(constant_div_i32s);
    MU_RUN_TEST(constant_div_f64s);
    MU_RUN_TEST(constant_div_i32_and_f64);
    MU_RUN_TEST(constant_div_bools);
    MU_RUN_TEST(constant_div_bool_and_i32);

    MU_RUN_TEST(constant_negate_i32);
    MU_RUN_TEST(constant_negate_f64);
    MU_RUN_TEST(constant_negate_bool);

    MU_RUN_TEST(variable_add);
    MU_RUN_TEST(variable_add_unions);
    MU_RUN_TEST(variable_add_unions_after_assign);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
