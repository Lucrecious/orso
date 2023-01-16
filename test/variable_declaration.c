#include "test.h"

INTERPRETER_TEST(declaration_i64_default,
    "x: i64; print_expr x;",
    "x (i64) => 0\n")

INTERPRETER_TEST(declaration_i64_implicit,
    "x := 3000000000; print_expr x;",
    "x (i64) => 3000000000\n")

INTERPRETER_TEST(declaration_i64_explicit,
    "x: i64 = 1; print_expr x;",
    "x (i64) => 1\n")


INTERPRETER_TEST(declaration_i32_default,
    "x: i32; print_expr x;",
    "x (i32) => 0\n")

INTERPRETER_TEST(declaration_i32_implicit,
    "x := 1; print_expr x;",
    "x (i32) => 1\n")

INTERPRETER_TEST(declaration_i32_explicit,
    "x: i32 = 42; print_expr x;",
    "x (i32) => 42\n")

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


INTERPRETER_TEST(declaration_symbol_default,
    "x: symbol; print_expr x;",
    "x (symbol) => ''\n")

INTERPRETER_TEST(declaration_symbol_implicit,
    "x := 'foo'; print_expr x;",
    "x (symbol) => 'foo'\n")

INTERPRETER_TEST(declaration_symbol_explicit,
    "x: symbol = 'bar'; print_expr x;",
    "x (symbol) => 'bar'\n")


INTERPRETER_TEST(declaration_string_default,
    "x: string; print_expr x;",
    "x (string) => \n")

INTERPRETER_TEST(declaration_string_implicit,
    "x := \"foo\"; print_expr x;",
    "x (string) => foo\n")

INTERPRETER_TEST(declaration_string_explicit,
    "x: string = \"bar\"; print_expr x;",
    "x (string) => bar\n")


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


INTERPRETER_TEST(declaration_union_explicit_default_void,
    "x: bool | void; print_expr x;",
    "x (bool|void) => null\n")

INTERPRETER_TEST(declaration_union_explicit,
    "x: bool | void = true; print_expr x;",
    "x (bool|void) => true\n")

INTERPRETER_TEST(declaration_union_explicit_no_void,
    "x: bool | string = \"true\"; print_expr x;",
    "x (bool|string) => true\n")


MU_TEST_SUITE(tests) {
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

    MU_RUN_TEST(declaration_union_explicit_default_void);
    MU_RUN_TEST(declaration_union_explicit);
    MU_RUN_TEST(declaration_union_explicit_no_void);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
