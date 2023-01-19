#include "test.h"

INTERPRETER_TEST(i64_default,
    "x: i64; print_expr x;",
    "x (i64) => 0\n")

INTERPRETER_TEST(i64_implicit,
    "x := 3000000000; print_expr x;",
    "x (i64) => 3000000000\n")

INTERPRETER_TEST(i64_explicit,
    "x: i64 = 1; print_expr x;",
    "x (i64) => 1\n")


INTERPRETER_TEST(local_i64_default,
    "{ x: i64; print_expr x; };",
    "x (i64) => 0\n")

INTERPRETER_TEST(local_i64_implicit,
    "{ x := 3000000000; print_expr x; };",
    "x (i64) => 3000000000\n")

INTERPRETER_TEST(local_i64_explicit,
    "{ x: i64 = 1; print_expr x; };",
    "x (i64) => 1\n")


INTERPRETER_TEST(i32_default,
    "x: i32; print_expr x;",
    "x (i32) => 0\n")

INTERPRETER_TEST(i32_implicit,
    "x := 1; print_expr x;",
    "x (i32) => 1\n")

INTERPRETER_TEST(i32_explicit,
    "x: i32 = 42; print_expr x;",
    "x (i32) => 42\n")

INTERPRETER_TEST(i32_explicit_bool,
    "x: i32 = true; print_expr x;",
    "x (i32) => 1\n")


INTERPRETER_TEST(local_i32_default,
    "{ x: i32; print_expr x; };",
    "x (i32) => 0\n")

INTERPRETER_TEST(local_i32_implicit,
    "{ x := 1; print_expr x; };",
    "x (i32) => 1\n")

INTERPRETER_TEST(local_i32_explicit,
    "{ x: i32 = 42; print_expr x; };",
    "x (i32) => 42\n")

INTERPRETER_TEST(local_i32_explicit_bool,
    "{ x: i32 = true; print_expr x; };",
    "x (i32) => 1\n")


INTERPRETER_TEST(f64_default,
    "x: f64; print_expr x;",
    "x (f64) => 0.0\n")

INTERPRETER_TEST(f64_implicit,
    "x := 0.5; print_expr x;",
    "x (f64) => 0.5\n")

INTERPRETER_TEST(f64_explicit,
    "x: f64 = 420.69; print_expr x;",
    "x (f64) => 420.69\n")


INTERPRETER_TEST(local_f64_default,
    "{ x: f64; print_expr x; };",
    "x (f64) => 0.0\n")

INTERPRETER_TEST(local_f64_implicit,
    "{ x := 0.5; print_expr x; };",
    "x (f64) => 0.5\n")

INTERPRETER_TEST(local_f64_explicit,
    "{ x: f64 = 420.69; print_expr x; };",
    "x (f64) => 420.69\n")


INTERPRETER_TEST(symbol_default,
    "x: symbol; print_expr x;",
    "x (symbol) => ''\n")

INTERPRETER_TEST(symbol_implicit,
    "x := 'foo'; print_expr x;",
    "x (symbol) => 'foo'\n")

INTERPRETER_TEST(symbol_explicit,
    "x: symbol = 'bar'; print_expr x;",
    "x (symbol) => 'bar'\n")


INTERPRETER_TEST(local_symbol_default,
    "{ x: symbol; print_expr x; };",
    "x (symbol) => ''\n")

INTERPRETER_TEST(local_symbol_implicit,
    "{ x := 'foo'; print_expr x; };",
    "x (symbol) => 'foo'\n")

INTERPRETER_TEST(local_symbol_explicit,
    "{ x: symbol = 'bar'; print_expr x; };",
    "x (symbol) => 'bar'\n")


INTERPRETER_TEST(string_default,
    "x: string; print_expr x;",
    "x (string) => \n")

INTERPRETER_TEST(string_implicit,
    "x := \"foo\"; print_expr x;",
    "x (string) => foo\n")

INTERPRETER_TEST(string_explicit,
    "x: string = \"bar\"; print_expr x;",
    "x (string) => bar\n")


INTERPRETER_TEST(local_string_default,
    "{ x: string; print_expr x; };",
    "x (string) => \n")

INTERPRETER_TEST(local_string_implicit,
    "{ x := \"foo\"; print_expr x; };",
    "x (string) => foo\n")

INTERPRETER_TEST(local_string_explicit,
    "{ x: string = \"bar\"; print_expr x; };",
    "x (string) => bar\n")


INTERPRETER_TEST(void_default,
    "x: void; print_expr x;",
    "x (void) => null\n")

INTERPRETER_TEST(void_implicit,
    "x := null; print_expr x;",
    "x (void) => null\n")

INTERPRETER_TEST(void_explicit,
    "x: void = null; print_expr x;",
    "x (void) => null\n")


INTERPRETER_TEST(local_void_default,
    "{ x: void; print_expr x; };",
    "x (void) => null\n")

INTERPRETER_TEST(local_void_implicit,
    "{ x := null; print_expr x; };",
    "x (void) => null\n")

INTERPRETER_TEST(local_void_explicit,
    "{ x: void = null; print_expr x; };",
    "x (void) => null\n")


INTERPRETER_TEST(bool_default,
    "x: bool; print_expr x;",
    "x (bool) => false\n")

INTERPRETER_TEST(bool_implicit,
    "x := true; print_expr x;",
    "x (bool) => true\n")

INTERPRETER_TEST(bool_explicit,
    "x: bool = false; print_expr x;",
    "x (bool) => false\n")


INTERPRETER_TEST(local_bool_default,
    "{ x: bool; print_expr x; };",
    "x (bool) => false\n")

INTERPRETER_TEST(local_bool_implicit,
    "{ x := true; print_expr x; };",
    "x (bool) => true\n")

INTERPRETER_TEST(local_bool_explicit,
    "{ x: bool = false; print_expr x; };",
    "x (bool) => false\n")


INTERPRETER_TEST(union_explicit_default_void,
    "x: bool|void; print_expr x;",
    "x (void) => null\n")

INTERPRETER_TEST(union_explicit,
    "x: bool|void = true; print_expr x;",
    "x (bool) => true\n")

INTERPRETER_TEST(union_explicit_no_void,
    "x: bool|string = \"true\"; print_expr x;",
    "x (string) => true\n")


INTERPRETER_TEST(local_union_explicit_default_void,
    "{ x: bool|void; print_expr x; };",
    "x (void) => null\n")

INTERPRETER_TEST(local_union_explicit,
    "{ x: bool|void = true; print_expr x; };",
    "x (bool) => true\n")

INTERPRETER_TEST(local_union_explicit_no_void,
    "{ x: bool|string = \"true\"; print_expr x; };",
    "x (string) => true\n")


INTERPRETER_TEST(declaration_from_block,
    "x := { 5 }; print_expr x;",
    "x (i32) => 5\n")

INTERPRETER_TEST(declaration_from_block_multiple_expressions,
    "x := 1; y := { x = x + 5; x + 10; }; print_expr x; print_expr y;",
    "x (i32) => 6\ny (i32) => 16\n")

INTERPRETER_TEST(declaration_from_empty_block,
    "foo := {}; print_expr foo;",
    "foo (void) => null\n")

INTERPRETER_TEST(declaration_from_block_only_declarations,
    "foo := { var bar := 0; }; print_expr foo;",
    "foo (void) => null\n")


INTERPRETER_TEST(local_declaration_from_block,
    "{ x := { 5 }; print_expr x; };",
    "x (i32) => 5\n")

INTERPRETER_TEST(local_declaration_from_block_multiple_expressions,
    "{ x := 1; y := { x = x + 5; x + 10; }; print_expr x; print_expr y; };",
    "x (i32) => 6\ny (i32) => 16\n")

INTERPRETER_TEST(local_declaration_from_empty_block,
    "{ foo := {}; print_expr x; };",
    "foo (void) => null\n")

INTERPRETER_TEST(local_declaration_from_block_only_declarations,
    "{ foo := { var bar := 0; }; print_expr foo; };",
    "foo (void) => null\n")


INTERPRETER_TEST(block_implication_variable_declaration_and_then_expression_statement,
    "print_expr { var y := 10; 10; };",
    "{ var y := 10; 10; } (i32) => 10\n")

INTERPRETER_TEST(block_implication_expression_statement_and_then_variable_declaration,
    "print_expr { 10; var y := 10; };",
    "{ 10; var y := 10; } (void) => null\n")


INTERPRETER_ERROR_TEST(missing_end_semicolin,
    "x := 0",
    ORSO_ERROR_COMPILE, 0, "Error at end: Expect end of declaration semicolin.")

INTERPRETER_ERROR_TEST(missing_bar_between_types,
    "x: string i32;",
    ORSO_ERROR_COMPILE, 0, "Error at 'i32': Expect end of declaration semicolin.")

INTERPRETER_ERROR_TEST(type_mismatch,
    "x: string = 0;",
    ORSO_ERROR_COMPILE, 0, "Must cast expression explicitly to match var type.")

INTERPRETER_ERROR_TEST(type_mismatch_union,
    "x: string|bool = 0;",
    ORSO_ERROR_COMPILE, 0, "Type mismatch between expression and declaration.")

INTERPRETER_ERROR_TEST(default_value_required_for_non_void_unions,
    "x: string|i32;",
    ORSO_ERROR_COMPILE, 0, "Non-void union types must have a default value.")

INTERPRETER_ERROR_TEST(undefined_type,
    "x: str;",
    ORSO_ERROR_COMPILE, 0, "Type str does not exist.")

INTERPRETER_ERROR_TEST(too_many_types,
    "x: string|symbol|i64|f64|void = null;",
    ORSO_ERROR_COMPILE, 0, "Orso only allows for a maximum of 4 types in a union.")

INTERPRETER_ERROR_TEST(duplicate_definitions,
    "x: string; x: void;",
    ORSO_ERROR_COMPILE, 0, "Duplicate variable definition of 'x'.")


MU_TEST_SUITE(tests) {
    MU_RUN_TEST(i64_default);
    MU_RUN_TEST(i64_implicit);
    MU_RUN_TEST(i64_explicit);

    MU_RUN_TEST(local_i64_default);
    MU_RUN_TEST(local_i64_implicit);
    MU_RUN_TEST(local_i64_explicit);

    MU_RUN_TEST(i32_default);
    MU_RUN_TEST(i32_implicit);
    MU_RUN_TEST(i32_explicit);
    MU_RUN_TEST(i32_explicit_bool);

    MU_RUN_TEST(local_i32_default);
    MU_RUN_TEST(local_i32_implicit);
    MU_RUN_TEST(local_i32_explicit);
    MU_RUN_TEST(local_i32_explicit_bool);

    MU_RUN_TEST(f64_default);
    MU_RUN_TEST(f64_implicit);
    MU_RUN_TEST(f64_explicit);

    MU_RUN_TEST(local_f64_default);
    MU_RUN_TEST(local_f64_implicit);
    MU_RUN_TEST(local_f64_explicit);

    MU_RUN_TEST(symbol_default);
    MU_RUN_TEST(symbol_implicit);
    MU_RUN_TEST(symbol_explicit);

    MU_RUN_TEST(local_symbol_default);
    MU_RUN_TEST(local_symbol_implicit);
    MU_RUN_TEST(local_symbol_explicit);

    MU_RUN_TEST(string_default);
    MU_RUN_TEST(string_implicit);
    MU_RUN_TEST(string_explicit);

    MU_RUN_TEST(local_string_default);
    MU_RUN_TEST(local_string_implicit);
    MU_RUN_TEST(local_string_explicit);

    MU_RUN_TEST(void_default);
    MU_RUN_TEST(void_implicit);
    MU_RUN_TEST(void_explicit);

    MU_RUN_TEST(local_void_default);
    MU_RUN_TEST(local_void_implicit);
    MU_RUN_TEST(local_void_explicit);

    MU_RUN_TEST(bool_default);
    MU_RUN_TEST(bool_implicit);
    MU_RUN_TEST(bool_explicit);

    MU_RUN_TEST(local_bool_default);
    MU_RUN_TEST(local_bool_implicit);
    MU_RUN_TEST(local_bool_explicit);

    MU_RUN_TEST(union_explicit_default_void);
    MU_RUN_TEST(union_explicit);
    MU_RUN_TEST(union_explicit_no_void);

    MU_RUN_TEST(local_union_explicit_default_void);
    MU_RUN_TEST(local_union_explicit);
    MU_RUN_TEST(local_union_explicit_no_void);

    MU_RUN_TEST(declaration_from_block);
    MU_RUN_TEST(declaration_from_block_multiple_expressions);
    MU_RUN_TEST(declaration_from_empty_block);
    MU_RUN_TEST(declaration_from_block_only_declarations);

    MU_RUN_TEST(local_declaration_from_block);
    MU_RUN_TEST(local_declaration_from_block_multiple_expressions);
    MU_RUN_TEST(local_declaration_from_empty_block);
    MU_RUN_TEST(local_declaration_from_block_only_declarations);

    MU_RUN_TEST(missing_end_semicolin);
    MU_RUN_TEST(missing_bar_between_types);
    MU_RUN_TEST(type_mismatch);
    MU_RUN_TEST(type_mismatch_union);
    MU_RUN_TEST(default_value_required_for_non_void_unions);
    MU_RUN_TEST(undefined_type);
    MU_RUN_TEST(too_many_types);
    MU_RUN_TEST(duplicate_definitions);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
