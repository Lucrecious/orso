#include "test.h"

INTERPRETER_TEST(not_not_string,
	"print not not \"hello world\";",
	"true\n")

INTERPRETER_TEST(not_not_symbol,
	"print not not 'hello world';",
	"true\n")

INTERPRETER_TEST(not_not_void,
	"print not not null;",
	"false\n")

INTERPRETER_TEST(not_not_i32_zero,
	"print not not 0;",
	"false\n")

INTERPRETER_TEST(not_not_i32_non_zero,
	"print not not 42;",
	"true\n")

INTERPRETER_TEST(not_not_f64_zero,
	"print not not 0.0;",
	"false\n")

INTERPRETER_TEST(not_not_f64_non_zero,
	"print not not 42.0;",
	"true\n")

INTERPRETER_TEST(not_not_false,
	"print not not false;",
	"false\n")

INTERPRETER_TEST(not_not_true,
	"print not not true;",
	"true\n")

INTERPRETER_TEST(not_not_string_void_string,
	"x := if true { \"hello world\"; };\nprint_expr x;\nprint not not x;",
	"x (string|void) => hello world\ntrue\n")

INTERPRETER_TEST(not_not_string_void_void,
	"x := if false { \"hello world\"; };\nprint_expr x;\nprint not not x;",
	"x (string|void) => null\nfalse\n")

INTERPRETER_TEST(or_define,
	"x := \"hello world\" or null; print_expr x;",
	"x (string|void) => hello world\n")

INTERPRETER_TEST(and_define,
	"x := \"\" and null; print_expr x;",
	"x (string|void) => null\n")

INTERPRETER_TEST(or_shortcircuit_on_block,
	"x := false or { x := 10; y := 2; x * y; } or false;\nprint x;",
	"20\n")

INTERPRETER_TEST(and_return_last,
	"x := true and  { x := 10; y := 2; x * y; } and false;\nprint x;",
	"false\n")

INTERPRETER_TEST(and_and_ors_infers_full_type,
	"y := \"\" or null;\ny = \"hello\";\nx := true and y and false;\nprint_expr x;",
	"x (string|bool|void) => false\n")

INTERPRETER_TEST(bracketed_expressions_narrows_type,
	"y := \"\" or null;\ny = \"hello\";\nx := true and (y) and false;\nprint_expr x;",
	"x (string|bool) => false\n")

INTERPRETER_TEST(or_union_with_different_types,
    "x := 42 or \"hello world\"; print_expr x;",
    "x (string|i32) => 42\n")

INTERPRETER_TEST(and_union_with_different_types,
    "x := 42 and \"hello world\"; print_expr x;",
    "x (string|i32) => hello world\n")

INTERPRETER_TEST(or_nested_unions,
    "x := 42 or (\"hello world\" or 3.14); print_expr x;",
    "x (string|f64|i32) => 42\n")

INTERPRETER_TEST(and_nested_unions,
    "x := 42 and (\"hello world\" or 3.14); print_expr x;",
    "x (string|f64|i32) => hello world\n")

INTERPRETER_TEST(or_union_with_void,
    "x := null or (\"hello world\" or 42); print_expr x;",
    "x (string|i32|void) => hello world\n")

INTERPRETER_TEST(and_union_with_void,
    "x := null and (\"hello world\" or 42); print_expr x;",
    "x (string|i32|void) => null\n")

INTERPRETER_TEST(or_union_returning_different_types,
    "x := false or (if true { 42; } else { \"hello world\"; }); print_expr x;",
    "x (string|i32|bool) => 42\n")

INTERPRETER_TEST(and_union_returning_different_types,
    "x := true and (if true { 42; } else { \"hello world\"; }); print_expr x;",
    "x (string|i32|bool) => 42\n")

INTERPRETER_TEST(or_union_with_multiple_conditions,
    "x := (false or 42) and (true or \"hello world\"); print_expr x;",
    "x (string|i32|bool) => true\n")

INTERPRETER_TEST(and_union_with_multiple_conditions,
    "x := (true and 42) or (false and \"hello world\"); print_expr x;",
    "x (string|i32|bool) => 42\n")

INTERPRETER_TEST(or_union_with_multiple_conditions_void,
    "x := (false or null) and (true or \"hello world\"); print_expr x;",
    "x (string|bool|void) => null\n")

INTERPRETER_TEST(and_union_with_multiple_conditions_void,
    "x := (true and null) or (false and \"hello world\"); print_expr x;",
    "x (string|bool|void) => false\n")


MU_TEST_SUITE(tests) {
    MU_RUN_TEST(not_not_string);
    MU_RUN_TEST(not_not_symbol);
    MU_RUN_TEST(not_not_void);
    MU_RUN_TEST(not_not_i32_zero);
    MU_RUN_TEST(not_not_i32_non_zero);
    MU_RUN_TEST(not_not_f64_zero);
    MU_RUN_TEST(not_not_f64_non_zero);
    MU_RUN_TEST(not_not_false);
    MU_RUN_TEST(not_not_true);
    MU_RUN_TEST(not_not_string_void_string);
    MU_RUN_TEST(not_not_string_void_void);

    MU_RUN_TEST(or_define);
    MU_RUN_TEST(and_define);
    MU_RUN_TEST(or_shortcircuit_on_block);
    MU_RUN_TEST(and_return_last);
    MU_RUN_TEST(and_and_ors_infers_full_type);
    MU_RUN_TEST(bracketed_expressions_narrows_type);

    MU_RUN_TEST(or_union_with_different_types);
    MU_RUN_TEST(and_union_with_different_types);
    MU_RUN_TEST(or_nested_unions);
    MU_RUN_TEST(and_nested_unions);
    MU_RUN_TEST(or_union_with_void);
    MU_RUN_TEST(and_union_with_void);
    MU_RUN_TEST(or_union_returning_different_types);
    MU_RUN_TEST(and_union_returning_different_types);
    MU_RUN_TEST(or_union_with_multiple_conditions);
    MU_RUN_TEST(and_union_with_multiple_conditions);
    MU_RUN_TEST(or_union_with_multiple_conditions_void);
    MU_RUN_TEST(and_union_with_multiple_conditions_void);
}
int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
