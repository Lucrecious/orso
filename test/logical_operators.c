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
}
int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
