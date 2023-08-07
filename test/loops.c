#include "test.h"

INTERPRETER_TEST(while_countdown,
	"count := 3; while count { print count; count = count - 1; };",
	"3\n2\n1\n")

INTERPRETER_TEST(while_else,
	"count := 3; while count { print count; count = count - 1; } else { print 'done!'; };",
	"3\n2\n1\n'done!'\n")

INTERPRETER_TEST(until_countup,
	"count := 0; until count > 2 { print count; count = count + 1; };",
	"0\n1\n2\n")

INTERPRETER_TEST(until_else,
	"count := 0; until count > 2 { print count; count = count + 1; } else { print 'done!'; };",
	"0\n1\n2\n'done!'\n")

INTERPRETER_TEST(while_inside_expression,
	"count := 0; x := while count < 2 { count = count + 1; } else { 0; } + 1; print x;",
	"1\n")

INTERPRETER_TEST(until_inside_expression,
	"count := 0; x := until count > 2 { count = count + 1; } else { 0; } + 1; print x;",
	"1\n")

INTERPRETER_TEST(while_no_else_type,
	"x := while false { 42; }; print_expr x;",
	"x (i32|void) => null\n")

INTERPRETER_TEST(while_with_else_type,
	"x := while false { 42; } else { 'hello world'; }; print_expr x;",
	"x (symbol|i32) => 'hello world'\n")

INTERPRETER_TEST(until_no_else_type,
	"x := until true { 42; }; print_expr x;",
	"x (i32|void) => null\n")

INTERPRETER_TEST(until_with_else_type,
	"x := until true { 42; } else { 'hello world'; }; print_expr x;",
	"x (symbol|i32) => 'hello world'\n")

INTERPRETER_TEST(no_braces_no_else,
	"y := 2; while y = y - 1 do print 1;",
	"1\n")

INTERPRETER_TEST(no_braces_else,
	"y := 2; while y = y - 1 do print 1 else print 2;",
	"1\n2\n")

INTERPRETER_TEST(no_braces_more_branches,
	"y := -2; until not (y = y + 1) do print 1 else if true then print 2 else print 3;",
	"1\n2\n")

INTERPRETER_TEST(no_braces_mix_branches,
	"y := -2; until not (y = y + 1) do print 1 else unless false { print 2; } else print 3;",
	"1\n2\n")

INTERPRETER_TEST(cannot_use_then_instead_of_do,
	"y := -2; until not (y = y + 1) then print 1 else unless false { print 2; } else print 3;",
	"1\n2\n")

// INTERPRETER_TEST(for_in_integer,
// 	"for i in 3 { print i; }",
// 	"0\n1\n2\n")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(while_countdown);
    MU_RUN_TEST(while_else);
    MU_RUN_TEST(until_countup);
    MU_RUN_TEST(until_else);
    MU_RUN_TEST(while_inside_expression);
    MU_RUN_TEST(until_inside_expression);
    MU_RUN_TEST(while_no_else_type);
    MU_RUN_TEST(while_with_else_type);
    MU_RUN_TEST(until_no_else_type);
    MU_RUN_TEST(until_with_else_type);
    MU_RUN_TEST(no_braces_no_else);
    MU_RUN_TEST(no_braces_else);
    MU_RUN_TEST(no_braces_more_branches);
    MU_RUN_TEST(no_braces_mix_branches);
    MU_RUN_TEST(cannot_use_then_instead_of_do);
    //MU_RUN_TEST(for_in_integer);
}
int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
