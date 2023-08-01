#include "test.h"

INTERPRETER_TEST(then_branch_print,
    "if true { print 'then'; };",
    "'then'\n")

INTERPRETER_TEST(else_branch_print,
    "if false { print 'then'; } else { print 'else'; };",
    "'else'\n")

INTERPRETER_TEST(unless_branch_print,
    "unless false { print 'unless'; };",
    "'unless'\n")

INTERPRETER_TEST(unless_else_branch_print,
    "unless true { print 'unless'; } else { print 'else'; };",
    "'else'\n")

INTERPRETER_TEST(elseif_branch_print,
    "if false { print 'then'; } else if true { print 'elseif'; } else { print 'else'; };",
    "'elseif'\n")

INTERPRETER_TEST(elseunless_branch_print,
    "if false { print 'then'; } else unless false { print 'unlessif'; } else { print 'else'; };",
    "'unlessif'\n")

INTERPRETER_TEST(if_union_condition_not_null,
	"x := if true { \"hello world\"; };\nif x {\n	print x;\n};",
	"hello world\n")

INTERPRETER_TEST(if_union_condition_is_null,
	"x := if true { \"hello world\"; };\nx = null;\nif not x {\n	print x;\n};",
	"null\n")

INTERPRETER_TEST(multiple_if_statements,
    "if true { print 'first'; };\nif true { print 'second'; };",
    "'first'\n'second'\n")

INTERPRETER_TEST(if_nested_else_if,
    "if false { print 'first'; } else if false { print 'second'; } else if true { print 'third'; };",
    "'third'\n")

INTERPRETER_TEST(unless_nested_else_unless,
    "unless true { print 'first'; } else unless true { print 'second'; } else { print 'third'; };",
    "'third'\n")

INTERPRETER_TEST(unless_nested_else_if,
    "unless true { print 'first'; } else if true { print 'second'; } else { print 'third'; };",
    "'second'\n")

INTERPRETER_TEST(if_else_nested_if,
    "if false { print 'first'; } else { if true { print 'second'; } else { print 'third'; }; };",
    "'second'\n")

INTERPRETER_TEST(if_else_nested_unless,
    "if false { print 'first'; } else { unless false { print 'second'; } else { print 'third'; }; };",
    "'second'\n")

INTERPRETER_TEST(if_condition_is_string,
	"x := \"hello\";\nif x { print x; };",
	"hello\n")

INTERPRETER_TEST(else_ifset,
	"x := 2;\ny := 1;\nz := if x < y { x - y; } else { y - x; };\nprint z;",
	"-1\n")

INTERPRETER_TEST(no_braces_no_else,
	"y := 10; x := if y < 20 then print 1;",
	"1\n")

INTERPRETER_TEST(no_braces_else,
	"y := 10; x := if y < 20 then print 1 else print 2;",
	"1\n")

INTERPRETER_TEST(no_braces_more_branches,
	"y := 10; x := unless y < 5 then print 1 else if x < 20 then print 2 else print 3; print x;",
	"1\nnull\n")

INTERPRETER_TEST(no_braces_mix_branches,
	"y := 10; x := if y < 5 then print 1 else unless x < 20 { print 2; } else print 3; print x;",
	"3\nnull\n")

INTERPRETER_TEST(cannot_use_do_instead_of_then,
	"y := 10; x := if y < 5 do print 1 else unless x < 20 { print 2; } else print 3; print x;",
	"3\nnull\n")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(then_branch_print);
    MU_RUN_TEST(else_branch_print);
    MU_RUN_TEST(unless_branch_print);
    MU_RUN_TEST(unless_else_branch_print);
    MU_RUN_TEST(elseif_branch_print);
    MU_RUN_TEST(elseunless_branch_print);
    MU_RUN_TEST(if_union_condition_not_null);
    MU_RUN_TEST(if_union_condition_is_null);
    MU_RUN_TEST(multiple_if_statements);
    MU_RUN_TEST(if_nested_else_if);
    MU_RUN_TEST(unless_nested_else_unless);
    MU_RUN_TEST(unless_nested_else_if);
    MU_RUN_TEST(if_else_nested_if);
    MU_RUN_TEST(if_else_nested_unless);
    MU_RUN_TEST(if_condition_is_string);
    MU_RUN_TEST(else_ifset);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
