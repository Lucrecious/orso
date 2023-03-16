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
	"null")

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
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
