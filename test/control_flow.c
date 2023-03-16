#include "test.h"

INTERPRETER_TEST(then_branch_print,
    "if true { print 'then'; };",
    "'then'\n")

INTERPRETER_TEST(else_branch_print,
    "if false { print 'then'; } else { print 'else'; };",
    "'else'\n")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(then_branch_print);
    MU_RUN_TEST(else_branch_print);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
