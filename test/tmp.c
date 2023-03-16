#include "test.h"

INTERPRETER_TEST(test,
    "x: i32|void = 42; y := -x; print_expr y;",
    "y (i32) => -42\n")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(test);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
