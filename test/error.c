
#include "test.h"


INTERPRETER_ERROR_TEST(invalid_character,
    "x: `",
    ORSO_ERROR_COMPILE, 0, "Error: Unexpected character.")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(invalid_character);
}

int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
