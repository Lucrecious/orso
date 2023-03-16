#include "test.h"

INTERPRETER_TEST(test,
	"x := if true { \"hello world\"; };\nx = null;\nif not x {\n	print x;\n};",
	"null")

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
