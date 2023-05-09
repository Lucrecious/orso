#include "test.h"

INTERPRETER_TEST(while_countdown,
	"count := 3; while count { print count; count = count - 1; };",
	"3\n2\n1\n")

INTERPRETER_TEST(until_countup,
	"count := 0; until count > 2 { print count; count = count + 1; };",
	"0\n1\n2\n")

INTERPRETER_TEST(for_in_integer,
	"for i in 3 { print i; }",
	"0\n1\n2\n")

MU_TEST_SUITE(tests) {
    MU_RUN_TEST(while_countdown);
    MU_RUN_TEST(until_countup);
    MU_RUN_TEST(for_in_integer);
}
int main(int argc, char** argv) {
    (void)argc; // unused
    (void)argv; // unused

    MU_RUN_SUITE(tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
