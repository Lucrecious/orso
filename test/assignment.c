#include <stdio.h>

#include "minunit.h"

#include "error_codes.h"

MU_TEST_SUITE(all_tests) {
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
