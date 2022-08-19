#include <stdio.h>

#include "minunit.h"

#include "savine.h"
#include "error_codes.h"

MU_TEST(integer_assignment_inferred) {
    struct Savine_Context savine_context;
    savine_init(&savine_context);

    savine_run_code(&savine_context, "foo := 42");

    long foo;
    savine_get_i64(&savine_context, "foo", &foo);

    MU_ASSERT(savine_context.error == SavineError_OK, "must be no error");
    MU_ASSERT(foo == 42, "error, foo != 42");
}

MU_TEST(integer_assignment_static_typed) {
    struct Savine_Context savine_context;
    savine_init(&savine_context);

    savine_run_code(&savine_context, "foo: i64 = 42");

    long foo;
    savine_get_i64(&savine_context, "foo", &foo);

    MU_ASSERT(savine_context.error == SavineError_OK, "must be no error");
    MU_ASSERT(foo == 42, "error, foo != 42");
}

MU_TEST_SUITE(all_tests) {
    MU_RUN_TEST(integer_assignment_inferred);
    MU_RUN_TEST(integer_assignment_static_typed);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
