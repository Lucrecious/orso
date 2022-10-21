#include <stdio.h>

#include "minunit.h"

#include "def.h"
#include "virtual_machine.h"
#include "error_codes.h"

MU_TEST(retrieve_i32) {
    SavineEnv env;
    savine_env_init(&env);

    InterpretResult result = savine_env_feed_line("var thing := 42");

    SavineScope scope;
    savine_env_current_scope(&env, &scope);
    i32 thing = savine_scope_get_i32(&scope, "thing");

    savine_env_free(&env);

    MU_ASSERT(result == SAVINE_INTERPRET_OK, "must interpret correctly");
    MU_ASSERT(thing == 42, "must be able to retrieve value");
}

MU_TEST_SUITE(all_tests) {
    MU_RUN_TEST(retrieve_i32);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
