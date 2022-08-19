#include "list.h"
#include "mathutils.h"
#include "minunit.h"

MU_TEST(creation) {
    struct SavineList list;
    savine_list_new(&list, sizeof(i32), 5);

    savine_list_free(&list);
}

MU_TEST(grow) {
    struct SavineList list;
    savine_list_new(&list, sizeof(i32), 1);

    MU_ASSERT_INT_EQ(1, list.items.size);

    i32 i = 0;
    savine_list_push_back(&list, &i);

    i++;
    savine_list_push_back(&list, &i);

    i++;
    savine_list_push_back(&list, &i);

    MU_ASSERT(list.items.size > 1, "list must grow to accomodate");

    savine_list_free(&list);
}

MU_TEST_SUITE(all_tests) {
    MU_RUN_TEST(creation);
    MU_RUN_TEST(grow);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}