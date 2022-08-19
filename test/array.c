#include <stdlib.h>

#include "array.h"
#include "mathutils.h"
#include "minunit.h"

MU_TEST(creation) {
    struct SavineArray array;
    savine_array_new(&array, sizeof(i32), 5);

    savine_array_free(&array);
}

MU_TEST(insertion) {
    struct SavineArray array;
    savine_array_new(&array, sizeof(i32), 5);

    i32 n = 10;
    savine_array_insert(&array, 0, &n);

    i32 out;
    savine_array_get(&array, 0, &out);

    savine_array_free(&array);

    MU_ASSERT_INT_EQ(10, out);
}

struct MyStruct {
    i32 foo;
    f64 bar;
};

MU_TEST(insertion_structs) {
    struct SavineArray array;
    savine_array_new(&array, sizeof(struct MyStruct), 10);

    struct MyStruct my_struct = { .foo = 10, .bar = 2.0 };
    savine_array_insert(&array, 9, &my_struct);

    my_struct.foo = 0;
    my_struct.bar = 0;

    savine_array_get(&array, 9, &my_struct);
    savine_array_free(&array);

    MU_ASSERT_INT_EQ(10, my_struct.foo);
    MU_ASSERT_DOUBLE_EQ(2.0, my_struct.bar);

}

MU_TEST(variant_array) {
    struct SavineArray array;
    savine_array_new(&array, imax(sizeof(struct MyStruct), sizeof(i32)), 2);

    i32 other_thing = 24;
    struct MyStruct thing = { .foo = 42, .bar = 4.2 };

    savine_array_insert_as(&array, 0, &other_thing, sizeof(i32));
    savine_array_insert_as(&array, 1, &thing, sizeof(struct MyStruct));

    other_thing = 0;
    thing.foo = 0;
    thing.bar = 0.0;

    savine_array_get_as(&array, 0, &other_thing, sizeof(i32));
    savine_array_get_as(&array, 1, &thing, sizeof(struct MyStruct));
    savine_array_free(&array);

    MU_ASSERT_INT_EQ(24, other_thing);
    MU_ASSERT_INT_EQ(42, thing.foo);
    MU_ASSERT_DOUBLE_EQ(4.2, thing.bar);
}

MU_TEST_SUITE(all_tests) {
    MU_RUN_TEST(creation);
    MU_RUN_TEST(insertion);
    MU_RUN_TEST(insertion_structs);
    MU_RUN_TEST(variant_array);
}

int main(int argc, char** argv) {
    MU_RUN_SUITE(all_tests);
    MU_REPORT();
    return MU_EXIT_CODE;
}
