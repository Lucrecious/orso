#include <stdio.h>

#include "savine.h"
#include "savine_string.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    // Savine_Context context;
    // savine_init(&context);

    // savine_run_code(&context, "thing := 10");

    // printf("Finished\n");

    savine_string_init_interned();

    char* thing1 = c_str_stack_to_heap("thing1");
    char* thing2 = c_str_stack_to_heap("thing1");
    char* thing3 = c_str_stack_to_heap("thing2");
    char* thing4 = c_str_stack_to_heap("thing2");

    SavineString* s1 = c_strn_to_savine_string(thing1, 6);
    SavineString* s2 = c_strn_to_savine_string(thing2, 6);
    SavineString* s3 = c_strn_to_savine_string(thing3, 6);
    SavineString* s4 = c_strn_to_savine_string(thing4, 6);

    printf("%d, %d, %d\n", s1 == s2, s1 == s3, s3 == s4);

    savine_string_free_interned();

    return 0;
}
