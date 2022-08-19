#include <stdio.h>

#include "savine.h"
#include "savine_string.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    savine_string_init_interned();

    Savine_Context context;
    savine_init(&context);

    savine_run_code(&context, "thing: i32 = 10");

    printf("Finished\n");

    return 0;
}
