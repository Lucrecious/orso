#include <stdio.h>

#include "chunk.h"
#include "debug.h"
#include "virtual_machine.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    Chunk chunk;
    chunk.max_stack_size = 256;

    SavineVM vm;
    savine_vm_init(&vm);

    savine_interpret(&vm, "(1 + 2/-3 + 4 / (5 - 6)");

    savine_vm_free(&vm);

    return 0;
}
