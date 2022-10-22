#include <stdio.h>

#include "chunk.h"
#include "debug.h"
#include "virtual_machine.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    SavineVM vm;
    savine_vm_init(&vm);

    savine_interpret(&vm, "1 / 2.0");

    savine_vm_free(&vm);

    return 0;
}
