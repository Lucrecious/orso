#include <stdio.h>

#include "chunk.h"
#include "debug.h"
#include "virtual_machine.h"

#define PROJECT_NAME "orso"

int main(int argc, char **argv) {
    OrsoVM vm;
    orso_vm_init(&vm);

    orso_interpret(&vm, "(6.0 + 5) > 10");

    orso_vm_free(&vm);

    return 0;
}
