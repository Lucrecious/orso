#include <stdio.h>

#include "chunk.h"
#include "common.h"
#include "debug.h"
#include "virtual_machine.h"

#define PROJECT_NAME "orso"

void error(OrsoErrorType error, i32 line, const char* message) {
    switch (error) {
        case ORSO_ERROR_COMPILE: {
            fprintf(stderr, "[line %d] %s\n", line, message);
            break;
        }
        default: break;
    }
}

int main(int argc, char **argv) {
    OrsoVM vm;
    orso_vm_init(&vm);

    orso_interpret(&vm, "", error);
    //orso_interpret(&vm, "1 == 2", error);

    orso_vm_free(&vm);

    return 0;
}
