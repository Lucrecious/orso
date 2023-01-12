#include <stdio.h>

#include "interpreter.h"

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

void write(const char* chars) {
    printf(chars);
}

int main(int argc, char **argv) {
    char source_code[1000];

    OrsoInterpreter interpreter;

    orso_interpreter_init(&interpreter, write, error);
    printf("Orso interpreter initialized.\n\n");

    for (;;) {
        printf("orso>> ");
        if (fgets(source_code, 1000, stdin) == NULL) {
            break;
        }
        

        if (source_code[0] == '\n') {
            break;
        }

        orso_interpreter_run(&interpreter, source_code);

        printf("\n");
    }

    orso_interpreter_free(&interpreter);

    return 0;
}
