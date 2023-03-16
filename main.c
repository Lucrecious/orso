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
    printf("%s", chars);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        argv[1] = "script.orso";
    }

    FILE* file;
    file = fopen(argv[1], "r");

    if (file == NULL) {
        printf("Error opening file: %s\n", argv[1]);
        exit(1);
    }

    fseek(file, 0L, SEEK_END);
    long int size = ftell(file);
    rewind(file);

    char source[size + 1];

    fread(source, size, 1, file);

    fclose(file);

    source[size] = '\0';

    OrsoInterpreter interpreter;

    orso_interpreter_init(&interpreter, write, error);

    orso_interpreter_run(&interpreter, source);

    orso_interpreter_free(&interpreter);

    return 0;
}
