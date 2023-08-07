#include <stdio.h>
#include <stdlib.h>

#include "interpreter.h"

#define PROJECT_NAME "orso"

void myerror(OrsoErrorType error, i32 line, const char* message) {
    switch (error) {
        case ORSO_ERROR_COMPILE: {
            fprintf(stderr, "[line %d] %s\n", line, message);
            break;
        }
        default: break;
    }
}

void mywrite(const char* chars) {
    printf("%s", chars);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("You must provide a file.");
        exit(1);
    }

    char* path = realpath(argv[1], NULL);
    printf("---- %s\n", path);

    FILE* file;
    file = fopen(path, "r");
    free(path);

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

    OrsoVM vm;
    orso_vm_init(&vm, mywrite);
    orso_run_source(&vm, source, myerror);
    orso_vm_free(&vm);

    return 0;
}
