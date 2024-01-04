#include <stdio.h>
#include <stdlib.h>

#include "interpreter.h"
#include "error.h"

#include <libtcc.h>

#define PROJECT_NAME "orso"

void myerror(OrsoError error) {
    switch (error.type) {
        case ORSO_ERROR_COMPILE: {
            fprintf(stderr, "[line %d] %s\n", error.region.token.line + 1, error.message);
            break;
        }
        default: break;
    }
}

void mywrite(const char* chars) {
    printf("%s", chars);
}

typedef int (*int_getter)(void*);

struct TEST {
    short int x;
    char c;
};

int main(int argc, char **argv) {
    printf("size of char inside struct: %ld and size of int: %ld\n", sizeof(struct TEST), sizeof(int long));
    // if (argc < 2) {
    //     printf("No script file provided!\n");
    //     return 1;
    // }

    // TCCState *s = tcc_new();
    // if (!s) {
    //     printf("Could not create tcc state\n");
    //     return 1;
    // }

    // tcc_set_output_type(s, TCC_OUTPUT_MEMORY);

    // tcc_add_include_path(s, "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include");
    // tcc_add_library_path(s, "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib");


    // if (tcc_add_file(s, argv[1]) == -1) {
    //     printf("Could not add file: %s\n", argv[1]);
    //     return 1;
    // }

    // if (tcc_relocate(s, TCC_RELOCATE_AUTO) < 0) {
    //     printf("Could not relocate\n");
    //     return 1;
    // }

    // void* point = tcc_get_symbol(s, "my_point");
    // int_getter get_y = (int_getter)tcc_get_symbol(s, "get_y");
    // if (!point || !get_y) {
    //     printf("Could not find required symbols\n");
    //     return 1;
    // }

    // int y = get_y(point);
    // printf("y is %d\n", y);

    // tcc_delete(s);
    // return 0;

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
    orso_vm_init(&vm, mywrite, 1000);
    orso_run_source(&vm, source, myerror);
    orso_vm_free(&vm);

    return 0;
}

// TODO: do error messages that act more like a tutorial 
