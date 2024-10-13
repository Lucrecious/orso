#include <stdio.h>
#include <stdlib.h>

#include "interpreter.h"
#include "error.h"

#define STRINGT_IMPLEMENTATION
#include "stringt.h"
#undef STRINGT_IMPLEMENTATION

#define ARENA_IMPLEMENTATION
#include "arena.h"
#undef ARENA_IMPLEMENTATION

#include <libtcc.h>

#define PROJECT_NAME "orso"

/*

[ ] Use generic hash table with allocators
[ ] Use arena allocator for everything instead of ORSO_ALLOCATE
[ ] For debugging, instead of printing the disaseembly, write it in the file
[ ] For debugging, instead of printing ast write it in a file instead
[ ] Better error messages

[ ] Get DearImGui set up but only for debugging version, use macros and nob to make sure of this
    [ ] Make a nice debugging view that matches commands to lines of code
    [ ] Make a nice debugging view for the ast nodes

*/

void log_fatal(cstr_t format, ...) {
    va_list args;
    va_start(args, format);

    printf("[FATAL] ");
    vprintf(format, args);
    printf("\n");

    va_end(args);
}

void myerror(error_t error) {
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

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: orso <path/to/file.odl>\n");
        exit(1);
    }

    arena_t allocator = {0};

    string_t path = cstr2string(argv[1], &allocator);

    FILE* file;
    file = fopen(path.cstr, "r");

    if (file == NULL) {
        log_fatal("Unable to open file: %s\n", path.cstr);
        exit(1);
    }

    if (fseek(file, 0L, SEEK_END) != 0) {
        log_fatal("Unable to seek in file: %s\n", path.cstr);
        exit(1);
    }

    long int size = ftell(file);
    rewind(file);

    string_t source;
    {
        char source_[size + 1];

        fread(source_, size, 1, file);

        fclose(file);

        source_[size] = '\0';

        source = cstr2string(source_, &allocator);
    }

    vm_t vm;
    vm_init(&vm, mywrite, 1000);
    orso_run_source(&vm, source.cstr, myerror);
    vm_free(&vm);

    return 0;
}

// TODO: do error messages that act more like a tutorial 
