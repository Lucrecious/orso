#include <stdio.h>
#include <stdlib.h>

#include "error.h"

#define STRINGT_IMPLEMENTATION
#include "stringt.h"
#undef STRINGT_IMPLEMENTATION

#define ARENA_IMPLEMENTATION
#include "arena.h"
#undef ARENA_IMPLEMENTATION

#define TMP_IMPLEMENTATION
#include "tmp.h"
#undef TMP_IMPLEMENTATION

#include <libtcc.h>

#define PROJECT_NAME "orso"

#include "debug.h"

#include "parser.h"
#include "static_analyzer.h"

#define CODEGEN2_IMPLEMENTATION
#include "codegen2.h"
#undef CODEGEN2_IMPLEMENTATION

#define VM2_IMPLEMENTATION
#include "vm2.h"
#undef VM2_IMPLEMENTATION

#include "debugger.h"
#include "debugger.c"

/*

[ ] Rewrite tracing logic and better disassembler log
[ ] Rewrite casting system
[ ] Look into and implement a better way of handling the op codes
[ ] Untangle allocators
[ ] Allow for "native" structs
[ ] Unions as a native struct
[ ] Write printf in orso itself
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
        case ERROR_COMPILE: {
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
        println("must have a file as input");
        exit(1);
    }

    arena_t allocator = {0};

    string_t path = cstr2string(argv[1], &allocator);
    FILE* file = fopen(path.cstr, "r");

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

    ast_t ast = {0};
    ast_init(&ast);

    bool success = parse(&ast, path, source.cstr, myerror);
    unless (success) {
        log_fatal("Unable to parse source.");
        exit(1);
    }

    analyzer_t analyzer = {0};
    analyzer_init(&analyzer, mywrite, myerror);
    success = resolve_ast(&analyzer, &ast);

    unless (success) {
        log_fatal("Unable to statically analyze.");
        exit(1);
    }

    vm2_t vm = {0};
    vm2_init(&vm);

    generate_code_for_vm2(&vm, &ast);
    exit(0);

    debugger_t debugger = {0};
    debugger_init(&debugger, &allocator);

    do {
        bool success = debugger_step(&debugger, &vm);
        if (!success) break;
    } while (true);

    return 0;
}