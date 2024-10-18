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

#include "debug.h"

/*

[ ] Use tsoding-like arrays with allocators and replace sb
[ ] Allow for "native" structs
[ ] Unions as a native struct
[ ] Write printf in orso itself
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
        printf("Usage: \n");
        printf("    orso [-[baAt]] <path/to/file.odl>        without any option, simply runs orso file in the interpreter\n");
        printf("                                             with options:\n");
        printf("                                                   -b prints the byte code of the given source\n");
        printf("                                                   -a prints the ast of the given source before static analysis\n");
        printf("                                                   -A prints the ast of the given source after static analysis\n");
        printf("                                                   -t runs the source in the interpreter, and prints the running byte code\n");
        printf("Examples:\n");
        printf("orso -t examples/hello_world.odl        runs and prints the byte code as the program runs\n");
        printf("orso -ab examples/hello_world.odl       prints the byte code and ast of the program without running \n");
        printf("orso examples/hello_world.odl           runs the program in the interpreter\n");
        exit(1);
    }

    arena_t allocator = {0};

    bool print_ast_before = false;
    bool print_ast_after = false;
    bool print_byte_code = false;
    bool trace = false;
    string_t path;
    if (argc == 2) {
        path = cstr2string(argv[1], &allocator);
    } else if (argc == 3) {
        char *options = argv[1];
        size_t options_len = strlen(options);

        if (options_len < 2 || options[0] != '-') {
            log_fatal("Invalid argument. Check usage: ./orso");
            exit(1);
        }

        for (size_t i = 1; i < options_len; ++i) {
            if (options[i] == 't') trace = true;
            else if (options[i] == 'a') print_ast_before = true;
            else if (options[i] == 'A') print_ast_after = true;
            else if (options[i] == 'b') print_byte_code = true;
            else {
                log_fatal("Unknown option: %c. Check usage: ./orso", options[i]);
                exit(1);
            }
        }

        path = cstr2string(argv[2], &allocator);
    }

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


    unless (!print_ast_before && !print_ast_after && !print_byte_code && !trace) {
        vm_t vm;
        vm_init(&vm, mywrite, 1000);

        ast_t ast;
        ast_init(&ast, &vm.symbols);
        bool success = parse(&ast, source.cstr, myerror);

        if (print_ast_before) {
            if (!success) {
                log_fatal("Unable to parse ast. Cannot print.");
                exit(1);
            }

            ast_print(&ast, "before static analysis");
        }

        analyzer_t analyzer;
        analyzer_init(&analyzer, mywrite, myerror);
        success = resolve_ast(&analyzer, &ast);

        if (print_ast_after) {
            ast_print(&ast, "after static analysis");
        }

        if ((print_byte_code || trace) && !success) {
            log_fatal("unable to resolve ast, and cannot trace nor generate the byte code");
            exit(1);
        }

        function_t *main = generate_code(&vm, &ast);
        unless (main) {
            log_fatal("no main function. cannot trace nor generate the byte code");
            exit(1);
        }

        if (print_byte_code) {
            chunk_disassemble(&main->chunk, "main");
        }

        if (trace) {
            vm.type_set = &ast.type_set;
            vm_begin(&vm, main);
            do {
                // printf("--- %d ---", vm.frames[vm.frame_count-1].ip);
                vm_print_stack(&vm);
                vm_disassemble_current_instruction(&vm);
            } while (vm_step(&vm));
        }
    } else {
        vm_t vm;
        vm_init(&vm, mywrite, 1000);
        vm_run_source(&vm, source.cstr, myerror);
        vm_free(&vm);
    }

    return 0;
}