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

#define TMP_IMPLEMENTATION
#include "tmp.h"
#undef TMP_IMPLEMENTATION

#include <libtcc.h>

#define PROJECT_NAME "orso"

#include "debug.h"

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

static bool compile_program(vm_t *vm, ast_t *ast) {
    function_t *main = generate_code(vm, ast);
    return main != NULL;
}

static void vm_run(vm_t *vm) {
    orso_call_function(vm, vm->entry_point, vm->error_fn);
}

static string_t get_input(arena_t *allocator) {
    char input[256] = {0};

    fgets(input, 256, stdin);
    // scanf("%255[^\n]", input);

    string_t s = cstr2string(input, allocator);
    return s;
}

static void show_line(vm_t *vm, size_t bytecode_around) {
    source_location_t source_location = vm_find_source_location(vm);
    if (cstr_eq(source_location.file_path.cstr, "")) {
        println("no more frames");
    } else {
        printfln("%s:%zu:%zu", source_location.file_path.cstr, source_location.line+1, source_location.column);
    }

    call_frame_t *frame = &vm->frames[vm->frame_count-1];
    chunk_t *chunk = &frame->function->chunk;
    i32 offset = frame->ip - chunk->code.items;
    assert(offset >= 0);
    size_t offset_ = (size_t)offset;

    size_t begin = bytecode_around > offset_ ? 0 : offset_ - bytecode_around;
    size_t end = ((offset_+1+bytecode_around) > chunk->code.count) ? chunk->code.count : (offset_+1+bytecode_around);

    for (size_t i = begin; i < end; ++i) {
        if (i == offset_) {
            printf(">>>> ");
        }
        disassemble_instruction(&vm->type_set->types, chunk, i);
    }
}

static bool try_vm_step(vm_t *vm) {
    if (vm->frame_count > 0) return vm_step(vm);
    return false;
}

typedef struct breakpoints_t breakpoints_t;
struct breakpoints_t {
    source_location_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

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

    vm_t vm = {0};
    vm_init(&vm, mywrite, myerror, 1000*1000);

    function_t *main = generate_code(&vm, &ast);

    unless (main) {
        log_fatal("Unable to compile program.");
        exit(1);
    }

    vm_begin(&vm, main);

    breakpoints_t breakpoints = {.allocator=&allocator};

    arena_t frame_allocator = {0};

    do {
        arena_reset(&frame_allocator);

        printf(">> ");
        string_t input = get_input(&frame_allocator);
        strings_t command_n_args = string_split(input.cstr, " \n", &frame_allocator);
        if (command_n_args.count == 0) {
            continue;
        }

        string_t command = command_n_args.items[0];
        if (cstr_eq(command.cstr, "quit") || cstr_eq(command.cstr, "q")) {
            break;
        } else if (cstr_eq(command.cstr, "show")) {
            size_t amount = 0;
            if (command_n_args.count > 1) {
                string_t arg = command_n_args.items[1];
                amount = strtoul(arg.cstr, NULL, 10);
            }
            show_line(&vm, amount);
        } else if (cstr_eq(command.cstr, "break")) {
            if (command_n_args.count != 3) {
                printfln("expected 2 arguments (file and line) but got %zu", command_n_args.count-1);
                continue;
            }

            string_t file_path = command_n_args.items[1];
            string_t line_number = command_n_args.items[2];
            size_t line = strtoul(line_number.cstr, NULL, 10);
        } else if (cstr_eq(command.cstr, "stepo")) {
            source_location_t source_location = vm_find_source_location(&vm);

            while (try_vm_step(&vm)) {
                source_location_t new_location = vm_find_source_location(&vm);
                if (new_location.line != source_location.line || !string_eq(source_location.file_path, new_location.file_path)) {
                    show_line(&vm, 3);
                    break;
                }
            }
        } else if (cstr_eq(command.cstr, "stepi")) {
            if (try_vm_step(&vm)) {
                show_line(&vm, 0);
            }
        } else if (cstr_eq(command.cstr, "run")) {
            while(try_vm_step(&vm));
        } else {
            printfln("unknown command: %s", command.cstr);
        }

    } while (true);

    return 0;
}