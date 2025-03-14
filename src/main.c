#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "../nob.h"
#undef UNREACHABLE
#undef UNUSED
#undef sv_eq
#undef NOB_IMPLEMENTATION

#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

#define VM_IMPLEMENTATION
#include "vm.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define TMP_IMPLEMENTATION
#include "tmp.h"

#define LOG_IMPLEMENTATION
#include "log.h"

#define STRINGT_IMPLEMENTATION
#include "stringt.h"

#include "static_analyzer.h"

#define CODEGEN_IMPLEMENTATION
#include "codegen.h"

#define CODEGENC_IMPLEMENTATION
#include "codegenc.h"

#define MEMARR_IMPLEMENTATION
#include "memarr.h"

#define DYNLIB_IMPLEMENTATION
#include "dynlib.h"

#define TMPFILE_IMPLEMENTATION
#include "tmpfile.h"

#include "debugger.h"

#include "parser.h"

#include "error.h"

#define CC_IMPLEMENTATION
#include "cc.h"


string_view_t get_line(string_view_t source, string_view_t somewhere_in_source) {
    if (somewhere_in_source.data >= source.data + source.length || somewhere_in_source.data < source.data) {
        return lit2sv("");
    }

    char *s = (char*)somewhere_in_source.data;
    until (*s == '\n' || s == source.data)  {
        --s;
    }

    char *e = (char*)somewhere_in_source.data;
    until (*e == '\n' || *e == '\0') {
        ++e;
    }

    string_view_t view = {.length = e - s, .data = s};
    return view;
}

static void print_error_location_hint(token_t line_token, size_t column_hint) {
    string_view_t source_line = get_line(line_token.source, line_token.view);
    fprintf(stderr, "%.*s\n", (int)source_line.length, source_line.data);
    tmp_arena_t *tmp_arena = allocator_borrow();
    string_builder_t sb = {.allocator=tmp_arena->allocator};

    for (size_t i = 0; i < source_line.length; ++i) {
        if (i == column_hint) {
            sb_add_char(&sb, '^');
        } else {
            sb_add_char(&sb, ' ');
        }
    }

    if (column_hint == source_line.length) {
        sb_add_char(&sb, '^');
    }

    fprintf(stderr, "%.*s\n", (int)sb.count, sb.items);

    allocator_return(tmp_arena);
}

void myerror(ast_t *ast, error_t error) {
    UNUSED(ast);

    error_source_t error_source = error_sources[error.type];

    size_t line;
    size_t column;
    cstr_t file_path;
    if (error_source == ERROR_SOURCE_ANALYSIS) {
        line = error.node->start.loc.line + 1;
        column = error.node->start.loc.column + 1;
        file_path = error.node->start.loc.filepath.cstr;
    } else {
        line = error.after_token.loc.line + 1;
        column = error.after_token.loc.column + 1;
        file_path = error.after_token.loc.filepath.cstr;
    }

    fprintf(stderr, "%s:%lu:%lu: %s\n", file_path, line, column, error.message);

    switch (error_source) {
    case ERROR_SOURCE_PARSER: {
        print_error_location_hint(error.after_token, token_end_loc(&error.after_token).column);
        break;
    }

    case ERROR_SOURCE_PARSEREX: {
        switch (error.type) {
        case ERROR_PARSEREX_EXPECTED_EOF_AFTER_MODULE:
        case ERROR_PARSEREX_EXPECTED_SEMICOLON_AFTER_STRUCT_DECLARATION:
        case ERROR_PARSEREX_EXPECTED_SEMICOLON_AFTER_DECLARATION: {
            print_error_location_hint(error.node->end, token_end_loc(&error.node->end).column);
            break;
        }

        case ERROR_PARSEREX_EXPECTED_TYPE:
        case ERROR_PARSEREX_EXPECTED_EXPRESSION:
        case ERROR_PARSEREX_TOO_MANY_PARAMETERS:
        case ERROR_PARSEREX_EXPECTED_DECLARATION: {
            print_error_location_hint(error.node->start, error.node->start.loc.column);
            break;
        }

        default:UNREACHABLE();
        }
        break;
    }

    case ERROR_SOURCE_ANALYSIS: {
        print_error_location_hint(error.node->start, error.node->start.loc.column);
        break;
    }
    case ERROR_SOURCE_CODEGEN: break;
    }
}

void print_errors(ast_t *ast) {
    for (size_t i = 0; i < ast->errors.count; ++i) {
        error_t error = ast->errors.items[i];
        myerror(ast, error);
    }
}

void mywrite(cstr_t chars) {
    printf("%s", chars);
}

static void print_usage() {
    // println("usage:");
    // println("  orso <filepath> [...]        - runs the given file in the orso interpreter and the optional program arguments");
    // println("  orso debug <filepath>        - runs the given file in the orso interpreter debugger");
    // println("  orso build <filepath>        - creates a build.odl file with template code for building orso projects natively");
    // println("  orso hello <filepath>        - generates a small sample demo of orso into the given file path");
    // println("");
    println("usage:");
    println("  orso                         - prints this usage text");
    println("  orso run <filepath>          - runs the interpreter on given <filepath>");
    println("  orso debug <filepath>        - runs the given <filepath> in the orso interpreter debugger");
    println("  orso build <in> <out>        - creates a native build of the given <in> into an exe <out>");
    println("");
}

static bool load_file(string_t in, string_t *source, arena_t *arena) {
    String_Builder sb = {0};
    bool success = nob_read_entire_file(in.cstr, &sb);
    if (!success) return false;

    string_builder_t sb_ = {.items=sb.items, .count=sb.count};
    string_t src = sb_render(&sb_, arena);

    nob_sb_free(sb);

    *source = src;

    return true;
}

ast_t *build_ast(string_t source, arena_t *arena, string_t file_path) {
    ast_t *ast = arena_alloc(arena, sizeof(ast_t));
    ast_init(ast, arena);
    ast->vm = vm_default(arena);

    bool parsed = parse(ast, file_path, string2sv(source));

    if (parsed) {
        resolve_ast(ast);
    }

    return ast;
}

bool generate_exe(ast_t *ast, string_t output_path) {
    tmp_arena_t *tmp = allocator_borrow();

    string_builder_t sb = {.allocator=tmp->allocator};

    compile_ast_to_c(ast, &sb);

    bool success = write_entire_file("./build/tmp.c", sb.items, sb.count);

    cc_t cc = cc_make(CC_GCC, tmp->allocator);
    cc.output_type = CC_EXE;
    cc.output_path = string_copy(output_path, tmp->allocator);

    cc_source(&cc, lit2str("./build/tmp.c"));

    cc_include_dir(&cc, lit2str("./lib"));
    cc_no_warning(&cc, lit2str("unused-value"));
    
    // stops warnings from ((x == y)) type of conditions
    cc_no_warning(&cc, lit2str("parentheses-equality"));

    cc_build(&cc);

    allocator_return(tmp);


    return success;
}

bool compile(string_t input_file_path, string_t output_file_path) {
    arena_t arena = {0};

    string_t source;
    bool success = load_file(input_file_path, &source, &arena);
    if (!success) exit(1);

    ast_t *ast = build_ast(source, &arena, input_file_path);

    bool result = false;

    if (!ast->resolved) {
        print_errors(ast);
        return_defer(false);
    }

    success = generate_exe(ast, output_file_path);
    if (!success) {
        print_errors(ast);
        return_defer(false);
        goto defer;
    }

    return_defer(true);

defer:
    arena_free(&arena);
    return result;
}

bool interpret(string_t input_file_path) {
    arena_t arena = {0};

    string_t source;
    bool success = load_file(input_file_path, &source, &arena);
    if (!success) exit(1);

    ast_t *ast = build_ast(source, &arena, input_file_path);

    bool result = false;

    if (!ast->resolved) {
        print_errors(ast);
        return_defer(false);
    }

    success = compile_program(ast->vm, ast);
    if (!success) {
        print_errors(ast);
        return_defer(false);
        goto defer;
    }

    function_t *main_or_null = find_main_or_null(ast);
    if (main_or_null) {
        vm_fresh_run(ast->vm, main_or_null);
    }


    return_defer(true);

defer:
    arena_free(&arena);
    return result;
}

bool debug(string_t input_file_path) {
    arena_t arena = {0};

    string_t source;
    bool success = load_file(input_file_path, &source, &arena);
    if (!success) exit(1);

    ast_t *ast = build_ast(source, &arena, input_file_path);

    bool result = false;

    if (!ast->resolved) {
        print_errors(ast);
        return_defer(false);
    }

    success = compile_program(ast->vm, ast);
    if (!success) {
        print_errors(ast);
        return_defer(false);
        goto defer;
    }

    function_t *main_or_null = find_main_or_null(ast);
    if (main_or_null) {
        debugger_t debugger = {0};
        debugger_init(&debugger, &arena);

        vm_set_entry_point(ast->vm, main_or_null);
        while(debugger_step(&debugger, ast->vm));
    }

    return_defer(true);

defer:
    arena_free(&arena);
    return result;
}

int main(int argc, char **argv) {
    shift(argv, argc);

    arena_t arena = {0};

    if (argc) {
        cstr_t option = shift(argv, argc);
        if (strncmp(option, "build", 5) == 0) {
            cstr_t output = "a.out";
            if (argc > 2) {
                fprintf(stderr, "build option requires at least an input and optional output odl file\n");
                exit(1);
            }

            cstr_t input = shift(argv, argc);
            if (argc) {
                output = shift(argv, argc);
            }

            string_t sinput = cstr2string(input, &arena);
            string_t soutput = cstr2string(output, &arena);

            compile(sinput, soutput);

        } else if (strncmp(option, "run", 3) == 0) {
            if (argc != 1) {
                fprintf(stderr, "run option requires input odl file\n");
                exit(1);
            }

            cstr_t filename = shift(argv, argc);
            string_t file = {.cstr=filename, .length=strlen(filename)};
            interpret(file);
        } else if (strncmp(option, "dbg", 3) == 0) {
            if (argc != 1) {
                fprintf(stderr, "dbg option requires input odl file\n");
                exit(1);
            }

            cstr_t filename = shift(argv, argc);
            string_t file = {.cstr=filename, .length=strlen(filename)};
            debug(file);
        }
    } else {
        print_usage();
        exit(0);
    }
}

