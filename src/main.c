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


void myerror(ast_t *ast, error_t error) {
    UNUSED(ast);

    tmp_arena_t *tmp = allocator_borrow();
    string_t error_str = error2richstring(ast, error, tmp->allocator);
    fprintf(stderr, "%s", error_str.cstr);
    allocator_return(tmp);
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
    println("  orso ast <in>                - outputs ast for orso file for debugging");
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

    {
        string_t core_path = lit2str("./bin/core.or");
        Nob_String_Builder sb = {0};
        bool success = nob_read_entire_file(core_path.cstr, &sb);
        if (!success) abort();
        
        string_builder_t sb_ = {.items=sb.items, .count=sb.count};

        string_t core_source = sb_render(&sb_, ast->arena);

        nob_sb_free(sb);

        ast_node_t *core = parse_source_into_module(ast, core_path, string2sv(core_source));
        ast->core_module_or_null = core;
    }
    
    tmp_arena_t *tmp = allocator_borrow();
    {
        ast_node_t *program = parse_source_into_module(ast, file_path, string2sv(source));

        string_t programid = ast_generate_moduleid(file_path, tmp->allocator);
        ast_add_module(ast, program, programid);
    }
    allocator_return(tmp);

    if (ast->errors.count == 0) {
        resolve_ast(ast);
    }

    return ast;
}

bool generate_exe(ast_t *ast, string_t output_path) {
    tmp_arena_t *tmp = allocator_borrow();

    strings_t sources = {0};
    strings_t libs = {0};

    bool success = compile_ast_to_c(ast, lit2str("./build/"), &sources, &libs, tmp->allocator);

    if (success) {
        cc_t cc = cc_make(CC_GCC, tmp->allocator);
        cc.output_type = CC_EXE;
        cc.output_path = string_copy(output_path, tmp->allocator);

        for (size_t i = 0; i < sources.count; ++i) {
            string_t src = sources.items[i];
            cc_source(&cc, src);
        }

        for (size_t i = 0; i < libs.count; ++i) {
            string_t libpath = libs.items[i];
            cc_libpath(&cc, libpath);
        }

        cc_include_dir(&cc, lit2str("./lib"));
        cc_no_warning(&cc, lit2str("unused-value"));
        
        // stops warnings from ((x == y)) type of conditions
        cc_no_warning(&cc, lit2str("parentheses-equality"));

        cc_flag(&cc, lit2str("-std=c99"));

        cc_flag(&cc, lit2str("-framework"));
        cc_flag(&cc, lit2str("IOKit"));
        cc_flag(&cc, lit2str("-framework"));
        cc_flag(&cc, lit2str("Cocoa"));
        cc_flag(&cc, lit2str("-framework"));
        cc_flag(&cc, lit2str("OpenGL"));

        success = cc_build(&cc);
    }

    allocator_return(tmp);


    return success;
}

void print_ast(string_t input_file_path) {
    arena_t arena = {0};

    string_t source;
    bool success = load_file(input_file_path, &source, &arena);
    if (!success) exit(1);

    ast_t *ast = build_ast(source, &arena, input_file_path);

    if (!ast->resolved) {
        print_errors(ast);
    }
    ast_print(ast, "program");
    arena_free(&arena);
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

    // function_t *main_or_null = find_main_or_null(ast);
    // if (main_or_null) {
    //     vm_fresh_run(ast->vm, main_or_null);
    // }

    UNREACHABLE();


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

    // function_t *main_or_null = find_main_or_null(ast,);
    // if (main_or_null) {
    //     debugger_t debugger = {0};
    //     debugger_init(&debugger, &arena);

    //     vm_set_entry_point(ast->vm, main_or_null);
    //     while(debugger_step(&debugger, ast->vm));
    // }
    UNREACHABLE();

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
        } else if (strncmp(option, "ast", 4) == 0) {
            if (argc > 2) {
                fprintf(stderr, "requires file to resolve");
                exit(1);
            }

            cstr_t input = shift(argv, argc);

            string_t sinput = cstr2string(input, &arena);

            print_ast(sinput);
        }
    } else {
        print_usage();
        exit(0);
    }
}

