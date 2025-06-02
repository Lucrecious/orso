#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "../nob.h"
#undef UNREACHABLE
#undef sv_eq
#undef NOB_IMPLEMENTATION

#include "orso.h"

#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

#define ARENA_IMPLEMENTATION
#include "arena.h"

#define VM_IMPLEMENTATION
#include "vm.h"

#define TMP_IMPLEMENTATION
#include "tmp.h"

#define LOG_IMPLEMENTATION
#include "log.h"

#define STRINGT_IMPLEMENTATION
#include "stringt.h"

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

#include "static_analyzer.h"

#include "parser.h"

#include "error.h"

#define CC_IMPLEMENTATION
#include "cc.h"

static void myerror(ast_t *ast, error_t error) {
    UNUSED(ast);

    tmp_arena_t *tmp = allocator_borrow();
    orstring_t error_str = error2richstring(ast, error, tmp->allocator);
    fprintf(stderr, "%s", error_str.cstr);
    allocator_return(tmp);
}

static void print_errors(ast_t *ast) {
    for (size_t i = 0; i < ast->errors.count; ++i) {
        error_t error = ast->errors.items[i];
        myerror(ast, error);
    }
}

static bool load_file(orstring_t in, orstring_t *source, arena_t *arena) {
    String_Builder sb = {0};
    bool success = nob_read_entire_file(in.cstr, &sb);
    if (!success) return false;

    string_builder_t sb_ = {.items=sb.items, .count=sb.count};
    orstring_t src = sb_render(&sb_, arena);

    nob_sb_free(sb);

    *source = src;

    return true;
}

static ast_t *build_ast(orstring_t source, arena_t *arena, orstring_t file_path) {
    ast_t *ast = arena_alloc(arena, sizeof(ast_t));
    ast_init(ast, arena);
    ast->vm = vm_default(arena);

    {
        orstring_t core_path = lit2str("./bin/core.or");
        Nob_String_Builder sb = {0};
        bool success = nob_read_entire_file(core_path.cstr, &sb);
        if (!success) abort();
        
        string_builder_t sb_ = {.items=sb.items, .count=sb.count};

        orstring_t core_source = sb_render(&sb_, ast->arena);

        nob_sb_free(sb);

        ast_node_t *core = parse_source_into_module(ast, core_path, string2sv(core_source));
        ast->core_module_or_null = core;
    }
    
    tmp_arena_t *tmp = allocator_borrow();
    {
        ast_node_t *program = parse_source_into_module(ast, file_path, string2sv(source));

        orstring_t programid = ast_generate_moduleid(file_path, tmp->allocator);
        ast_add_module(ast, program, programid);
    }
    allocator_return(tmp);

    if (ast->errors.count == 0) {
        resolve_ast(ast);
    }

    return ast;
}

static bool generate_exe(ast_t *ast, orstring_t output_path) {
    tmp_arena_t *tmp = allocator_borrow();

    strings_t sources = {0};
    strings_t libs = {0};

    bool success = compile_ast_to_c(ast, lit2str("./build/"), &sources, &libs, tmp->allocator);

    if (success) {
        cc_t cc = cc_make(CC_GCC, tmp->allocator);
        cc.output_type = CC_EXE;
        cc.output_path = string_copy(output_path, tmp->allocator);

        for (size_t i = 0; i < sources.count; ++i) {
            orstring_t src = sources.items[i];
            cc_source(&cc, src);
        }

        for (size_t i = 0; i < libs.count; ++i) {
            orstring_t libpath = libs.items[i];
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

int orso_build(orso_compiler_t *compiler) {
    arena_t arena = {0};

    orstring_t source;
    bool success = load_file(compiler->root_source, &source, &arena);
    if (!success) return 0;

    ast_t *ast = build_ast(source, &arena, compiler->root_source);

    bool result = false;

    if (!ast->resolved) {
        print_errors(ast);
        return_defer(false);
    }

    orstring_t output_file_path = string_path_combine(compiler->build_dir, compiler->output_name, &arena);
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

void print_ast(orstring_t input_file_path) {
    arena_t arena = {0};

    orstring_t source;
    bool success = load_file(input_file_path, &source, &arena);
    if (!success) exit(1);

    ast_t *ast = build_ast(source, &arena, input_file_path);

    if (!ast->resolved) {
        print_errors(ast);
    }
    ast_print(ast, "program");
    arena_free(&arena);
}

bool interpret(orstring_t input_file_path) {
    arena_t arena = {0};

    orstring_t source;
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

bool debug(orstring_t input_file_path) {
    arena_t arena = {0};

    orstring_t source;
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