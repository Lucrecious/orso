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

void print_errors(ast_t *ast) {
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

ast_t *orbuild_ast(orstring_t source, arena_t *arena, orstring_t file_path) {
    ast_t *ast = arena_alloc(arena, sizeof(ast_t));
    ast_init(ast, arena);

    tmp_arena_t *tmp = allocator_borrow();

    {
        
        orstring_t exe_dir = path_get_executable_dir(tmp->allocator);
        orstring_t std_path = path_combine(string2sv(exe_dir), lit2sv("std"), ast->arena);
        array_push(&ast->search_paths, std_path);
    }

    ast->vm = vm_default(arena);

    {

        orstring_t exe_path = path_get_executable_dir(tmp->allocator);
        orstring_t core_path = path_combine(string2sv(exe_path),  lit2sv("std"ORFILE_SEP"core.or"), tmp->allocator);
        Nob_String_Builder sb = {0};
        bool success = nob_read_entire_file(core_path.cstr, &sb);
        if (!success) abort();
        
        string_builder_t sb_ = {.items=sb.items, .count=sb.count};

        orstring_t core_source = sb_render(&sb_, ast->arena);

        nob_sb_free(sb);

        ast_node_t *core = parse_source_into_module(ast, core_path, string2sv(core_source));
        ast->core_module_or_null = core;
    }
    
    {
        ast_node_t *program = parse_source_into_module(ast, file_path, string2sv(source));

        orstring_t programid = ast_generate_moduleid(file_path, tmp->allocator);
        ast_add_module(ast, program, programid);
    }

    if (ast->errors.count == 0) {
        resolve_ast(ast);
    }

    allocator_return(tmp);

    return ast;
}

static bool generate_exe(ast_t *ast, orso_compiler_t *compiler, orstring_t output_path) {

    strings_t sources = {0};
    strings_t libs = {0};

    bool success = nob_mkdir_if_not_exists(compiler->build_dir.cstr);
    if (!success) return false;

    tmp_arena_t *tmp = allocator_borrow();

    success = compile_ast_to_c(ast, compiler->build_dir, &sources, &libs, tmp->allocator);

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

        // todo
        nob_log(NOB_WARNING, "Hardcoded the library for orso here, need robust solution");
        cc_libpath(&cc, lit2str("./bin/liborso.a"));
        
        // stops warnings from ((x == y)) type of conditions
        cc_no_warning(&cc, lit2str("parentheses-equality"));

        cc_flag(&cc, lit2str("-std=c99"));

        for (orsint i = 0; i < compiler->cflags.count; ++i) {
            cc_flag(&cc, compiler->cflags.items[i]);
        }

        for (orsint i = 0; i < compiler->linker_flags.count; ++i) {
            cc_linker_flag(&cc, compiler->linker_flags.items[i]);
        }

        success = cc_build(&cc);
    }

    allocator_return(tmp);


    return success;
}

bool orbuild(orso_compiler_t *compiler) {
    arena_t arena = {0};

    orstring_t source;
    bool success = load_file(compiler->src, &source, &arena);
    if (!success) return 0;

    ast_t *ast = orbuild_ast(source, &arena, compiler->src);

    bool result = false;

    if (ast->errors.count > 0) {
        print_errors(ast);
        return_defer(false);
    }

    orstring_t output_file_path = path_combine(string2sv(compiler->build_dir), string2sv(compiler->output_name), &arena);
    {
        success = generate_exe(ast, compiler, output_file_path);
    }
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

    ast_t *ast = orbuild_ast(source, &arena, input_file_path);

    if (!ast->resolved) {
        print_errors(ast);
    }
    ast_print(ast, "program");
    arena_free(&arena);
}

bool orinterpret(orstring_t input_file_path) {
    arena_t arena = {0};

    orstring_t source;
    bool success = load_file(input_file_path, &source, &arena);
    if (!success) exit(1);

    ast_t *ast = orbuild_ast(source, &arena, input_file_path);

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

    ast_node_t *module;
    function_t *main_or_null;
    kh_foreach_value(ast->moduleid2node, module, {
        main_or_null = find_main_or_null(module);
        if (main_or_null) break;
    });

    if (main_or_null) {
        vm_fresh_run(ast->vm, main_or_null);
    }

    return_defer(true);

defer:
    arena_free(&arena);
    return result;
}

void orassert(bool test, orstring_t message) {
    if (test) return;
    if (message.length == 0) {
        printf("assert failed\n");
    } else {
        printf("assert failed: %s\n", message.cstr);
    }
    abort();
}

bool ordebug(orstring_t input_file_path) {
    arena_t arena = {0};

    orstring_t source;
    bool success = load_file(input_file_path, &source, &arena);
    if (!success) exit(1);

    ast_t *ast = orbuild_ast(source, &arena, input_file_path);

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

    ast_node_t *module;
    function_t *main_or_null;
    kh_foreach_value(ast->moduleid2node, module, {
        main_or_null = find_main_or_null(module);
        if (main_or_null) break;
    });

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

void *orrealloc(void *ptr, size_t new_size) {
    ptr = realloc(ptr, new_size);
    return ptr;
}

void orprintint(orint num) {
    printf("%d\n", num);
}

void orprintln(orstring_t s) {
    printf("%s\n", s.cstr);
}

orf32 orsinf(orf32 value) {
    return sinf(value);
}

orf32 orcosf(orf32 value) {
    return cosf(value);
}

orf32 oratan2f(orf32 y, orf32 x) {
    return atan2f(y, x);
}

orf32 orsqrtf(orf32 v) {
    return sqrtf(v);
}

orf32 orrandf() {
    return (orf32)rand()/(orf32)(RAND_MAX);
}

bool orshell_run(void *cmds, size_t count) {
    orstring_t *scmds = (orstring_t*)cmds;

    Nob_Cmd cmd = {0};
    for (size_t i = 0; i < count; ++i) {
        orstring_t s = scmds[i];
        nob_cmd_append(&cmd, s.cstr);
    }

    bool success = nob_cmd_run_sync(cmd);

    nob_cmd_free(cmd);

    return success;
}

#ifdef _WIN32
#else

#include <time.h>
#include <unistd.h>
#include <sys/mman.h>

void *orreserve(size_t size) {
    void *addr = mmap(NULL, size, PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS,-1, 0);
    return addr;
}

bool ormarkro(void *addr, size_t size) {
    bool success = (oru8)(mprotect(addr, size, PROT_READ) == 0);
    return success;
}

bool ormarkrw(void *addr, size_t size) {
    bool success = (bool)(mprotect(addr, size, PROT_READ|PROT_WRITE) == 0);
    return success;
}

bool orfree(void *addr, size_t size) {
    bool success = (bool)(munmap(addr, size) == 0);
    return success;
}

size_t orpagesize(void) {
    size_t pagesize = (size_t)getpagesize();
    return pagesize;
}

#endif