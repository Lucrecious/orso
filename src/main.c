#define NOB_IMPLEMENTATION
#define NOB_STRIP_PREFIX
#include "../nob.h"
#undef UNREACHABLE
#undef UNUSED
#undef sv_eq
#undef NOB_IMPLEMENTATION

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

#include "debugger.h"

#include "parser.h"

#include "common.h"

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

void myerror(error_t error) {
    size_t line = error.token.location.line + 1;
    size_t column = error.token.location.column + 1;
    // cstr_t file = error.first.file_path.cstr;

    string_view_t source_line = get_line(error.token.source, error.token.view);

    fprintf(stderr, "%s:%lu:%lu: %s\n", "todo", line, column, error.message);
    fprintf(stderr, "%.*s\n", (int)source_line.length, source_line.data);

    tmp_arena_t *tmp_arena = allocator_borrow();
    string_builder_t sb = {.allocator=tmp_arena->allocator};

    for (size_t i = 0; i < source_line.length; ++i) {
        if (i == column-1) {
            sb_add_char(&sb, '^');
        } else {
            sb_add_char(&sb, ' ');
        }
    }

    if (column-1 == source_line.length) {
        sb_add_char(&sb, '^');
    }

    fprintf(stderr, "%.*s\n", (int)sb.count, sb.items);

    allocator_return(tmp_arena);
}

void mywrite(cstr_t chars) {
    printf("%s", chars);
}


// todo: fix memory leak because too lazy to write an ast_dup function right now
bool parse_expr_cstr(ast_t *ast, string_view_t expr_source, string_t file_path) {
    bool success = parse_expr(ast, file_path, expr_source, myerror);

    if (success) {
        analyzer_t analyzer = {0};
        analyzer_init(&analyzer, mywrite, myerror);
        analyzer.ast = ast;

        success = resolve_ast(&analyzer, ast);

        analyzer_free(&analyzer);
    }

    return success;
}

static void *vm_run_function(vm_t *vm, function_t *function) {
    vm->call_frame = (call_frame_t){.pc = 0, .function = function};
    until (vm->halted) {
        vm_step(vm);
    }

    return &vm->registers[REG_RESULT];
}

typedef enum compiler_mode_type_t compiler_mode_type_t;
enum compiler_mode_type_t {
    COMPILER_MODE_TEST,
    COMPILER_MODE_TEST_GEN,
};

typedef struct compiler_mode_t compiler_mode_t;
struct compiler_mode_t  {
    compiler_mode_type_t type;
    string_t file_or_dir;
};

compiler_mode_t get_compiler_mode_from_args(int argc, char **argv, arena_t *arena) {
    compiler_mode_t mode = {0};
    nob_shift_args(&argc, &argv);

    while (argc > 0) {
        cstr_t arg = shift_args(&argc, &argv);
        if (cstr_eq(arg, "test")) {
            mode.type = COMPILER_MODE_TEST;
        } else if (cstr_eq(arg, "testgen")) {
            mode.type = COMPILER_MODE_TEST_GEN;
        } else {
            mode.file_or_dir = cstr2string(arg, arena);
        }
    }

    return mode;
}

bool get_first_different_line(string_view_t expected, string_view_t actual, size_t *first_different_line) {
    *first_different_line = 0;
    bool is_different = expected.length != actual.length;

    size_t min_size = expected.length < actual.length ? expected.length : actual.length;
    for (size_t i = 0; i < min_size; ++i) {
        if (expected.data[i] != actual.data[i]) {
            return false;
        }

        *first_different_line += (actual.data[i] == '\n');
    }

    return !is_different;
}

string_t coutput_file_from_edl(string_t file, arena_t *arena) {
    string_view_t coutput_file_prefix = string2sv(file);
    coutput_file_prefix.length -= 4;
    string_t coutput_file = string_format("%.*s.c", arena, coutput_file_prefix.length, coutput_file_prefix.data);
    return coutput_file;
}

void test_expr_file(string_t expr_file, arena_t *arena) {
    String_Builder sb = {0};
    bool success = read_entire_file(expr_file.cstr, &sb);

    if (!success) {
        nob_log(ERROR, "could not read file at %s", expr_file.cstr);
        return;
    }
    
    String_View nob_sv = sb_to_sv(sb);
    string_view_t code = { .data = nob_sv.data, .length = nob_sv.count };
    code = string2sv(sv2string(code, arena));

    ast_t ast = {0};
    ast_init(&ast, megabytes(2));

    success = parse_expr_cstr(&ast, code, expr_file);
    unless (success) {
        nob_log(ERROR, "could not parse: %s", expr_file.cstr);
        return;
    }
    
    i64 resultvm = INT64_MIN;
    if (true)
    {
        memarr_t *memory = arena_alloc(arena, sizeof(memarr_t));
        *memory = (memarr_t){0};

        memarr_init(memory, megabytes(2.5));
        size_t stack_size = (size_t)megabytes(0.5);
        memory->count = stack_size;
        memset(memory->data, 0, stack_size);
        
        function_t *expr_function = new_function(lit2str("<none>"), memory, arena);

        compile_expr_to_function(expr_function, &ast);

        vm_t vm = {0};
        vm_init(&vm);
        vm.registers[REG_STACK_FRAME].as.u = stack_size;
        vm.registers[REG_STACK_BOTTOM].as.u = stack_size;

        {
            resultvm = *((i64*)vm_run_function(&vm, expr_function));
        }
        {
            // UNUSED(vm_run_function);
            // vm_set_entry_point(&vm, expr_function);

            // debugger_t debugger = {0};
            // debugger_init(&debugger, &arena);
            // while (debugger_step(&debugger, &vm));
        }
    }

    i64 resultc = INT64_MIN;
    string_t cexpr_str;
    if (true)
    {
        cexpr_str = compile_expr_to_c(&ast, arena);

        cc_t cc = cc_make(CC_GCC, arena);
        cc_mem_source(&cc, cexpr_str);

        cc_include_dir(&cc, lit2str("./lib"));
        cc_no_warning(&cc, lit2str("unused-value"));

        cc.output_type = CC_DYNAMIC;
        cc.output_name = lit2str("liborso.so");

        bool success = cc_build(&cc);

        if (!success) {
            nob_log(ERROR, "could not build c version of file: %s", expr_file.cstr);
            return;
        }

        dynlib_t lib = dynlib_load(lit2str("./build/liborso.so"));

        i64 (*expr)(void) = dynlib_symbol(lib, lit2str("expr"));

        resultc = expr();
    }

    string_t coutput_file = coutput_file_from_edl(expr_file, arena);

    string_view_t ccode = {0};
    {
        String_Builder sb = {0};
        if (file_exists(coutput_file.cstr) && read_entire_file(coutput_file.cstr, &sb)) {
            String_View nob_sv = sb_to_sv(sb);
            ccode = (string_view_t){.data = nob_sv.data, .length=nob_sv.count};
        } else {
        }
    }

    nob_log(INFO, "---- test: %s", expr_file.cstr);
    if (resultc != resultvm) {
        nob_log(ERROR, "c value != vm value; %lld != %lld", resultc, resultvm);
    }

    size_t first_different_line = 0;
    bool is_same = false;
    if (ccode.length) {
        is_same = get_first_different_line(ccode, string2sv(cexpr_str), &first_different_line);
        if (!is_same) {
            nob_log(WARNING, "generated c file does not match expected c file starting at %zu", first_different_line+1);
        }
    } else {
        nob_log(WARNING, "no c file output to test against; test will fail");
    }

    nob_log(INFO, "TEST: %s\n", (resultc == resultvm && is_same) ? "PASS" : "FAIL");
}

void test_gen_expr_file(string_t expr_file, arena_t *arena) {
    String_Builder sb = {0};
    bool success = read_entire_file(expr_file.cstr, &sb);

    if (!success) {
        nob_log(ERROR, "could not read file at %s", expr_file.cstr);
        return;
    }
    
    String_View nob_sv = sb_to_sv(sb);
    string_view_t code = { .data = nob_sv.data, .length = nob_sv.count };
    code = string2sv(sv2string(code, arena));

    ast_t ast = {0};
    ast_init(&ast, megabytes(2));

    success = parse_expr_cstr(&ast, code, expr_file);
    unless (success) {
        nob_log(ERROR, "could not parse: %s", expr_file.cstr);
        return;
    }

    string_t expr_str = compile_expr_to_c(&ast, arena);
    string_t coutput_file = coutput_file_from_edl(expr_file, arena);

    success = write_entire_file(coutput_file.cstr, expr_str.cstr, expr_str.length);

    unless (success) {
        nob_log(ERROR, "could not write to file: %s", coutput_file.cstr);
    }
}

int main(int argc, char **argv) {
    arena_t arena = {0};

    compiler_mode_t mode = get_compiler_mode_from_args(argc, argv, &arena);
    if (sv_ends_with(string2sv(mode.file_or_dir), ".edl")) {
        switch (mode.type) {
            case COMPILER_MODE_TEST: {
                test_expr_file(mode.file_or_dir, &arena);
                break;
            }

            case COMPILER_MODE_TEST_GEN: {
                test_gen_expr_file(mode.file_or_dir, &arena);
                break;
            }
        }
    } else {
        File_Paths paths = {0};
        unless (read_entire_dir(mode.file_or_dir.cstr, &paths)) {
            nob_log(ERROR, "could not read files in dir: %s", mode.file_or_dir.cstr);
            return 1;
        }

        arena_t test_arena = {0};
        for (size_t i = 0; i < paths.count; ++i) {
            arena_reset(&test_arena);

            string_t file = string_format("%s/%s", &test_arena, mode.file_or_dir.cstr, paths.items[i]);
            if (sv_ends_with(string2sv(file), ".edl")) {
                switch (mode.type) {
                    case COMPILER_MODE_TEST: {
                        test_expr_file(file, &arena);
                        break;
                    }

                    case COMPILER_MODE_TEST_GEN: {
                        test_gen_expr_file(mode.file_or_dir, &arena);
                        break;
                    }
                }
            }
        }
    }
}

