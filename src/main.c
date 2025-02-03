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

void mywrite(cstr_t chars) {
    printf("%s", chars);
}


bool parse_expr_cstr(ast_t *ast, string_view_t expr_source, string_t file_path, env_t *program_env_or_null, ast_node_t **result_expr) {
    bool success = parse_expr(ast, file_path, expr_source, myerror, result_expr);

    if (success) {
        analyzer_t analyzer = {0};
        analyzer_init(&analyzer, program_env_or_null, mywrite, myerror);
        analyzer.ast = ast;

        success = resolve_ast_expr(&analyzer, ast, *result_expr);

        analyzer_free(&analyzer);
    }

    return success;
}

typedef enum compiler_mode_type_t compiler_mode_type_t;
enum compiler_mode_type_t {
    COMPILER_MODE_TEST,
    COMPILER_MODE_TEST_GEN,
    COMPILER_MODE_DEBUG,
    COMPILER_MODE_AST,
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
        } else if (cstr_eq(arg, "dbg")) {
            mode.type = COMPILER_MODE_DEBUG;
        } else if (cstr_eq(arg, "ast")) {
            mode.type = COMPILER_MODE_AST;
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

bool parse_expr_file(ast_t *ast, string_t expr_file, env_t *program_env_or_null, ast_node_t **out_expr) {
    String_Builder sb = {0};
    bool success = read_entire_file(expr_file.cstr, &sb);

    if (!success) {
        return false;
    }
    
    String_View nob_sv = sb_to_sv(sb);
    string_view_t code = { .data = nob_sv.data, .length = nob_sv.count };

    ast_init(ast);
    code = string2sv(sv2string(code, &ast->allocator));

    success = parse_expr_cstr(ast, code, expr_file, program_env_or_null, out_expr);

    sb_free(sb);

    unless (success) {
        return false;
    }

    return true;
}

static ast_node_t *parse_module(ast_t *ast, string_t filepath) {
    String_Builder sb = {0};
    bool success = read_entire_file(filepath.cstr, &sb);
    if (!success) {
        nob_log(ERROR, "cannot read given module at %s", filepath.cstr);
        exit(1);
    }

    string_t source;
    {
        string_t tmp;
        tmp.cstr = sb.items;
        tmp.length = sb.count;

        source = string_copy(tmp, &ast->allocator);
    }

    sb_free(sb);

    ast_node_t *module = ast_begin_module(ast);
    success = parse_string_to_module(ast, module, filepath, source, myerror);

    ast_end_module(module);

    return module;
}

env_t make_test_env(ast_t *ast, vm_t *vm, arena_t *program_arena) {
    memarr_t *memory = arena_alloc(program_arena, sizeof(memarr_t));
    *memory = (memarr_t){0};

    memarr_init(memory, megabytes(2.5));
    size_t stack_size = (size_t)megabytes(0.5);
    memory->count = stack_size;
    memset(memory->data, 0, stack_size);

    vm_init(vm);
    vm->registers[REG_STACK_FRAME].as.u = stack_size;
    vm->registers[REG_STACK_BOTTOM].as.u = stack_size;
    
    env_t env = {.vm=(vm), .memory=(memory), .arena=(program_arena)};

    ast_node_t *module = parse_module(ast, str("./core.odl"));
    ast_add_module(ast, module, str("core"));

    analyzer_t analyzer = {0};
    analyzer_init(&analyzer, &env, mywrite, myerror);

    resolve_ast(&analyzer, ast);

    analyzer_free(&analyzer);

    return env;
}

static function_t *compile_expr_and_prepare_vm(vm_t *vm, ast_t *ast, env_t *env, ast_node_t *expr) {
    function_t *init_function = new_function(env->memory, env->arena);
    compile_modules(ast, myerror, env->memory, env->arena, init_function);
    vm_fresh_run(vm, init_function);

    function_t *expr_function = new_function(env->memory, env->arena);
    compile_expr_to_function(expr_function, ast, expr, myerror, env->memory, env->arena);

    return expr_function;
}

void test_expr_file(string_t expr_file, arena_t *test_arena) {
    nob_log(INFO, "---- test: %s", expr_file.cstr);

    ast_t ast = {0};
    ast_init(&ast);

    vm_t vm = {0};
    env_t env = make_test_env(&ast, &vm, test_arena);

    ast_node_t *expr;
    unless (parse_expr_file(&ast, expr_file, &env, &expr)) {
        nob_log(ERROR, "could not parse: %s", expr_file.cstr);
        return;
    }
    
    s64 resultvm = INT64_MIN;
    if (true)
    {
        function_t *expr_function = compile_expr_and_prepare_vm(&vm, &ast, &env, expr);

        vm_fresh_run(&vm, expr_function);
        resultvm = vm.registers[REG_RESULT].as.s;
    }

    s64 resultc = INT64_MIN;
    string_t cexpr_str;
    if (true)
    {
        cexpr_str = compile_expr_to_c(&ast, expr, test_arena);

        cc_t cc = cc_make(CC_GCC, test_arena);
        cc.output_type = CC_DYNAMIC;

        cc_mem_source(&cc, cexpr_str);

        cc_include_dir(&cc, lit2str("./lib"));
        cc_no_warning(&cc, lit2str("unused-value"));
        
        // stops warnings from ((x == y)) type of conditions
        cc_no_warning(&cc, lit2str("parentheses-equality"));

        string_view_t filename_ = sv_filename(string2sv(expr_file));
        string_t filename = string_format("%.*s.so", test_arena, filename_.length, filename_.data);
        cc.output_name = filename;

        bool success = cc_build(&cc);

        if (!success) {
            nob_log(ERROR, "could not build c version of file: %s", expr_file.cstr);
            return;
        }

        
        string_t dll_path = string_format("%s/%s", test_arena, cc.build_dir.cstr, filename.cstr);
        dynlib_t lib = dynlib_load(dll_path);

        s64 (*expr)(void) = dynlib_symbol(lib, lit2str("expr"));

        dynlib_unload(lib);

        resultc = expr();
    }

    string_t coutput_file = coutput_file_from_edl(expr_file, test_arena);

    string_view_t ccode = {0};
    {
        String_Builder sb = {0};
        if (file_exists(coutput_file.cstr) && read_entire_file(coutput_file.cstr, &sb)) {
            String_View nob_sv = sb_to_sv(sb);
            ccode = (string_view_t){.data = nob_sv.data, .length=nob_sv.count};
        } else {
        }
    }

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
    ast_t ast = {0};
    ast_init(&ast);

    vm_t vm = {0};
    env_t env = make_test_env(&ast, &vm, arena);

    ast_node_t *expr;
    unless (parse_expr_file(&ast, expr_file, &env, &expr)) {
        nob_log(ERROR, "could not parse: %s", expr_file.cstr);
        return;
    }

    string_t expr_str = compile_expr_to_c(&ast, expr, arena);
    string_t coutput_file = coutput_file_from_edl(expr_file, arena);

    bool success = write_entire_file(coutput_file.cstr, expr_str.cstr, expr_str.length);

    unless (success) {
        nob_log(ERROR, "could not write to file: %s", coutput_file.cstr);
    }
}

void debug_expr_file(string_t expr_file, arena_t *arena) {
    ast_t ast = {0};
    ast_init(&ast);

    vm_t vm = {0};
    env_t env = make_test_env(&ast, &vm, arena);


    ast_node_t *expr;
    unless (parse_expr_file(&ast, expr_file, &env, &expr)) {
        nob_log(ERROR, "could not parse: %s", expr_file.cstr);
        return;
    }

    function_t *expr_function = compile_expr_and_prepare_vm(&vm, &ast, &env, expr);
    vm_set_entry_point(&vm, expr_function);

    debugger_t debugger = {0};
    debugger_init(&debugger, arena);
    while (debugger_step(&debugger, &vm));

}

void ast_expr_file(string_t expr_file, arena_t *arena) {
    UNUSED(arena);

    ast_t ast = {0};
    ast_init(&ast);

    ast_node_t *expr;
    unless (parse_expr_file(&ast, expr_file, NULL, &expr)) {
        nob_log(ERROR, "could not parse: %s", expr_file.cstr);
    }

    ast_print(&ast, expr_file.cstr);
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

            case COMPILER_MODE_DEBUG: {
                debug_expr_file(mode.file_or_dir, &arena);
                break;
            }

            case COMPILER_MODE_AST: {
                ast_expr_file(mode.file_or_dir, &arena);
                break;
            }
        }
    } else {
        File_Paths paths = {0};
        unless (read_entire_dir(mode.file_or_dir.cstr, &paths)) {
            nob_log(ERROR, "could not read files in dir: %s", mode.file_or_dir.cstr);
            return 1;
        }

        arena_t loop_arena = {0};
        for (size_t i = 0; i < paths.count; ++i) {
            arena_reset(&loop_arena);

            string_t file = string_format("%s/%s", &loop_arena, mode.file_or_dir.cstr, paths.items[i]);
            if (sv_ends_with(string2sv(file), ".edl")) {
                switch (mode.type) {
                    case COMPILER_MODE_TEST: {
                        test_expr_file(file, &loop_arena);
                        break;
                    }

                    case COMPILER_MODE_TEST_GEN: {
                        test_gen_expr_file(file, &loop_arena);
                        break;
                    }

                    case COMPILER_MODE_DEBUG: {
                        debug_expr_file(file, &loop_arena);
                        break;
                    }

                    case COMPILER_MODE_AST: {
                        ast_expr_file(mode.file_or_dir, &loop_arena);
                        break;
                    }
                }
            }
        }
    }
}

