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


string_view_t get_line(cstr_t source, cstr_t somewhere_in_source) {
    char *s = (char*)somewhere_in_source;
    until (*s == '\n' || s == source)  {
        --s;
    }

    char *e = (char*)somewhere_in_source;
    until (*e == '\n' || *e == '\0') {
        ++e;
    }

    string_view_t view = {.length = e - s, .data = s};
    return view;
}

void myerror(error_t error, cstr_t source) {
    size_t line = error.first.location.line + 1;
    size_t column = error.first.location.column + 1;
    cstr_t file = error.first.file_path.cstr;

    string_view_t source_line = get_line(source, error.first.view.data);

    fprintf(stderr, "%s:%lu:%lu: %s\n", file, line, column, error_messages[error.type]);
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
bool parse_expr_cstr(ast_t *ast, cstr_t expr_source, string_t file_path) {
    ast->source = expr_source;
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

int main(int argc, char **argv) {
    arena_t arena = {0};

    if (argc < 2) {
        nob_log(ERROR, "needs input file");
        return 1;
    }

    cstr_t path = argv[1];
    String_Builder sb = {0};
    bool success = read_entire_file(path, &sb);

    if (!success) {
        nob_log(ERROR, "could not read file at %s", path);
        return 1;
    }
    
    String_View nob_sv = sb_to_sv(sb);
    string_view_t sv = { .data = nob_sv.data, .length = nob_sv.count };
    
    string_t code = sv2string(sv, &arena);

    ast_t ast = {0};
    ast_init(&ast, megabytes(2));

    success = parse_expr_cstr(&ast, code.cstr, lit2str(""));
    unless (success) return 1;

    i64 resultc = INT64_MIN;
    if (false)
    {
        string_t expr_str = compile_expr_to_c(&ast, &arena);

        cc_t cc = cc_make(CC_GCC, &arena);
        cc_mem_source(&cc, expr_str);

        cc_include_dir(&cc, lit2str("./lib"));

        cc.output_type = CC_DYNAMIC;
        cc.output_name = lit2str("liborso.so");

        bool success = cc_build(&cc);

        if (!success) {
            return 1;
        }

        dynlib_t lib = dynlib_load(lit2str("./build/liborso.so"));

        i64 (*expr)(void) = dynlib_symbol(lib, lit2str("expr"));

        resultc = expr();
    }
    
    i64 resultvm = INT64_MIN;
    if (true)
    {
        memarr_t *memory = arena_alloc(&arena, sizeof(memarr_t));
        *memory = (memarr_t){0};

        memarr_init(memory, megabytes(2.5));
        size_t stack_size = (size_t)megabytes(0.5);
        memory->count = stack_size;
        memset(memory->data, 0, stack_size);
        
        function_t *expr_function = new_function(lit2str("<none>"), memory, &arena);

        compile_expr_to_function(expr_function, &ast);

        vm_t vm = {0};
        vm_init(&vm);
        vm.registers[REG_STACK_FRAME].as.u = stack_size;
        vm.registers[REG_STACK_BOTTOM].as.u = stack_size;

        {
            // resultvm = *((i64*)vm_run_function(&vm, expr_function));
        }
        {
            UNUSED(vm_run_function);
            vm_set_entry_point(&vm, expr_function);

            debugger_t debugger = {0};
            debugger_init(&debugger, &arena);
            while (debugger_step(&debugger, &vm));
        }

    }

    nob_log(INFO, "test\n-- %s:\ncgen = %lld;\nvmgen = %lld;\n%s", path, resultc, resultvm, (resultc == resultvm) ? "pass" : "fail");
}

