#include "parser.h"

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "error.h"
#include "type.h"
#include "type_set.h"
#include "stringt.h"
#include "tmp.h"
#include "../nob.h"

#include "vm.h"
#include "static_analyzer.h"
#include "orso.h"

/*
program                  -> declaration* EOF
declaration              -> definition | statement
definition               -> IDENTIFIER `:` ((logic_or ((`=` | `:`) expression)?) | ((`=` | `:`) expression) `;`
parameters               -> parameter (`,` parameter)*
parameter                -> IDENTIFIER `:` ((logic_or (`=` expression)?)
statement                -> expression_statement
expression_statement     -> expression `;`

directive                -> `#` ~(\s)*
expression               -> directive? (assignment | block | branch)
block                    -> `{` declaration* `}`
branch                   -> (`if` | `unless` | `while` | `until` ) expression (block | (`then` | `do`) expression) (`else` expression)?

assignment               -> (call `.`)? IDENTIFIER `=` expression
                          | logic_or
logic_or                 -> logic_and (`or` logic_and)*
logic_and                -> equality (`and` equality)*
equality                 -> comparison ((`!=` | `==`) comparison)*
comparison               -> term ((`<` | `>` | `<=` | `>=`) term)*
term                     -> factor ((`+` | `-`) factor)*
factor                   -> logical_operations ((`/` | `*`) logical_operations)*
logical_operations       -> unary ((`|` |`&`) unary)*
unary                    -> (`not` | `-` | `&`) unary | call
call                     -> primary ( `(` arguments? `)` ) | `.` IDENTIFIER )*
arguments                -> argument (`,` argument)*
argument                 -> (IDENTIFIER `:`)? expression

primary                  -> `true` | `false` | `null` | IDENTIFIER | INTEGER | DECIMAL
                          | STRING | SYMBOL | `(` expression `)` | function_definition | function_type | struct_definition 
                          | type_init
function_definition      -> `(` parameters? `)` `->` logic_or block
function_type            -> `(` (logic_or (`,` logic_or)*)? `)` `->` logic_or
struct_definition        -> `struct` `{` definition* `}`
type_init                ->  logic_or`{` `}`
*/

static khint_t type_hash(ortype_t id) {
    return kh_int64_hash_func((khint64_t)id.i);
}

static int type_equal_(ortype_t a, ortype_t b) {
    return ortypeid_eq(a, b);
}

implement_table(t2w, ortype_t, orword_t, type_hash, type_equal_)
implement_table(t2n, ortype_t, ast_node_t*, type_hash, type_equal_)
implement_table(type2ns, ortype_t, ast_node_and_scope_t, type_hash, type_equal_)
implement_table(fn2an, void*, ast_node_t*, hashptr_, hasheq_);

static bool streq___(const orstring_t a, const orstring_t b) {
    return string_eq(a, b);
}

uint32_t fnv1_hash__(orstring_t s) {
	uint32_t hash = 0x811C9DC5;

	for (size_t i = 0; i < (size_t)s.length; i++) {
		hash *= 0x01000193;
		hash ^= (uint32_t)(unsigned char)s.cstr[i];
	}

	return hash;
}

implement_table(s2s, orstring_t, oristring_t, fnv1_hash__, streq___)
implement_table(s2w, oristring_t, orword_t, hashptr_, hasheq_)
implement_table(s2n, orstring_t, ast_node_t*, fnv1_hash__, streq___)
implement_table(s2fis, orstring_t, ffi_t*, fnv1_hash__, streq___)

khint_t hashptr_(void *ptr) {
    return (khint_t)(oru64)ptr;
}

bool hasheq_(void *a, void *b) {
    return a == b;
}

implement_table(p2s, void*, orstring_t, hashptr_, hasheq_);

typedef struct parser_t {
    ast_t *ast;
    lexer_t lexer;
    token_t previous;
    token_t current;

    bool inside_type_context;

    bool had_error;
    bool panic_mode;
} parser_t;

typedef enum {
    PREC_NONE,
    PREC_BLOCK,       // {}, branch expressions (if/unless/while/until else)
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_CAST,        // as
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_BITWISE_OR,  // | and type separator
    PREC_UNARY,       // - not &
    PREC_CALL,        // . ()
    PREC_PRIMARY
} prec_t;

typedef ast_node_t* (*ParseFn)(parser_t*);

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    prec_t precedence;
} parse_rule_t;

#define c2i(lit) ast_sv2istring(ast, lit2sv(lit))

static void intrinsic_struct_init(ast_t *ast) {
    #define offset(t, field_name) (((void*)(&(t).field_name)) - ((void*)&(t)))

    ortype_t charptr = type_set_fetch_pointer(&ast->type_set, ast->type_set.char_);

    // str8_t
    struct_binding_t *string_binding = begin_struct_binding(&ast->type_set, lit2str("orstring_t"));
    {
        orstring_t t;
        struct_field_bind(string_binding, charptr, c2i("cstr"), offset(t, cstr));
        struct_field_bind(string_binding, ast->type_set.sint_, c2i("length"), offset(t, length));
        end_struct_binding(string_binding, &ast->type_set);

        ast->type_set.str8_t_ = string_binding->type;
        
        typedata_t *std = type2typedata(&ast->type_set.types, string_binding->type);
        std->kind = TYPE_STRING;
    }

    ortype_t str8_ptr = type_set_fetch_pointer(&ast->type_set, ast->type_set.str8_t_);

    // str8s_t
    struct_binding_t *str8s_binding = begin_struct_binding(&ast->type_set, lit2str("orstr8s_t"));
    {
        orstr8s_t t;
        struct_field_bind(str8s_binding, str8_ptr, c2i("items"), offset(t, items));
        struct_field_bind(str8s_binding, ast->type_set.sint_, c2i("count"), offset(t, count));
        struct_field_bind(str8s_binding, ast->type_set.sint_, c2i("capacity"), offset(t, capacity));
        end_struct_binding(str8s_binding, &ast->type_set);
    }

    // compiler
    struct_binding_t *compiler_binding = begin_struct_binding(&ast->type_set, lit2str("orso_compiler_t"));
    {
        orso_compiler_t t;
        struct_field_bind(compiler_binding, string_binding->type, c2i("src"), offset(t, src));
        struct_field_bind(compiler_binding, string_binding->type, c2i("build_dir"), offset(t, build_dir));
        struct_field_bind(compiler_binding, string_binding->type, c2i("output_name"), offset(t, output_name));
        struct_field_bind(compiler_binding, str8s_binding->type, c2i("cflags"), offset(t, cflags));
        struct_field_bind(compiler_binding, str8s_binding->type, c2i("linker_flags"), offset(t, linker_flags));
        end_struct_binding(compiler_binding, &ast->type_set);

        ast->type_set.compiler = compiler_binding->type;
    }

    // lib_t
    struct_binding_t *lib_binding = begin_struct_binding(&ast->type_set, lit2str("orlib_t"));
    {
        orlib_t t;
        struct_field_bind(lib_binding, ast->type_set.str8_t_, c2i("call_conv"), offset(t, call_conv));
        struct_field_bind(lib_binding, ast->type_set.str8_t_, c2i("static_path"), offset(t, static_path));
        struct_field_bind(lib_binding, ast->type_set.str8_t_, c2i("shared_path"), offset(t, shared_path));

        end_struct_binding(lib_binding, &ast->type_set);

        ast->type_set.lib_ = lib_binding->type;
    }
}

static void directives_init(ast_t *ast, orintrinsic_fns_t *fns) {

    // run
    {
        orintrinsic_fn_t fn = {0};
        fn.name = ast_sv2istring(ast, lit2sv("run"));
        array_push(fns, fn);
    }

    // insert
    {
        orintrinsic_fn_t fn = {0};
        fn.name = ast_sv2istring(ast, lit2sv("insert"));
        array_push(fns, fn);
    }

    // load
    {
        orintrinsic_fn_t fn = {0};
        fn.name = ast_sv2istring(ast, lit2sv("load"));
        fn.has_varargs = false;
        fn.arg_types = (types_t){.allocator=ast->arena};
        array_push(&fn.arg_types, ast->type_set.str8_t_);

        array_push(fns, fn);
    }

    // intrinsic
    {
        orintrinsic_fn_t fn = {0};
        fn.name = ast_sv2istring(ast, lit2sv("intrinsic"));
        fn.has_varargs = false;
        fn.arg_types = (types_t){.allocator=ast->arena};
        array_push(&fn.arg_types, ast->type_set.str8_t_);
        array_push(&fn.arg_types, ast->type_set.type_);

        array_push(fns, fn);
    }

    // fficall
    {
        orintrinsic_fn_t fn = {0};
        fn.name = ast_sv2istring(ast, lit2sv("fficall"));
        fn.has_varargs = true;
        fn.arg_types = (types_t){.allocator=ast->arena};

        array_push(&fn.arg_types, ast->type_set.lib_);
        array_push(&fn.arg_types, ast->type_set.type_);
        array_push(&fn.arg_types, ast->type_set.str8_t_);

        array_push(fns, fn);
    }

    // icall
    {
        orintrinsic_fn_t fn = {0};
        fn.name = ast_sv2istring(ast, lit2sv("icall"));
        fn.has_varargs = true;
        fn.arg_types = (types_t){.allocator=ast->arena};

        array_push(&fn.arg_types, ast->type_set.str8_t_);
        array_push(fns, fn);
    }
}

static void *orso_icall_arg(void *args, size_t *offset, size_t size_bytes)  {
    void *ptr = args + (*offset);
    size_bytes = orb2w2b(size_bytes);
    *offset += size_bytes;
    return ptr;
}

static void __orrealloc(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);

    size_t offset = 0;
    size_t new_cap = *(size_t*)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);
    void *ptr = *(void**)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);

    void *new_ptr = orrealloc(ptr, new_cap);
    
    *(void**)(result) = new_ptr;
}

static void __orprintint(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(result);
    size_t offset = 0;
    orint num = *(orint*)orso_icall_arg(args_reverse_order, &offset, sizeof(orint));
    orprintint(num);
}

orstring_t ast_orstr2str(type_table_t *type_set, void *start);
static void __orshell_run(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    size_t offset = 0;
    size_t count = *(size_t*)orso_icall_arg(args_reverse_order, &offset, sizeof(size_t));
    void *strsptr = *(void**)orso_icall_arg(args_reverse_order, &offset, sizeof(void*));

    orstring_t strs[count];
    typedata_t *strtd = type2typedata(&vm->types->types, vm->types->str8_t_);
    void *start = strsptr;
    for (size_t i = 0; i < count; ++i) {
        orstring_t s = ast_orstr2str(vm->types, start);
        strs[i] = s;
        start += strtd->size;
    }

    bool success = orshell_run((void*)&strs, count);

    *(bool*)result = success;
}

static void __orbuild(struct vm_t *vm, void *args_reverse_order, void *result) {
    size_t offset = 0;
    void *struct_ptr = *(void**)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);

    orso_compiler_t compiler = {0};
    typedata_t *compilertd = type2typedata(&vm->types->types, vm->types->compiler);
    extract_struct_from_binding(compilertd->as.struct_.binding_or_null, vm->types, struct_ptr, &compiler);

    bool success = orbuild(&compiler);

    *(bool*)result = success;
}

static void __orreserve(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);

    size_t offset = 0;
    size_t memsize = *(size_t*)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);

    void *ptr = orreserve(memsize);
    
    *(void**)(result) = ptr;
}

static void __ormarkro(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);

    size_t offset = 0;
    size_t size = *(size_t*)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);
    void *ptr = *(void**)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);

    bool success = ormarkro(ptr, size);
    
    *(bool*)(result) = success;
}

static void __ormarkrw(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);

    size_t offset = 0;
    size_t size = *(size_t*)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);
    void *ptr = *(void**)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);

    bool success = ormarkrw(ptr, size);
    
    *(bool*)(result) = success;
}

static void __orfree(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);

    size_t offset = 0;
    size_t size = *(size_t*)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);
    void *ptr = *(void**)orso_icall_arg(args_reverse_order, &offset, ORWORD_SIZE);

    bool success = orfree(ptr, size);
    
    *(bool*)(result) = success;
}

static void __orpagesize(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(args_reverse_order);

    size_t size = orpagesize();
    
    *(size_t*)(result) = size;
}

static void __orprintln(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(result);

    size_t offset = 0;
    typedata_t *str8td = type2typedata(&vm->types->types, vm->types->str8_t_);
    void *str8_arg = orso_icall_arg(args_reverse_order, &offset, str8td->size);

    orstring_t str8 = {0};
    extract_struct_from_binding(str8td->as.struct_.binding_or_null, vm->types, str8_arg, &str8);

    orprintln(str8);
}

static void __orsinf(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(result);

    size_t offset = 0;
    typedata_t *valuetd = type2typedata(&vm->types->types, vm->types->f32_);
    orf32 value = *(orf32*)orso_icall_arg(args_reverse_order, &offset, valuetd->size);

    value = orsinf(value);

    memcpy(result, &value, sizeof(orf32));
}

static void __orcosf(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(result);

    size_t offset = 0;
    typedata_t *valuetd = type2typedata(&vm->types->types, vm->types->f32_);
    orf32 value = *(orf32*)orso_icall_arg(args_reverse_order, &offset, valuetd->size);

    value = orcosf(value);

    *(orf32*)result = value;
}

static void __oratan2f(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(result);

    size_t offset = 0;
    typedata_t *valuetd = type2typedata(&vm->types->types, vm->types->f32_);
    orf32 y = *(orf32*)orso_icall_arg(args_reverse_order, &offset, valuetd->size);
    orf32 x = *(orf32*)orso_icall_arg(args_reverse_order, &offset, valuetd->size);

    orf32 angle = oratan2f(y, x);

    *(orf32*)result = angle;
}

static void __orsqrtf(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(result);

    size_t offset = 0;
    typedata_t *valuetd = type2typedata(&vm->types->types, vm->types->f32_);
    orf32 value = *(orf32*)orso_icall_arg(args_reverse_order, &offset, valuetd->size);

    value = orsqrtf(value);

    *(orf32*)result = value;
}

static void __orrandf(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(args_reverse_order);

    orf32 value = orrandf();

    *(orf32*)result = value;
}

static void __orassert(struct vm_t *vm, void *args_reverse_order, void *result) {
    NOB_UNUSED(vm);
    NOB_UNUSED(result);

    size_t offset = 0;
    typedata_t *str8td = type2typedata(&vm->types->types, vm->types->str8_t_);
    void *str8_arg = orso_icall_arg(args_reverse_order, &offset, str8td->size);
    bool test = *(bool*)orso_icall_arg(args_reverse_order, &offset, sizeof(bool));

    orstring_t str8 = {0};
    extract_struct_from_binding(str8td->as.struct_.binding_or_null, vm->types, str8_arg, &str8);

    orassert(test, str8);
}

void intrinsics_init(ast_t *ast, orintrinsic_fns_t *fns) {
    ortype_t voidptr = type_set_fetch_pointer(&ast->type_set, ortypeid(TYPE_VOID));

    {
        // assert
        {
            orintrinsic_fn_t fn = {};
            fn.name = ast_sv2istring(ast, lit2sv("assert"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, ast->type_set.bool_);
            array_push(&fn.arg_types, ast->type_set.str8_t_);

            fn.ret_type = ast->type_set.void_;

            fn.fnptr = __orassert;

            array_push(fns, fn);
        }

        // printlin
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("println"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, ast->type_set.str8_t_);

            fn.ret_type = voidptr;

            fn.fnptr = __orprintln;

            array_push(fns, fn);
        }

        // realloc
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("realloc"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, voidptr);
            array_push(&fn.arg_types, ast->type_set.size_t_);

            fn.ret_type = voidptr;

            fn.fnptr = __orrealloc;

            array_push(fns, fn);
        }

        // printint
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("printint"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, ast->type_set.int_);

            fn.ret_type = voidptr;

            fn.fnptr = __orprintint;

            array_push(fns, fn);
        }

        // sinf
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("sinf"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, ast->type_set.f32_);

            fn.ret_type = ast->type_set.f32_;

            fn.fnptr = __orsinf;

            array_push(fns, fn);
        }

        // cosf
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("cosf"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, ast->type_set.f32_);

            fn.ret_type = ast->type_set.f32_;

            fn.fnptr = __orcosf;

            array_push(fns, fn);
        }

        // sqrtf
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("sqrtf"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, ast->type_set.f32_);

            fn.ret_type = ast->type_set.f32_;

            fn.fnptr = __orsqrtf;

            array_push(fns, fn);
        }

        // randf
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("randf"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            fn.ret_type = ast->type_set.f32_;

            fn.fnptr = __orrandf;

            array_push(fns, fn);
        }

        // atan2f
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("atan2f"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            array_push(&fn.arg_types, ast->type_set.f32_);
            array_push(&fn.arg_types, ast->type_set.f32_);

            fn.ret_type = ast->type_set.f32_;

            fn.fnptr = __oratan2f;

            array_push(fns, fn);
        }

        // shell_run
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("shell_run"));
            fn.has_varargs = false;
            fn.arg_types = (types_t){.allocator=ast->arena};

            ortype_t strptrs = type_set_fetch_pointer(&ast->type_set, ast->type_set.str8_t_);
            array_push(&fn.arg_types, strptrs);
            array_push(&fn.arg_types, ast->type_set.size_t_);

            fn.ret_type = ast->type_set.bool_;

            fn.fnptr = __orshell_run;

            array_push(fns, fn);
        }

        // compiler_build
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("build"));
            fn.has_varargs = false;

            fn.arg_types = (types_t){.allocator=ast->arena};

            ortype_t compiler_ptr = type_set_fetch_pointer(&ast->type_set, ast->type_set.compiler);
            array_push(&fn.arg_types, compiler_ptr);

            fn.ret_type = ast->type_set.bool_;

            fn.fnptr = __orbuild;

            array_push(fns, fn);
        }

        // reserve
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("reserve"));
            fn.has_varargs = false;

            fn.arg_types = (types_t){.allocator=ast->arena};
            array_push(&fn.arg_types, ast->type_set.size_t_);

            fn.ret_type = voidptr;

            fn.fnptr = __orreserve;

            array_push(fns, fn);
        }

        // markro
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("markro"));
            fn.has_varargs = false;

            fn.arg_types = (types_t){.allocator=ast->arena};
            array_push(&fn.arg_types, voidptr);
            array_push(&fn.arg_types, ast->type_set.size_t_);

            fn.ret_type = ast->type_set.bool_;

            fn.fnptr = __ormarkro;

            array_push(fns, fn);
        }

        // markrw
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("markrw"));
            fn.has_varargs = false;

            fn.arg_types = (types_t){.allocator=ast->arena};
            array_push(&fn.arg_types, voidptr);
            array_push(&fn.arg_types, ast->type_set.size_t_);

            fn.ret_type = ast->type_set.bool_;

            fn.fnptr = __ormarkrw;

            array_push(fns, fn);
        }

        // free
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("free"));
            fn.has_varargs = false;

            fn.arg_types = (types_t){.allocator=ast->arena};
            array_push(&fn.arg_types, voidptr);
            array_push(&fn.arg_types, ast->type_set.size_t_);

            fn.ret_type = ast->type_set.bool_;

            fn.fnptr = __orfree;

            array_push(fns, fn);
        }

        // pagesize
        {
            orintrinsic_fn_t fn = {0};
            fn.name = ast_sv2istring(ast, lit2sv("pagesize"));
            fn.has_varargs = false;

            fn.arg_types = (types_t){.allocator=ast->arena};
            fn.ret_type = ast->type_set.size_t_;

            fn.fnptr = __orpagesize;

            array_push(fns, fn);
        }
    }

    #undef offset
}

void ast_init(ast_t *ast, arena_t *arena) {
    *ast = zer0(ast_t);
    ast->arena = arena;
    ast->resolved = false;
    
    type_set_init(&ast->type_set, ast->arena);

    memarr_init(&ast->multiword_data, megabytes(0.5));

    ast->errors = (errors_t){.allocator=arena};

    ast->strings = table_new(s2s, ast->arena);
    table_put(s2s, ast->strings, *oriemptystr, oriemptystr);

    ast->builtins = table_new(s2w, ast->arena);
    ast->intrinsicfn2cname = table_new(p2s, ast->arena);
    ast->fn2an = table_new(fn2an, ast->arena);
    ast->moduleid2node = table_new(s2n, ast->arena);
    ast->ffis = table_new(s2fis, ast->arena);
    ast->t2n = table_new(t2n, ast->arena);

    ast->search_paths = (strings_t){.allocator=ast->arena};

    ast->global_decls_in_resolution_order = (ast_nodes_t){.allocator=ast->arena};

    intrinsic_struct_init(ast);

    ast->intrinsics = (orintrinsic_fns_t){.allocator=ast->arena};
    intrinsics_init(ast, &ast->intrinsics);

    ast->directives = (orintrinsic_fns_t){.allocator=ast->arena};
    directives_init(ast, &ast->directives);

    ast->type_to_zero_word = table_new(t2w, ast->arena);
}

ast_node_t nil_node = {
    .node_type=AST_NODE_TYPE_NONE,
    .start = nil_token,
    .end = nil_token,
    .operator = nil_token,

    .value_type = ortypeid(TYPE_INVALID),
    .lvalue_node = &nil_node,
    .ref_decl = &nil_node,
    .jmp_out_scope_node = &nil_node,
    .branch_type = BRANCH_TYPE_IF,
    .ccode_break_label = lit2str(""),
    .ccode_continue_label = lit2str(""),
    .ccode_var_name = lit2str(""),
    .identifier = &oremptystr,
    .label = &oremptystr,
    .last_statement = &nil_node,
};

ast_node_t *ast_node_new(arena_t *arena, ast_node_type_t node_type, token_t start) {
    ast_node_t *node = (ast_node_t*)arena_alloc(arena, sizeof(ast_node_t));
    *node = (ast_node_t){0};
    
    node->node_type = node_type;
    node->start = start;
    node->end = start;
    node->operator = start;
    node->value_type.i = TYPE_UNRESOLVED;
    node->condition_negated = false;
    node->branch_type = BRANCH_TYPE_IF;
    node->requires_tmp_for_cgen = true;
    node->vm_jmp_index = 0;
    node->ccode_break_label = lit2str("");
    node->ccode_continue_label = lit2str("");
    node->ccode_var_name = lit2str("");
    node->ffi_or_null = NULL;
    node->subscript_call_or_null = NULL;

    node->param_end = 0;

    node->is_global = false;
    node->is_mutable = false;
    node->is_exported = false;
    node->is_macro = false;
    node->identifier = oriemptystr;
    node->label = oriemptystr;;
    node->ref_decl = &nil_node;
    node->arg_index = 0;
    node->value_offset = 0;
    node->is_compile_time_param = false;
    node->is_in_outer_function_scope = false;
    node->is_subscript_function = false;

    node->jmp_nodes.allocator = arena;
    node->jmp_out_scope_node = &nil_node;

    node->lvalue_node = &nil_node;
    node->expr_val = ast_node_val_nil();
    node->is_free_number = false;

    node->last_statement = &nil_node;

    node->filepath = lit2str("");

    node->call_scope = NULL;

    node->children = (ast_nodes_t){.allocator=arena};
    node->owned_funcdefs = (ast_nodes_t){.allocator=arena};
    node->func_deps = (functions_t){.allocator=arena};
    node->module_deps = (ast_nodes_t){.allocator=arena};
    node->realized_copies = (inferred_copies_t){.allocator=arena};
    node->type_decl_patterns = (type_patterns_t){.allocator=arena};
    node->tmp_decls = (ast_nodes_t){.allocator=arena};

    switch (node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: break;

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }
        
        case AST_NODE_TYPE_MODULE: break;
        case AST_NODE_TYPE_EXPRESSION_BLOCK: break;
        
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: break;

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            array_push(&node->children, &nil_node);
            break;
        }
    
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: {
            array_push(&node->children, &nil_node);
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT: break;

        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: break;
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: break;
        
        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: break;
        case AST_NODE_TYPE_EXPRESSION_NIL: break;
        
        case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS: {
            array_push(&node->children, &nil_node);
            break;
        }

        case AST_NODE_TYPE_NONE: break;// UNREACHABLE();
    }

    return node;
}

type_path_t *type_path_copy(type_path_t *path, arena_t *arena) {
    if (path == NULL) return NULL;

    type_path_t *new_path = arena_alloc(arena, sizeof(type_path_t));
    *new_path = *path;
    new_path->next = type_path_copy(new_path->next, arena);

    return new_path;
}

ast_node_t *ast_node_copy(arena_t *arena, ast_node_t *node) {
    ast_node_t *copy = ast_node_new(arena, node->node_type, node->start);
    // todo: i wanna do this instead somehow and copy the rest
    // *copy = *node;

    copy->arg_index = node->arg_index;
    copy->branch_type = node->branch_type;
    copy->ccode_break_label = node->ccode_break_label;
    copy->ccode_continue_label = node->ccode_continue_label;
    copy->ccode_var_name = node->ccode_var_name;

    copy->subscript_call_or_null = NULL;
    MUST(node->subscript_call_or_null == NULL);

    if (node->defined_scope.creator) {
        scope_init(&copy->defined_scope, arena, node->defined_scope.type, node->defined_scope.outer, copy);

        oristring_t name;
        orword_t word;
        int _a;
        kh_foreach(node->defined_scope.definitions, name, word, {
            khint_t index = kh_put(s2w, copy->defined_scope.definitions, name, &_a);
            kh_val(copy->defined_scope.definitions, index) = word;
        });
    }

    for (size_t i = copy->children.count; i < node->children.count; ++i) {
        array_push(&copy->children, &nil_node);
    }

    for (size_t i = 0; i < node->children.count; ++i) {
        ast_node_t *child_copy = node->children.items[i];
        child_copy = ast_node_copy(arena, child_copy);
        copy->children.items[i] = child_copy;
    }

    copy->condition_negated = node->condition_negated;
    copy->end = node->end;
    copy->expr_val = node->expr_val;
    copy->filepath = string_copy(node->filepath, arena);
    copy->has_default_value = node->has_default_value;
    copy->identifier = node->identifier;
    copy->is_compile_time_param = node->is_compile_time_param;
    copy->is_consumed = node->is_consumed;
    copy->is_exported = node->is_exported;
    copy->is_free_number = node->is_free_number;
    copy->is_global = node->is_global;
    copy->is_mutable = node->is_mutable;
    copy->is_macro = node->is_macro;
    copy->is_in_outer_function_scope = node->is_in_outer_function_scope;
    copy->is_subscript_function = node->is_subscript_function;

    copy->call_scope = node->call_scope;

    ASSERT(copy->jmp_nodes.count == 0, "need to convert references to other nodes to relative ones");
    ASSERT(copy->jmp_out_scope_node == &nil_node, "need to convert references to other nodes to relative ones");

    copy->label = node->label;

    ASSERT(copy->last_statement= &nil_node, "need to convert references to other nodes to relative ones");
    ASSERT(copy->lvalue_node== &nil_node, "need to convert references to other nodes to relative ones");

    copy->node_type = node->node_type;

    copy->operator = node->operator;

    MUST(copy->owned_funcdefs.count == 0);
    MUST(copy->module_deps.count == 0);

    copy->param_end = node->param_end;

    copy->realized_copies = node->realized_copies;
    ASSERT(copy->ref_decl == &nil_node, "need to convert references to other nodes to relative ones");
    copy->requires_tmp_for_cgen = node->requires_tmp_for_cgen;
    copy->start = node->start;

    for (size_t i = 0; i < node->type_decl_patterns.count; ++i) {
        type_pattern_t pattern = node->type_decl_patterns.items[i];

        pattern.expected = type_path_copy(pattern.expected, arena);

        array_push(&copy->type_decl_patterns, pattern);
    }

    copy->value_offset = node->value_offset;
    copy->value_type = node->value_type;
    copy->vm_jmp_index = node->vm_jmp_index;
    copy->vm_val_dst = node->vm_val_dst;

    return copy;
}

ast_node_t *ast_begin_module(ast_t *ast) {
    ast_node_t *module = ast_node_new(ast->arena, AST_NODE_TYPE_MODULE, nil_token);
    return module;
}

void ast_end_module(ast_node_t *module) {
    if (module->children.count == 0) return;

    token_t min_token = module->children.items[0]->start;
    token_t max_token = module->children.items[0]->end;
    for (size_t i = 1; i < module->children.count; ++i) {
        token_t start = module->children.items[i]->start;
        token_t end = module->children.items[i]->end;

        if (start.loc.line < min_token.loc.line
        || (start.loc.line == min_token.loc.line && start.loc.column < min_token.loc.column)) min_token = start;

        if (end.loc.line > max_token.loc.line
        || (end.loc.line == max_token.loc.line && end.loc.column > max_token.loc.column)) max_token = end;
    }

    module->start = min_token;
    module->end = max_token;
}

void ast_add_module(ast_t *ast, ast_node_t *module, orstring_t moduleid) {
    moduleid = string_copy(moduleid, ast->arena);
    table_put(s2n, ast->moduleid2node, moduleid, module);
}

ast_node_t *ast_implicit_expr(ast_t *ast, ortype_t type, orword_t value, token_t where) {
    ast_node_t *expr = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_PRIMARY, where);
    expr->value_type = type;
    expr->expr_val = ast_node_val_word(value);
    return expr;
}

oristring_t ast_sv2istring(ast_t *ast, string_view_t view) {
    tmp_arena_t *tmp = allocator_borrow();
    orstring_t s = sv2string(view, tmp->allocator);

    oristring_t is;
    if (!table_get(s2s, ast->strings, s, &is)) {
        orstring_t *i = arena_alloc(ast->arena, sizeof(orstring_t));
        *i = sv2string(view, ast->arena);
        is = i;

        table_put(s2s, ast->strings, *i, i);
    }

    allocator_return(tmp);

    return is;
}

oristring_t ast_next_tmp_name(ast_t *ast) {
    tmp_arena_t *tmp = allocator_borrow();

    size_t next = ++ast->tmp_counter;
    orstring_t tmpname = string_format("tmp%zu", tmp->allocator, next);

    oristring_t result = ast_sv2istring(ast, string2sv(tmpname));

    allocator_return(tmp);

    return result;
}


ast_node_t *ast_decldef(ast_t *ast, oristring_t identifier, ast_node_t *type_expr, ast_node_t *init_expr, token_t start) {
    ast_node_t *definition_node = ast_node_new(ast->arena, AST_NODE_TYPE_DECLARATION_DEFINITION, start);
    definition_node->identifier = identifier;
    an_decl_type(definition_node) = type_expr;
    an_decl_expr(definition_node) = init_expr;
    definition_node->end = init_expr->end;

    return definition_node;
}

ast_node_t *ast_nil(ast_t *ast, ortype_t value_type, token_t token_location) {
    ast_node_t *nil_node = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_NIL, token_location);
    nil_node->value_type = value_type;
    nil_node->expr_val = zero_value(ast, value_type);
    return nil_node;
}

ast_node_t *ast_def_value(ast_t *ast, oristring_t identifier, token_t start) {
    ast_node_t *def_value = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_DEF_VALUE, start);
    def_value->identifier = identifier;
    return def_value;
}

ast_node_t *ast_cast(ast_t *ast, ast_node_t *type_expr, ast_node_t *expr) {
    ast_node_t *cast_ = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_CAST, type_expr->start);
    cast_->end = expr->end;

    an_cast_type(cast_) = type_expr;
    an_cast_expr(cast_) = expr;
    return cast_;
}
ast_node_t *ast_struct_begin(ast_t *ast, token_t start) {
    ast_node_t *struct_ = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_STRUCT, start);
    struct_->start = start;

    return struct_;
}

void ast_struct_add_param(ast_node_t *struct_, ast_node_t *decl) {
    array_insert(&struct_->children, struct_->param_end, decl);
    ++struct_->param_end;
}

void ast_struct_add_decl(ast_node_t *struct_, ast_node_t *decl) {
    array_push(&struct_->children, decl);
}

void ast_struct_end(ast_node_t *struct_, token_t end) {
    struct_->end = end;
}

static ast_node_t *ast_primaryu(ast_t *ast, oru64 value, ortype_t type, token_t token) {
    if (TYPE_IS_UNRESOLVED(type)) {
        type = ast->type_set.u64_;
    }

    typedata_t *td = type2typedata(&ast->type_set.types, type);

    orword_t word = {0};
    switch ((num_size_t)td->size) {
    case NUM_SIZE_8: {
        oru8 v = orcast(oru8, value);
        word.as.u = v;
        break;
    }

    case NUM_SIZE_16: {
        oru16 v = orcast(oru16, value);
        word.as.u = v;
        break;
    }

    case NUM_SIZE_32: {
        oru32 v = orcast(oru32, value);
        word.as.u = v;
        break;
    }

    case NUM_SIZE_64: {
        word.as.u = value;
        break;
    }

    default: UNREACHABLE();
    }

    ast_node_t *primary = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_PRIMARY, token);
    primary->value_type = type;
    primary->expr_val = ast_node_val_word(word);

    return primary;
}

static ast_node_t *ast_primaryi(ast_t *ast, ors64 value, ortype_t type, token_t token) {
    if (TYPE_IS_UNRESOLVED(type)) {
        type = ast->type_set.sint_;
    }

    typedata_t *td = type2typedata(&ast->type_set.types, type);

    orword_t word = {0};
    switch ((num_size_t)td->size) {
    case NUM_SIZE_8: {
        ors8 v = orcast(ors8, value);
        word.as.s = v;
        break;
    }

    case NUM_SIZE_16: {
        ors16 v = orcast(ors16, value);
        word.as.s = v;
        break;
    }

    case NUM_SIZE_32: {
        ors32 v = orcast(ors32, value);
        word.as.s = v;
        break;
    }

    case NUM_SIZE_64: {
        word.as.s = value;
        break;
    }

    default: UNREACHABLE();
    }

    ast_node_t *primary = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_PRIMARY, token);
    primary->expr_val = ast_node_val_word(word);
    primary->value_type = type;

    return primary;
}

static ast_node_t *ast_primaryf(ast_t *ast, orf64 value, ortype_t type, token_t token) {
    if (TYPE_IS_UNRESOLVED(type)) {
        type = ast->type_set.f32_;
    }

    typedata_t *td = type2typedata(&ast->type_set.types, type);

    orword_t word = {0};
    switch ((num_size_t)td->size) {
    case NUM_SIZE_8: UNREACHABLE(); break;
    case NUM_SIZE_16: UNREACHABLE(); break;
    case NUM_SIZE_32: {
        orf32 v = (orf32)value;
        word.as.d = v;
        break;
    }

    case NUM_SIZE_64: {
        word.as.d = value;
        break;
    }
    }

    ast_node_t *primary = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_PRIMARY, token);
    primary->expr_val = ast_node_val_word(word);
    primary->value_type = type;

    return primary;
}

static ast_node_t *ast_primaryb(ast_t *ast, bool value, ast_node_t extra_params) {
    ast_node_t *primary = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_PRIMARY, extra_params.start);

    bool byte_value = (bool)value;
    orword_t word = {.as.s=byte_value};

    primary->expr_val = ast_node_val_word(word);
    primary->value_type = ortypeid(TYPE_BOOL);

    return primary;
}

static ast_node_t *ast_primary_str(ast_t *ast, token_t where) {
    ast_node_t *primary = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_PRIMARY, where);
    primary->value_type = ortypeid(TYPE_UNRESOLVED);

    return primary;
}

ast_node_t *ast_statement(ast_t *ast, ast_node_t *expr) {
    ast_node_t *statement_node = ast_node_new(ast->arena, AST_NODE_TYPE_DECLARATION_STATEMENT, expr->start);
    an_expression(statement_node) = expr;
    statement_node->end = expr->end;
    return statement_node;
}

static ast_node_t *ast_break(ast_t *ast, ast_node_t *expr, token_t label, token_t jmp_token) {
    ast_node_t *break_ = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_JMP, jmp_token);
    an_expression(break_) = expr;
    break_->identifier = ast_sv2istring(ast, label.view);
    break_->end = expr->end;
    return break_;
}

static ast_node_t *ast_continue(ast_t *ast, token_t label, token_t jmp_token) {
    ast_node_t *continue_ = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_JMP, jmp_token);
    an_expression(continue_) = ast_nil(ast, ortypeid(TYPE_VOID), token_implicit_at_end(label));
    continue_->identifier = ast_sv2istring(ast, label.view);
    continue_->end = label;
    return continue_;
}

static ast_node_t *ast_return(ast_t *ast, ast_node_t *expr, token_t jmp_token) {
    ast_node_t *return_ = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_JMP, jmp_token);
    an_expression(return_) = expr;
    return_->end = expr->end;
    return return_;
}

ast_node_t *ast_block_begin(ast_t *ast, token_t start) {
    ast_node_t *block = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_BLOCK, start);
    return block;
}

void ast_block_decl(ast_node_t *block, ast_node_t *decl) {
    array_push(&block->children, decl);
}

void ast_block_end(ast_node_t *block, token_t end) {
    block->end = end;

    if (block->children.count == 0) {
        block->node_type = AST_NODE_TYPE_EXPRESSION_NIL;
        block->value_type = ortypeid(TYPE_VOID);
    }
}

static ast_node_t *ast_while(ast_t *ast, ast_node_t *cond, bool cond_negated, ast_node_t *then, ast_node_t *else_, token_t while_token) {
    ast_node_t *while_ = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_BRANCHING, while_token);
    while_->branch_type = BRANCH_TYPE_WHILE;
    while_->condition_negated = cond_negated;
    an_condition(while_) = cond;
    an_then(while_) = then;
    an_else(while_) = else_;

    while_->end = else_->end;

    return while_;
}

static ast_node_t *ast_ifthen(ast_t *ast, ast_node_t *cond, bool cond_negated, ast_node_t *then, ast_node_t *else_, token_t if_token) {
    ast_node_t *ifthen = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_BRANCHING, if_token);
    ifthen->branch_type = BRANCH_TYPE_IF;
    ifthen->condition_negated = cond_negated;
    an_condition(ifthen) = cond;
    an_then(ifthen) = then;
    an_else(ifthen) = else_;

    ifthen->end = else_->end;

    return ifthen;
}

ast_node_t *ast_do(ast_t *ast, oristring_t label, ast_node_t *expr, token_t do_token) {
    ast_node_t *do_ = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_BRANCHING, do_token);
    do_->branch_type = BRANCH_TYPE_DO;
    do_->identifier = label;
    an_expression(do_) = expr;
    do_->end = expr->end;
    return do_;
}

ast_node_t *ast_inferred_type_decl(ast_t *ast, token_t squiggle_token, oristring_t identifer, token_t end) {
    ast_node_t *inferred_type_decl = ast_node_new(ast->arena, AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL, squiggle_token);
    inferred_type_decl->end = end;
    inferred_type_decl->identifier = identifer;
    inferred_type_decl->is_mutable = false;
    inferred_type_decl->value_type = ortypeid(TYPE_UNRESOLVED);

    return inferred_type_decl;
}

static ast_node_t *ast_binary(ast_t *ast, token_t operator, ast_node_t *left, ast_node_t *right) {
    ast_node_t *binary = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_BINARY, left->start);
    binary->operator = operator;
    binary->end = right->end;
    an_lhs(binary) = left;
    an_rhs(binary) = right;

    return binary;
}

static ast_node_t *ast_array_type(ast_t *ast, ast_node_t *size_expr, ast_node_t *type_expr) {
    ast_node_t *array_size = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE, size_expr->start);
    an_array_size_expr(array_size) = size_expr;
    an_array_type_expr(array_size) = type_expr;
    array_size->end = type_expr->end;
    return array_size;
}

static ast_node_t *ast_item_access(ast_t *ast, ast_node_t *accessee, ast_node_t *accessor) {
    ast_node_t *item_access = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS, accessee->start);
    an_item_accessee(item_access) = accessee;
    an_item_accessor(item_access) = accessor;
    item_access->end = accessor->end;
    return item_access;
}

ast_node_t *ast_unary(ast_t *ast, token_t operator, ast_node_t *operand) {
    ast_node_t *unary = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_UNARY, operator);
    unary->operator = operator;
    unary->end = operand->end;
    an_operand(unary) = operand;
    return unary;
}

static ast_node_t *ast_for(ast_t *ast, ast_node_t *decl, ast_node_t *cond, ast_node_t *incr, ast_node_t *loop, ast_node_t *then, token_t start) {
    ast_node_t *n = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_BRANCHING, start);
    n->branch_type = BRANCH_TYPE_FOR;
    an_for_decl(n) = decl;
    an_condition(n) = cond;
    an_for_incr(n) = incr;
    an_then(n) = loop;
    an_else(n) = then;

    return n;
}

ast_node_t *ast_call_begin(ast_t *ast, ast_node_t *callee_or_null, token_t start) {
    ast_node_type_t type;
    if (callee_or_null) {
        type = AST_NODE_TYPE_EXPRESSION_CALL;
    } else {
        type = AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL;
    }

    ast_node_t *call = ast_node_new(ast->arena, type, start);
    if (callee_or_null) an_callee(call) = callee_or_null;

    return call;
}

void ast_call_end(ast_node_t *call, token_t end) {
    call->end = end;
}

static void parser_init(parser_t *parser, ast_t *ast, orstring_t file_path, string_view_t source) {
    lexer_init(&parser->lexer, string_copy(file_path, ast->arena), source);
    parser->ast = ast;
    parser->had_error = false;
    parser->panic_mode = false;
    parser->inside_type_context = false;
}

static void parser_error(parser_t *parser, error_t error) {
    if (parser->panic_mode) return;

    parser->had_error = true;
    parser->panic_mode = true;
    array_push(&parser->ast->errors, error);
}

static void advance(parser_t *parser) {
    parser->previous = parser->current;
    parser->current = lexer_next_token(&parser->lexer);
}

static bool consume(parser_t *parser, token_type_t type) {
    if (parser->current.type == type) {
        advance(parser);
        return true;
    }
    return false;
}

static FORCE_INLINE bool check(parser_t* parser, token_type_t type) {
    return parser->current.type == type;
}

static bool match(parser_t* parser, token_type_t type) {
    if (!check(parser, type)) {
        return false;
    }

    advance(parser);
    return true;
}

static void synchronize(parser_t* parser) {
    parser->panic_mode = false;

    until (check(parser, TOKEN_EOF)) {
        switch (parser->previous.type) {
            case TOKEN_SEMICOLON:
                return;
            
            default: break;
        }

        advance(parser);

    }
}

static ast_node_t *parse_decl(parser_t *parser);
static ast_node_t *parse_expression(parser_t *parser);
static ast_node_t *parse_statement(parser_t *parser);
static parse_rule_t *parser_get_rule(token_type_t type);
static bool check_expression(parser_t *parser);
static ast_node_t* parse_precedence(parser_t* parser, prec_t precedence);

bool ast_node_type_is_expression(ast_node_type_t node_type) {
    switch (node_type) {
        case AST_NODE_TYPE_EXPRESSION_CASE:
            return true;
        
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_NONE:
            return false;
    }
}

token_t token_implicit_at_start(token_t token) {
    token.view.length = 0;
    return token;
}

token_t token_implicit_at_end(token_t token) {
    token.view.data += token.view.length;
    token.loc.column += token.view.length;
    token.view.length = 0;
    return token;
}

static ast_node_t *parse_number(parser_t *parser) {
    switch (parser->previous.type)  {
        case TOKEN_INTEGER: {
            token_t token = parser->previous;
            string_view_t numsv = token.view;
            // todo: check for overflow on u64 and s64
            oru64 value = cstrn_to_u64(numsv.data, numsv.length);

            bool is_free_number = false;

            ortype_t type = ortypeid(TYPE_UNRESOLVED);
            if (sv_ends_with(numsv, "s")) {
                type = parser->ast->type_set.sint_;
            } else if (sv_ends_with(numsv, "u")) {
                type = parser->ast->type_set.uint_;
            } else if (sv_ends_with(numsv, "s8")) {
                type = parser->ast->type_set.s8_;
            } else if (sv_ends_with(numsv, "u8")) {
                type = parser->ast->type_set.u8_;
            } else if (sv_ends_with(numsv, "s16")) {
                type = parser->ast->type_set.s16_;
            } else if (sv_ends_with(numsv, "u16")) {
                type = parser->ast->type_set.u16_;
            } else if (sv_ends_with(numsv, "s32")) {
                type = parser->ast->type_set.s32_;
            } else if (sv_ends_with(numsv, "u32")) {
                type = parser->ast->type_set.u32_;
            } else if (sv_ends_with(numsv, "s64")) {
                type = parser->ast->type_set.s64_;
            } else if (sv_ends_with(numsv, "u64")) {
                type = parser->ast->type_set.u64_;
            } else if (sv_ends_with(numsv, "sz")) {
                type = parser->ast->type_set.size_t_;
            } else if (sv_ends_with(numsv, "d")) {
                type = parser->ast->type_set.f64_;
            } else if (sv_ends_with(numsv, "f")) {
                type = parser->ast->type_set.f32_;
            } else {
                is_free_number = true;
            }

            num_type_t num_type = NUM_TYPE_SIGNED;
            unless (TYPE_IS_UNRESOLVED(type)) {
                typedata_t *td = type2typedata(&parser->ast->type_set.types, type);
                num_type = td->as.num;

                switch((num_size_t)td->size) {
                case NUM_SIZE_8: {
                    switch (num_type) {
                    case NUM_TYPE_SIGNED: {
                        if (value > INT8_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = "syn.overflow.s8-value",
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in an 's8'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                        break;
                    }

                    case NUM_TYPE_UNSIGNED: {
                        if (value > UINT8_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = "syn.overflow.u8-value",
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 'u8'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                        break;
                    }

                    case NUM_TYPE_FLOAT: UNREACHABLE(); break;
                    }

                    break;
                }

                case NUM_SIZE_16: {
                    switch (num_type) {
                    case NUM_TYPE_SIGNED: {
                        if (value > INT16_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = "syn.overflow.s16-value",
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 's16'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                        break;
                    }

                    case NUM_TYPE_UNSIGNED: {
                        if (value > UINT16_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = "syn.overflow.u16-value",
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 'u16'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                        break;
                    }
                    case NUM_TYPE_FLOAT: UNREACHABLE(); break;
                    }

                    break;
                }

                case NUM_SIZE_32: {
                    switch (num_type) {
                    case NUM_TYPE_SIGNED: {
                        if (value > INT32_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = "syn.overflow.s32-value",
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 's32'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                        break;
                    }

                    case NUM_TYPE_UNSIGNED: {
                        if (value > UINT32_MAX) {
                            parser_error(parser, OR_ERROR(
                                .tag = "syn.overflow.u32-value",
                                .level = ERROR_SOURCE_PARSER,
                                .msg = lit2str("number too large to fit in a 'u32'"),
                                .args = ORERR_ARGS(error_arg_token(token)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                        break;
                    }

                    case NUM_TYPE_FLOAT: {
                        break;
                    }
                    }
                    break;
                }

                case NUM_SIZE_64: break;
                }
            }


            ast_node_t *primary;
            switch (num_type) {
            case NUM_TYPE_SIGNED: {
                primary = ast_primaryi(parser->ast, (ors32)value, type, parser->previous);
                break;
            }

            case NUM_TYPE_UNSIGNED: {
                primary = ast_primaryu(parser->ast, value, type, parser->previous);
                break;
            }

            case NUM_TYPE_FLOAT: {
                primary = ast_primaryf(parser->ast, value, type, parser->previous);
                break;
            }
            }

            primary->is_free_number = is_free_number;

            return primary;
        }

        case TOKEN_FLOAT: {
            orf64 value = cstrn_to_f64(parser->previous.view.data, parser->previous.view.length);

            bool is_number_free = false;
            ortype_t type = parser->ast->type_set.f64_;
            if (sv_ends_with(parser->previous.view, "f")) {
                type = parser->ast->type_set.f32_;
            } else if (sv_ends_with(parser->previous.view, "d")) {
                type = parser->ast->type_set.f64_;
            } else {
                is_number_free = true;
            }

            ast_node_t *primary = ast_primaryf(parser->ast, value, type, parser->previous);
            primary->is_free_number = is_number_free;
            return primary;
        }

        default: UNREACHABLE();
    }
}

static ast_node_t *parse_jmp(parser_t *parser) {
    token_t jmp_token = parser->previous;
    token_type_t jmp_type = parser->previous.type;

    token_t label = token_implicit_at_end(jmp_token);
    if (jmp_type == TOKEN_BREAK || jmp_type == TOKEN_CONTINUE) {
        if (match(parser, TOKEN_COLON)) {
            unless (consume(parser, TOKEN_IDENTIFIER)) {
                parser_error(parser, OR_ERROR(
                    .tag = "syn.missing-colon.break-or-continue",
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected a jmp label after ':'"),
                    .args = ORERR_ARGS(error_arg_token(parser->previous)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
            label = parser->previous;
        }
    }

    ast_node_t *expr = NULL;

    bool has_expr = jmp_type == TOKEN_BREAK || jmp_type == TOKEN_RETURN;
    if (has_expr && check_expression(parser)) {
        expr = parse_expression(parser);
    } else {
        expr = ast_nil(parser->ast, ortypeid(TYPE_VOID), token_implicit_at_end(parser->previous));
    }


    switch (jmp_type) {
        case TOKEN_BREAK: return ast_break(parser->ast, expr, label, jmp_token);
        case TOKEN_CONTINUE: return ast_continue(parser->ast, label, jmp_token);
        case TOKEN_RETURN: return ast_return(parser->ast, expr, jmp_token);
        default: UNREACHABLE();
    }
}

static ast_node_t *parse_literal(parser_t *parser) {
    switch (parser->previous.type) {
        case TOKEN_FALSE:
        case TOKEN_TRUE: {
            bool is_true = parser->previous.type == TOKEN_TRUE;
            return ast_primaryb(parser->ast, is_true, (ast_node_t) {
                .start = parser->previous,
            });
            break;
        }

        case TOKEN_STRING: {
            return ast_primary_str(parser->ast, parser->previous);
        }

        case TOKEN_SYMBOL: {
            // todo
            UNREACHABLE();
            break;
        }

        default: UNREACHABLE();
    }
}

static oristring_t parse_label_or_nil(parser_t *parser) {
    oristring_t label = oriemptystr;

    if (check(parser, TOKEN_IDENTIFIER)) {
        lexer_t lookahead = parser->lexer;
        token_t maybe_colon = lexer_next_token(&lookahead);
        if (maybe_colon.type == TOKEN_COLON) {
            bool success = consume(parser, TOKEN_IDENTIFIER);

            label = ast_sv2istring(parser->ast, parser->previous.view);

            success &= consume(parser, TOKEN_COLON);
            MUST(success);
        }
    }

    return label;
}

static void parse_call_arguments(parser_t *parser, ast_node_t *parent) {
    unless (check(parser, TOKEN_PARENTHESIS_CLOSE)) {
        do {
            oristring_t label = parse_label_or_nil(parser);
            if (check_expression(parser)) {
                ast_node_t *argument = parse_expression(parser);
                argument->label = label;
                array_push(&parent->children, argument);
            } else {
                ast_node_t *unique_nil_node = ast_node_new(parser->ast->arena, AST_NODE_TYPE_NONE, parser->previous);
                unique_nil_node->label = label;
                array_push(&parent->children, unique_nil_node);
            }
        } while (match(parser, TOKEN_COMMA));
    }

    unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-rparen.call-args",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected close parenthesis"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }
}

static ast_node_t *parse_builtin_call(parser_t *parser) {
    token_t identifier = parser->previous;

    unless (consume(parser, TOKEN_PARENTHESIS_OPEN)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-lparen.builtin-call",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected open parenthesis after builtin function name"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *n = ast_call_begin(parser->ast, NULL, identifier);
    n->identifier = ast_sv2istring(parser->ast, identifier.view);
    n->operator = identifier;
    parse_call_arguments(parser, n);
    ast_call_end(n, parser->previous);

    return n;
}

ast_node_t *ast_assignment(ast_t *ast, ast_node_t *lhs, ast_node_t *rhs, token_t equals) {
    ast_node_t *assignment = ast_binary(ast, equals, lhs, rhs);
    assignment->node_type = AST_NODE_TYPE_EXPRESSION_ASSIGNMENT;
    return assignment;
}

static ast_node_t *convert_assignment_expression(parser_t *parser, ast_node_t *left, ast_node_t *assignment) {
    (void)parser;
    assignment->start = left->start;
    an_lhs(assignment) = left;

    return assignment;
}

static ast_node_t *convert_call_expression(ast_node_t *left_operand, ast_node_t *call) {
    call->start = left_operand->start;
    an_callee(call) = left_operand;
    return call;
}

static ast_node_t *convert_function_definition(parser_t *parser, ast_node_t *left_operand, ast_node_t *func_def) {
    for (size_t i = an_func_def_arg_start(left_operand); i < an_func_def_arg_end(left_operand); ++i) {
        ast_node_t *parameter = left_operand->children.items[i];
        if (parameter->node_type != AST_NODE_TYPE_DECLARATION_DEFINITION) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.expected-decl.function-params",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("only declarations are allowed as function parameters"),
                .args = ORERR_ARGS(error_arg_node(parameter)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    ast_node_t *block = an_func_def_block(func_def);

    func_def->children.count = 0;
    func_def->start = left_operand->start;

    array_push(&func_def->children, an_func_def_return(left_operand));
    array_push(&func_def->children, block);

    for (size_t i = an_func_def_arg_start(left_operand); i < an_func_def_arg_end(left_operand); ++i) {
        array_push(&func_def->children, left_operand->children.items[i]);
    }

    // prevents freeing the node  parameter and return expressions
    left_operand->node_type = AST_NODE_TYPE_NONE;

    return func_def;
}

static ast_node_t *parse_assignment(parser_t *parser) {
    token_t equals = parser->previous;

    ast_node_t *rhs = parse_expression(parser);

    ast_node_t *assignment = ast_assignment(parser->ast, &nil_node, rhs, equals);

    return assignment;
}

static ast_node_t *parse_def_value(parser_t *parser) {
    bool in_outer_function_scope = false;
    if (parser->previous.type == TOKEN_HAT) {
        in_outer_function_scope = true;
        if (!consume(parser, TOKEN_IDENTIFIER)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.expected-identifier.hat",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected identifier after '^'"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }
    ast_node_t *def_value = ast_def_value(parser->ast, ast_sv2istring(parser->ast, parser->previous.view), parser->previous);
    def_value->is_in_outer_function_scope = in_outer_function_scope;
    return def_value;
}

static ast_node_t *parse_block(parser_t *parser) {
    ast_node_t *block = ast_block_begin(parser->ast, parser->previous);

    until (check(parser, TOKEN_BRACE_CLOSE) || check(parser, TOKEN_EOF)) {
        if (match(parser, TOKEN_SEMICOLON)) {
            continue;
        }

        ast_node_t *decl_node = parse_decl(parser);

        unless (consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-semicolon.block-decl-or-state",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after declaration or statement"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }

        ast_block_decl(block, decl_node);
    }

    unless (consume(parser, TOKEN_BRACE_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-rbrace.block",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected '}' to close block"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_block_end(block, parser->previous);

    return block;
}

static ast_node_t *parse_do(parser_t *parser) {
    token_t do_token = parser->previous;

    token_t label = nil_token;

    if (match(parser, TOKEN_COLON)) {
        unless (consume(parser, TOKEN_IDENTIFIER)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-ident.do-label",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected identifier after ':' on a do expression"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
        label = parser->previous;
    }

    ast_node_t *expr = parse_expression(parser);

    ast_node_t *do_ = ast_do(parser->ast, ast_sv2istring(parser->ast, label.view), expr, do_token);

    return do_;
}

static ast_node_t *parse_branch(parser_t *parser) {
    token_t start_token = parser->previous;
    token_type_t branch_token_type = parser->previous.type;
    bool condition_negated = false;
    ast_branch_type_t branch_type = BRANCH_TYPE_IF;

    ast_node_t *condition = NULL;
    ast_node_t *then_branch = NULL;
    ast_node_t *else_branch = NULL;

    ast_node_t *decl_block = NULL;

    if (branch_token_type == TOKEN_UNLESS || branch_token_type == TOKEN_UNTIL) {
        condition_negated = true;
    }

    if (branch_token_type == TOKEN_WHILE || branch_token_type == TOKEN_UNTIL) {
        branch_type = BRANCH_TYPE_WHILE;
    } else {
        branch_type = BRANCH_TYPE_IF;
    }

    decl_block = ast_block_begin(parser->ast, start_token);

    // declares before condition for blocks 
    ast_node_t *last_decl;
    while (true) {
        last_decl = parse_decl(parser);

        if (consume(parser, TOKEN_SEMICOLON)) {
            ast_block_decl(decl_block, last_decl);
            continue;
        }
        break;
    }

    condition = last_decl;
    if (last_decl->node_type != AST_NODE_TYPE_DECLARATION_STATEMENT) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-expr.branch-condition",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected expression as the condition for '$0.kind$' branch"),
            .args = ORERR_ARGS(error_arg_token(start_token), error_arg_node(last_decl)),
            .show_code_lines = ORERR_LINES(1),
        ));
    } else {
        condition = an_expression(last_decl);
    }

    // condition = parse_expression(parser);
    if (match(parser, TOKEN_BRACE_OPEN)) {
        then_branch = parse_block(parser);
    } else {
        if (branch_type == BRANCH_TYPE_WHILE) {
            unless (consume(parser, TOKEN_DO)) {
                parser_error(parser, OR_ERROR(
                    .tag = "syn.expected-do-or-open-brace.branch-while-post-condition",
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected 'do' or '{' after $0.kind$ condition"),
                    .args = ORERR_ARGS(error_arg_token(start_token), error_arg_token(parser->current)),
                    .show_code_lines = ORERR_LINES(1),
                ));
            }
        } else {
            unless (consume(parser, TOKEN_THEN)) {
                parser_error(parser, OR_ERROR(
                    .tag = "syn.expected-then-or-open-brace.branch-if-post-condition",
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected 'then' or '{' after $0.kind$ condition"),
                    .args = ORERR_ARGS(error_arg_token(start_token), error_arg_token(parser->current)),
                    .show_code_lines = ORERR_LINES(1),
                ));
            }
        }

        then_branch = parse_expression(parser);
    }

    token_type_t next_token_check = TOKEN_ERROR;
    switch (branch_type) {
        case BRANCH_TYPE_WHILE: next_token_check = TOKEN_THEN; break;
        case BRANCH_TYPE_IF: next_token_check = TOKEN_ELSE; break;
        default: UNREACHABLE();
    }

    unless (match(parser, next_token_check)) {
        else_branch = ast_nil(parser->ast, ortypeid(TYPE_VOID), token_implicit_at_end(then_branch->end));
    } else {
        else_branch = parse_expression(parser);
    }

    switch (branch_type) {
        case BRANCH_TYPE_WHILE: {
            ast_node_t *while_ = ast_while(parser->ast, condition, condition_negated, then_branch, else_branch, start_token);
            ASSERT(decl_block, "must");

            if (decl_block->children.count > 0) {
                ast_block_decl(decl_block, ast_statement(parser->ast, while_));
                ast_block_end(decl_block, while_->end);
                while_ = decl_block;
            }

            return while_;
        }
        case BRANCH_TYPE_IF: {
            ast_node_t *ifthen = ast_ifthen(parser->ast, condition, condition_negated, then_branch, else_branch, start_token);
            ASSERT(decl_block, "must");

            if (decl_block->children.count > 0) {
                ast_block_decl(decl_block, ast_statement(parser->ast, ifthen));
                ast_block_end(decl_block, ifthen->end);
                ifthen = decl_block;
            }
            return ifthen;
        }
        
        case BRANCH_TYPE_DO:
        case BRANCH_TYPE_FOR: UNREACHABLE(); break; // handled separately
    }
}

static ast_node_t *parse_for(parser_t *parser) {
    token_t start = parser->previous;

    ast_node_t *decl;
    if (!match(parser, TOKEN_SEMICOLON)) {
        decl = parse_decl(parser);
        unless (consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-semicolon.for-init-decl",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after the first declaration in a for expression"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    } else {
        decl = &nil_node;
    }

    ast_node_t *cond;
    token_t end_of_check = start;
    if (!match(parser, TOKEN_SEMICOLON)) {
        cond = parse_expression(parser);
        unless(consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-semicolon.for-condition",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after the condition in a for expression"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
            end_of_check = start;
        } else {
            end_of_check = parser->previous;
        }
    } else {
        cond = &nil_node;
    }

    ast_node_t *increment;
    unless (check(parser, TOKEN_DO) || check(parser, TOKEN_BRACE_OPEN)) {
        increment = parse_expression(parser);
    } else {
        increment = &nil_node;
    }

    ast_node_t *loop = &nil_node;
    if (match(parser, TOKEN_DO)) {
        loop = parse_expression(parser);
    } else if (match(parser, TOKEN_BRACE_OPEN)) {
        loop = parse_block(parser);
    } else {
        parser_error(parser, OR_ERROR(
            .tag = "syn.expected-do-or-open-brace.for-post-header",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected 'do' or '{' to open 'for' expression"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *then;
    if (match(parser, TOKEN_THEN)) {
        then = parse_expression(parser);
    } else {
        then = ast_nil(parser->ast, ortypeid(TYPE_VOID), token_implicit_at_end(loop->end));
    }

    // true implicit condition if nothing
    if (an_is_none(cond)) {
        cond = ast_implicit_expr(parser->ast, ortypeid(TYPE_BOOL), ORWORDU(1), token_implicit_at_start(end_of_check));
    }

    ast_node_t *for_ = ast_for(parser->ast, decl, cond, increment, loop, then, start);

    return for_;
}

static bool has_matching_parenthesis(parser_t *parser, bool *is_function_signature) {
    token_t parenthesis_open = parser->previous;
    ASSERT(parenthesis_open.type == TOKEN_PARENTHESIS_OPEN, "must be starting to open a parenthesis");

    lexer_t look_ahead_lexer = parser->lexer;
    ors32 parenthesis_level = 1;
    token_t next = parser->current;
    // get matching close parenthesis
    while (true) {
        if (next.type == TOKEN_EOF) {
            return false;
        }

        if (next.type == TOKEN_PARENTHESIS_CLOSE) {
            parenthesis_level--;
        } else if (next.type == TOKEN_PARENTHESIS_OPEN) {
            parenthesis_level++;
        }

        if (parenthesis_level == 0) {
            break;
        }

        next = lexer_next_token(&look_ahead_lexer);
    }

    next = lexer_next_token(&look_ahead_lexer);

    *is_function_signature = (next.type == TOKEN_ARROW_RIGHT || next.type == TOKEN_BRACE_OPEN);

    return parenthesis_level == 0;
}

static bool is_incoming_decl_def(parser_t* parser) {
    lexer_t lookahead_lexer = parser->lexer;
    while (lookahead_lexer.previous_token.type == TOKEN_DIRECTIVE) {
        lexer_next_token(&lookahead_lexer);
    }

    if (lookahead_lexer.previous_token.type != TOKEN_IDENTIFIER) {
        return false;
    }

    token_t token = lexer_next_token(&lookahead_lexer);

    return token.type == TOKEN_COLON;
}

static ast_node_t *parse_decl_def(parser_t *parser);

static void parse_parameters(parser_t *parser, ast_nodes_t *children) {
    while (true) {
        bool is_compile_time_param = match(parser, TOKEN_BANG);
        token_t bang = parser->previous;

        ast_node_t *decl = parse_decl(parser);
        decl->is_compile_time_param = is_compile_time_param;

        if (decl->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION && decl->has_default_value && is_compile_time_param) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.invalid-default.compile-time-param",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("compile-time param '$1.$' cannot have a default value"),
                .args = ORERR_ARGS(error_arg_node(an_decl_expr(decl)), error_arg_str(parser->ast, *decl->identifier)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }

        if (decl->node_type == AST_NODE_TYPE_DECLARATION_STATEMENT && is_compile_time_param) {
            ast_node_t *expr = an_expression(decl);
            if (expr->node_type != AST_NODE_TYPE_EXPRESSION_DEF_VALUE) {
                parser_error(parser, OR_ERROR(
                    .tag = "syn.missing-ident.compile-time-param",
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected identifier after '!'"),
                    .args = ORERR_ARGS(error_arg_node(decl)), 
                    .show_code_lines = ORERR_LINES(0),
                )); 
            } else {
                expr->start = bang;
                expr->node_type = AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL;
            }
        }

        array_push(children, decl);

        if (!match(parser, TOKEN_COMMA)) {
            break;
        }
    }
}

static void parse_function_signature(parser_t *parser, ast_node_t *func_sig) {
    ASSERT(func_sig->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");
    func_sig->children.count = 0;

    array_push(&func_sig->children, &nil_node);
    array_push(&func_sig->children, &nil_node);

    if (!consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
        parse_parameters(parser, &func_sig->children);

        unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-rparen.signature-arg|skip",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ')' after the function signature arguments"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    if (match(parser, TOKEN_ARROW_RIGHT)) {
        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = true;
        an_func_def_return(func_sig) = parse_expression(parser);
        parser->inside_type_context = inside_type_context;
    } else {
        an_func_def_return(func_sig) = ast_implicit_expr(parser->ast, ortypeid(TYPE_TYPE), ORWORDT(ortypeid(TYPE_VOID)), token_implicit_at_end(func_sig->end));
    }
}

static ast_node_t *parse_function_definition(parser_t *parser) {
    ast_node_t *func_def = ast_node_new(parser->ast->arena, AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, parser->previous);
    an_func_def_block(func_def) = parse_block(parser);
    func_def->end = parser->previous;

    return func_def;
}

static ast_node_t *parse_array_type(parser_t *parser) {
    ast_node_t *size_expr = &nil_node;

    if (true || !check(parser, TOKEN_BRACKET_CLOSE)) {
        size_expr = parse_expression(parser);
    }

    unless (consume(parser, TOKEN_BRACKET_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-rsquare.arr-type",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected ']' after the size expression in type"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *type_expr = parse_precedence(parser, (prec_t)(PREC_CALL + 1));

    ast_node_t *array_type = ast_array_type(parser->ast, size_expr, type_expr);
    return array_type;
}

static ast_node_t *parse_item_access(parser_t *parser) {
    ast_node_t *access_expr = parse_expression(parser);

    unless (consume(parser, TOKEN_BRACKET_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-rsquare.arr-size",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected ']' after the size expression in item access"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *item_access = ast_item_access(parser->ast, &nil_node, access_expr);
    return item_access;
}

static ast_node_t *parse_grouping_or_function_signature_or_definition(parser_t *parser) {
    ast_node_type_t node_type;
    bool is_function_signature;
    if (!has_matching_parenthesis(parser, &is_function_signature)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-rparen.open-paren",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected matching ')' for '('"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    if (is_function_signature) {
        node_type = AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE;
    } else  {
        node_type = AST_NODE_TYPE_EXPRESSION_GROUPING;
    }

    ast_node_t *expr = ast_node_new(parser->ast->arena, node_type, parser->previous);

    if (node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE) {
        parse_function_signature(parser, expr);

        expr->value_type = ortypeid(TYPE_UNRESOLVED);
        expr->end = parser->previous;

        if (match(parser, TOKEN_BRACE_OPEN)) {
            ast_node_t *definition = parse_function_definition(parser);
            expr = convert_function_definition(parser, expr, definition);
        } else {
            for (size_t i = an_func_def_arg_start(expr); i < an_func_def_arg_end(expr); ++i) {
                ast_node_t *parameter = expr->children.items[i];
                if (parameter->node_type != AST_NODE_TYPE_DECLARATION_STATEMENT) {
                    parser_error(parser, OR_ERROR(
                        .tag = "syn.missing-expr.func-sig-param",
                        .level = ERROR_SOURCE_PARSER,
                        .msg = lit2str("only expressions are allowed in function signature"),
                        .args = ORERR_ARGS(error_arg_node(parameter)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                } else {
                    parameter = an_expression(parameter);
                    expr->children.items[i] = parameter;
                }
            }
        }
    } else {
        an_operand(expr) = parse_expression(parser);

        unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-rparen.group",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ')' for group"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }

        expr->value_type = an_operand(expr)->value_type;
    }

    expr->end = parser->previous;

    return expr;
}

static ast_node_t *parse_call(parser_t *parser) {
    ast_node_t *callee = ast_nil(parser->ast, ortypeid(TYPE_VOID), token_implicit_at_end(parser->previous));
    ast_node_t *call = ast_call_begin(parser->ast, callee, parser->previous);
    MUST(call->children.count == 1);
    parse_call_arguments(parser, call);

    ast_call_end(call, parser->previous);

    return call;
}

static ast_node_t *parse_unary(parser_t *parser) {
    token_t operator = parser->previous;
    ast_node_t *operand = parse_precedence(parser, PREC_UNARY);

    unless (operator.type == TOKEN_PLUS_PLUS || operator.type == TOKEN_MINUS_MINUS) {
        ast_node_t *unary = ast_unary(parser->ast, operator, operand);
        return unary;
    } else {
        token_t equals = operator;
        equals.type= (equals.type == TOKEN_MINUS_MINUS ? TOKEN_MINUS_EQUAL : TOKEN_PLUS_EQUAL);
        equals.view.length = 0;

        ast_node_t *one = ast_implicit_expr(parser->ast, parser->ast->type_set.u64_, ORWORDU(1), equals);
        one->is_free_number = true;

        ast_node_t *assignment = ast_assignment(parser->ast, operand, one, equals);

        return assignment;
    }
}

static ast_node_t *parse_inferred_type_decl(parser_t *parser) {
    token_t first_token = parser->previous;

    ast_node_t *expr = parse_expression(parser);
    switch (expr->node_type) {
    case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
        expr = ast_inferred_type_decl(parser->ast, first_token, expr->identifier, expr->end);
        break;
    }

    case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
        expr->is_macro = true;
        break;
    }

    default: {
        parser_error(parser, OR_ERROR(
            .tag = "syn.expects-def-val-or-funcdef.bang.",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected either an identifier or funcdef after '!'"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
        break;
    }
    }
    return expr;
}

static ast_node_t *parse_directive(parser_t *parser) {
    bool use_bracket = false;

    ast_node_t *directive = ast_node_new(parser->ast->arena, AST_NODE_TYPE_EXPRESSION_DIRECTIVE, parser->previous);
    string_view_t dirv = parser->previous.view;
    {
        dirv.data += 1;
        --dirv.length;
    }
    directive->identifier = ast_sv2istring(parser->ast, dirv);

    if (match(parser, TOKEN_PARENTHESIS_OPEN)) {
        use_bracket = true;
    }

    do {
        ast_node_t *arg = parse_expression(parser);
        array_push(&directive->children, arg);

        if (!match(parser, TOKEN_COMMA)) break;
    } while (true);

    if (use_bracket) {
        unless (consume(parser, TOKEN_PARENTHESIS_CLOSE)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-rparen.directive-call-args",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ')' after the directive call arguments"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    directive->end = parser->previous;

    return directive;
}

static ast_node_t *parse_binary(parser_t *parser) {
    token_t operator = parser->previous;

    parse_rule_t *rule = parser_get_rule(operator.type);

    ast_node_t *rhs = parse_precedence(parser, (prec_t)(rule->precedence + 1));

    ast_node_t *binary = ast_binary(parser->ast, operator, &nil_node, rhs);

    return binary;
}

static ast_node_t *parse_cast(parser_t *parser) {
    parse_rule_t *rule = parser_get_rule(parser->previous.type);
    ast_node_t *expr_type = parse_precedence(parser, (prec_t)(rule->precedence+1));
    // ast_node_t *expr_type = parse_precedence(parser, PREC_CAST);

    ast_node_t *cast_node = ast_cast(parser->ast, expr_type, &nil_node);
    
    return cast_node;
}

static ast_node_t *parse_struct_def(parser_t *parser) {
    token_t struct_keyword = parser->previous;

    ast_node_t *struct_ = ast_struct_begin(parser->ast, struct_keyword);

    if (match(parser, TOKEN_PARENTHESIS_OPEN)) {
        while (true) {
            if (match(parser, TOKEN_PARENTHESIS_CLOSE)) break;

            if (struct_->param_end > 0) {
                if (!consume(parser, TOKEN_COMMA)) {
                    parser_error(parser, OR_ERROR(
                        .tag = "syn.missing-comma.struct-params",
                        .level = ERROR_SOURCE_PARSER,
                        .msg = lit2str("expected ',' after param"),
                        .args = ORERR_ARGS(error_arg_token(parser->current)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }
            }

            ast_node_t *decl = parse_decl_def(parser);

            ast_struct_add_param(struct_, decl);

            if (decl->has_default_value) {
                parser_error(parser, OR_ERROR(
                    .tag = "syn.expected-no-default.param-struct",
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("struct parameters cannot use default values"),
                    .args = ORERR_ARGS(error_arg_node(an_decl_expr(decl))),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
        }
    }

    unless (consume(parser, TOKEN_BRACE_OPEN)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-lbrace.struct-def",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected '{' after struct keyword"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }


    while (is_incoming_decl_def(parser)) {
        ast_node_t *decl = parse_decl_def(parser);
        ast_struct_add_decl(struct_, decl);

        unless(consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-semicolon.struct-field",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after struct field" ),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    unless(consume(parser, TOKEN_BRACE_CLOSE)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-rbrace.struct-def",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected '}' to close struct"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_struct_end(struct_, parser->previous);

    return struct_;
}

static ast_node_t *ast_begin_list_initializer(ast_t *ast, token_t open_bracket) {
    ast_node_t *initializer = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST, open_bracket);
    return initializer;
}

static void ast_end_list_initializer(ast_node_t *initializer, token_t close_bracket) {
    initializer->end = close_bracket;
}
ast_node_t *ast_dot_access(ast_t *ast, ast_node_t *lhs, token_t identifier, token_t start) {
    ast_node_t *dot_access = ast_node_new(ast->arena, AST_NODE_TYPE_EXPRESSION_DOT_ACCESS, start);
    dot_access->identifier = ast_sv2istring(ast, identifier.view);
    dot_access->end = identifier;
    an_dot_lhs(dot_access) = lhs;

    return dot_access;
}

static ast_node_t *parse_dot(parser_t *parser) {
    token_t start_dot = parser->previous;
    if (match(parser, TOKEN_BRACE_OPEN)) {
        ast_node_t *initiailizer = ast_begin_list_initializer(parser->ast, start_dot);

        unless (match(parser, TOKEN_BRACE_CLOSE)) {
            do {
                if (check(parser, TOKEN_BRACE_CLOSE)) {
                } else if (check(parser, TOKEN_COMMA)) {
                    ast_node_t *none = &nil_node;
                    array_push(&initiailizer->children, none);
                } else {
                    oristring_t label = parse_label_or_nil(parser);
                    ast_node_t *argument = parse_expression(parser);
                    argument->label = label;

                    array_push(&initiailizer->children, argument);
                }
            } while (match(parser, TOKEN_COMMA));

            unless (consume(parser, TOKEN_BRACE_CLOSE)) {
                parser_error(parser, OR_ERROR(
                    .tag = "syn.missing-rbrace.init-list",
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("expected '}' after an initialization list"),
                    .args = ORERR_ARGS(error_arg_token(parser->current)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
        }

        ast_end_list_initializer(initiailizer, parser->previous);

        return initiailizer;
    }

    if (match(parser, TOKEN_IDENTIFIER)) {
        token_t identifier = parser->previous;

        ast_node_t *dot_expression = ast_dot_access(parser->ast, &nil_node, identifier, start_dot);
        dot_expression->end = parser->current;

        return dot_expression;
    }
    
    parser_error(parser, OR_ERROR(
        .tag = "syn.expected-ident-or-open-brace.post-dot",
        .level = ERROR_SOURCE_PARSER,
        .msg = lit2str("expected an identifier or open brace after '.'"),
        .args = ORERR_ARGS(error_arg_token(parser->current)),
        .show_code_lines = ORERR_LINES(0),
    ));

    return &nil_node;
}

parse_rule_t rules[] = {
    [TOKEN_PARENTHESIS_OPEN]        = { parse_grouping_or_function_signature_or_definition,   parse_call,       PREC_CALL },
    [TOKEN_PARENTHESIS_CLOSE]       = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BRACE_OPEN]              = { parse_block,        NULL,               PREC_BLOCK },
    [TOKEN_BRACE_CLOSE]             = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BRACKET_OPEN]            = { parse_array_type,   parse_item_access,  PREC_CALL },
    [TOKEN_BRACKET_CLOSE]           = { NULL,               NULL,               PREC_NONE },
    [TOKEN_COMMA]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_DOT]                     = { parse_dot,          parse_dot,          PREC_CALL },
    [TOKEN_MINUS]                   = { parse_unary,        parse_binary,       PREC_TERM },
    [TOKEN_PLUS]                    = { NULL,               parse_binary,       PREC_TERM },
    [TOKEN_STAR]                    = { parse_unary,        parse_binary,       PREC_FACTOR },
    [TOKEN_SLASH]                   = { NULL,               parse_binary,       PREC_FACTOR },
    [TOKEN_COLON]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_PERCENT]                 = { NULL,               parse_binary,       PREC_TERM },
    [TOKEN_PERCENT_PERCENT]         = { NULL,               parse_binary,       PREC_TERM },

    [TOKEN_EQUAL]                   = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_PLUS_EQUAL]              = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_MINUS_EQUAL]             = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_SLASH_EQUAL]             = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_STAR_EQUAL]              = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_PERCENT_EQUAL]           = { NULL,               parse_assignment,   PREC_ASSIGNMENT},
    [TOKEN_PERCENT_PERCENT_EQUAL]   = { NULL,               parse_assignment,   PREC_ASSIGNMENT},

    [TOKEN_BANG]                    = { parse_inferred_type_decl, NULL,         PREC_UNARY },
    [TOKEN_LESS]                    = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_GREATER]                 = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_SEMICOLON]               = { NULL,               NULL,               PREC_NONE },
    [TOKEN_BAR]                     = { NULL,               parse_binary,       PREC_BITWISE_OR },
    [TOKEN_AMPERSAND]               = { parse_unary,        NULL,               PREC_UNARY },
    [TOKEN_AMPERSAND_AMPERSAND]     = { parse_unary,        NULL,               PREC_UNARY },
    [TOKEN_SQUIGGLE]                = { NULL,               NULL,               PREC_NONE },
    [TOKEN_PLUS_PLUS]               = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_MINUS_MINUS]             = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_LESS_LESS]               = { NULL,               NULL,               PREC_NONE },
    [TOKEN_AS]                      = { NULL,               parse_cast,         PREC_CAST },
    [TOKEN_EQUAL_EQUAL]             = { NULL,               parse_binary,       PREC_EQUALITY },
    [TOKEN_BANG_EQUAL]              = { NULL,               parse_binary,       PREC_EQUALITY },
    [TOKEN_LESS_EQUAL]              = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_GREATER_EQUAL]           = { NULL,               parse_binary,       PREC_COMPARISON },
    [TOKEN_ARROW_RIGHT]             = { NULL,               NULL,               PREC_NONE },
    [TOKEN_DIRECTIVE]               = { parse_directive,    NULL,               PREC_NONE },
    [TOKEN_IDENTIFIER]              = { parse_def_value,    NULL,               PREC_NONE },
    [TOKEN_HAT]                     = { parse_def_value,    NULL,               PREC_NONE },
    [TOKEN_STRING]                  = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_SYMBOL]                  = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_INTEGER]                 = { parse_number,       NULL,               PREC_NONE },
    [TOKEN_FLOAT]                   = { parse_number,       NULL,               PREC_NONE },
    [TOKEN_ANNOTATION]              = { NULL,               NULL,               PREC_NONE },
    [TOKEN_STRUCT]                  = { parse_struct_def,   NULL,               PREC_NONE },
    [TOKEN_NOT]                     = { parse_unary,        NULL,               PREC_NONE },
    [TOKEN_AND]                     = { NULL,               parse_binary,       PREC_AND },
    [TOKEN_OR]                      = { NULL,               parse_binary,       PREC_OR },
    [TOKEN_IF]                      = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_THEN]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_UNLESS]                  = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_WHILE]                   = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_UNTIL]                   = { parse_branch,       NULL,               PREC_BLOCK },
    [TOKEN_FOR]                     = { parse_for,          NULL,               PREC_BLOCK },
    [TOKEN_DO]                      = { parse_do,           NULL,               PREC_NONE },
    [TOKEN_ELSE]                    = { NULL,               NULL,               PREC_NONE },
    [TOKEN_TRUE]                    = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_FALSE]                   = { parse_literal,      NULL,               PREC_NONE },
    [TOKEN_TYPEOF]                  = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_SIZEOF]                  = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_LEN]                     = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_OFFSETPTR]               = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_PTRDIFF]                 = { parse_builtin_call, NULL,               PREC_NONE },
    [TOKEN_RETURN]                  = { parse_jmp,          NULL,               PREC_NONE },
    [TOKEN_BREAK]                   = { parse_jmp,          NULL,               PREC_NONE },
    [TOKEN_CONTINUE]                = { parse_jmp,          NULL,               PREC_NONE },
    [TOKEN_ERROR]                   = { NULL,               NULL,               PREC_NONE },
    [TOKEN_EOF]                     = { NULL,               NULL,               PREC_NONE },
    [TOKEN_SIZE]                    = { NULL,               NULL,               PREC_NONE },
};

static bool check_expression(parser_t *parser) {
    return parser_get_rule(parser->current.type)->prefix != NULL;
}

static ast_node_t *parse_precedence(parser_t *parser, prec_t precedence) {
    ParseFn prefix_rule = parser_get_rule(parser->current.type)->prefix;
    ast_node_t *left_operand;

    if (prefix_rule == NULL) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.expected-expr-token.expr",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected expression"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }
    advance(parser);

    if (prefix_rule) {
        left_operand = prefix_rule(parser);
    } else {
        left_operand = ast_node_new(parser->ast->arena, AST_NODE_TYPE_NONE, parser->current);
    }

    while (precedence <= parser_get_rule(parser->current.type)->precedence) {
        advance(parser);

        ParseFn infix_rule = parser_get_rule(parser->previous.type)->infix;
        if (!infix_rule) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.invalid-op.op",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("token '$0.$' is not an infix operator"),
                .args = ORERR_ARGS(error_arg_token(parser->previous)),
                .show_code_lines = ORERR_LINES(0),
            ));
            break;
        }

        ast_node_t *right_operand = infix_rule(parser);

        switch (right_operand->node_type) {
            case AST_NODE_TYPE_EXPRESSION_CAST: {
                ast_node_t *expr = left_operand;
                ast_node_t *cast_type = an_cast_type(right_operand);
                an_cast_expr(right_operand) = expr;
                an_cast_type(right_operand) = cast_type;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
                ast_node_t *accessee = left_operand;
                an_item_accessee(right_operand) = accessee;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_BINARY: {
                an_lhs(right_operand) = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
                an_list_lhs(right_operand) = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS: {
                an_lhs(right_operand) = left_operand;
                right_operand->start = left_operand->start;
                left_operand = right_operand;
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_CALL: {
                left_operand = convert_call_expression(left_operand, right_operand);
                break;
            }

            case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
                left_operand = convert_assignment_expression(parser, left_operand, right_operand);
                break;
            }

            case AST_NODE_TYPE_NONE: {
                left_operand = right_operand;
                break;
            }

            default: UNREACHABLE();
        }
    }

    return left_operand;
}

static parse_rule_t *parser_get_rule(token_type_t type) {
    return &rules[type];
}

static ast_node_t *parse_expression(parser_t *parser) {
    bool inside_type_context = parser->inside_type_context;
    parser->inside_type_context = inside_type_context;

    ast_node_t *expression_node = parse_precedence(parser, parser->inside_type_context ? PREC_OR : PREC_ASSIGNMENT);
    parser->inside_type_context = inside_type_context;

    return expression_node;
}

ast_node_t *parse_expression_string(ast_t *ast, orstring_t code, bool *had_error) {
    parser_t parser = {0};
    parser_init(&parser, ast, lit2str("<@insert>"), string2sv(code));
    
    advance(&parser);

    ast_node_t *expr = parse_expression(&parser);

    *had_error = parser.had_error;

    return expr;
}

static ast_node_t *parse_statement(parser_t *parser) {
    ast_node_t *expr = parse_expression(parser);
    ast_node_t *statement_node = ast_statement(parser->ast, expr);
    return statement_node;
}

static ast_node_t *parse_decl_def(parser_t *parser) {
    bool is_subscript_function = false;
    {
        while (match(parser, TOKEN_DIRECTIVE)) {
            token_t dir_token = parser->previous;
            if (sv_eq(dir_token.view, SV("@subscript"))) {
                is_subscript_function = true;
            } else {
                parser_error(parser, OR_ERROR(
                    .tag = "syn.unknown-directive.decl-def",
                    .level = ERROR_SOURCE_PARSER,
                    .msg = lit2str("unknown declaration directive"),
                    .args = ORERR_ARGS(error_arg_token(parser->previous)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                parser->panic_mode = false;
            }
        }
    }

    unless (consume(parser, TOKEN_IDENTIFIER)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-identifier.decl",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected indentifier for variable or constant"),
            .args = ORERR_ARGS(error_arg_token(parser->current)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    token_t identifier = parser->previous;

    unless (consume(parser, TOKEN_COLON)) {
        parser_error(parser, OR_ERROR(
            .tag = "syn.missing-colon.post-decl-ident",
            .level = ERROR_SOURCE_PARSER,
            .msg = lit2str("expected ':' after declaration identifier"),
            .args = ORERR_ARGS(error_arg_token(identifier)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    ast_node_t *type_expr = NULL;
    if (!check(parser, TOKEN_EQUAL) && !check(parser, TOKEN_COLON)) {

        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = true;
        type_expr = parse_expression(parser);
        parser->inside_type_context = inside_type_context;

    } else {
        type_expr = ast_implicit_expr(parser->ast, ortypeid(TYPE_TYPE), ORWORDT(ortypeid(TYPE_UNRESOLVED)), token_implicit_at_end(parser->previous));
    }

    ASSERT(type_expr, "should be set by now");

    bool is_mutable = false;
    bool requires_expression = false;
    if (match(parser, TOKEN_EQUAL)) {
        requires_expression = true;
        is_mutable = true;
    } else if (match(parser, TOKEN_COLON)) {
        requires_expression = true;
        is_mutable = false;
    } else {
        is_mutable = true;
    }

    bool has_default_value;
    ast_node_t *init_expr = NULL;
    if (requires_expression) {
        has_default_value = true;
        bool inside_type_context = parser->inside_type_context;
        parser->inside_type_context = false;
        init_expr = parse_expression(parser);
        parser->inside_type_context = inside_type_context;
    } else {
        has_default_value = false;
        init_expr = ast_nil(parser->ast, ortypeid(TYPE_UNRESOLVED), token_implicit_at_end(parser->previous));
    }

    ASSERT(init_expr, "should be set by now");

    oristring_t ident = ast_sv2istring(parser->ast, identifier.view);
    ast_node_t *decldef = ast_decldef(parser->ast, ident, type_expr, init_expr, identifier);
    decldef->is_mutable = is_mutable;
    decldef->is_subscript_function = is_subscript_function;
    decldef->has_default_value = has_default_value;
    return decldef;
}

static ast_node_t *parse_decl(parser_t *parser) {
    ast_node_t *node = NULL;

    if (is_incoming_decl_def(parser)) {
        node = parse_decl_def(parser);
    } else {
        node = parse_statement(parser);
    }

    return node;
}

static void parse_into_module(parser_t *parser, ast_node_t *module) {
    until (match(parser, TOKEN_EOF)) {
        ast_node_t *decldef = parse_decl(parser);
        array_push(&module->children, decldef);

        if (decldef->node_type == AST_NODE_TYPE_DECLARATION_STATEMENT) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.expected-decl.module",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected declaration, not a statement, in module scope"),
                .args = ORERR_ARGS(error_arg_node(decldef)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }

        unless (consume(parser, TOKEN_SEMICOLON)) {
            parser_error(parser, OR_ERROR(
                .tag = "syn.missing-semicolon.module-decl",
                .level = ERROR_SOURCE_PARSER,
                .msg = lit2str("expected ';' after declaration in module"),
                .args = ORERR_ARGS(error_arg_token(parser->current)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }

        synchronize(parser);
    }
}

bool parse_string_to_module(ast_t *ast, ast_node_t *module, orstring_t filepath, string_view_t source) {
    parser_t parser = {0};
    parser_init(&parser, ast, filepath, source);
    
    advance(&parser);

    parse_into_module(&parser, module);
    module->filepath = string_copy(filepath, ast->arena);

    return !parser.had_error;
}

ast_node_t *parse_source_into_module(ast_t *ast, orstring_t file_path, string_view_t source) {
    ast_node_t *module = ast_begin_module(ast);
    parse_string_to_module(ast, module, file_path, source);
    ast_end_module(module);

    return module;
}

token_type_t parser_opeq2op(token_type_t type) {
    switch (type) {
    case TOKEN_PLUS_EQUAL: return TOKEN_PLUS;
    case TOKEN_MINUS_EQUAL: return TOKEN_MINUS;
    case TOKEN_STAR_EQUAL: return TOKEN_STAR;
    case TOKEN_SLASH_EQUAL: return TOKEN_SLASH;
    case TOKEN_PERCENT_EQUAL: return TOKEN_PERCENT;
    case TOKEN_PERCENT_PERCENT_EQUAL: return TOKEN_PERCENT_PERCENT;
    case TOKEN_EQUAL: return TOKEN_EQUAL;
    default: return TOKEN_ERROR;
    }
}

orword_t *ast_multiword_value(ast_t *ast, size_t size_words) {
    orword_t z = {.as.u = 0};

    orword_t *start = (orword_t*)(ast->multiword_data.data + ast->multiword_data.count);
    for (size_t i = 0; i < size_words; ++i) {
        memarr_push(&ast->multiword_data, &z, ORWORD_SIZE);
    }

    return start;
}

orword_t ast_mem2word(ast_t *ast, void *data, ortype_t type) {
    typedata_t *td = ast_type2td(ast, type);

    switch (td->kind) {
    case TYPE_MODULE:
    case TYPE_UNREACHABLE:
    case TYPE_PARAM_STRUCT:
    case TYPE_INFERRED_FUNCTION:
    case TYPE_UNRESOLVED:
    case TYPE_INVALID: return (orword_t){0};

    case TYPE_VOID: return (orword_t){0};

    case TYPE_BOOL: return (orword_t){ .as.s = (*((bool*)data)) };

    case TYPE_POINTER:
    case TYPE_FUNCTION:
    case TYPE_TYPE: return *((orword_t*)data);

    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_FLOAT: return td->size == NUM_SIZE_32 ? ORWORDD(*((orf32*)data)) : *((orword_t*)data);
        case NUM_TYPE_SIGNED:
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: return ORWORDU(*((oru8*)data));
            case NUM_SIZE_16: return ORWORDU(*((oru16*)data));
            case NUM_SIZE_32: return ORWORDU(*((oru32*)data));
            case NUM_SIZE_64: return *((orword_t*)data);
            }
        }
        }
    }

    case TYPE_STRING:
    case TYPE_STRUCT:
    case TYPE_ARRAY: {
        if (td->size > ORWORD_SIZE) {
            return ORWORDP(data);
        } else {
            return *((orword_t*)data);
        }
    }

    case TYPE_COUNT: UNREACHABLE(); return (orword_t){0};
    }
}

bool ast_find_intrinsic_funcname(orintrinsic_fns_t fns, oristring_t name, orintrinsic_fn_t *fn) {
    for (size_t i = 0; i < fns.count; ++i) {
        orintrinsic_fn_t *f = &fns.items[i];
        if (name == f->name) {
            *fn = *f;
            return true;
        }
    }

    return false;
}

ast_node_val_t zero_value(ast_t *ast, ortype_t type) {
    if (TYPE_IS_UNRESOLVED(type)) return ast_node_val_word(ORWORDU(0));

    typedata_t *type_info = ast->type_set.types.items[type.i];
    orword_t value = {0};
    if (type_info->size > ORWORD_SIZE) {
        typedata_t *td = type2typedata(&ast->type_set.types, type);
        orword_t *data = ast_multiword_value(ast, orb2w(td->size));
        value = ORWORDP(data);
    }

    ast_node_val_t val = ast_node_val_word(value);
    return val;
}

static void print_indent(oru32 level) {
    for (oru32 i = 0; i < level; ++i) {
        printf(" ");
    }
}

static void print_line(const orcstr_t format, ...) {
	va_list args;
	va_start(args, format);

	vprintf(format, args);

	va_end(args);

	printf("\n");
}

static void ast_print_ast_node(typedatas_t types, ast_node_t *node, oru32 level) {
    tmp_arena_t *tmp = allocator_borrow();

    #define type2cstr(node) (type_to_string(types, node->value_type, tmp->allocator).cstr)

    switch (node->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            orstring_t label = string_format("binary (%.*s): %s", tmp->allocator, node->operator.view.length, node->operator.view.data, type2cstr(node));

            print_indent(level);
            print_line("%s", label.cstr);

            print_indent(level + 1);
            print_line("left");
            ast_print_ast_node(types, an_lhs(node), level+2);

            print_indent(level + 1);
            print_line("right");
            ast_print_ast_node(types, an_rhs(node), level+2);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_NIL: {
            print_indent(level);
            orstring_t label = string_format("nil: %s", tmp->allocator, type2cstr(node));
            print_line("%s", label.cstr);
            break;
        }

        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: {
            print_indent(level);
            orstring_t label = string_format("inferred type decl: %s = %s", tmp->allocator, node->identifier->cstr, type_to_string(types, node->expr_val.word.as.t, tmp->allocator));
            print_line("%s", label.cstr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            print_indent(level);
            print_line("group (...): %s", type2cstr(node));
            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: {
            print_indent(level);
            print_line("array type: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("size");
            ast_print_ast_node(types, an_array_size_expr(node), level + 2);

            print_indent(level + 1);
            print_line("type");
            ast_print_ast_node(types, an_array_type_expr(node), level + 2);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
            print_indent(level);
            print_line("item access: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("accessee");
            ast_print_ast_node(types, an_item_accessee(node), level + 2);

            print_indent(level + 1);
            print_line("accessor");
            ast_print_ast_node(types, an_item_accessor(node), level + 2);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: {
            print_indent(level);
            print_line("directive(%s): %s", node->identifier->cstr, type2cstr(node));

            print_indent(level + 1);
            print_line("args");
            for (size_t i = an_dir_arg_start(node); i < an_dir_arg_end(node); ++i) {
                ast_node_t *child = node->children.items[i];
                ast_print_ast_node(types, child, level + 2);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
            print_indent(level);
            print_line("dot: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("lhs");
            ast_print_ast_node(types, an_list_lhs(node), level+2);
            for (size_t i = an_list_start(node); i < an_list_end(node); ++i) {
                ast_node_t *arg = node->children.items[i];
                print_indent(level + 1);
                print_line("arg %llu", i);
                if (arg) {
                    ast_print_ast_node(types, arg, level + 2);
                } else {
                    print_indent(level+2);
                    print_line("<default>");
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            print_indent(level);
            print_line("cast: %s", type2cstr(node));

            ast_print_ast_node(types, an_rhs(node), level + 1);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            print_indent(level);
            print_line("unary (%.*s): %s", node->operator.view.length, node->operator.view.data, type2cstr(node));
            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            print_indent(level);
            print_line("primary (%.*s): %s", node->start.view.length, node->start.view.data, type2cstr(node));;
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            print_indent(level);
            print_line("def value (%s): %s", node->identifier->cstr, type2cstr(node));
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            print_indent(level);
            print_line("=: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("lvalue");
            ast_print_ast_node(types, an_lhs(node), level + 2);

            print_indent(level + 1);
            print_line("rhs");
            ast_print_ast_node(types, an_rhs(node), level + 2);
            break;
        }

        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            print_indent(level);
            print_line("{...}: %s", type2cstr(node));
            for (size_t i = 0; i < node->children.count; ++i) {
                ast_print_ast_node(types, node->children.items[i], level + 1);
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            print_indent(level);
            switch (node->branch_type) {
                case BRANCH_TYPE_DO: {
                    print_line("branch (do:%.*s): %s", node->start.view.length, node->start.view.data, type2cstr(node));

                    print_indent(level + 1);
                    print_line("do");
                    ast_print_ast_node(types, an_then(node), level + 2);

                    print_indent(level + 1);
                    print_line("then");
                    ast_print_ast_node(types, an_else(node), level + 2);
                    break;
                }

                case BRANCH_TYPE_FOR: {
                    print_line("branch (for:%.*s): %s", node->start.view.length, node->start.view.data, type2cstr(node));

                    print_indent(level + 1);
                    print_line("decl");
                    ast_print_ast_node(types, an_for_decl(node), level + 2);

                    print_indent(level + 1);
                    print_line("cond");
                    ast_print_ast_node(types, an_condition(node), level + 2);

                    print_indent(level + 1);
                    print_line("incr");
                    ast_print_ast_node(types, an_for_incr(node), level + 2);

                    print_indent(level + 1);
                    print_line("do");
                    ast_print_ast_node(types, an_then(node), level + 2);

                    print_indent(level + 1);
                    print_line("then");
                    ast_print_ast_node(types, an_else(node), level + 2);
                    break;
                }

                case BRANCH_TYPE_IF:
                case BRANCH_TYPE_WHILE: {
                    orcstr_t branch = NULL;
                    if (node->branch_type == BRANCH_TYPE_IF) {
                        if (node->condition_negated) {
                            branch = "unless";
                        } else {
                            branch = "if";
                        }
                    } else {
                        if (node->condition_negated) {
                            branch = "until";
                        } else {
                            branch = "while";
                        }
                    }

                    print_line("branch (%s): %s", branch, type2cstr(node));

                    print_indent(level + 1);
                    print_line("condition");
                    ast_print_ast_node(types, an_condition(node), level + 2);

                    print_indent(level + 1);
                    print_line("then");
                    ast_print_ast_node(types, an_then(node), level + 2);

                    print_indent(level + 1);
                    print_line("else");
                    ast_print_ast_node(types, an_else(node), level + 2);
                    break;
                }
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: {
            print_indent(level);
            print_line("builtin-call(%.*s): %s", node->identifier->cstr, type2cstr(node));
            
            print_indent(level+1);
            print_line("arguments");

            for (size_t i = an_bcall_arg_start(node); i < an_bcall_arg_end(node); ++i) {
                ast_node_t *arg = node->children.items[i];
                ast_print_ast_node(types, arg, level+2);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            print_indent(level);
            print_line("call: %s", type2cstr(node));

            print_indent(level+1);
            print_line("callee");
            ast_print_ast_node(types, an_callee(node), level + 2);
            
            print_indent(level+1);
            print_line("arguments");

            for (size_t i = an_call_arg_start(node); i < an_call_arg_end(node); ++i) {
                ast_node_t *arg = node->children.items[i];
                ast_print_ast_node(types, arg, level+2);
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            print_indent(level);
            print_line("function definition: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("definition");
            ast_print_ast_node(types, an_func_def_block(node), level + 2);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_STRUCT: {
            print_indent(level);
            print_line("struct: %s", type2cstr(node));

            print_indent(level + 1);
            print_line("members");
            for (size_t i = an_struct_start(node); i < an_struct_end(node); ++i) {
                ast_node_t *declaration = node->children.items[i];
                ast_print_ast_node(types, declaration, level + 2);
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            print_indent(level);
            print_line("signature: %s", type2cstr(node));
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            print_indent(level);
            print_line("statement expression: %s", type2cstr(node));
            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            print_indent(level);
            orstring_t label = *node->identifier;;
            if (node->identifier->length == 0) {
                label = lit2str("<none>");
            }
            print_line("%.*s:%s", node->start.view.length, node->start.view.data, label.cstr);

            ast_print_ast_node(types, an_operand(node), level + 1);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS: {
            print_indent(level);
            print_line(".: %s", type2cstr(node));

            print_indent(level+1);
            print_line("item");
            ast_print_ast_node(types, an_lhs(node), level+2);

            print_indent(level + 1);
            print_line("accessor (%s)", node->identifier->cstr);
            break;
        }

        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            print_indent(level);
            print_line("declaration (%s): %s", node->identifier->cstr, type2cstr(node));

            if (an_decl_type(node) != AST_NODE_TYPE_NONE) {
                print_indent(level+1);
                print_line("type expr");
                ast_print_ast_node(types, an_decl_type(node), level+2);
            }

            if (an_decl_expr(node) != AST_NODE_TYPE_NONE) {
                print_indent(level+1);
                print_line("initial value");
                ast_print_ast_node(types, an_decl_expr(node), level+2);
            }
            break;
        }

        case AST_NODE_TYPE_NONE: {
            print_indent(level);
            print_line("<none>");
            break;
        }
    }

    allocator_return(tmp);
}

void ast_print_node(ast_t *ast, ast_node_t *node) {
    ast_print_ast_node(ast->type_set.types, node, 0);
}

void ast_print(ast_t* ast, const char* name) {
    ORUNUSED(name);

    ast_node_t *module;
    kh_foreach_value(ast->moduleid2node, module, {
        ast_print_ast_node(ast->type_set.types, module, 0);
    });
}
