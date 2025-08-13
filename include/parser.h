#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"
#include "type.h"
#include "type_set.h"
#include "arena.h"
#include "memarr.h"
#include "error.h"

#include "table.h"

struct vm_t;

typedef void (*intrinsic_fn_t)(struct vm_t *vm, void *args_reverse_order, void *result);

typedef struct bools_t bools_t;
struct bools_t {
    bool *items;
    size_t capacity;
    size_t count;
    arena_t *allocator;
};

struct ast_node_t;
typedef struct ast_node_t ast_node_t;

typedef struct ast_nodes_t ast_nodes_t;
struct ast_nodes_t {
    ast_node_t **items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef enum scope_type_t {
    SCOPE_TYPE_NONE                     = 0,
    SCOPE_TYPE_MODULE                   = 1 << 0,
    SCOPE_TYPE_FUNCDEF                  = 1 << 1,
    SCOPE_TYPE_FUNC_DEF_BODY            = 1 << 2,
    SCOPE_TYPE_JMPABLE                  = 1 << 3,
    SCOPE_TYPE_BLOCK                    = 1 << 4,
    SCOPE_TYPE_STRUCT                   = 1 << 5,
    SCOPE_TYPE_CONDITION                = 1 << 6,
    SCOPE_TYPE_TYPE_CONTEXT             = 1 << 7,
    SCOPE_TYPE_FOLD_DIRECTIVE           = 1 << 8,
    SCOPE_TYPE_INFERRED_PARAMS          = 1 << 9,
    SCOPE_TYPE_MACRO                    = 1 << 10,
} scope_type_t;

typedef struct ffi_t ffi_t;
struct ffi_t {
    orstring_t funcname;
    orstring_t libpath;
    orstring_t callconv;
    ortype_t return_type;
    types_t arg_types;
    ast_node_t *node;
};

declare_table(s2s, orstring_t, oristring_t)
declare_table(s2w, oristring_t, orword_t)
declare_table(s2n, orstring_t, ast_node_t*)
declare_table(s2fis, orstring_t, ffi_t*)

typedef struct scope_t {
    ast_node_t *creator;
    
    table_t(s2w) *definitions;
    ast_nodes_t subscript_decls;
    scope_type_t type;
    struct scope_t *outer;
} scope_t;

typedef struct function_definition_pair_t {
    ast_node_t *ast_defintion;
} function_definition_pair_t;

typedef enum ast_node_type_t {
    AST_NODE_TYPE_NONE,

    AST_NODE_TYPE_MODULE,
    AST_NODE_TYPE_DECLARATION_DEFINITION, // for declaring globals, locals and constants
    AST_NODE_TYPE_DECLARATION_STATEMENT, // for expressions

    AST_NODE_TYPE_EXPRESSION_JMP, // return, break, continue
    AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL,
    AST_NODE_TYPE_EXPRESSION_CAST,
    AST_NODE_TYPE_EXPRESSION_BINARY,
    AST_NODE_TYPE_EXPRESSION_DOT_ACCESS,
    AST_NODE_TYPE_EXPRESSION_UNARY,
    AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE,
    AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS,
    AST_NODE_TYPE_EXPRESSION_GROUPING,
    AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL,
    AST_NODE_TYPE_EXPRESSION_CALL,
    AST_NODE_TYPE_EXPRESSION_PRIMARY,
    AST_NODE_TYPE_EXPRESSION_DEF_VALUE,
    AST_NODE_TYPE_EXPRESSION_NIL,
    AST_NODE_TYPE_EXPRESSION_ASSIGNMENT,
    AST_NODE_TYPE_EXPRESSION_BLOCK,
    AST_NODE_TYPE_EXPRESSION_BRANCHING,
    AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION,
    AST_NODE_TYPE_EXPRESSION_STRUCT,
    AST_NODE_TYPE_EXPRESSION_ENUM,
    AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE,
    AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST,
    AST_NODE_TYPE_EXPRESSION_DIRECTIVE,
} ast_node_type_t;

#define AST_NODE_TYPE_EXPRESSION_CASE \
AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: \
case AST_NODE_TYPE_EXPRESSION_BINARY: \
case AST_NODE_TYPE_EXPRESSION_BLOCK: \
case AST_NODE_TYPE_EXPRESSION_BRANCHING: \
case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: \
case AST_NODE_TYPE_EXPRESSION_CALL: \
case AST_NODE_TYPE_EXPRESSION_CAST: \
case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: \
case AST_NODE_TYPE_EXPRESSION_NIL: \
case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: \
case AST_NODE_TYPE_EXPRESSION_STRUCT: \
case AST_NODE_TYPE_EXPRESSION_ENUM: \
case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: \
case AST_NODE_TYPE_EXPRESSION_GROUPING: \
case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: \
case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: \
case AST_NODE_TYPE_EXPRESSION_PRIMARY: \
case AST_NODE_TYPE_EXPRESSION_JMP: \
case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: \
case AST_NODE_TYPE_EXPRESSION_UNARY: \
case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: \
case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: \
case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS

extern ast_node_t nil_node;

#define an_idx(n, idx) ((idx) < (n)->children.count ? (n)->children.items[idx] : &nil_node)
#define an_operand(n) ((n)->children.items[0])
#define an_expression(n) an_operand(n)
#define an_lhs(n) ((n)->children.items[0])
#define an_rhs(n) ((n)->children.items[1])
#define an_cast_expr(n) ((n)->children.items[0])
#define an_cast_type(n) ((n)->children.items[1])
#define an_decl_type(n) ((n)->children.items[0])
#define an_decl_expr(n) ((n)->children.items[1])

// if, unless, while, until, do, for
#define an_condition(n) ((n)->children.items[0])
#define an_then(n) ((n)->children.items[1])
#define an_else(n) ((n)->children.items[2])
#define an_for_decl(n) ((n)->children.items[3])
#define an_for_incr(n) ((n)->children.items[4])


#define an_func_def_return(n) ((n)->children.items[0])
#define an_func_def_block(n) ((n)->children.items[1])
#define an_func_def_arg_start(n) (2)
#define an_func_def_arg_end(n) ((n)->children.count)
#define an_callee(n) ((n)->children.items[0])
#define an_call_arg_start(n) (1)
#define an_call_arg_end(n) ((n)->children.count)
#define an_bcall_arg_start(n) (0)
#define an_bcall_arg_end(n) ((n)->children.count)
#define an_dir_arg_start(n) (0)
#define an_dir_arg_end(n) ((n)->children.count)
#define an_list_lhs(n) ((n)->children.items[0])
#define an_list_start(n) (1)
#define an_list_end(n) ((n)->children.count)
#define an_array_size_expr(n) ((n)->children.items[0])
#define an_array_type_expr(n) ((n)->children.items[1])
#define an_item_accessee(n) ((n)->children.items[0])
#define an_item_accessor(n) ((n)->children.items[1])
#define an_struct_param_start(n) (0)
#define an_struct_param_end(n) ((n)->param_end)
#define an_struct_start(n) ((n)->param_end)
#define an_struct_end(n) ((n)->children.count)
#define an_enum_start(n) (0)
#define an_enum_end(n) ((n)->children.count)
#define an_dot_lhs(n) ((n)->children.items[0])

#define an_fficall_lib(n) ((n)->children.items[0])
#define an_fficall_rettype(n) ((n)->children.items[1])
#define an_fficall_funcname(n) ((n)->children.items[2])
#define an_fficall_arg_start(n) (3)
#define an_fficall_arg_end(n) ((n)->children.count)

#define an_icall_funcname(n) ((n)->children.items[0])
#define an_icall_arg_start(n) (1)
#define an_icall_arg_end(n) ((n)->children.count)

#define an_is_none(n)    ((n)->node_type == AST_NODE_TYPE_NONE)
#define an_is_notnone(n) ((n)->node_type != AST_NODE_TYPE_NONE)

#define an_is_implicit(n) ((n)->start.view.length == 0)

typedef enum ast_branch_type_t ast_branch_type_t;
enum ast_branch_type_t {
    BRANCH_TYPE_WHILE,
    BRANCH_TYPE_IF,
    BRANCH_TYPE_DO,
    BRANCH_TYPE_FOR,
};

typedef struct ast_node_val_t ast_node_val_t;
struct ast_node_val_t {
    bool is_concrete;
    orword_t word;
};

#define ast_node_val_nil() ((ast_node_val_t){0})
#define ast_node_val_word(w) ((ast_node_val_t){.word = (w), .is_concrete=true});

#define an_is_constant(n) (!(n)->is_mutable)

typedef enum match_type_t match_type_t;
enum match_type_t {
    MATCH_TYPE_IDENTIFIER,
    MATCH_TYPE_POINTER,
    MATCH_TYPE_ARRAY_TYPE,
    MATCH_TYPE_ARRAY_SIZE,
    MATCH_TYPE_STRUCT_PARAM,
    MATCH_TYPE_SIG_ARG,
    MATCH_TYPE_SIG_RET,
};

typedef struct matched_value_t matched_value_t;
struct matched_value_t {
    ortype_t type;
    orword_t word;
};

typedef struct matched_values_t matched_values_t;
struct matched_values_t {
    matched_value_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct type_path_t type_path_t;
struct type_path_t {
    match_type_t kind;
    type_path_t *next;
    size_t index;
};

typedef struct type_pattern_t type_pattern_t;
struct type_pattern_t {
    type_path_t *expected;
    oristring_t identifier;
};

typedef struct inferred_copy_t inferred_copy_t;
struct inferred_copy_t {
    matched_values_t key;
    ast_node_t *copy;
};

typedef struct inferred_copies_t inferred_copies_t;
struct inferred_copies_t {
    inferred_copy_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct type_patterns_t type_patterns_t;
struct type_patterns_t {
    type_pattern_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef enum reg_t reg_t;
enum reg_t {
    REG_NULL = 0,
    REG_RESULT = 'r',
    REG_T = 't',
    REG_U = 'u',
    REG_STACK_BOTTOM = 's',
    REG_STACK_FRAME = 'f',
};

typedef enum val_dst_type_t val_dst_type_t;
enum val_dst_type_t {
    VAL_DST_VOID,
    VAL_DST_REG,
    VAL_DST_STACK_POINT,
    VAL_DST_RETURN,
};

typedef struct val_dst_t val_dst_t;
struct val_dst_t {
    val_dst_type_t type;
    size_t stack_point;
    reg_t reg;
};

typedef struct orintrinsic_fn_t orintrinsic_fn_t;
struct orintrinsic_fn_t {
    oristring_t name;
    ortype_t ret_type;
    types_t arg_types;

    bool has_varargs;

    intrinsic_fn_t fnptr;
};

typedef struct orintrinsic_fns_t orintrinsic_fns_t;
struct orintrinsic_fns_t {
    orintrinsic_fn_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

struct function_t;

typedef struct functions_t functions_t;
struct functions_t {
    struct function_t **items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

// the ast node is pretty large and redundant right now on purpose
// once the ast node is stable, it'll be compressed since many of these
// fields are mutally exclusive
struct ast_node_t {
    ast_node_type_t node_type;

    token_t start, end, operator;

    ortype_t value_type;

    bool requires_tmp_for_cgen;

    ast_branch_type_t branch_type;
    bool condition_negated;

    scope_t defined_scope;
    scope_t *call_scope;

    // todo: to optimize for space consider merging these into a flag enum
    bool is_global;
    bool has_default_value;
    bool is_mutable;
    bool is_subscript_function;
    bool is_intrinsic;
    bool is_exported;
    bool is_consumed;
    bool is_compile_time_param;
    bool is_free_number;
    bool is_macro;
    bool is_in_outer_function_scope;

    ast_node_t *ref_decl;
    type_patterns_t type_decl_patterns;

    orintrinsic_fn_t intrinsic_fn;

    ffi_t *ffi_or_null;
    orstring_t filepath;

    size_t param_end;
    size_t arg_index;
    size_t value_offset;

    ast_node_val_t expr_val;

    ast_node_t *lvalue_node;

    ast_node_t *last_statement;

    ast_nodes_t children;

    ast_nodes_t owned_funcdefs;
    ast_nodes_t module_deps;
    ast_nodes_t tmp_decls;
    functions_t func_deps;

    ast_node_t *subscript_call_or_null;

    oristring_t identifier;
    oristring_t label;

    ast_node_t *jmp_out_scope_node;
    ast_nodes_t jmp_nodes;

    inferred_copies_t realized_copies;

    size_t vm_jmp_index;
    size_t vm_clean_stack_size;
    val_dst_t vm_val_dst;
    orstring_t ccode_break_label;
    orstring_t ccode_continue_label;
    orstring_t ccode_var_name;
    orstring_t ccode_associated_h;
};

declare_table(t2w, ortype_t, orword_t)
declare_table(t2n, ortype_t, ast_node_t*)

typedef struct ast_node_and_scope_t {
    ast_node_t *node;
    scope_t *scope;
} ast_node_and_scope_t;

declare_table(type2ns, ortype_t, ast_node_and_scope_t)

typedef struct fd_pairs_t fd_pairs_t;
struct fd_pairs_t {
    function_definition_pair_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

declare_table(p2s, void*, orstring_t);

typedef struct errors_t errors_t;
struct errors_t {
    error_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

khint_t hashptr_(void *ptr);
bool hasheq_(void *a, void *b);

declare_table(fn2an, void*, ast_node_t*);

typedef struct ast_t {
    arena_t *arena;

    bool resolved;
    type_table_t type_set;

    void *vm;

    memarr_t multiword_data;

    errors_t errors;

    size_t tmp_counter;

    table_t(s2s) *strings;
    table_t(s2w) *builtins;
    table_t(p2s) *intrinsicfn2cname;
    table_t(fn2an) *fn2an;
    table_t(s2fis) *ffis;
    table_t(t2n) *t2n;

    orintrinsic_fns_t directives;
    orintrinsic_fns_t intrinsics;

    strings_t search_paths;

    ast_node_t *core_module_or_null;
    table_t(s2n) *moduleid2node;

    ast_nodes_t global_decls_in_resolution_order;

    table_t(t2w) *type_to_zero_word;

    oristring_t arrayind2name[4][4];
} ast_t;

void ast_print_node(ast_t *ast, ast_node_t *node);
void ast_print(ast_t *ast, const char *name);

oristring_t ast_sv2istring(ast_t *ast, string_view_t view);
oristring_t ast_next_tmp_name(ast_t *ast);

bool parse_string_to_module(ast_t *ast, ast_node_t *module, orstring_t filepath, string_view_t source);
ast_node_t *parse_source_into_module(ast_t *ast, orstring_t file_path, string_view_t source);
token_type_t parser_opeq2op(token_type_t type);

void ast_init(ast_t *ast, arena_t *arena);

#define ast_type2td(ast, type) type2typedata(&((ast)->type_set.types), (type))

void scope_init(scope_t *scope, arena_t *allocator, scope_type_t type, scope_t *outer, ast_node_t *creator_expression);

ast_node_t *ast_node_new(arena_t *arena, ast_node_type_t node_type, token_t start);
ast_node_t *ast_node_copy(arena_t *arena, ast_node_t *node);
ast_node_t *ast_nil(ast_t *ast, ortype_t value_type, token_t token_location);
ast_node_t *ast_decldef(ast_t *ast, oristring_t identifier, ast_node_t *type_expr, ast_node_t *init_expr, token_t start);
ast_node_t *ast_statement(ast_t *ast, ast_node_t *expr);
ast_node_t *ast_def_value(ast_t *ast, oristring_t identifer, token_t start);
ast_node_t *ast_inferred_type_decl(ast_t *ast, token_t squiggle_token, oristring_t identifer, token_t end);
ast_node_t *ast_cast(ast_t *ast, ast_node_t *expr_type, ast_node_t *expr);
ast_node_t *ast_unary(ast_t *ast, token_t operator, ast_node_t *operand);
ast_node_t *ast_begin_module(ast_t *ast);
ast_node_t *ast_assignment(ast_t *ast, ast_node_t *lhs, ast_node_t *rhs, token_t equals);
ast_node_t *ast_item_access(ast_t *ast, ast_node_t *accessee, ast_node_t *accessor);

ast_node_t *ast_call_begin(ast_t *ast, ast_node_t *callee_or_null, token_t start);
void ast_call_end(ast_node_t *call, token_t end);

ast_node_t *ast_do(ast_t *ast, oristring_t label, ast_node_t *expr, token_t do_token);
ast_node_t *ast_block_begin(ast_t *ast, token_t start);
void ast_block_decl(ast_node_t *block, ast_node_t *decl);
void ast_block_end(ast_node_t *block, token_t end);

void ast_end_module(ast_node_t *module);
void ast_add_module(ast_t *ast, ast_node_t *module, orstring_t moduleid);
ast_node_t *ast_implicit_expr(ast_t *ast, ortype_t type, orword_t value, token_t where);
orword_t *ast_multiword_value(ast_t *ast, size_t size_words);
orword_t ast_mem2word(ast_t *ast, void *data, ortype_t type);
ast_node_t *parse_expression_string(ast_t *ast, orstring_t code, bool *had_error);

bool ast_find_intrinsic_funcname(orintrinsic_fns_t fns, oristring_t name, orintrinsic_fn_t *fn);

bool ast_node_type_is_expression(ast_node_type_t node_type);

token_t token_implicit_at_start(token_t token);
token_t token_implicit_at_end(token_t token);

ast_node_val_t zero_value(ast_t *ast, ortype_t type);

#endif
