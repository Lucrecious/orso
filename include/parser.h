#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"
#include "type.h"
#include "type_set.h"
#include "arena.h"
#include "memarr.h"
#include "error.h"

#include "table.h"

typedef void (*intrinsic_fn_t)(types_t *types, void *args_reverse_order, void *result);

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

declare_table(s2w, orstring_t, orword_t)
declare_table(s2n, orstring_t, ast_node_t*)
declare_table(s2fis, orstring_t, ffi_t*)

typedef struct scope_t {
    ast_node_t *creator;
    
    table_t(s2w) *definitions;
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

typedef struct ast_struct_t {
    ast_nodes_t declarations;
} ast_struct_t;

typedef struct ast_call_t {
    ast_node_t *callee;
    ast_nodes_t arguments;
} ast_call_t;

typedef struct ast_member_access_t {
    ast_node_t *lhs;

    token_t identifier;
    ast_node_t *referencing_declaration;
} ast_member_access_t;

typedef struct ast_type_initializer_t ast_type_initializer_t;
struct ast_type_initializer_t {
    ast_node_t *type;
    ast_nodes_t arguments;
};

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
#define an_dot_lhs(n) ((n)->children.items[0])

#define an_fficall_libpath(n) ((n)->children.items[0])
#define an_fficall_callconv(n) ((n)->children.items[1])
#define an_fficall_rettype(n) ((n)->children.items[2])
#define an_fficall_funcname(n) ((n)->children.items[3])
#define an_fficall_arg_start(n) (4)
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
    token_t identifier;
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
    orstring_t name;
    types_t arg_types;

    bool has_varargs;
};

typedef struct orintrinsic_fns_t orintrinsic_fns_t;
struct orintrinsic_fns_t {
    orintrinsic_fn_t *items;
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

    // todo: to optimize for space consider merging these into a flag enum
    bool is_global;
    bool has_default_value;
    bool is_mutable;
    bool is_intrinsic;
    bool is_exported;
    bool is_consumed;
    bool is_compile_time_param;
    bool is_free_number;

    ast_node_t *ref_decl;
    type_patterns_t type_decl_patterns;

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

    token_t identifier;
    token_t label;

    ast_node_t *jmp_out_scope_node;
    ast_nodes_t jmp_nodes;

    inferred_copies_t realized_copies;

    size_t vm_jmp_index;
    val_dst_t vm_val_dst;
    orstring_t ccode_break_label;
    orstring_t ccode_continue_label;
    orstring_t ccode_var_name;
    orstring_t ccode_associated_h;
};

declare_table(t2w, ortype_t, orword_t)

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

    table_t(s2w) *builtins;
    table_t(p2s) *intrinsicfn2cname;
    table_t(fn2an) *fn2an;
    table_t(s2fis) *ffis;

    orintrinsic_fns_t directives;

    ast_node_t *core_module_or_null;
    table_t(s2n) *moduleid2node;

    ast_nodes_t global_decls_in_resolution_order;

    table_t(t2w) *type_to_zero_word;
    table_t(type2ns) *type_to_creation_node;
} ast_t;

void ast_print_node(ast_t *ast, ast_node_t *node);
void ast_print(ast_t *ast, const char *name);

bool parse_string_to_module(ast_t *ast, ast_node_t *module, orstring_t filepath, string_view_t source);
ast_node_t *parse_source_into_module(ast_t *ast, orstring_t file_path, string_view_t source);
token_type_t parser_opeq2op(token_type_t type);

void ast_init(ast_t *ast, arena_t *arena);

#define ast_type2td(ast, type) type2typedata(&((ast)->type_set.types), (type))

void scope_init(scope_t *scope, arena_t *allocator, scope_type_t type, scope_t *outer, ast_node_t *creator_expression);

ast_node_t *ast_node_new(arena_t *arena, ast_node_type_t node_type, token_t start);
ast_node_t *ast_node_copy(arena_t *arena, ast_node_t *node);
ast_node_t *ast_nil(ast_t *ast, ortype_t value_type, token_t token_location);
ast_node_t *ast_decldef(ast_t *ast, token_t identifier, ast_node_t *type_expr, ast_node_t *init_expr);
ast_node_t *ast_def_value(ast_t *ast, token_t identifer);
ast_node_t *ast_inferred_type_decl(ast_t *ast, token_t squiggle_token, token_t identifer);
ast_node_t *ast_cast(ast_t *ast, ast_node_t *expr_type, ast_node_t *expr);
ast_node_t *ast_begin_module(ast_t *ast);
void ast_end_module(ast_node_t *module);
void ast_add_module(ast_t *ast, ast_node_t *module, orstring_t moduleid);
ast_node_t *ast_implicit_expr(ast_t *ast, ortype_t type, orword_t value, token_t where);
orword_t *ast_multiword_value(ast_t *ast, size_t size_words);
orword_t ast_mem2word(ast_t *ast, void *data, ortype_t type);

bool ast_find_intrinsic_funcname(orintrinsic_fns_t fns, string_view_t name, orintrinsic_fn_t *fn);

bool ast_node_type_is_expression(ast_node_type_t node_type);

token_t token_implicit_at_start(token_t token);
token_t token_implicit_at_end(token_t token);

ast_node_val_t zero_value(ast_t *ast, ortype_t type);

#endif
