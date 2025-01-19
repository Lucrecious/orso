#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"
#include "type.h"
#include "type_set.h"
#include "arena.h"
#include "memarr.h"
#include "slot.h"
#include "error.h"

#include "table.h"

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
    SCOPE_TYPE_FUNCDEF                 = 1 << 1,
    SCOPE_TYPE_FUNC_DEF_BODY            = 1 << 2,
    SCOPE_TYPE_JMPABLE                  = 1 << 3,
    SCOPE_TYPE_BLOCK                    = 1 << 4,
    SCOPE_TYPE_STRUCT                   = 1 << 5,
    SCOPE_TYPE_CONDITION                = 1 << 6,
    SCOPE_TYPE_TYPE_CONTEXT             = 1 << 7
} scope_type_t;

declare_table(s2w, string_t, word_t)

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
    AST_NODE_TYPE_EXPRESSION_CAST,
    AST_NODE_TYPE_EXPRESSION_BINARY,
    AST_NODE_TYPE_EXPRESSION_DOT,
    AST_NODE_TYPE_EXPRESSION_UNARY,
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
    AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION,
    AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE,
    AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER,
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
case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: \
case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: \
case AST_NODE_TYPE_EXPRESSION_GROUPING: \
case AST_NODE_TYPE_EXPRESSION_PRIMARY: \
case AST_NODE_TYPE_EXPRESSION_JMP: \
case AST_NODE_TYPE_EXPRESSION_UNARY: \
case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: \
case AST_NODE_TYPE_EXPRESSION_DOT

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

typedef struct value_index_t value_index_t;
struct value_index_t {
    bool exists;
    size_t index;
};

#define value_index_(i) ((value_index_t){.exists=true, .index=(i)})
#define value_index_nil() ((value_index_t){.exists=false, .index=0})

value_index_t memarr_push_value(memarr_t *arr, void *data, size_t size_bytes);

ast_node_t nil_node;

#define an_idx(n, idx) ((idx) < (n)->children.count ? (n)->children.items[idx] : &nil_node)
#define an_operand(n) ((n)->children.items[0])
#define an_expression(n) an_operand(n)
#define an_lhs(n) ((n)->children.items[0])
#define an_rhs(n) ((n)->children.items[1])
#define an_decl_type(n) ((n)->children.items[0])
#define an_decl_expr(n) ((n)->children.items[1])
#define an_condition(n) ((n)->children.items[0])
#define an_then(n) ((n)->children.items[1])
#define an_else(n) ((n)->children.items[2])
#define an_func_def_arg_start(n) (0)
#define an_func_def_arg_end(n) ((n)->children.count-2)
#define an_func_def_return(n) ((n)->children.items[(n)->children.count-2])
#define an_func_def_block(n) ((n)->children.items[(n)->children.count-1])
#define an_callee(n) ((n)->children.items[0])
#define an_call_arg_start(n) (1)
#define an_call_arg_end(n) ((n)->children.count)
#define an_bcall_arg_start(n) (0)
#define an_bcall_arg_end(n) ((n)->children.count)

#define an_is_none(n)    ((n)->node_type == AST_NODE_TYPE_NONE)
#define an_is_notnone(n) ((n)->node_type != AST_NODE_TYPE_NONE)

#define an_is_implicit(n) ((n)->start.view.length == 0)

typedef enum ast_branch_type_t ast_branch_type_t;
enum ast_branch_type_t {
    BRANCH_TYPE_LOOPING,
    BRANCH_TYPE_IFTHEN,
    BRANCH_TYPE_DO,
};

typedef struct ast_node_val_t ast_node_val_t;
struct ast_node_val_t {
    bool is_concrete;
    word_t word;
};

#define ast_node_val_nil() ((ast_node_val_t){0})
#define ast_node_val_word(w) ((ast_node_val_t){.word = (w), .is_concrete=true});

struct ast_node_t {
    ast_node_type_t node_type;

    token_t start, end, operator;

    type_t value_type;

    bool requires_tmp_for_cgen;

    ast_branch_type_t branch_type;
    bool condition_negated;

    bool is_mutable;
    ast_node_t *ref_decl;

    bool fold;
    bool foldable;
    i32 fold_level_resolved_at;

    bool is_free_number;
    ast_node_val_t expr_val;

    ast_node_t *lvalue_node;

    ast_node_t *last_statement;

    ast_nodes_t children;

    token_t identifier;

    ast_node_t *jmp_out_scope_node;
    ast_nodes_t jmp_nodes;

    size_t vm_jmp_index;
    size_t vm_stack_point;
    string_t ccode_break_label;
    string_t ccode_continue_label;
    string_t ccode_var_name;

    union {
        // structs
        ast_struct_t struct_;

        // type initializer
        ast_type_initializer_t initiailizer;
    } as;
};

declare_table(ptr2word, type_t, word_t)

typedef struct ast_node_and_scope_t {
    ast_node_t *node;
    scope_t *scope;
} ast_node_and_scope_t;

declare_table(type2ns, type_t, ast_node_and_scope_t)

typedef struct fd_pairs_t fd_pairs_t;
struct fd_pairs_t {
    function_definition_pair_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct ast_t {
    arena_t allocator;

    bool resolved;
    type_table_t type_set;

    table_t(s2w) *builtins;

    arena_t *function_arena;

    ast_node_t *root;
    
    arena_t constant_arena;
    memarr_t constants;

    table_t(ptr2word) *type_to_zero_word;
    table_t(type2ns) *type_to_creation_node;

    table_t(s2w) *symbols;
} ast_t;

void ast_print(ast_t *ast, const char *name);

value_index_t ast_push_constant_(ast_t *ast, void *data, type_t type);
bool parse_expr(ast_t *ast, string_t file_path, string_view_t source, error_function_t error_fn);
bool parse(ast_t *ast, string_t file_path, string_view_t source, error_function_t error_fn);

type_t valin2type(ast_t *ast, value_index_t index);
value_index_t type2valin(ast_t *ast, type_t t);

i64 valin2i(ast_t *ast, value_index_t index, num_size_t numsize);
value_index_t i2valin(ast_t *ast, i64 value, num_size_t numsize);

u64 valin2u(ast_t *ast, value_index_t index, num_size_t numsize);
value_index_t u2valin(ast_t *ast, u64 value, num_size_t numsize);

f64 valin2d(ast_t *ast, value_index_t index, num_size_t numsize);
value_index_t d2valin(ast_t *ast, f64 value, num_size_t numsize);

bool valin2bool(ast_t *ast, value_index_t value_index);
value_index_t bool2valin(ast_t *ast, bool value);

void ast_init(ast_t *ast, size_t memory_size_bytes);
void ast_free(ast_t *ast);

ast_node_t* ast_node_new(ast_t *ast, ast_node_type_t node_type, token_t start);
ast_node_t *ast_nil(ast_t *ast, type_t value_type, token_t token_location);
ast_node_t *ast_cast(ast_t *ast, type_t destination_type, ast_node_t *expr, token_t cast_token);

bool ast_node_type_is_decl_or_stmt(ast_node_type_t node_type);
bool ast_node_type_is_expression(ast_node_type_t node_type);

token_t token_implicit_at_start(token_t token);
token_t token_implicit_at_end(token_t token);

ast_node_val_t zero_value(ast_t *ast, type_t type);

#endif
