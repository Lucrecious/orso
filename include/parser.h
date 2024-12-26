#ifndef PARSER_H_
#define PARSER_H_

#include "lexer.h"
#include "object.h"
#include "symbol_table.h"
#include "type.h"
#include "type_set.h"
#include "arena.h"
#include "memarr.h"

#include "table.h"

typedef enum return_guarentee_t {
    RETURN_GUARENTEE_NONE,
    RETURN_GUARENTEE_MAYBE,
    RETURN_GUARENTEE_YES,
} return_guarentee_t;

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
    SCOPE_TYPE_MODULE,
    SCOPE_TYPE_FUNCTION_PARAMETERS,
    SCOPE_TYPE_FUNCTION_BODY,
    SCOPE_TYPE_BLOCK,
    SCOPE_TYPE_STRUCT,
} scope_type_t;

typedef struct scope_t {
    ast_node_t *creator;
    symbol_table_t named_entities;
    scope_type_t type;
    struct scope_t *outer;
} scope_t;

typedef struct function_definition_pair_t {
    function_t_ *function;
    ast_node_t *ast_defintion;
} function_definition_pair_t;

typedef enum ast_node_type_t {
    AST_NODE_TYPE_UNDEFINED, // TODO: try to remove this shit
    AST_NODE_TYPE_DECLARATION,
    AST_NODE_TYPE_STATEMENT_RETURN,
    AST_NODE_TYPE_STATEMENT_EXPRESSION,
    AST_NODE_TYPE_EXPRESSION_PRINT, // TODO: remove in favor of a native function
    AST_NODE_TYPE_EXPRESSION_PRINT_EXPR, // TODO: remove in favor of a native function
    AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT,
    AST_NODE_TYPE_EXPRESSION_BINARY,
    AST_NODE_TYPE_EXPRESSION_DOT,
    AST_NODE_TYPE_EXPRESSION_UNARY,
    AST_NODE_TYPE_EXPRESSION_GROUPING,
    AST_NODE_TYPE_EXPRESSION_CALL,
    AST_NODE_TYPE_EXPRESSION_PRIMARY,
    AST_NODE_TYPE_EXPRESSION_ENTITY,
    AST_NODE_TYPE_EXPRESSION_ASSIGNMENT,
    AST_NODE_TYPE_EXPRESSION_BLOCK,
    AST_NODE_TYPE_EXPRESSION_BRANCHING,
    AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION,
    AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION,
    AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE,
    AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER,

    /*
    * This is a special case expression. It's simply an expression with a statement inside.
    * This is used for branching.
    */
    AST_NODE_TYPE_EXPRESSION_STATEMENT,
} ast_node_type_t;

#define AST_NODE_TYPE_EXPRESSION_CASE \
AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: \
case AST_NODE_TYPE_EXPRESSION_BINARY: \
case AST_NODE_TYPE_EXPRESSION_BLOCK: \
case AST_NODE_TYPE_EXPRESSION_BRANCHING: \
case AST_NODE_TYPE_EXPRESSION_CALL: \
case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: \
case AST_NODE_TYPE_EXPRESSION_ENTITY: \
case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: \
case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: \
case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: \
case AST_NODE_TYPE_EXPRESSION_GROUPING: \
case AST_NODE_TYPE_EXPRESSION_PRIMARY: \
case AST_NODE_TYPE_EXPRESSION_UNARY: \
case AST_NODE_TYPE_EXPRESSION_PRINT: \
case AST_NODE_TYPE_EXPRESSION_PRINT_EXPR: \
case AST_NODE_TYPE_EXPRESSION_STATEMENT: \
case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: \
case AST_NODE_TYPE_EXPRESSION_DOT

typedef struct ast_function_t {
    ast_nodes_t parameter_nodes;
    ast_node_t *return_type_expression;

    ast_node_t *block;

    bool compilable;
} ast_function_t;

typedef struct ast_struct_t {
    ast_nodes_t declarations;
} ast_struct_t;

typedef struct ast_call_t {
    ast_node_t *callee;
    ast_nodes_t arguments;
} ast_call_t;

typedef struct ast_branch_t {
    ast_node_t *condition;
    ast_node_t *then_expression;
    ast_node_t *else_expression;

    bool looping;
    bool condition_negated;
} ast_branch_t;

typedef struct ast_binary_t {
    ast_node_t *lhs;
    ast_node_t *rhs;
} ast_binary_t;

typedef struct ast_member_access_t {
    ast_node_t *lhs;

    token_t identifier;
    ast_node_t *referencing_declaration;
} ast_member_access_t;

typedef struct ast_declaration_t {
    bool is_mutable;
    i32 fold_level_resolved_at;

    token_t identifier;
    ast_node_t *type_expression;
    
    ast_node_t *initial_value_expression;
} ast_declaration_t;

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

bool memarr_push_value(memarr_t *arr, void *data, size_t size_bytes, value_index_t *out_index);

struct ast_node_t {
    ast_node_type_t node_type;

    token_t start, end, operator;

    // expressions TODO: Fill this in for *everything*, declarations, statements included
    // TODO: only use value type
    type_t value_type;
    //AccessIdentifiers *value_type_identifiers, *value_type_narrowed_identifiers;

    // branching, blocks
    return_guarentee_t return_guarentee;

    bool is_in_type_context;

    bool fold;

    bool foldable;

    // primary, folding value, declaration default value
    value_index_t value_index;

    ast_node_t *lvalue_node;

    union {
        ast_declaration_t declaration;

        // statement, print, print_expr, cast, grouping
        ast_node_t *expression;
        ast_node_t *statement; // for readability

        ast_call_t call;

        // binary, assignment
        ast_binary_t binary;

        // block (declarations or statements)
        ast_nodes_t block;

        // branching (if, unless, while, until)
        ast_branch_t branch;

        // function signatures and defintions
        ast_function_t function;

        // structs
        ast_struct_t struct_;

        // member access, entity
        ast_member_access_t dot;

        // type initializer
        ast_type_initializer_t initiailizer;
    } as;
};

declare_table(ptr2sizet, type_t, size_t)

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

    symbol_table_t builtins;

    fd_pairs_t function_definition_pairs;

    ast_node_t *root;
    memarr_t constants;

    table_t(ptr2sizet) *type_to_zero_index;
    table_t(type2ns) *type_to_creation_node;

    symbol_table_t symbols;
} ast_t;

void ast_print(ast_t *ast, const char *name);

value_index_t add_value_to_ast_constant_stack(ast_t *ast, void *data, type_t type);
bool parse_expr(ast_t *ast, string_t file_path, cstr_t source, error_function_t error_fn);
bool parse(ast_t *ast, string_t file_path, cstr_t source, error_function_t error_fn);
type_t get_folded_type(ast_t *ast, value_index_t index);

void ast_init(ast_t *ast, size_t memory_size_bytes);
void ast_free(ast_t *ast);

ast_node_t* ast_node_new(ast_t *ast, ast_node_type_t node_type, bool is_in_type_context, token_t start);

bool ast_node_type_is_decl_or_stmt(ast_node_type_t node_type);
bool ast_node_type_is_expression(ast_node_type_t node_type);

value_index_t zero_value(ast_t *ast, type_t type, symbol_table_t *symbol_table);

#endif
