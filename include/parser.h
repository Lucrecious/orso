#ifndef PARSER_H_
#define PARSER_H_

#include "chunk.h"
#include "lexer.h"
#include "object.h"
#include "symbol_table.h"
#include "type.h"
#include "type_set.h"
#include "arena.h"

#include "table.h"

typedef enum return_guarentee_t {
    ORSO_NO_RETURN_GUARENTEED,
    ORSO_MAYBE_RETURNS,
    ORSO_RETURN_GUARENTEED,
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
    function_t *function;
    ast_node_t *ast_defintion;
} function_definition_pair_t;

typedef enum ast_node_type_t {
    ORSO_AST_NODE_TYPE_UNDEFINED, // TODO: try to remove this shit
    ORSO_AST_NODE_TYPE_DECLARATION,
    ORSO_AST_NODE_TYPE_STATEMENT_RETURN,
    ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION,
    ORSO_AST_NODE_TYPE_EXPRESSION_PRINT, // TODO: remove in favor of a native function
    ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR, // TODO: remove in favor of a native function
    ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT,
    ORSO_AST_NODE_TYPE_EXPRESSION_BINARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_DOT,
    ORSO_AST_NODE_TYPE_EXPRESSION_UNARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING,
    ORSO_AST_NODE_TYPE_EXPRESSION_CALL,
    ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY,
    ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY,
    ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT,
    ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK,
    ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING,
    ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION,
    ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION,
    ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE,
    ORSO_AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER,

    /*
    * This is a special case expression. It's simply an expression with a statement inside.
    * This is used for branching.
    */
    ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT,
} ast_node_type_t;

#define ORSO_AST_NODE_TYPE_EXPRESSION_CASE \
ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK: \
case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING: \
case ORSO_AST_NODE_TYPE_EXPRESSION_CALL: \
case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: \
case ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: \
case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: \
case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING: \
case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY: \
case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR: \
case ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT: \
case ORSO_AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: \
case ORSO_AST_NODE_TYPE_EXPRESSION_DOT

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

struct ast_node_t {
    ast_node_type_t node_type;

    token_t start, end, operator;

    // expressions TODO: Fill this in for *everything*, declarations, statements included
    // TODO: only use value type
    type_t *value_type, *value_type_narrowed;
    //AccessIdentifiers *value_type_identifiers, *value_type_narrowed_identifiers;

    // branching, blocks
    return_guarentee_t return_guarentee;

    bool is_in_type_context;

    bool fold;

    bool foldable;

    // primary, folding value, declaration default value
    i32 value_index;

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
    } data;
};

declare_table(ptr2i32, void*, i32)

typedef struct ast_node_and_scope_t {
    ast_node_t *node;
    scope_t *scope;
} ast_node_and_scope_t;

declare_table(type2ns, type_t*, ast_node_and_scope_t)

typedef struct fd_pairs_t fd_pairs_t;
struct fd_pairs_t {
    function_definition_pair_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct ast_t {
    arena_t allocator;

    i32 void_index;
    i32 true_index;

    bool resolved;
    type_set_t type_set;

    symbol_table_t builtins;

    fd_pairs_t function_definition_pairs;

    ast_node_t *root;
    types_t folded_constant_types;
    slots_t folded_constants;

    table_t(ptr2i32) *type_to_zero_index;
    table_t(type2ns) *type_to_creation_node;

    symbol_table_t *symbols;
} ast_t;

void ast_print(ast_t *ast, const char *name);

i32 add_value_to_ast_constant_stack(ast_t *ast, slot_t *value, type_t *type);
bool parse(ast_t *ast, const char *source, error_function_t error_fn);
type_t *get_folded_type(ast_t *ast, i32 index);

void ast_init(ast_t *ast, symbol_table_t* symbols);
void orso_ast_free(ast_t *ast);

ast_node_t* orso_ast_node_new(ast_t *ast, ast_node_type_t node_type, bool is_in_type_context, token_t start);

bool orso_ast_node_type_is_decl_or_stmt(ast_node_type_t node_type);
bool orso_ast_node_type_is_expression(ast_node_type_t node_type);

i32 orso_zero_value(ast_t *ast, type_t *type, symbol_table_t *symbol_table);

#endif
