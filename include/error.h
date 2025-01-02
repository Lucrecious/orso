#ifndef ERROR_H_
#define ERROR_H_

#include "lexer.h"

#define ERROR_XMACRO(error_type, message) error_type,
typedef enum error_type_t error_type_t;
enum error_type_t {
#include "error.x"
ERROR_COUNT,
};

#undef ERROR_XMACRO

cstr_t const error_messages[ERROR_COUNT];

typedef struct ast_node_t ast_node_t;
typedef struct error_t {
    error_type_t type;
    cstr_t message;
    ast_node_t *nodes[2];
    token_t token;
    token_type_t expected_token_type;
} error_t;

#define MAX_PARAMETERS 100

typedef struct ast_t ast_t;
typedef void (*error_function_t)(ast_t *ast, error_t error);

#define make_error_(et, n1, n2, t, ett) ((error_t){ .type = (et), .message = error_messages[et], .nodes = {n1, n2}, .token = (t), .expected_token_type = (ett) })
#define make_error(et) make_error_(et, &nil_node, &nil_node, nil_token, TOKEN_ERROR)
#define make_error_token(et, t) make_error_(et, &nil_node, &nil_node, t, TOKEN_ERROR)
#define make_unexpected_error(t, ett) make_error_(ERROR_PARSER_UNEXPECTED_TOKEN, &nil_node, &nil_node, t, ett)
#define make_error_node(et, n) make_error_(et, n, &nil_node, (n)->start, TOKEN_ERROR)
#define make_error_nodes(et, n1, n2) make_error_(et, n1, n2, (n1)->start, TOKEN_ERROR)
#define make_type_mismatch_error(ex, got) make_error_nodes(ERROR_ANALYSIS_TYPE_MISMATCH, ex, got)

#endif
