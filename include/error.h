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
    ast_node_t *node;
    token_t token;
    token_type_t expected_token_type;
} error_t;

#define MAX_PARAMETERS 100

typedef struct ast_t ast_t;
typedef void (*error_function_t)(ast_t *ast, error_t error);

#define make_error(et, n, t, ett) ((error_t){ .type = (et), .message = error_messages[et], .node = (n), .token = (t), .expected_token_type = (ett) })
#define make_error_no_args(et) make_error(et, &nil_node, nil_token, TOKEN_ERROR)
#define make_error_token(et, t, ett) make_error(et, &nil_node, t, ett)
#define make_error_node(et, n) make_error(et, n, (n)->start, (n)->start.type)

#endif
