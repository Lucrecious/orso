#ifndef ERROR_H_
#define ERROR_H_

#include "lexer.h"

typedef enum error_source_t error_source_t;
enum error_source_t {
    ERROR_SOURCE_PARSER,
    ERROR_SOURCE_PARSEREX,
    ERROR_SOURCE_ANALYSIS,
    ERROR_SOURCE_CODEGEN,
};

#define ERROR_XMACRO(error_type, message, error_source_type) error_type,
typedef enum error_type_t error_type_t;
enum error_type_t {
#include "error.x"
ERROR_COUNT,
};

#undef ERROR_XMACRO

cstr_t const error_messages[ERROR_COUNT];
error_source_t error_sources[ERROR_COUNT];

typedef struct ast_node_t ast_node_t;
typedef struct error_t {
    error_type_t type;
    cstr_t message;
    ast_node_t *node;
    token_t got_token;
    token_t after_token;
    bool is_warning;
} error_t;

#define MAX_PARAMETERS 100

typedef struct ast_t ast_t;
typedef void (*error_function_t)(ast_t *ast, error_t error);

#define make_error(et, n, gt, at) ((error_t){ .type = (et), .message = error_messages[et], .node = (n), .got_token = (gt), .after_token = (at), .is_warning=false })
#define make_error_no_args(et) make_error(et, &nil_node, nil_token, nil_token)
#define make_warning(warning_type, token) ((error_t){ .type = (ERROR_NONE), .message = error_messages[ERROR_NONE], .node = (&nil_node), .got_token = (nil_token), .after_token = (nil_token), .is_warning=true })
#define make_error_token(et, gt, at) make_error(et, &nil_node, gt, at)
#define make_error_node(et, n) make_error(et, n, (n)->start, (n)->end)

#endif
