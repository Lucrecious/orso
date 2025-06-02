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

typedef enum error_arg_type_t error_arg_type_t;
enum error_arg_type_t {
    ERROR_ARG_TYPE_NONE,
    ERROR_ARG_TYPE_NODE,
    ERROR_ARG_TYPE_TOKEN,
    ERROR_ARG_TYPE_SIZE,
    ERROR_ARG_TYPE_TYPE,
    ERROR_ARG_TYPE_PTR,
    ERROR_ARG_TYPE_STRING,
};

typedef struct ast_node_t ast_node_t;

typedef struct error_arg_t error_arg_t;
struct error_arg_t {
    error_arg_type_t type;
    ast_node_t *node_or_null;
    token_t token;
    size_t size;
    ortype_t type_type;
    void *ptr;
    orstring_t str;
};

#define MAX_ERROR_ARGS 8

typedef struct error_t {
    orcstr_t tag;
    orstring_t msg;
    error_source_t level;
    size_t arg_count;
    error_arg_t args[MAX_ERROR_ARGS];
    size_t show_line_count;
    ors64 show_code_lines[MAX_ERROR_ARGS];

} error_t;

#define MAX_PARAMETERS 100

typedef struct ast_t ast_t;
typedef void (*error_function_t)(ast_t *ast, error_t error);

#define make_error(et, n, gt, at) ((error_t){ .type = (et), .message = error_messages[et], .node = (n), .got_token = (gt), .after_token = (at), .is_warning=false })
#define make_error_no_args(et) make_error(et, &nil_node, nil_token, nil_token)
#define make_warning(warning_type, token) ((error_t){ .type = (ERROR_NONE), .message = error_messages[ERROR_NONE], .node = (&nil_node), .got_token = (nil_token), .after_token = (nil_token), .is_warning=true })
#define make_error_token(et, gt, at) make_error(et, &nil_node, gt, at)
#define make_error_node(et, n) make_error(et, n, (n)->start, (n)->end)

#define OR_ERROR(...) ((error_t){__VA_ARGS__})
#define ORERR_ARGS(...) {__VA_ARGS__}, .arg_count = (sizeof((error_arg_t[]){__VA_ARGS__})/sizeof(error_arg_t))
#define ORERR_LINES(...) {__VA_ARGS__}, .show_line_count = (sizeof((ors64[]){__VA_ARGS__})/sizeof(ors64))

orstring_t error2richstring(ast_t *ast, error_t error, arena_t *arena);

error_arg_t error_arg_token(token_t token);
error_arg_t error_arg_node(ast_node_t *node);
error_arg_t error_arg_sz(size_t sz);
error_arg_t error_arg_type(ortype_t type);
error_arg_t error_arg_ptr(void *ptr);
error_arg_t error_arg_str(ast_t *ast, orstring_t str);

#endif
