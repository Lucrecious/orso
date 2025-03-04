#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "type_set.h"
#include "arena.h"
#include "vm.h"

typedef struct analyzer_t {
    error_function_t error_fn;
    ast_t *ast;
    bool had_error;
    env_t *env_or_null;

    function_t placeholder;

    struct {
        ast_node_t **items;
        size_t capacity;
        size_t count;
        arena_t *allocator;
    } pending_dependencies;

    arena_t allocator;
} analyzer_t;

void analyzer_init(analyzer_t *analyzer, env_t *env, write_function_t write_fn, error_function_t error_fn);
void analyzer_free(analyzer_t *analyzer);

bool resolve_ast(analyzer_t *analyzer, ast_t *ast);
bool resolve_ast_expr(analyzer_t *analyzer, ast_t *ast, ast_node_t *expr);

#endif
