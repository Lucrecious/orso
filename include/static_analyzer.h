#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "type_set.h"
#include "arena.h"
#include "vm.h"

typedef struct analyzer_t {
    ast_t *ast;
    bool had_error;
    env_t *run_env;

    function_t placeholder;

    struct {
        ast_node_t **items;
        size_t capacity;
        size_t count;
        arena_t *allocator;
    } pending_dependencies;

    arena_t *arena;
} analyzer_t;

bool resolve_ast(ast_t *ast);

#endif
