#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "type_set.h"
#include "arena.h"
#include "vm.h"

typedef struct analyzer_t {
    ast_t *ast;
    bool had_error;
    vm_t *run_vm;

    function_t placeholder;

    ast_nodes_t run_required_uncompiled_funcdefs;

    struct {
        ast_node_t **items;
        size_t capacity;
        size_t count;
        arena_t *allocator;
    } pending_dependencies;

    arena_t *arena;
} analyzer_t;

bool resolve_ast(ast_t *ast);
function_t *find_main_or_null(ast_t *ast);

word_t ast_struct_item_get(ast_t *ast, type_t struct_type, string_view_t field_name, word_t struct_);
void ast_struct_item_set(ast_t *ast, type_t struct_type, string_view_t field_name, word_t *struct_, word_t value);
#endif
