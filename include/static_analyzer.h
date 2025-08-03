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
function_t *find_main_or_null(ast_node_t *module);
orstring_t ast_generate_moduleid(orstring_t file_path, arena_t *arena);

orstring_t ast_orstr2str(type_table_t *type_set, void *start);
orword_t ast_struct_item_get(typedatas_t *types, ortype_t struct_type, oristring_t field_name, void *struct_);
void ast_struct_item_set(ast_t *ast, ortype_t struct_type, oristring_t field_name, orword_t *struct_, orword_t value);
ast_node_t *stan_load_module_or_errornull(analyzer_t *analyzer, ast_t *ast, ast_node_t *arg_ref, string_view_t module_path);
#endif
