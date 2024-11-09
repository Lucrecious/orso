#ifndef STATIC_ANALYZER_H_
#define STATIC_ANALYZER_H_

#include "parser.h"
#include "common.h"
#include "type_set.h"
#include "virtual_machine.h"
#include "arena.h"

struct interpreter_t;

typedef struct analysis_dependency_t {
    i32 fold_level;
    ast_node_t* ast_node; 
} analysis_dependency_t;

typedef struct analysis_dependencies_t analysis_dependencies_t;
struct analysis_dependencies_t {
    analysis_dependency_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct analyzer_t {
    error_function_t error_fn;
    symbol_table_t symbols;
    vm_t vm;

    ast_t* ast;

    analysis_dependencies_t dependencies;

    bool had_error;

    arena_t allocator;
} analyzer_t;

void analyzer_init(analyzer_t *analyzer, write_function_t write_fn, error_function_t error_fn);
void analyzer_free(analyzer_t *analyzer);

bool resolve_ast(analyzer_t *analyzer, ast_t *ast);

#endif
