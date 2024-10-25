#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "parser.h"
#include "chunk.h"
#include "type_set.h"
#include "virtual_machine.h"

typedef struct code_builder_t {
    vm_t *vm;
    ast_t *ast;
} code_builder_t;

void code_builder_init(code_builder_t *builder, vm_t *vm, ast_t *ast);
void code_builder_free(code_builder_t *builder);

void compile_function(vm_t *vm, ast_t *ast, function_t *function, ast_node_t *function_definition_expression);
function_t *generate_expression_function(code_builder_t *builder, ast_node_t *expression, bool is_folding_time, arena_t *allocator);
function_t *generate_code(vm_t *vm, ast_t *ast);

#endif
