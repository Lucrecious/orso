#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "parser.h"
#include "chunk.h"
#include "type_set.h"
#include "virtual_machine.h"

typedef struct OrsoCodeBuilder {
    vm_t* vm;
    ast_t* ast;
} OrsoCodeBuilder;

void orso_code_builder_init(OrsoCodeBuilder* builder, vm_t* vm, ast_t* ast);
void orso_code_builder_free(OrsoCodeBuilder* builder);

void orso_compile_function(vm_t* vm, ast_t* ast, function_t* function, ast_node_t* function_definition_expression);
function_t* orso_generate_expression_function(OrsoCodeBuilder* builder, ast_node_t* expression, bool is_folding_time);
function_t* orso_generate_code(vm_t* vm, ast_t* ast);

#endif
