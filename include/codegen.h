#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "parser.h"
#include "chunk.h"
#include "type_set.h"
#include "virtual_machine.h"

typedef struct OrsoCodeBuilder {
    OrsoVM* vm;
    OrsoAST* ast;
} OrsoCodeBuilder;

void orso_code_builder_init(OrsoCodeBuilder* builder, OrsoVM* vm, OrsoAST* ast);
void orso_code_builder_free(OrsoCodeBuilder* builder);

OrsoFunction* orso_generate_expression_function(OrsoCodeBuilder* builder, OrsoExpressionNode* expression, bool folded_constants_only);
OrsoFunction* orso_generate_code(OrsoVM* vm, OrsoAST* ast);

#endif
