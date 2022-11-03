#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "parser.h"
#include "chunk.h"
#include "virtual_machine.h"

bool orso_generate_code(OrsoVM* vm, OrsoAST* ast, Chunk* chunk);

#endif