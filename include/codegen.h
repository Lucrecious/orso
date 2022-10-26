#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "parser.h"
#include "chunk.h"

bool orso_generate_code(OrsoAST* ast, Chunk* chunk);

#endif