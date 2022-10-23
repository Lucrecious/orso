#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "abstract_syntax_tree.h"
#include "chunk.h"

bool orso_generate_code(OrsoAST* ast, Chunk* chunk);

#endif