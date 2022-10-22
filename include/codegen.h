#ifndef CODEGEN_H_
#define CODEGEN_H_

#include "abstract_syntax_tree.h"
#include "chunk.h"

bool savine_generate_code(SavineAST* ast, Chunk* chunk);

#endif