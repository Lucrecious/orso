#ifndef DEBUG_H_
#define DEBUG_H_

#include "chunk.h"

void chunk_disassemble(types_t *types, chunk_t *chunk, const char *name);

i32 disassemble_instruction(types_t *types, chunk_t *chunk, i32 offset);

#endif
