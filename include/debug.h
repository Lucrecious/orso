#ifndef DEBUG_H_
#define DEBUG_H_

#include "chunk.h"

void chunk_disassemble(chunk_t *chunk, const char *name);

i32 disassemble_instruction(chunk_t *chunk, i32 offset);

#endif
