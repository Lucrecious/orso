#ifndef DEBUG_H_
#define DEBUG_H_

#include "chunk.h"

void chunk_disassemble(Chunk* chunk, const char* name);

i32 disassemble_instruction(Chunk* chunk, i32 offset);

#endif
