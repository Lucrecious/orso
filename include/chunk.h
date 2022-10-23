#ifndef BYTE_CODE_H_
#define BYTE_CODE_H_

#include "def.h"
#include "value.h"
#include "opcodes.h"

typedef struct Chunk {
    u32 max_stack_size;
    OrsoValue* constants;
    i32* lines; // run-length encoded
    byte* code;
} Chunk;

void chunk_init(Chunk* chunk);
void chunk_write(Chunk* chunk, byte item, i32 line);
void chunk_write_constant(Chunk* chunk, OrsoValue value, i32 line);
i32 chunk_get_line(Chunk* chunk, i32 offset);
i32 chunk_add_constant(Chunk* chunk, OrsoValue value);

void chunk_free(Chunk* chunk);

#endif