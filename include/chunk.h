#ifndef BYTE_CODE_H_
#define BYTE_CODE_H_

#include "def.h"
#include "opcodes.h"

typedef i32 Value;

void print_value(Value value);

typedef struct Chunk {
    Value* constants;
    i32* lines; // run-length encoded
    byte* code;
} Chunk;

void chunk_init(Chunk* chunk);
void chunk_write(Chunk* chunk, byte item, i32 line);
void chunk_write_constant(Chunk* chunk, Value value, i32 line);
i32 chunk_get_line(Chunk* chunk, i32 offset);
i32 chunk_add_constant(Chunk* chunk, Value value);

void chunk_free(Chunk* chunk);

#endif