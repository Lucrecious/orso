#ifndef CHUNK_H_
#define CHUNK_H_

#include "def.h"
#include "type.h"
#include "slot.h"

typedef struct Chunk {
#ifdef DEBUG_TRACE_EXECUTION
    OrsoType** constant_types;
#endif
    OrsoSlot* constants;
    i32* lines; // run-length encoded
    byte* code;
} Chunk;

void chunk_init(Chunk* chunk);
void chunk_write(Chunk* chunk, byte byte, i32 line);
i32 chunk_get_line(Chunk* chunk, i32 offset);

#ifdef DEBUG_TRACE_EXECUTION
#define CHUNK_ADD_CONSTANT(chunk, data, size, type) chunk_add_constant(chunk, data, size, type)
u32 chunk_add_constant(Chunk* chunk, byte* data, u32 size, OrsoType* type);
#else
#define CHUNK_ADD_CONSTANT(chunk, data, size, type) chunk_add_constant(chunk, data, size)
u32 chunk_add_constant(Chunk* chunk, byte* data, u32 size);
#endif

void orso_print_slot(OrsoSlot* slot, OrsoType* type);

void chunk_free(Chunk* chunk);

#endif
