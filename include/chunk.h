#ifndef CHUNK_H_
#define CHUNK_H_

#include "def.h"
#include "type.h"
#include "slot.h"

typedef struct Chunk {
    OrsoSlot* constants;
    u32* constant_object_offsets;

    i32* lines; // run-length encoded
    byte* code;
} Chunk;

void chunk_init(Chunk* chunk);
void chunk_write(Chunk* chunk, byte byte, i32 line);
i32 chunk_get_line(Chunk* chunk, i32 offset);
u32 chunk_add_constant(Chunk* chunk, OrsoSlot value, bool is_object);

void orso_print_slot(OrsoSlot slot, OrsoType* type);

void chunk_free(Chunk* chunk);

#endif
