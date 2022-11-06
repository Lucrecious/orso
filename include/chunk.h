#ifndef CHUNK_H_
#define CHUNK_H_

#include "def.h"
#include "object.h"
#include "opcodes.h"
#include "instructions.h"
#include "symbol_table.h"
#include "type.h"

typedef struct Chunk {
    u32 max_stack_size;
    OrsoSlot* constants;
    u32* constant_object_offsets;

    i32* lines; // run-length encoded
    OrsoInstruction* code;
} Chunk;

void chunk_init(Chunk* chunk);
void chunk_write(Chunk* chunk, const OrsoInstruction* instruction, i32 line);
i32 chunk_get_line(Chunk* chunk, i32 offset);
i32 chunk_add_constant(Chunk* chunk, OrsoSlot value, bool is_object);

void orso_print_slot(OrsoSlot slot, OrsoType type);

void chunk_free(Chunk* chunk);

#endif