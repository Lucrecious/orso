#ifndef BYTE_CODE_H_
#define BYTE_CODE_H_

#include "def.h"
#include "value.h"
#include "opcodes.h"
#include "instructions.h"
#include "type.h"

typedef union OrsoSlot {
    i64 i;
    f64 f;
    ptr p;
} OrsoSlot;

typedef struct Chunk {
    u32 max_stack_size;
    OrsoSlot* constants;
    i32* lines; // run-length encoded
    OrsoInstruction* code;
} Chunk;

void chunk_init(Chunk* chunk);
void chunk_write(Chunk* chunk, const OrsoInstruction* instruction, i32 line);
void chunk_write_constant(Chunk* chunk, OrsoSlot value, i32 line);
i32 chunk_get_line(Chunk* chunk, i32 offset);
i32 chunk_add_constant(Chunk* chunk, OrsoSlot value);

void orso_print_slot(OrsoSlot slot, OrsoType type);

void chunk_free(Chunk* chunk);

#endif