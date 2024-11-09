#ifndef CHUNK_H_
#define CHUNK_H_

#include "def.h"
#include "type.h"
#include "slot.h"

typedef struct slots_t slots_t;
struct slots_t {
    slot_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct code_t code_t;
struct code_t {
    byte *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct i32s_t i32s_t;
struct i32s_t {
    i32 *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct chunk_t {
    type_ids_t constant_types;
    slots_t constants;
    i32s_t lines; // run-length encoded
    code_t code;
} chunk_t;

void chunk_init(chunk_t *chunk, arena_t *allocator);
void chunk_write(chunk_t *chunk, byte byte, i32 line);
i32 chunk_get_line(chunk_t *chunk, i32 offset);

u32 chunk_add_constant(chunk_t *chunk, byte* data, u32 size, type_id_t type_id);

void orso_print_slot(slot_t *slot, types_t *types, type_id_t type_id);

void chunk_free(chunk_t *chunk);

#endif
