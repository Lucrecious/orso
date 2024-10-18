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

typedef struct types_t types_t;
struct types_t {
    type_t **items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct chunk_t {
#ifdef DEBUG
    types_t constant_types;
#endif
    slots_t constants;
    i32 *lines; // run-length encoded
    byte *code;
} chunk_t;

void chunk_init(chunk_t *chunk, arena_t *allocator);
void chunk_write(chunk_t *chunk, byte byte, i32 line);
i32 chunk_get_line(chunk_t *chunk, i32 offset);

#ifdef DEBUG
#define CHUNK_ADD_CONSTANT(chunk, data, size, type) chunk_add_constant(chunk, data, size, type)
u32 chunk_add_constant(chunk_t *chunk, byte* data, u32 size, type_t *type);
#else
#define CHUNK_ADD_CONSTANT(chunk, data, size, type) chunk_add_constant(chunk, data, size)
u32 chunk_add_constant(chunk_t *chunk, byte *data, u32 size);
#endif

void orso_print_slot(slot_t *slot, type_t *type);

void chunk_free(chunk_t *chunk);

#endif
