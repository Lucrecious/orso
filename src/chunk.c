#include "chunk.h"

#include <stdio.h>

#include "def.h"
#include "object.h"
#include "tmp.h"

u32 chunk_add_constant(chunk_t* chunk, byte* data, u32 size, type_t type) {
    u32 slot_size = bytes_to_slots(size);
    u32 index = chunk->constants.count;
    for (size_t i = 0; i < slot_size; i++) {
        array_push(&chunk->constants, (slot_t){ .as.i = 0 });
        array_push(&chunk->constant_types, typeid(TYPE_INVALID));
    }

    memcpy(chunk->constants.items + index, data, size);

    chunk->constant_types.items[index] = type;

    return index;
}

void chunk_init(chunk_t *chunk, arena_t *allocator) {
    chunk->constant_types = (types_t){.allocator = allocator};
    chunk->constants = (slots_t){.allocator = allocator};
    chunk->code = (code_t){.allocator = allocator};
    chunk->lines = (i32s_t){.allocator = allocator};
}

void chunk_free(chunk_t* chunk) {
    (void)chunk;
}

void chunk_write(chunk_t* chunk, byte byte, i32 line) {
    array_push(&chunk->code, byte);

    i32 lines_count = chunk->lines.count;
    if ((lines_count > 1 && chunk->lines.items[lines_count - 2] == line)) {
        chunk->lines.items[lines_count - 1]++;
    } else {
        array_push(&chunk->lines, line);
        array_push(&chunk->lines, 1);
    }
}

i32 chunk_get_line(chunk_t* chunk, i32 offset) {
    for (size_t i = 0; i < chunk->lines.count; i += 2) {
        if (offset < chunk->lines.items[i + 1]) {
            return chunk->lines.items[i];
        }

        offset -= chunk->lines.items[i + 1];
    }

    return -1;
}