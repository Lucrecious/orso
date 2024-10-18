#include "chunk.h"

#include <stdio.h>

#include "def.h"
#include "object.h"
#include "tmp.h"

#ifdef DEBUG
u32 chunk_add_constant(chunk_t* chunk, byte* data, u32 size, type_t* type)
#else
u32 chunk_add_constant(chunk_t* chunk, byte* data, u32 size)
#endif
{
    u32 slot_size = orso_bytes_to_slots(size);
    u32 index = chunk->constants.count;
    for (size_t i = 0; i < slot_size; i++) {
        array_push(&chunk->constants, (slot_t){ .as.i = 0 });
#ifdef DEBUG
        array_push(&chunk->constant_types, &OrsoTypeInvalid);
#endif
    }

    memcpy(chunk->constants.items + index, data, size);

#ifdef DEBUG
    chunk->constant_types.items[index] = type;
#endif

    return index;
}

void chunk_init(chunk_t *chunk, arena_t *allocator) {
#ifdef DEBUG
    chunk->constant_types = (types_t){.allocator = allocator};
#endif
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

void orso_print_slot(slot_t *slot, type_t *type) {
    switch (type->kind) {
        case ORSO_TYPE_INVALID:
        case ORSO_TYPE_UNDEFINED:
        case ORSO_TYPE_UNRESOLVED:
        case ORSO_TYPE_VOID:
        case ORSO_TYPE_BOOL:
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64:
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64:
        case ORSO_TYPE_TYPE:
        case ORSO_TYPE_STRUCT: break;

        case ORSO_TYPE_STRING:
        case ORSO_TYPE_SYMBOL:
        case ORSO_TYPE_FUNCTION:
        case ORSO_TYPE_NATIVE_FUNCTION:
        case ORSO_TYPE_POINTER:
        case ORSO_TYPE_UNION: {
            if (slot->as.i == 0) {
                slot = NULL;
                type = &OrsoTypeUndefined;
            }
            break;
        }
    }
    
    tmp_arena_t *tmp = allocator_borrow(); {
        string_t s = slot_to_string(slot, type, tmp->allocator);
        printf("%s", s.cstr);
    } allocator_return(tmp);
}
