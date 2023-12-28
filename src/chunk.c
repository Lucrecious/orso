#include "chunk.h"

#include <stdio.h>

#include "def.h"
#include "object.h"
#include "sb.h"

#ifdef DEBUG_TRACE_EXECUTION
u32 chunk_add_constant(Chunk* chunk, byte* data, u32 size, OrsoType* type)
#else
u32 chunk_add_constant(Chunk* chunk, byte* data, u32 size)
#endif
{
    u32 slot_size = orso_bytes_to_slots(size);
    u32 index = sb_count(chunk->constants);
    for (size_t i = 0; i < slot_size; i++) {
        sb_push(chunk->constants, (OrsoSlot){ .as.i = 0 });
#ifdef DEBUG_TRACE_EXECUTION
        sb_push(chunk->constant_types, &OrsoTypeInvalid);
#endif
    }

    memcpy(chunk->constants + index, data, size);

#ifdef DEBUG_TRACE_EXECUTION
    chunk->constant_types[index] = type;
#endif

    return index;
}

void chunk_init(Chunk* chunk) {
#ifdef DEBUG_TRACE_EXECUTION
    chunk->constant_types = NULL;
#endif
    chunk->constants = NULL;
    chunk->code = NULL;
    chunk->lines = NULL;
}

void chunk_free(Chunk* chunk) {
    sb_free(chunk->code);
    sb_free(chunk->lines);
    sb_free(chunk->constants);
#ifdef DEBUG_TRACE_EXECUTION
    sb_free(chunk->constant_types);
#endif
    chunk_init(chunk);
}

void chunk_write(Chunk* chunk, byte byte, i32 line) {
    sb_push(chunk->code, byte);

    i32 lines_count = sb_count(chunk->lines);
    if ((lines_count > 1 && chunk->lines[lines_count - 2] == line)) {
        chunk->lines[lines_count - 1]++;
    } else {
        sb_push(chunk->lines, line);
        sb_push(chunk->lines, 1);
    }
}

i32 chunk_get_line(Chunk* chunk, i32 offset) {
    for (i32 i = 0; i < sb_count(chunk->lines); i += 2) {
        if (offset < chunk->lines[i + 1]) {
            return chunk->lines[i];
        }

        offset -= chunk->lines[i + 1];
    }

    return -1;
}

void orso_print_slot(OrsoSlot* slot, OrsoType* type) {
    // TODO: This should not be necessary... This only fails on global values because
    // there's a time during runtime when they are uninitialized. Honestly though....
    // initial values for globals should already be in the global values list. So global defines
    // would not even be part of the byte code
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
    char* cstr = orso_slot_to_new_cstrn(slot, type);
    printf("%s", cstr);
    free(cstr);
}
