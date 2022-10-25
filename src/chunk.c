#include "chunk.h"

#include <stdio.h>

#include "def.h"
#include "sb.h"

i32 chunk_add_constant(Chunk* chunk, OrsoSlot value) {
    i32 index = sb_count(chunk->constants);
    sb_push(chunk->constants, value);
    return index;
}

void chunk_init(Chunk* chunk) {
    chunk->max_stack_size = 0;
    chunk->constants = NULL;
    chunk->code = NULL;
    chunk->lines = NULL;
}

void chunk_free(Chunk* chunk) {
    sb_free(chunk->code);
    sb_free(chunk->lines);
    sb_free(chunk->constants);
}

void chunk_write(Chunk* chunk, const OrsoInstruction* instruction, i32 line) {
    sb_push(chunk->code, (*instruction));

    i32 lines_count = sb_count(chunk->lines);
    if ((lines_count > 1 && (chunk->lines[lines_count - 2] == line || line < 0))) {
        chunk->lines[lines_count - 1]++;
    } else {
        sb_push(chunk->lines, line);
        sb_push(chunk->lines, 1);
    }
}

void chunk_write_constant(Chunk* chunk, OrsoSlot value, i32 line) {
    i32 index = chunk_add_constant(chunk, value);
    const OrsoInstruction instruction = { .op_code = ORSO_OP_CONSTANT, .value = index };
    chunk_write(chunk, &instruction, line);
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

void orso_print_slot(OrsoSlot slot, OrsoType type) {
    switch (type) {
        case ORSO_TYPE_BOOL: if (slot.i) { printf("true"); } else { printf("false"); } break;
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: printf("%d", slot.i); break;
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: printf("%f", slot.f); break;
        case ORSO_TYPE_NULL: printf("null"); break;
        case ORSO_TYPE_STRING: printf("\"%s\"", ((OrsoString*)slot.p)->text); break;
        default: printf("i64(%lld), f64(%.2f), ptr(%x)", slot.i, slot.f, slot.p);
    }
    
}
