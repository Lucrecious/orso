#include "chunk.h"

#include <stdio.h>

#include "def.h"
#include "object.h"
#include "sb.h"

u32 chunk_add_constant(Chunk* chunk, OrsoSlot value) {
    u32 index = sb_count(chunk->constants);
    sb_push(chunk->constants, value);
    return index;
}

void chunk_init(Chunk* chunk) {
    chunk->constants = NULL;
    chunk->code = NULL;
    chunk->lines = NULL;
}

void chunk_free(Chunk* chunk) {
    sb_free(chunk->code);
    sb_free(chunk->lines);
    sb_free(chunk->constants);
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

void orso_print_slot(OrsoSlot slot, OrsoType* type) {
    char* cstr = orso_slot_to_new_cstrn(slot, type);
    printf("%s", cstr);
    free(cstr);
}
