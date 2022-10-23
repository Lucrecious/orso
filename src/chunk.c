#include "chunk.h"

#include <stdio.h>

#include "def.h"
#include "sb.h"

i32 chunk_add_constant(Chunk* chunk, SavineValue value) {
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

void chunk_write(Chunk* chunk, byte item, i32 line) {
    sb_push(chunk->code, item);

    i32 lines_count = sb_count(chunk->lines);
    if ((lines_count > 1 && chunk->lines[lines_count - 2] == line) || line < 0) {
        chunk->lines[lines_count - 1]++;
    } else {
        sb_push(chunk->lines, line);
        sb_push(chunk->lines, 1);
    }
}

void chunk_write_constant(Chunk* chunk, SavineValue value, i32 line) {
    i32 index = chunk_add_constant(chunk, value);
    if (index > 0xFF) {
        chunk_write(chunk, OP_CONSTANT_LONG, line);
        chunk_write(chunk, (index >> 16) & 0xFF, line);
        chunk_write(chunk, (index >> 8) & 0xFF, line);
        chunk_write(chunk, index & 0xFF, line);
    } else {
        chunk_write(chunk, OP_CONSTANT, line);
        chunk_write(chunk, index, line);
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