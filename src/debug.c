#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "sb.h"

static i32 constant_instruction(const char* name, Chunk* chunk, i32 offset) {
    byte constant = chunk->code[offset + 1];
    printf("%-16s %4d ", name, constant);
    print_value(chunk->constants.values[constant]);
    printf("\n");

    return offset + 2;
}

static i32 simple_instruction(const char* name, i32 offset) {
    printf("%s\n", name);
    return offset + 1;
}

static i32 disassemble_instruction(Chunk* chunk, i32 offset) {
    printf("%04d ", offset);


    if (offset > 0 && chunk_get_line(chunk, offset) == chunk_get_line(chunk, offset - 1)) {
        printf("   | ");
    } else {
        printf("%4d ", chunk_get_line(chunk, offset));
    }

    byte instruction = chunk->code[offset];
    switch(instruction) {
        case OP_CONSTANT:
            return constant_instruction("OP_CONSTANT", chunk, offset);
        case OP_RETURN:
            return simple_instruction("OP_RETURN", offset);
    }
}

void chunk_disassemble(Chunk* chunk, const char* name) {
    printf("=== %s ===\n", name);

    for (i32 offset = 0; offset < sb_count(chunk->code);) {
        offset = disassemble_instruction(chunk, offset);
    }
}