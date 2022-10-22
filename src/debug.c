#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "sb.h"

static i32 constant_instruction(const char* name, Chunk* chunk, i32 offset) {
    byte constant = chunk->code[offset + 1];
    printf("%-16s %4d ", name, constant);
    print_value(chunk->constants[constant]);
    printf("\n");

    return offset + 2;
}

static i32 constant_long_instruction(const char* name, Chunk* chunk, i32 offset) {
    i32 constant = chunk->code[offset + 1];
    constant <<= 8;
    constant |= chunk->code[offset + 2];
    constant <<= 8;
    constant |= chunk->code[offset + 3];
    printf("%-16s %d ", name, constant);
    print_value(chunk->constants[constant]);
    printf("\n");

    return offset + 4;
}

static i32 simple_instruction(const char* name, i32 offset) {
    printf("%s\n", name);
    return offset + 1;
}

i32 disassemble_instruction(Chunk* chunk, i32 offset) {
    printf("%04d ", offset);


    if (offset > 0 && chunk_get_line(chunk, offset) == chunk_get_line(chunk, offset - 1)) {
        printf("   | ");
    } else {
        printf("%4d ", chunk_get_line(chunk, offset));
    }

    byte instruction = chunk->code[offset];
    switch(instruction) {
        case OP_CONSTANT: return constant_instruction("OP_CONSTANT", chunk, offset);
        case OP_CONSTANT_LONG: return constant_long_instruction("OP_CONSTANT_LONG", chunk, offset);
        case OP_NEGATE_INT: return simple_instruction("OP_NEGATE_INT", offset);
        case OP_NEGATE_DOUBLE: return simple_instruction("OP_NEGATE_DOUBLE", offset);
        case OP_ADD_INT: return simple_instruction("OP_ADD_INT", offset);
        case OP_ADD_DOUBLE: return simple_instruction("OP_ADD_DOUBLE", offset);
        case OP_SUBTRACT_INT: return simple_instruction("OP_SUBTRACT_INT", offset);
        case OP_SUBTRACT_DOUBLE: return simple_instruction("OP_SUBTRACT_DOUBLE", offset);
        case OP_MULTIPLY_INT: return simple_instruction("OP_MULTIPLY_INT", offset);
        case OP_MULTIPLY_DOUBLE: return simple_instruction("OP_MULTIPLY_DOUBLE", offset);
        case OP_DIVIDE_INT: return simple_instruction("OP_DIVIDE_INT", offset);
        case OP_DIVIDE_DOUBLE: return simple_instruction("OP_DIVIDE_DOUBLE", offset);
        case OP_INT_TO_DOUBLE: return simple_instruction("OP_INT_TO_DOUBLE", offset);
        case OP_DOUBLE_TO_INT: return simple_instruction("OP_DOUBLE_TO_INT", offset);
        case OP_RETURN: return simple_instruction("OP_RETURN", offset);
        default: return simple_instruction("OP_UNKNOWN", offset);
    }
}

void chunk_disassemble(Chunk* chunk, const char* name) {
    printf("=== %s ===\n", name);

    for (i32 offset = 0; offset < sb_count(chunk->code);) {
        offset = disassemble_instruction(chunk, offset);
    }
}