#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "sb.h"

static i32 constant_instruction(const char* name, Chunk* chunk, i32 offset) {
    OrsoInstruction constant = chunk->code[offset];
    printf("%-16s %4d => ", name, constant.constant.index);
    orso_print_slot(chunk->constants[constant.constant.index],
#ifdef DEBUG_TRACE_EXECUTION
        chunk->constants[constant.constant.index].type
#else
        ORSO_TYPE_MAX
#endif
    );
    printf("\n");

    return offset + 1;
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

    OrsoOPCode instruction = chunk->code[offset].op_code;
    switch(instruction) {
        case ORSO_OP_POP: return simple_instruction("OP_POP", offset);
        case ORSO_OP_POP_PTR: return simple_instruction("OP_POP_PTR", offset);
        case ORSO_OP_PUSH_0: return simple_instruction("OP_PUSH_0", offset);
        case ORSO_OP_PUSH_1: return simple_instruction("OP_PUSH_1", offset);
        case ORSO_OP_NEGATE_I64: return simple_instruction("OP_NEGATE_I64", offset);
        case ORSO_OP_NEGATE_F64: return simple_instruction("OP_NEGATE_F64", offset);
        case ORSO_OP_ADD_I64: return simple_instruction("OP_ADD_I64", offset);
        case ORSO_OP_ADD_F64: return simple_instruction("OP_ADD_F64", offset);
        case ORSO_OP_SUBTRACT_I64: return simple_instruction("OP_SUBTRACT_I64", offset);
        case ORSO_OP_SUBTRACT_F64: return simple_instruction("OP_SUBTRACT_F64", offset);
        case ORSO_OP_MULTIPLY_I64: return simple_instruction("OP_MULTIPLY_I64", offset);
        case ORSO_OP_MULTIPLY_F64: return simple_instruction("OP_MULTIPLY_F64", offset);
        case ORSO_OP_DIVIDE_I64: return simple_instruction("OP_DIVIDE_I64", offset);
        case ORSO_OP_DIVIDE_F64: return simple_instruction("OP_DIVIDE_F64", offset);
        case ORSO_OP_I64_TO_F64: return simple_instruction("OP_I64_TO_F64", offset);
        case ORSO_OP_F64_TO_I64: return simple_instruction("OP_F64_TO_I64", offset);
        case ORSO_OP_LOGICAL_NOT: return simple_instruction("OP_LOGICAL_NOT", offset);
        case ORSO_OP_EQUAL_I64: return simple_instruction("OP_EQUAL_I64", offset);
        case ORSO_OP_EQUAL_F64: return simple_instruction("OP_EQUAL_F64", offset);
        case ORSO_OP_LESS_I64: return simple_instruction("OP_LESS_I64", offset);
        case ORSO_OP_LESS_F64: return simple_instruction("OP_LESS_F64", offset);
        case ORSO_OP_GREATER_I64: return simple_instruction("OP_GREATER_I64", offset);
        case ORSO_OP_GREATER_F64: return simple_instruction("OP_GREATER_F64", offset);
        case ORSO_OP_EQUAL_STRING: return simple_instruction("OP_EQUAL_STRING", offset);
        case ORSO_OP_CONSTANT: return constant_instruction("OP_CONSTANT", chunk, offset);
        case ORSO_OP_CONSTANT_PTR: return constant_instruction("OP_CONSTANT_PTR", chunk, offset);
        case ORSO_OP_DEFINE_GLOBAL: return constant_instruction("OP_DEFINE_GLOBAL", chunk, offset);
        case ORSO_OP_DEFINE_GLOBAL_PTR: return constant_instruction("OP_DEFINE_GLOBAL_PTR", chunk, offset);
        case ORSO_OP_GET_GLOBAL: return simple_instruction("OP_GET_GLOBAL", offset);
        case ORSO_OP_GET_GLOBAL_PTR: return simple_instruction("OP_GET_GLOBAL_PTR", offset);
        case ORSO_OP_SET_GLOBAL: return simple_instruction("OP_SET_GLOBAL", offset);
        case ORSO_OP_CONCAT_STRING: return simple_instruction("OP_CONCAT_STRING", offset);
        case ORSO_OP_RETURN: return simple_instruction("OP_RETURN", offset);
        case ORSO_OP_PRINT_EXPR: return simple_instruction("OP_PRINT_EXPR", offset);
        default: return simple_instruction("OP_UNKNOWN", offset);
    }
}

void chunk_disassemble(Chunk* chunk, const char* name) {
    printf("=== %s ===\n", name);

    for (i32 offset = 0; offset < sb_count(chunk->code);) {
        offset = disassemble_instruction(chunk, offset);
    }
}