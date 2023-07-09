#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "instructions.h"
#include "opcodes.h"
#include "sb.h"
#include "type_set.h"

static i32 constant_instruction(const char* name, Chunk* chunk, i32 offset, bool is_short) {
    u32 index;

    if (is_short) {
        index = chunk->code[offset + 1];
    } else {
        index = ORSO_u8s_to_u24(chunk->code[offset + 1], chunk->code[offset + 2], chunk->code[offset + 3]);
    }
    printf("%-16s %4d => ", name, index);
    orso_print_slot(chunk->constants[index],
#ifdef DEBUG_TRACE_EXECUTION
        chunk->constants[index].type
#else
        &OrsoTypeUnresolved
#endif
    );
    printf("\n");

    return is_short ? offset + 2 : offset + 4;
}

static i32 pop_scope_instruction(const char* name, Chunk* chunk, i32 offset) {
    byte stack_pop_count = chunk->code[offset + 1];
    byte block_slot_count = chunk->code[offset + 2];
    printf("%-16s %d %d", name, stack_pop_count, block_slot_count);
    printf("\n");

    return offset + 3;
}

static i32 instruction_3arg(const char* name, Chunk* chunk, i32 offset) {
    u32 index = ORSO_u8s_to_u24(chunk->code[offset + 1], chunk->code[offset + 2], chunk->code[offset + 3]);
    printf("%-16s %d", name, index);
    printf("\n");

    return offset + 4;
}

static i32 instruction_arg(const char* name, Chunk* chunk, i32 offset) {
    byte index = chunk->code[offset + 1];
    printf("%-16s %d", name, index);
    printf("\n");

    return offset + 2;
}

static int jump_instruction(const char* name, int sign, Chunk* chunk, int offset) {
    u16 jump = ORSO_u8s_to_u16(chunk->code[offset + 1], chunk->code[offset + 2]);
    printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
    return offset + 3;
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

    OrsoOPCode instruction = chunk->code[offset];
    switch(instruction) {
        case ORSO_OP_POP: return simple_instruction("OP_POP", offset);
        case ORSO_OP_POP_SCOPE: return pop_scope_instruction("OP_POP_SCOPE", chunk, offset);
        case ORSO_OP_PUSH_0: return simple_instruction("OP_PUSH_0", offset);
        case ORSO_OP_PUSH_1: return simple_instruction("OP_PUSH_1", offset);
        case ORSO_OP_PUSH_NULL_UNION: return simple_instruction("OP_PUSH_NULL_UNION", offset);
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
        case ORSO_OP_EQUAL_SYMBOL: return simple_instruction("OP_EQUAL_SYMBOL", offset);
        case ORSO_OP_CONSTANT: return constant_instruction("OP_CONSTANT", chunk, offset, false);
        case ORSO_OP_CONSTANT_SHORT: return constant_instruction("OP_CONSTANT_SHORT", chunk, offset, true);
        case ORSO_OP_DEFINE_GLOBAL: return instruction_3arg("OP_DEFINE_GLOBAL", chunk, offset);
        case ORSO_OP_SET_GLOBAL: return instruction_3arg("OP_SET_GLOBAL", chunk, offset);
        case ORSO_OP_GET_GLOBAL: return instruction_3arg("OP_GET_GLOBAL", chunk, offset);
        case ORSO_OP_GET_GLOBAL_SHORT: return instruction_arg("OP_GET_GLOBAL_SHORT", chunk, offset);
        case ORSO_OP_DEFINE_GLOBAL_UNION: return instruction_3arg("OP_DEFINE_GLOBAL_UNION", chunk, offset);
        case ORSO_OP_GET_GLOBAL_UNION: return instruction_3arg("OP_GET_GLOBAL_UNION", chunk, offset);
        case ORSO_OP_SET_GLOBAL_UNION: return instruction_3arg("OP_SET_GLOBAL_UNION", chunk, offset);
        case ORSO_OP_GET_LOCAL: return instruction_3arg("OP_GET_LOCAL", chunk, offset);
        case ORSO_OP_GET_LOCAL_SHORT: return instruction_arg("OP_GET_LOCAL_SHORT", chunk, offset); 
        case ORSO_OP_GET_LOCAL_UNION: return instruction_3arg("OP_GET_LOCAL_UNION", chunk, offset);
        case ORSO_OP_SET_LOCAL: return instruction_3arg("OP_SET_LOCAL", chunk, offset);
        case ORSO_OP_SET_LOCAL_UNION: return instruction_3arg("OP_SET_LOCAL_UNION", chunk, offset);
        case ORSO_OP_JUMP_IF_FALSE: return jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
        case ORSO_OP_JUMP_IF_TRUE: return jump_instruction("OP_JUMP_IF_TRUE", 1, chunk, offset);
        case ORSO_OP_JUMP: return jump_instruction("OP_JUMP", 1, chunk, offset);
        case ORSO_OP_LOOP: return jump_instruction("OP_LOOP", -1, chunk, offset);
        case ORSO_OP_CALL: return simple_instruction("OP_CALL", offset + 2);
        case ORSO_OP_PUT_IN_UNION: return simple_instruction("OP_PUT_IN_UNION", offset);
        case ORSO_OP_NARROW_UNION: return simple_instruction("OP_NARROW_UNION", offset);
        case ORSO_OP_CONCAT_STRING: return simple_instruction("OP_CONCAT_STRING", offset);
        case ORSO_OP_RETURN: return simple_instruction("OP_RETURN", offset + 1);
        case ORSO_OP_PRINT_EXPR: return simple_instruction("OP_PRINT_EXPR", offset);
        case ORSO_OP_PRINT: return simple_instruction("OP_PRINT", offset);
    }
}

void chunk_disassemble(Chunk* chunk, const char* name) {
    printf("=== %s ===\n", name);

    for (i32 offset = 0; offset < sb_count(chunk->code);) {
        offset = disassemble_instruction(chunk, offset);
    }
}
