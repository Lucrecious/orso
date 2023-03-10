#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "sb.h"

static i32 constant_instruction(const char* name, Chunk* chunk, i32 offset) {
    u32 index = ORSO_u8s_to_u24(chunk->code[offset + 1], chunk->code[offset + 2], chunk->code[offset + 3]);
    printf("%-16s %4d => ", name, index);
    orso_print_slot(chunk->constants[index],
#ifdef DEBUG_TRACE_EXECUTION
        chunk->constants[index].type.one
#else
        ORSO_TYPE_UNRESOLVED
#endif
    );
    printf("\n");

    return offset + 4;
}

static i32 instruction_3arg(const char* name, Chunk* chunk, i32 offset) {
    u32 index = ORSO_u8s_to_u24(chunk->code[offset + 1], chunk->code[offset + 2], chunk->code[offset + 3]);
    printf("%-16s %d", name, index);
    printf("\n");

    return offset + 4;
}

static i32 put_in_union_instruction(Chunk* chunk, i32 offset) {
    OrsoTypeKind type_kind = ORSO_u8s_to_TypeKind(chunk->code[offset + 1], chunk->code[offset + 2]);
    printf("%-16s %s\n", "OP_PUT_IN_UNION", orso_type_kind_to_cstr(type_kind));
    return offset + 3;
}

static i32 print_expr_instruction(Chunk* chunk, i32 offset) {
#define BYTE(N) chunk->code[offset + N]

    OrsoType type = ORSO_TYPE_ONE(ORSO_u8s_to_u64(BYTE(1), BYTE(2), BYTE(3), BYTE(4), BYTE(5), BYTE(6), BYTE(7), BYTE(8)));

    const char type_string[126];
    orso_type_to_cstr(type, (char*)type_string);

    printf("%-16s <%s>\n", "OP_PRINT_EXPR", type_string);

    return offset + 9;

#undef BYTE
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
        case ORSO_OP_POP_TOP_OBJECT: return simple_instruction("OP_POP_TOP_OBJECT", offset);
        case ORSO_OP_PUSH_TOP_OBJECT: return simple_instruction("OP_PUSH_TOP_OBJECT", offset);
        case ORSO_OP_PUSH_TOP_OBJECT_NULL: return simple_instruction("OP_PUSH_TOP_OBJECT_NULL", offset);
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
        case ORSO_OP_CONSTANT: return constant_instruction("OP_CONSTANT", chunk, offset);
        case ORSO_OP_DEFINE_GLOBAL: return instruction_3arg("OP_DEFINE_GLOBAL", chunk, offset);
        case ORSO_OP_GET_GLOBAL: return instruction_3arg("OP_GET_GLOBAL", chunk, offset);
        case ORSO_OP_SET_GLOBAL: return instruction_3arg("OP_SET_GLOBAL", chunk, offset);
        case ORSO_OP_SET_GLOBAL_GC_TYPE: return instruction_3arg("OP_SET_GLOBAL_GC_TYPE", chunk, offset);
        case ORSO_OP_DEFINE_GLOBAL_UNION: return instruction_3arg("OP_DEFINE_GLOBAL_UNION", chunk, offset);
        case ORSO_OP_GET_GLOBAL_UNION: return instruction_3arg("OP_GET_GLOBAL_UNION", chunk, offset);
        case ORSO_OP_SET_GLOBAL_UNION: return instruction_3arg("OP_SET_GLOBAL_UNION", chunk, offset);
        case ORSO_OP_GET_LOCAL: return instruction_3arg("OP_GET_LOCAL", chunk, offset);
        case ORSO_OP_GET_LOCAL_UNION: return instruction_3arg("OP_GET_LOCAL_UNION", chunk, offset);
        case ORSO_OP_SET_LOCAL: return instruction_3arg("OP_SET_LOCAL", chunk, offset);
        case ORSO_OP_SET_LOCAL_UNION: return instruction_3arg("OP_SET_LOCAL_UNION", chunk, offset);
        case ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE: return instruction_3arg("OP_UPDATE_GLOBAL_UNION_GC_TYPE", chunk, offset);
        case ORSO_OP_UPDATE_LOCAL_UNION_GC_TYPE: return instruction_3arg("OP_UPDATE_LOCAL_UNION_GC_TYPE", chunk, offset);
        case ORSO_OP_PUT_IN_UNION: return put_in_union_instruction(chunk, offset);
        case ORSO_OP_NARROW_UNION: return simple_instruction("OP_NARROW_UNION", offset);
        case ORSO_OP_CONCAT_STRING: return simple_instruction("OP_CONCAT_STRING", offset);
        case ORSO_OP_RETURN: return simple_instruction("OP_RETURN", offset);
        case ORSO_OP_PRINT_EXPR: return print_expr_instruction(chunk, offset);
        default: return simple_instruction("OP_UNKNOWN", offset);
    }
}

void chunk_disassemble(Chunk* chunk, const char* name) {
    printf("=== %s ===\n", name);

    for (i32 offset = 0; offset < sb_count(chunk->code);) {
        offset = disassemble_instruction(chunk, offset);
    }
}
