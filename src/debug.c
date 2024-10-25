#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "instructions.h"
#include "opcodes.h"
#include "type_set.h"

typedef enum {
    II_GLOBAL,
    II_LOCAL,
    II_CONSTANT,
} IndexInstruction;

static i32 index_instruction(const char *name, chunk_t *chunk, i32 offset, u32 index_size_bytes, IndexInstruction instruction) {
    u64 index = 0;
    for (size_t i = 0; i < index_size_bytes; i++) {
        index <<= 8;
        index |= (byte)chunk->code.items[offset + 1 + i];
    }

    switch (instruction) {
        case II_LOCAL:
        case II_GLOBAL: {
            printf("%-16s %4llu\n", name, index);
            break;
        }

        case II_CONSTANT: {
            printf("%-16s %4llu => ", name, index);
            orso_print_slot(chunk->constants.items + (index / sizeof(slot_t)),
        #ifdef DEBUG
                chunk->constant_types.items[index / sizeof(slot_t)]
        #else
                &OrsoTypeUnresolved
        #endif
            );
            printf("\n");
        }
    }


    return offset + index_size_bytes + 2;
}

static i32 instruction_arg(const char *name, chunk_t *chunk, i32 offset) {
    byte index = chunk->code.items[offset + 1];
    printf("%-16s %d", name, index);
    printf("\n");

    return offset + 2;
}

static int jump_instruction(const char *name, int sign, chunk_t *chunk, int offset) {
    if (sign < 0) {
        u16 jump = u8s_to_u16(chunk->code.items[offset + 1], chunk->code.items[offset + 2]);
        printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
        return offset + 3;
    } else {
        op_jump_t *jump = (op_jump_t*)(chunk->code.items+offset);
        printf("%-16s %d -> %lu\n", name, offset, offset + sizeof(op_jump_t) + jump->offset);
        return offset + sizeof(op_jump_t);
    }
}

static i32 simple_instruction(const char *name, i32 offset) {
    printf("%s\n", name);
    return offset + 1;
}

i32 disassemble_instruction(chunk_t *chunk, i32 offset) {
    printf("%04d ", offset);


    if (offset > 0 && chunk_get_line(chunk, offset) == chunk_get_line(chunk, offset - 1)) {
        printf("   | ");
    } else {
        printf("%4d ", chunk_get_line(chunk, offset));
    }

    op_code_t instruction = chunk->code.items[offset];
    switch(instruction) {
        case OP_NO_OP: return simple_instruction("OP_NO_OP", offset);
        case OP_POP: return simple_instruction("OP_POP", offset);
        case OP_POPN: {
            op_popn_t *popn = (op_popn_t*)(chunk->code.items + offset);
            printf("OP_POPN n: %d\n", popn->n);
            return offset + sizeof(op_popn_t);
        };
        case OP_POP_SCOPE: {
            op_pop_scope_t *pop_scope = (op_pop_scope_t*)(chunk->code.items + offset) ;
            printf("OP_POP_SCOPE scope_size_slots: %d, value_size_slots: %d\n", pop_scope->scope_size_slots, pop_scope->value_size_slots);
            return offset + sizeof(op_pop_scope_t);
        }
        case OP_PUSH_0: return simple_instruction("OP_PUSH_0", offset);
        case OP_PUSH_1: return simple_instruction("OP_PUSH_1", offset);
        case OP_PUSH_LOCAL_ADDRESS: return index_instruction("OP_PUSH_LOCAL_ADDRESS", chunk, offset, 2, II_LOCAL) - 1;
        case OP_PUSH_GLOBAL_ADDRESS: return index_instruction("OP_PUSH_GLOBAL_ADDRESS", chunk, offset, 4, II_GLOBAL) - 1;
        case OP_NEGATE_I64: return simple_instruction("OP_NEGATE_I64", offset);
        case OP_NEGATE_F64: return simple_instruction("OP_NEGATE_F64", offset);
        case OP_ADD_I64: return simple_instruction("OP_ADD_I64", offset);
        case OP_ADD_F64: return simple_instruction("OP_ADD_F64", offset);
        case OP_ADD_PTR_I64: return simple_instruction("OP_ADD_PTR_I64", offset);
        case OP_SUBTRACT_I64: return simple_instruction("OP_SUBTRACT_I64", offset);
        case OP_SUBTRACT_F64: return simple_instruction("OP_SUBTRACT_F64", offset);
        case OP_MULTIPLY_I64: return simple_instruction("OP_MULTIPLY_I64", offset);
        case OP_MULTIPLY_F64: return simple_instruction("OP_MULTIPLY_F64", offset);
        case OP_DIVIDE_I64: return simple_instruction("OP_DIVIDE_I64", offset);
        case OP_DIVIDE_F64: return simple_instruction("OP_DIVIDE_F64", offset);
        case OP_I64_TO_F64: return simple_instruction("OP_I64_TO_F64", offset);
        case OP_F64_TO_I64: return simple_instruction("OP_F64_TO_I64", offset);
        case OP_LOGICAL_NOT: return simple_instruction("OP_LOGICAL_NOT", offset);
        case OP_EQUAL_I64: return simple_instruction("OP_EQUAL_I64", offset);
        case OP_EQUAL_F64: return simple_instruction("OP_EQUAL_F64", offset);
        case OP_LESS_I64: return simple_instruction("OP_LESS_I64", offset);
        case OP_LESS_F64: return simple_instruction("OP_LESS_F64", offset);
        case OP_GREATER_I64: return simple_instruction("OP_GREATER_I64", offset);
        case OP_GREATER_F64: return simple_instruction("OP_GREATER_F64", offset);
        case OP_EQUAL_STRING: return simple_instruction("OP_EQUAL_STRING", offset);
        case OP_EQUAL_SYMBOL: return simple_instruction("OP_EQUAL_SYMBOL", offset);
        case OP_CONSTANT: {
            op_location_t *location = (op_location_t*)(chunk->code.items + offset);
            printf("OP_CONSTANT index: %d, size: %d\n", location->index_slots, location->size_bytes);
            return offset + sizeof(op_location_t);
        }
        case OP_LOCAL:  {
            op_location_t *location = (op_location_t*)(chunk->code.items + offset);
            printf("OP_LOCAL(index: %d, size: %d)\n", location->index_slots, location->size_bytes);
            return offset + sizeof(op_location_t);
        }
        case OP_GLOBAL: {
            op_location_t *location = (op_location_t*)(chunk->code.items + offset);
            printf("OP_GLOBAL(index: %d, size: %d)\n", location->index_slots, location->size_bytes);
            return offset + sizeof(op_location_t);
        }
        case OP_FIELD: {
            op_field_t *field = (op_field_t*)(chunk->code.items + offset);
            printf("OP_FIELD(value size: %d, offset: %d, size: %d)\n", field->value_size_bytes, field->offset_bytes, field->size_bytes);
            return offset + sizeof(op_location_t);
        }
        case OP_SET_LVALUE_SLOT: return simple_instruction("OP_SET_LVALUE_SLOT", offset);
        case OP_SET_LVALUE_I32: return simple_instruction  ("OP_SET_LVALUE_I32", offset);
        case OP_SET_LVALUE_F32: return simple_instruction("OP_SET_LVALUE_F32", offset);
        case OP_SET_LVALUE_BOOL: return simple_instruction("OP_SET_LVALUE_BOOL", offset);
        case OP_SET_LVALUE_BYTES: return instruction_arg("OP_SET_LVALUE", chunk, offset);
        case OP_JUMP_IF_UNION_FALSE: return jump_instruction("OP_JUMP_IF_UNION_FALSE", 1, chunk, offset);
        case OP_JUMP_IF_UNION_TRUE: return jump_instruction("OP_JUMP_IF_UNION_TRUE", 1, chunk, offset);
        case OP_JUMP_IF_FALSE: return jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
        case OP_JUMP_IF_TRUE: return jump_instruction("OP_JUMP_IF_TRUE", 1, chunk, offset);
        case OP_JUMP: return jump_instruction("OP_JUMP", 1, chunk, offset);
        case OP_LOOP: return jump_instruction("OP_LOOP", -1, chunk, offset);
        case OP_CALL: return simple_instruction("OP_CALL", offset + 2);
        case OP_PUT_IN_UNION: return instruction_arg("OP_PUT_IN_UNION", chunk, offset);
        case OP_NARROW_UNION: return simple_instruction("OP_NARROW_UNION", offset);
        case OP_CONCAT_STRING: return simple_instruction("OP_CONCAT_STRING", offset);
        case OP_RETURN: return simple_instruction("OP_RETURN", offset + 1);
        case OP_PRINT_EXPR: return simple_instruction("OP_PRINT_EXPR", offset);
        case OP_PRINT: return simple_instruction("OP_PRINT", offset);

#ifdef DEBUG
        case OP_PUSH_TYPE:
        case OP_POP_TYPE_N: {
            return simple_instruction("OP_DEBUG", offset);
        }
#endif
    }
}

void chunk_disassemble(chunk_t *chunk, const char *name) {
    printf("=== %s ===\n", name);

    for (size_t offset = 0; offset < chunk->code.count;) {
        offset = disassemble_instruction(chunk, offset);
    }
}
