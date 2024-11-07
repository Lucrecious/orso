#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "instructions.h"
#include "opcodes.h"
#include "tmp.h"
#include "type_set.h"

static i32 instruction(const char *name, i32 offset) {
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

    op_code_t op_code = chunk->code.items[offset];
    switch(op_code) {
        case OP_NO_OP: return instruction("OP_NO_OP", offset);
        case OP_POP: return instruction("OP_POP", offset);
        case OP_POPN: {
            op_popn_t *popn = (op_popn_t*)(chunk->code.items + offset);
            printf("OP_POPN(n: %d)\n", popn->n);
            return offset + sizeof(op_popn_t);
        };
        case OP_POP_SCOPE: {
            op_pop_scope_t *pop_scope = (op_pop_scope_t*)(chunk->code.items + offset) ;
            printf("OP_POP_SCOPE(scope_size_slots: %d, value_size_slots: %d)\n", pop_scope->scope_size_slots, pop_scope->value_size_slots);
            return offset + sizeof(op_pop_scope_t);
        }
        case OP_PUSH_0: return instruction("OP_PUSH_0", offset);
        case OP_PUSH_1: return instruction("OP_PUSH_1", offset);
        case OP_PUSH_LOCAL_ADDRESS: {
            op_push_address_t *push_address = (op_push_address_t*)(chunk->code.items + offset);
            printf("OP_PUSH_LOCAL_ADDRESS(index: %d)\n", push_address->index);
            return offset + sizeof(op_push_address_t);
        }

        case OP_PUSH_GLOBAL_ADDRESS: {
            op_push_address_t *push_address = (op_push_address_t*)(chunk->code.items + offset);
            printf("OP_PUSH_GLOBAL_ADDRESS(index: %d)\n", push_address->index);
            return offset + sizeof(op_push_address_t);
        }
        case OP_NEGATE_I64: return instruction("OP_NEGATE_I64", offset);
        case OP_NEGATE_F64: return instruction("OP_NEGATE_F64", offset);
        case OP_ADD_I64: return instruction("OP_ADD_I64", offset);
        case OP_ADD_F64: return instruction("OP_ADD_F64", offset);
        case OP_ADD_PTR_I64: return instruction("OP_ADD_PTR_I64", offset);
        case OP_SUBTRACT_I64: return instruction("OP_SUBTRACT_I64", offset);
        case OP_SUBTRACT_F64: return instruction("OP_SUBTRACT_F64", offset);
        case OP_MULTIPLY_I64: return instruction("OP_MULTIPLY_I64", offset);
        case OP_MULTIPLY_F64: return instruction("OP_MULTIPLY_F64", offset);
        case OP_DIVIDE_I64: return instruction("OP_DIVIDE_I64", offset);
        case OP_DIVIDE_F64: return instruction("OP_DIVIDE_F64", offset);
        case OP_I64_TO_F64: return instruction("OP_I64_TO_F64", offset);
        case OP_F64_TO_I64: return instruction("OP_F64_TO_I64", offset);
        case OP_LOGICAL_NOT: return instruction("OP_LOGICAL_NOT", offset);
        case OP_EQUAL_I64: return instruction("OP_EQUAL_I64", offset);
        case OP_EQUAL_F64: return instruction("OP_EQUAL_F64", offset);
        case OP_LESS_I64: return instruction("OP_LESS_I64", offset);
        case OP_LESS_F64: return instruction("OP_LESS_F64", offset);
        case OP_GREATER_I64: return instruction("OP_GREATER_I64", offset);
        case OP_GREATER_F64: return instruction("OP_GREATER_F64", offset);
        case OP_EQUAL_STRING: return instruction("OP_EQUAL_STRING", offset);
        case OP_EQUAL_SYMBOL: return instruction("OP_EQUAL_SYMBOL", offset);
        case OP_CONSTANT: {
            op_location_t *location = (op_location_t*)(chunk->code.items + offset);
            printf("OP_CONSTANT(index: %d, size: %d)\n", location->index_slots, location->size_bytes);
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
        case OP_FIELD_BYTE: {
            op_field_t *field = (op_field_t*)(chunk->code.items + offset);
            printf("OP_FIELD_I32(value size: %d, offset: %d, size: %d)\n", field->value_size_bytes, field->offset_bytes, field->size_bytes);
            return offset + sizeof(op_field_t);
        }

        case OP_FIELD_I32: {
            op_field_t *field = (op_field_t*)(chunk->code.items + offset);
            printf("OP_FIELD_I32(value size: %d, offset: %d, size: %d)\n", field->value_size_bytes, field->offset_bytes, field->size_bytes);
            return offset + sizeof(op_field_t);
        }

        case OP_FIELD_F32: {
            op_field_t *field = (op_field_t*)(chunk->code.items + offset);
            printf("OP_FIELD_F32(value size: %d, offset: %d, size: %d)\n", field->value_size_bytes, field->offset_bytes, field->size_bytes);
            return offset + sizeof(op_field_t);
        }

        case OP_FIELD_SLOT: { 
            op_field_t *field = (op_field_t*)(chunk->code.items + offset);
            printf("OP_FIELD_SLOT(value size: %d, offset: %d, size: %d)\n", field->value_size_bytes, field->offset_bytes, field->size_bytes);
            return offset + sizeof(op_field_t);
        }

        case OP_FIELD_BYTES: {
            op_field_t *field = (op_field_t*)(chunk->code.items + offset);
            printf("OP_FIELD_BYTES(value size: %d, offset: %d, size: %d)\n", field->value_size_bytes, field->offset_bytes, field->size_bytes);
            return offset + sizeof(op_field_t);
        }

        case OP_SET_LVALUE_SLOT: {
            // op_set_lvalue_t *set_lvalue = (op_set_lvalue_t*)(chunk->code.items + offset);
            printf("OP_SET_LVALUE_SLOT\n");
            return offset + sizeof(op_set_lvalue_t);
        }

        case OP_SET_LVALUE_I32: {
            // op_set_lvalue_t *set_lvalue = (op_set_lvalue_t*)(chunk->code.items + offset);
            printf("OP_SET_LVALUE_I32\n");
            return offset + sizeof(op_set_lvalue_t);
        }
        case OP_SET_LVALUE_F32: {
            // op_set_lvalue_t *set_lvalue = (op_set_lvalue_t*)(chunk->code.items + offset);
            printf("OP_SET_LVALUE_F32\n");
            return offset + sizeof(op_set_lvalue_t);
        }
        case OP_SET_LVALUE_BOOL: {
            // op_set_lvalue_t *set_lvalue = (op_set_lvalue_t*)(chunk->code.items + offset);
            printf("OP_SET_LVALUE_BOOL\n");
            return offset + sizeof(op_set_lvalue_t);
        }
        case OP_SET_LVALUE_BYTES: {
            op_set_lvalue_t *set_lvalue = (op_set_lvalue_t*)(chunk->code.items + offset);
            printf("OP_SET_LVALUE_BYTES(size_bytes: %d)\n", set_lvalue->size_bytes);
            return offset + sizeof(op_set_lvalue_t);
        }
        case OP_JUMP_IF_UNION_FALSE: {
            op_jump_t *jump = (op_jump_t*)(chunk->code.items + offset);
            printf("OP_JUMP_IF_UNION_FALSE(offset: %d)\n", jump->offset);
            return offset + sizeof(op_jump_t);
        }
        case OP_JUMP_IF_UNION_TRUE: {
            op_jump_t *jump = (op_jump_t*)(chunk->code.items + offset);
            printf("OP_JUMP_IF_UNION_TRUE(offset: %d)\n", jump->offset);
            return offset + sizeof(op_jump_t);
        }
        case OP_JUMP_IF_FALSE: {
            op_jump_t *jump = (op_jump_t*)(chunk->code.items + offset);
            printf("OP_JUMP_IF_FALSE(offset: %d)\n", jump->offset);
            return offset + sizeof(op_jump_t);
        }
        case OP_JUMP_IF_TRUE: {
            op_jump_t *jump = (op_jump_t*)(chunk->code.items + offset);
            printf("OP_JUMP_IF_TRUE(offset: %d)\n", jump->offset);
            return offset + sizeof(op_jump_t);
        }
        case OP_JUMP: {
            op_jump_t *jump = (op_jump_t*)(chunk->code.items + offset);
            printf("OP_JUMP(offset: %d)\n", jump->offset);
            return offset + sizeof(op_jump_t);
        }
        case OP_LOOP: {
            op_loop_t *loop = (op_loop_t*)(chunk->code.items + offset);
            printf("OP_LOOP(offset: %d)\n", loop->offset);
            return offset + sizeof(op_jump_t);
        }
        case OP_CALL: {
            op_call_t *loop = (op_call_t*)(chunk->code.items + offset);
            printf("OP_CALL(argument_slots: %d)\n", loop->argument_slots);
            return offset + sizeof(op_jump_t);
        }
        case OP_PUT_IN_UNION: {
            op_put_in_union_t *put_in_union = (op_put_in_union_t*)(chunk->code.items + offset);
            printf("OP_PUT_IN_UNION(size_bytes: %d)\n", put_in_union->size_bytes);
            return offset + sizeof(op_put_in_union_t);
        }
        case OP_NARROW_UNION: {
            op_narrow_union_t *narrow_union = (op_narrow_union_t*)(chunk->code.items + offset);
            printf("OP_NARROW_UNION(offset_bytes: %d)\n", narrow_union->offset_bytes);
            return offset + sizeof(op_narrow_union_t);
        }
        case OP_CONCAT_STRING: return instruction("OP_CONCAT_STRING", offset);
        case OP_RETURN: {
            op_return_t *return_ = (op_return_t*)(chunk->code.items + offset);
            printf("OP_RETURN(size_bytes: %d)\n", return_->size_slots);
            return offset + sizeof(op_return_t);
        }
        case OP_PRINT_EXPR:
        case OP_PRINT: return instruction("OP_PRINT", offset);

        case OP_PUSH_TYPE: {
            op_push_pop_type_t *push_pop = (op_push_pop_type_t*)(chunk->code.items + offset);
            tmp_arena_t *tmp = allocator_borrow(); {
                printf("OP_PUSH_TYPE(type: %s)\n", type_to_string(push_pop->data.type, tmp->allocator).cstr);
            } allocator_return(tmp);
            return offset + sizeof(op_push_pop_type_t);
        }

        case OP_POP_TYPE_N: {
            op_push_pop_type_t *push_pop = (op_push_pop_type_t*)(chunk->code.items + offset);
            printf("OP_POP_TYPE_N(n: %llu)\n", push_pop->data.n);
            return offset + sizeof(op_push_pop_type_t);
        }
    }
}

void chunk_disassemble(chunk_t *chunk, const char *name) {
    printf("=== %s ===\n", name);

    for (size_t offset = 0; offset < chunk->code.count;) {
        offset = disassemble_instruction(chunk, offset);
    }
}
