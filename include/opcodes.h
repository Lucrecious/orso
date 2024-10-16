#ifndef OPCODES_H_
#define OPCODES_H_

#include "def.h"

typedef enum op_code_t {
    ORSO_OP_NO_OP,

    ORSO_OP_POP,
    ORSO_OP_POPN,
    ORSO_OP_POP_SCOPE,

    ORSO_OP_I64_TO_F64,
    ORSO_OP_F64_TO_I64,

    ORSO_OP_ADD_I64, ORSO_OP_SUBTRACT_I64, ORSO_OP_MULTIPLY_I64, ORSO_OP_DIVIDE_I64,
    ORSO_OP_ADD_F64, ORSO_OP_SUBTRACT_F64, ORSO_OP_MULTIPLY_F64, ORSO_OP_DIVIDE_F64,
    ORSO_OP_ADD_PTR_I64,

    ORSO_OP_NEGATE_I64, ORSO_OP_NEGATE_F64,

    ORSO_OP_EQUAL_I64, ORSO_OP_LESS_I64, ORSO_OP_GREATER_I64,
    ORSO_OP_EQUAL_F64, ORSO_OP_LESS_F64, ORSO_OP_GREATER_F64,

    ORSO_OP_EQUAL_SYMBOL,
    ORSO_OP_EQUAL_STRING, ORSO_OP_CONCAT_STRING,

    ORSO_OP_LOGICAL_NOT,

    ORSO_OP_PUSH_1,
    ORSO_OP_PUSH_0,

    ORSO_OP_CONSTANT,

    ORSO_OP_LOCAL,
    ORSO_OP_GET_GLOBAL_8BIT_ADDRESS,
    ORSO_OP_GET_GLOBAL_16BIT_ADDRESS,
    ORSO_OP_GET_GLOBAL_32BIT_ADDRESS,

    ORSO_OP_GET_FIELD_VOID,
    ORSO_OP_GET_FIELD_BOOL,
    ORSO_OP_GET_FIELD_I32,
    ORSO_OP_GET_FIELD_F32,
    ORSO_OP_GET_FIELD_SLOT,
    ORSO_OP_GET_FIELD_BYTES,

    ORSO_OP_PUSH_GLOBAL_ADDRESS,
    ORSO_OP_PUSH_LOCAL_ADDRESS,

    ORSO_OP_SET_LVALUE_BOOL,
    ORSO_OP_SET_LVALUE_I32,
    ORSO_OP_SET_LVALUE_F32,
    ORSO_OP_SET_LVALUE_SLOT,
    ORSO_OP_SET_LVALUE_BYTES,

    // union stack manip
    ORSO_OP_PUT_IN_UNION,
    ORSO_OP_NARROW_UNION,

    // jumps
    ORSO_OP_JUMP_IF_FALSE,
    ORSO_OP_JUMP_IF_TRUE,
    ORSO_OP_JUMP_IF_UNION_FALSE,
    ORSO_OP_JUMP_IF_UNION_TRUE,
    ORSO_OP_JUMP,
    ORSO_OP_LOOP,

    // call
    ORSO_OP_CALL,
    ORSO_OP_RETURN,

    // builtin
    ORSO_OP_PRINT_EXPR,
    ORSO_OP_PRINT,
} op_code_t;

typedef struct op_code_location_t op_code_location_t;
struct op_code_location_t {
    op_code_t op;
    u32 index_slots;
    u16 size_bytes;
};

typedef struct op_code_popn_t op_code_popn_t;
struct op_code_popn_t {
    op_code_t op;
    byte n;
};

typedef struct op_code_pop_scope_t op_code_pop_scope_t;
struct op_code_pop_scope_t {
    op_code_t op;
    byte scope_size_slots;
    byte value_size_slots;
};

typedef struct op_code_jump_t op_code_jump_t;
struct op_code_jump_t {
    op_code_t op;
    u32 offset;
};

#endif
