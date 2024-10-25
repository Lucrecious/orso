#ifndef OPCODES_H_
#define OPCODES_H_

#include "def.h"

typedef enum op_code_t {
    OP_NO_OP,

    OP_POP,
    OP_POPN,
    OP_POP_SCOPE,

    OP_I64_TO_F64,
    OP_F64_TO_I64,

    OP_ADD_I64,
    OP_SUBTRACT_I64,
    OP_MULTIPLY_I64,
    OP_DIVIDE_I64,

    OP_ADD_F64,
    OP_SUBTRACT_F64,
    OP_MULTIPLY_F64,
    OP_DIVIDE_F64,

    OP_ADD_PTR_I64,

    OP_NEGATE_I64,
    OP_NEGATE_F64,

    OP_EQUAL_I64,
    OP_LESS_I64,
    OP_GREATER_I64,

    OP_EQUAL_F64,
    OP_LESS_F64,
    OP_GREATER_F64,

    OP_EQUAL_SYMBOL,

    OP_EQUAL_STRING,
    OP_CONCAT_STRING,

    OP_LOGICAL_NOT,

    OP_PUSH_1,
    OP_PUSH_0,

    OP_CONSTANT,
    OP_LOCAL,
    OP_GLOBAL,
    OP_FIELD,

    OP_PUSH_GLOBAL_ADDRESS,
    OP_PUSH_LOCAL_ADDRESS,

    OP_SET_LVALUE_BOOL,
    OP_SET_LVALUE_I32,
    OP_SET_LVALUE_F32,
    OP_SET_LVALUE_SLOT,
    OP_SET_LVALUE_BYTES,

    // union stack manip
    OP_PUT_IN_UNION,
    OP_NARROW_UNION,

    // jumps
    OP_JUMP_IF_FALSE,
    OP_JUMP_IF_TRUE,
    OP_JUMP_IF_UNION_FALSE,
    OP_JUMP_IF_UNION_TRUE,
    OP_JUMP,
    OP_LOOP,

    // call
    OP_CALL,
    OP_RETURN,

    // builtin
    OP_PRINT_EXPR,
    OP_PRINT,
} op_code_t;

typedef struct op_code_field_t op_code_field_t;
struct op_code_field_t {
    op_code_t op;
    u16 value_size_bytes;
    u16 offset_bytes;
    u16 size_bytes;
};

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
