#ifndef OPCODES_H_
#define OPCODES_H_

#include "def.h"

typedef enum OrsoOPCode {
    ORSO_OP_NO_OP,

    ORSO_OP_POP,
    ORSO_OP_POP_SCOPE,

    ORSO_OP_I64_TO_F64,
    ORSO_OP_F64_TO_I64,

    ORSO_OP_ADD_I64, ORSO_OP_SUBTRACT_I64, ORSO_OP_MULTIPLY_I64, ORSO_OP_DIVIDE_I64,
    ORSO_OP_ADD_F64, ORSO_OP_SUBTRACT_F64, ORSO_OP_MULTIPLY_F64, ORSO_OP_DIVIDE_F64,

    ORSO_OP_NEGATE_I64, ORSO_OP_NEGATE_F64,

    ORSO_OP_EQUAL_I64, ORSO_OP_LESS_I64, ORSO_OP_GREATER_I64,
    ORSO_OP_EQUAL_F64, ORSO_OP_LESS_F64, ORSO_OP_GREATER_F64,

    ORSO_OP_EQUAL_SYMBOL,
    ORSO_OP_EQUAL_STRING, ORSO_OP_CONCAT_STRING,

    ORSO_OP_LOGICAL_NOT,

    ORSO_OP_PUSH_1,
    ORSO_OP_PUSH_0,
    ORSO_OP_PUSH_NULL_UNION,

    ORSO_OP_CONSTANT_SHORT,
    ORSO_OP_CONSTANT,

    ORSO_OP_SET_LOCAL,
    ORSO_OP_GET_LOCAL,
    ORSO_OP_GET_LOCAL_SHORT,

    ORSO_OP_SET_LOCAL_UNION,
    ORSO_OP_GET_LOCAL_UNION,
    
    ORSO_OP_DEFINE_GLOBAL,
    ORSO_OP_SET_GLOBAL,
    ORSO_OP_GET_GLOBAL,
    ORSO_OP_GET_GLOBAL_SHORT,

    ORSO_OP_DEFINE_GLOBAL_UNION,
    ORSO_OP_GET_GLOBAL_UNION,
    ORSO_OP_SET_GLOBAL_UNION,

    ORSO_OP_PUT_IN_UNION,
    ORSO_OP_NARROW_UNION,

    ORSO_OP_JUMP_IF_FALSE,
    ORSO_OP_JUMP_IF_TRUE,
    ORSO_OP_JUMP_IF_UNION_FALSE,
    ORSO_OP_JUMP_IF_UNION_TRUE,
    ORSO_OP_JUMP,
    ORSO_OP_LOOP,

    ORSO_OP_CALL,

    ORSO_OP_PRINT_EXPR,
    ORSO_OP_PRINT,

    ORSO_OP_RETURN,
} OrsoOPCode;

#endif
