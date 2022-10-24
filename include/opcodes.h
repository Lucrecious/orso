#ifndef OPCODES_H_
#define OPCODES_H_

#include "def.h"

typedef enum OrsoOPCode {
    ORSO_OP_PUSH_I64,

    ORSO_OP_I64_TO_F64,
    ORSO_OP_F64_TO_I64,

    ORSO_OP_ADD_I64, ORSO_OP_SUBTRACT_I64, ORSO_OP_MULTIPLY_I64, ORSO_OP_DIVIDE_I64,
    ORSO_OP_ADD_F64, ORSO_OP_SUBTRACT_F64, ORSO_OP_MULTIPLY_F64, ORSO_OP_DIVIDE_F64,

    ORSO_OP_NEGATE_I64, ORSO_OP_NEGATE_F64,

    ORSO_OP_EQUAL_I64, ORSO_OP_LESS_I64, ORSO_OP_GREATER_I64,
    ORSO_OP_EQUAL_F64, ORSO_OP_LESS_F64, ORSO_OP_GREATER_F64,

    ORSO_OP_EQUAL_STRING,

    ORSO_OP_LOGICAL_NOT,

    ORSO_OP_PUSH_1,
    ORSO_OP_PUSH_0,

    ORSO_OP_CONSTANT,

    ORSO_OP_RETURN,
} OrsoOPCode;

#endif