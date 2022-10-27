#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_

#include "type.h"
#include "opcodes.h"

typedef struct OrsoInstruction {
    OrsoOPCode op_code;
    union {
        struct {
            i64 index;
#ifdef DEBUG_TRACE_EXECUTION
            OrsoType type;
#endif
        } constant;

        struct {
            OrsoType type;
            OrsoSlot string;
        } print_expr;
    };
} OrsoInstruction;

#endif