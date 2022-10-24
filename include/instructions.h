#ifndef INSTRUCTIONS_H_
#define INSTRUCTIONS_H_

#include "opcodes.h"

typedef struct OrsoInstruction {
    OrsoOPCode op_code;
    union {
        i64 value;
    };
} OrsoInstruction;

#endif