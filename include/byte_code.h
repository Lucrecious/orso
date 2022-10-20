#ifndef BYTE_CODE_H_
#define BYTE_CODE_H_

#include "def.h"
#include "opcodes.h"

typedef struct ByteCode {
    byte* code;
} ByteCode;

void byte_code_init(ByteCode* byte_code);

void byte_code_add_instruction(ByteCode* byte_code, byte instruction);

void byte_code_free(ByteCode* byte_code);

#endif