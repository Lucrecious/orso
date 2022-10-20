#include "byte_code.h"

#include "def.h"
#include "sb.h"


void byte_code_init(ByteCode* byte_code) {
    byte_code->code = NULL;
}

void byte_code_add_instruction(ByteCode* byte_code, byte instruction) {
    sb_push(byte_code->code, instruction);
}

void byte_code_free(ByteCode* byte_code) {
    sb_free(byte_code->code);
}