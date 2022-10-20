#include "debug.h"

#include <stdio.h>

#include "def.h"
#include "sb.h"

static i32 simple_instruction(const char* name, i32 offset) {
    printf("%s\n", name);
    return offset + 1;
}

static i32 disassemble_instruction(ByteCode* byte_code, i32 offset) {
    printf("%04d ", offset);

    byte instruction = byte_code->code[offset];
    switch(instruction) {
        case OP_RETURN:
            return simple_instruction("OP_RETURN", offset);
    }
}

void byte_code_disassemble(ByteCode* byte_code, const char* name) {
    printf("=== %s ===\n", name);

    for (i32 offset = 0; offset < sb_count(byte_code->code);) {
        offset = disassemble_instruction(byte_code, offset);
    }
}