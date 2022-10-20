#include <stdio.h>

#include "byte_code.h"
#include "debug.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    ByteCode byte_code;

    byte_code_init(&byte_code);

    byte_code_add_instruction(&byte_code, OP_RETURN);

    byte_code_disassemble(&byte_code, "my code");

    byte_code_free(&byte_code);

    return 0;
}
