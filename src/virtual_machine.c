#include "virtual_machine.h"

#include <stdio.h>

#include "debug.h"
#include "sb.h"

void savine_vm_init(SavineVM* vm) {
    vm->chunk = NULL;
}

void savine_vm_free(SavineVM* vm) {
}

static InterpretResult run(SavineVM* vm) {
#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm->chunk->constants[((u32)READ_BYTE() << 16) | ((u16)READ_BYTE() << 8) | READ_BYTE()])

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        disassemble_instruction(vm->chunk, (i32)(vm->ip - vm->chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                print_value(constant);
                printf("\n");
                break;
            }
            case OP_CONSTANT_LONG: {
                Value constant = READ_CONSTANT_LONG();
                print_value(constant);
                printf("\n");
                break;
            }
            case OP_RETURN: {
                return SAVINE_INTERPRET_OK;
            }
        }
    }

#undef READ_CONSTANT_LONG
#undef READ_CONSTANT
#undef READBYTE
}

InterpretResult savine_interpret(SavineVM* vm, Chunk* chunk) {
    vm->chunk = chunk;
    vm->ip = vm->chunk->code;

    return run(vm);
}

