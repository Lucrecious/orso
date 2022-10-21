#include "virtual_machine.h"

#include <stdio.h>

#include "debug.h"
#include "sb.h"

void savine_vm_init(SavineVM* vm) {
    vm->chunk = NULL;
    vm->stack = NULL;
    vm->stack_top = NULL;
}

void savine_vm_free(SavineVM* vm) {
}

static InterpretResult run(SavineVM* vm) {
#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm->chunk->constants[((u32)READ_BYTE() << 16) | ((u16)READ_BYTE() << 8) | READ_BYTE()])
#define BINARY_OP(op) \
    do { \
        i32 b = savine_pop(vm); \
        *(vm->stack_top - 1) = *(vm->stack_top - 1) op b; \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* slot = vm->stack; slot < vm->stack_top; slot++) {
            printf("[");
            print_value(*slot);
            printf("]");
        }
        printf("\n");
        disassemble_instruction(vm->chunk, (i32)(vm->ip - vm->chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                savine_push(vm, constant);
                break;
            }
            case OP_CONSTANT_LONG: {
                Value constant = READ_CONSTANT_LONG();
                savine_push(vm, constant);
                break;
            }
            case OP_NEGATE: {
                *(vm->stack_top - 1) = -(*(vm->stack_top - 1));
                break;
            }
            case OP_ADD: BINARY_OP(+); break;
            case OP_SUBTRACT: BINARY_OP(-); break;
            case OP_MULTIPLY: BINARY_OP(*); break;
            case OP_DIVIDE: BINARY_OP(/); break;
            case OP_RETURN: {
                print_value(savine_pop(vm));
                printf("\n");
                return SAVINE_INTERPRET_OK;
            }
        }
    }

#undef BINARY_OP
#undef READ_CONSTANT_LONG
#undef READ_CONSTANT
#undef READBYTE
}

InterpretResult savine_interpret(SavineVM* vm, Chunk* chunk) {
    vm->chunk = chunk;
    vm->ip = vm->chunk->code;

    vm->stack = ALLOCATE_N(Value, chunk->max_stack_size);
    vm->stack_top = vm->stack;

    InterpretResult result = run(vm);

    free(vm->stack);
    vm->stack_top = NULL;

    return result;
}

void savine_push(SavineVM* vm, Value value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

Value savine_pop(SavineVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

