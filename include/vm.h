#ifndef VM_H_
#define VM_H_

#include "def.h"
#include "parser.h"

typedef enum op_type_t op_type_t;
enum op_code_t {
    OP_NOP,

    OP_READU8_MEM_TO_REG,
    OP_READI32_MEM_TO_REG,
    OP_READF32_MEM_TO_REG,
    OP_READWORD_MEM_TO_REG,
};

typedef u32 memaddr_t;

// 64bit code
typedef struct instruction_t instruction_t;
struct instruction_t {
    byte op;

    union {
        byte _padding[7];
        struct {
            byte reg_result;
            byte reg_operand;
            u32 uimmediate;
        } add_reg_immediate;

        struct {
            byte reg_result;
            memaddr_t memory_address;
        } read_memory_to_reg;
    } as;
};

#define REGISTER_COUNT 64
#define CALL_STACK_COUNT 1024

typedef struct function_t function_t;
struct function_t {
    memarr_t *memory;
    struct {
        instruction_t *items;
        size_t count;
        size_t capacity;
        arena_t *allocator;
    } code;
};

function_t *new_function(memarr_t *memory, arena_t *arena);

typedef struct call_frame_t call_frame_t;
struct call_frame_t {
    function_t *function;
    instruction_t *ip;
};

typedef struct vm_t vm_t;
struct vm_t {
    call_frame_t call_frame;
    word_t registers[REGISTER_COUNT];
    bool halted;
};


void vm_init(vm_t *vm);
void vm_step(vm_t *vm);

#endif

#ifdef VM_IMPLEMENTATION

function_t *new_function(memarr_t *memory, arena_t *arena) {
    function_t *function = arena_alloc(arena, sizeof(function_t));
    *function = (function_t){0};
    function->memory = memory;
    function->code.allocator = arena;
    return function;
}

void vm_init(vm_t *vm) {
    *vm = (vm_t){0};
    // vm->pc = 0;
}

void vm_step(vm_t *vm) {
    #define IP_ADV(amount) (vm->call_frame.ip += amount)
    #define MEMORY (vm->call_frame.function->memory)

    instruction_t instruction = *vm->call_frame.ip;
    switch(instruction.op) {
        case OP_NOP: IP_ADV(1); break;

        case OP_READU8_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            byte value = MEMORY->data[memaddr];
            vm->registers[reg].as.u = value;

            IP_ADV(1);
            break;
        }

        case OP_READI32_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            i32 value = *((i32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.i = value;

            IP_ADV(1);
            break;
        }

        case OP_READF32_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            f32 value = *((f32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.d = value;

            IP_ADV(1);
            break;
        }

        case OP_READWORD_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            word_t value = *((word_t*)(MEMORY->data + memaddr));
            vm->registers[reg] = value;

            IP_ADV(1);
            break;
        }
    }
}

#undef VM_IMPLEMENTATION
#endif
