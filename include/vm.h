#ifndef VM_H_
#define VM_H_

typedef enum op_type_t op_type_t;
enum op_code_t {
    OP_NOP,

    OP_READU8_MEM_TO_REG,
    OP_READI32_MEM_TO_REG,
    OP_READF32_MEM_TO_REG,
    OP_READWORD_MEM_TO_REG,
};

typedef u32 memaddr_t;

// 64bit instructions
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

typedef struct vm_t vm_t;
struct vm_t {
    arena_t allocator;

    size_t pc;
    byte *memory;
    instruction_t *program;
    word_t registers[REGISTER_COUNT];

    bool halted;
};

void vm_init(vm_t *vm, size_t instruction_count, size_t memory_size);
void vm_step(vm_t *vm);

#endif

#ifdef VM_IMPLEMENTATION

void vm_init(vm_t *vm, size_t instruction_count, size_t memory_size) {
    vm->program = arena_alloc(&vm->allocator, instruction_count*sizeof(instruction_t));
    vm->memory = arena_alloc(&vm->allocator, memory_size);

    vm->halted = false;
    vm->pc = 0;
}

void vm_step(vm_t *vm) {
    instruction_t instruction = vm->program[vm->pc];
    switch(instruction.op) {
        case OP_NOP: ++vm->pc; break;

        case OP_READU8_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            byte value = vm->memory[memaddr];
            vm->registers[reg].as.u = value;

            ++vm->pc;
            break;
        }

        case OP_READI32_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            i32 value = *((i32*)(vm->memory + memaddr));
            vm->registers[reg].as.i = value;

            ++vm->pc;
            break;
        }

        case OP_READF32_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            f32 value = *((f32*)(vm->memory + memaddr));
            vm->registers[reg].as.d = value;

            ++vm->pc;
            break;
        }

        case OP_READWORD_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.read_memory_to_reg.memory_address;
            byte reg = instruction.as.read_memory_to_reg.reg_result;
            word_t value = *((word_t*)(vm->memory+memaddr));
            vm->registers[reg] = value;

            ++vm->pc;
            break;
        }
    }
}

#endif
