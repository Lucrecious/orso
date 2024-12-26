#ifndef VM_H_
#define VM_H_

#include "def.h"
#include "parser.h"

typedef enum op_code_t op_code_t;
enum op_code_t {
    OP_NOP,

    OP_MOVU8_MEM_TO_REG,
    OP_MOVI32_MEM_TO_REG,
    OP_MOVU32_MEM_TO_REG,
    OP_MOVF32_MEM_TO_REG,
    OP_MOVWORD_MEM_TO_REG,

    OP_MOVWORD_REG_TO_REGMEM,
    OP_MOVWORD_REGMEM_TO_REG,

    OP_SUBU_REG_IM32,
    OP_ADDU_REG_IM32,

    OP_ADDI_REG_REG,
    OP_SUBI_REG_REG,
    OP_MULI_REG_REG,
    OP_DIVI_REG_REG,

    OP_ADDU_REG_REG,
    OP_SUBU_REG_REG,
    OP_MULU_REG_REG,
    OP_DIVU_REG_REG,

    OP_ADDD_REG_REG,
    OP_SUBD_REG_REG,
    OP_MULD_REG_REG,
    OP_DIVD_REG_REG,

    OP_RETURN,
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
            u32 immediate;
        } bin_regu_immediateu;

        struct {
            byte reg_result;
            byte reg_op1;
            byte reg_op2;
        } bin_reg_to_reg;

        struct {
            byte reg_result;
            memaddr_t mem_address;
        } mov_mem_to_reg;

        struct {
            byte reg_source;
            byte regmem_destination;
        } mov_reg_to_regmem;

        struct {
            byte regmem_source;
            byte reg_destination;
        } mov_regmem_to_reg;
    } as;
};

#define REGISTER_COUNT 64
#define CALL_STACK_COUNT 1024

typedef struct function_t function_t;
struct function_t {
    string_t file_path;
    struct {
        text_location_t *items;
        size_t count;
        size_t capacity;
        arena_t *allocator;
    } locations;

    memarr_t *memory;
    struct {
        instruction_t *items;
        size_t count;
        size_t capacity;
        arena_t *allocator;
    } code;
};

function_t *new_function(string_t file_path, memarr_t *memory, arena_t *arena);

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

void vm_set_entry_point(vm_t *vm, function_t *entry_point);
void vm_step(vm_t *vm);

#endif

#ifdef VM_IMPLEMENTATION

function_t *new_function(string_t file_path, memarr_t *memory, arena_t *arena) {
    function_t *function = arena_alloc(arena, sizeof(function_t));
    *function = (function_t){0};
    function->memory = memory;
    function->file_path = string_copy(file_path, arena);

    function->locations.allocator = arena;
    function->code.allocator = arena;
    return function;
}

void vm_init(vm_t *vm) {
    *vm = (vm_t){0};
    // vm->pc = 0;
}

void vm_set_entry_point(vm_t *vm, function_t *entry_point) {
    vm->halted = false;
    vm->call_frame.function = entry_point;
    vm->call_frame.ip = 0;
}

void vm_step(vm_t *vm) {
#define IP_ADV(amount) (vm->call_frame.ip += amount)
#define MEMORY (vm->call_frame.function->memory)

    instruction_t instruction = *vm->call_frame.ip;
    op_code_t op = (op_code_t)instruction.op;
    switch(op) {
        case OP_NOP: IP_ADV(1); break;

        case OP_MOVU8_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.mov_mem_to_reg.mem_address;
            byte reg = instruction.as.mov_mem_to_reg.reg_result;
            byte value = MEMORY->data[memaddr];
            vm->registers[reg].as.u = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVI32_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.mov_mem_to_reg.mem_address;
            byte reg = instruction.as.mov_mem_to_reg.reg_result;
            i32 value = *((i32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.i = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVU32_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.mov_mem_to_reg.mem_address;
            byte reg = instruction.as.mov_mem_to_reg.reg_result;
            u32 value = *((u32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.i = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVF32_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.mov_mem_to_reg.mem_address;
            byte reg = instruction.as.mov_mem_to_reg.reg_result;
            f32 value = *((f32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.d = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVWORD_MEM_TO_REG: {
            memaddr_t memaddr = instruction.as.mov_mem_to_reg.mem_address;
            byte reg = instruction.as.mov_mem_to_reg.reg_result;
            word_t value = *((word_t*)(MEMORY->data + memaddr));
            vm->registers[reg] = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVWORD_REG_TO_REGMEM: {
            byte reg = instruction.as.mov_reg_to_regmem.reg_source;
            word_t value = vm->registers[reg];

            byte regmem = instruction.as.mov_reg_to_regmem.regmem_destination;
            memaddr_t memaddr = (u32)vm->registers[regmem].as.u;

            memcpy(MEMORY->data + memaddr, &value, sizeof(word_t));

            IP_ADV(1);
            break;
        }

        case OP_MOVWORD_REGMEM_TO_REG: {
            byte regmem = instruction.as.mov_regmem_to_reg.regmem_source;
            memaddr_t memaddr = vm->registers[regmem].as.u;

            byte reg = instruction.as.mov_regmem_to_reg.reg_destination;

            memcpy(&vm->registers[reg], MEMORY->data + memaddr, sizeof(word_t));

            IP_ADV(1);
            break;
        }

        case OP_ADDU_REG_IM32: {
            byte s = instruction.as.bin_regu_immediateu.reg_operand;
            u32 immediate = instruction.as.bin_regu_immediateu.immediate;

            byte d = instruction.as.bin_regu_immediateu.reg_result;

            vm->registers[d].as.u = vm->registers[s].as.u + immediate;

            IP_ADV(1);
            break;
        }

        case OP_SUBU_REG_IM32: {
            byte s = instruction.as.bin_regu_immediateu.reg_operand;
            u32 immediate = instruction.as.bin_regu_immediateu.immediate;

            byte d = instruction.as.bin_regu_immediateu.reg_result;

            vm->registers[d].as.u = vm->registers[s].as.u - immediate;

            IP_ADV(1);
            break;
        }

        case OP_ADDI_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.i = vm->registers[a].as.i + vm->registers[b].as.i;

            IP_ADV(1);
            break;
        }

        case OP_SUBI_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.i = vm->registers[a].as.i - vm->registers[b].as.i;

            IP_ADV(1);
            break;
        }

        case OP_MULI_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.i = vm->registers[a].as.i * vm->registers[b].as.i;

            IP_ADV(1);
            break;
        }

        case OP_DIVI_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.i = vm->registers[a].as.i / vm->registers[b].as.i;

            IP_ADV(1);
            break;
        }

        case OP_ADDU_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.u = vm->registers[a].as.u + vm->registers[b].as.u;

            IP_ADV(1);
            break;
        }

        case OP_SUBU_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.u = vm->registers[a].as.u - vm->registers[b].as.u;

            IP_ADV(1);
            break;
        }

        case OP_MULU_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.u = vm->registers[a].as.u * vm->registers[b].as.u;

            IP_ADV(1);
            break;
        }

        case OP_DIVU_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.u = vm->registers[a].as.u / vm->registers[b].as.u;

            IP_ADV(1);
            break;
        }

        case OP_ADDD_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.d = vm->registers[a].as.d + vm->registers[b].as.d;

            IP_ADV(1);
            break;
        }

        case OP_SUBD_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.d = vm->registers[a].as.d - vm->registers[b].as.d;

            IP_ADV(1);
            break;
        }

        case OP_MULD_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.d = vm->registers[a].as.d * vm->registers[b].as.d;

            IP_ADV(1);
            break;
        }

        case OP_DIVD_REG_REG: {
            byte a = instruction.as.bin_reg_to_reg.reg_op1;
            byte b = instruction.as.bin_reg_to_reg.reg_op2;
            byte c = instruction.as.bin_reg_to_reg.reg_result;

            vm->registers[c].as.d = vm->registers[a].as.d / vm->registers[b].as.d;

            IP_ADV(1);
            break;
        }

        case OP_RETURN: {
            vm->halted = true;
            break;
        }
    }

#undef MEMORY
#undef IP_ADV
}

#undef VM_IMPLEMENTATION
#endif
