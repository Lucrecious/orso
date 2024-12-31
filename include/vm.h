#ifndef VM_H_
#define VM_H_

#include "def.h"
#include "slot.h"
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

    OP_GTD_REG_REG,
    OP_GED_REG_REG,
    OP_LTD_REG_REG,
    OP_LED_REG_REG,
    OP_EQD_REG_REG,
    OP_NQD_REG_REG,

    OP_GTI_REG_REG,
    OP_GEI_REG_REG,
    OP_LTI_REG_REG,
    OP_LEI_REG_REG,
    OP_EQI_REG_REG,
    OP_NQI_REG_REG,

    OP_GTU_REG_REG,
    OP_GEU_REG_REG,
    OP_LTU_REG_REG,
    OP_LEU_REG_REG,
    OP_EQU_REG_REG,
    OP_NQU_REG_REG,

    OP_JMP_IF_REG_CONDITION,
    OP_JMP,

    OP_RETURN,
};

typedef u32 memaddr_t;

// todo: actually check if it his is 64bit
typedef struct instruction_t instruction_t;
struct instruction_t {
    union {
        byte _padding[7];

        struct {
            u32 forward;
            byte condition_reg;
            byte check_for;
        } jmp;

        struct {
            byte reg_result;
            byte reg_operand;
            u32 immediate;
        } binu_reg_immediate;

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
    byte op;
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
    size_t pc;
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

// #define VM_IMPLEMENTATION
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
    vm->call_frame.pc = 0;
}

void vm_step(vm_t *vm) {
#define IP_ADV(amount) (vm->call_frame.pc += amount)
#define MEMORY (vm->call_frame.function->memory)

    instruction_t in = vm->call_frame.function->code.items[vm->call_frame.pc];
    op_code_t op = (op_code_t)in.op;
    switch(op) {
        case OP_NOP: IP_ADV(1); break;

        case OP_JMP: {
            u32 jmp_amount = in.as.jmp.forward;
            IP_ADV(jmp_amount);
            break;
        }

        case OP_JMP_IF_REG_CONDITION: {
            byte reg = in.as.jmp.condition_reg;
            bool check_for = in.as.jmp.check_for;
            bool is_true = (check_for == vm->registers[reg].as.u);
            if (is_true) {
                u32 jmp_amount = in.as.jmp.forward;
                IP_ADV(jmp_amount);
            } else {
                IP_ADV(1);
            }
            break;
        }

        case OP_MOVU8_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            byte value = MEMORY->data[memaddr];
            vm->registers[reg].as.u = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVI32_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            i32 value = *((i32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.i = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVU32_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            u32 value = *((u32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.i = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVF32_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            f32 value = *((f32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.d = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVWORD_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            word_t value = *((word_t*)(MEMORY->data + memaddr));
            vm->registers[reg] = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVWORD_REG_TO_REGMEM: {
            byte reg = in.as.mov_reg_to_regmem.reg_source;
            word_t value = vm->registers[reg];

            byte regmem = in.as.mov_reg_to_regmem.regmem_destination;
            memaddr_t memaddr = (u32)vm->registers[regmem].as.u;

            memcpy(MEMORY->data + memaddr, &value, sizeof(word_t));

            IP_ADV(1);
            break;
        }

        case OP_MOVWORD_REGMEM_TO_REG: {
            byte regmem = in.as.mov_regmem_to_reg.regmem_source;
            memaddr_t memaddr = vm->registers[regmem].as.u;

            byte reg = in.as.mov_regmem_to_reg.reg_destination;

            memcpy(&vm->registers[reg], MEMORY->data + memaddr, sizeof(word_t));

            IP_ADV(1);
            break;
        }

        case OP_ADDU_REG_IM32: {
            byte s = in.as.binu_reg_immediate.reg_operand;
            u32 immediate = in.as.binu_reg_immediate.immediate;

            byte d = in.as.binu_reg_immediate.reg_result;

            vm->registers[d].as.u = vm->registers[s].as.u + immediate;

            IP_ADV(1);
            break;
        }

        case OP_SUBU_REG_IM32: {
            byte s = in.as.binu_reg_immediate.reg_operand;
            u32 immediate = in.as.binu_reg_immediate.immediate;

            byte d = in.as.binu_reg_immediate.reg_result;

            vm->registers[d].as.u = vm->registers[s].as.u - immediate;

            IP_ADV(1);
            break;
        }

        #define case_bin_reg_reg(name, v, op) case OP_##name##_REG_REG: {\
            byte a = in.as.bin_reg_to_reg.reg_op1; \
            byte b = in.as.bin_reg_to_reg.reg_op2; \
            byte c = in.as.bin_reg_to_reg.reg_result; \
            vm->registers[c].as.v = vm->registers[a].as.v op vm->registers[b].as.v;\
            IP_ADV(1); \
            break; \
        }\
        break

        case_bin_reg_reg(ADDI, i, +);
        case_bin_reg_reg(SUBI, i, -);
        case_bin_reg_reg(MULI, i, *);
        case_bin_reg_reg(DIVI, i, /);

        case_bin_reg_reg(ADDU, u, +);
        case_bin_reg_reg(SUBU, u, -);
        case_bin_reg_reg(MULU, u, *);
        case_bin_reg_reg(DIVU, u, /);

        case_bin_reg_reg(ADDD, d, +);
        case_bin_reg_reg(SUBD, d, -);
        case_bin_reg_reg(MULD, d, *);
        case_bin_reg_reg(DIVD, d, /);

        case_bin_reg_reg(GTI, i, >);
        case_bin_reg_reg(GEI, i, >=);
        case_bin_reg_reg(LTI, i, <);
        case_bin_reg_reg(LEI, i, <=);
        case_bin_reg_reg(EQI, i, ==);
        case_bin_reg_reg(NQI, i, !=);

        case_bin_reg_reg(GTU, u, >);
        case_bin_reg_reg(GEU, u, >=);
        case_bin_reg_reg(LTU, u, <);
        case_bin_reg_reg(LEU, u, <=);
        case_bin_reg_reg(EQU, u, ==);
        case_bin_reg_reg(NQU, u, !=);

        case_bin_reg_reg(GTD, d, >);
        case_bin_reg_reg(GED, d, >=);
        case_bin_reg_reg(LTD, d, <);
        case_bin_reg_reg(LED, d, <=);
        case_bin_reg_reg(EQD, d, ==);
        case_bin_reg_reg(NQD, d, !=);

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
