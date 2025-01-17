#ifndef VM_H_
#define VM_H_

#include "def.h"
#include "slot.h"
#include "parser.h"

typedef void (*write_function_t)(const char* chars);

typedef enum op_code_t op_code_t;
enum op_code_t {
    OP_NOP,

    OP_MOVI8_MEM_TO_REG,
    OP_MOVU8_MEM_TO_REG,

    OP_MOVI16_MEM_TO_REG,
    OP_MOVU16_MEM_TO_REG,

    OP_MOVI32_MEM_TO_REG,
    OP_MOVU32_MEM_TO_REG,

    OP_MOVF32_MEM_TO_REG,

    OP_MOVWORD_MEM_TO_REG,

    OP_MOV_REG_TO_REG,

    OP_MOVWORD_REG_TO_REGMEM,
    OP_MOVWORD_REGMEM_TO_REG,

    OP_SUBU_REG_IM32,
    OP_ADDU_REG_IM32,

    OP_ADDI_REG_REG,
    OP_SUBI_REG_REG,
    OP_MULI_REG_REG,
    OP_DIVI_REG_REG,
    OP_REMI_REG_REG,
    OP_MODI_REG_REG,

    OP_ADDU_REG_REG,
    OP_SUBU_REG_REG,
    OP_MULU_REG_REG,
    OP_DIVU_REG_REG,
    OP_REMU_REG_REG,
    OP_MODU_REG_REG,

    OP_ADDD_REG_REG,
    OP_SUBD_REG_REG,
    OP_MULD_REG_REG,
    OP_DIVD_REG_REG,
    OP_REMD_REG_REG,
    OP_MODD_REG_REG,

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
    OP_LOOP,

    OP_NOT,

    OP_NEGATEI,
    OP_NEGATED,

    OP_INCREMENTD,
    OP_DECREMENTD,
    OP_INCREMENTI,
    OP_DECREMENTI,
    OP_INCREMENTU,
    OP_DECREMENTU,

    OP_CALL,
    OP_RETURN,
};

typedef u32 memaddr_t;

// todo: reduce from 12 to 8 bytes
typedef struct instruction_t instruction_t;
struct instruction_t {
    byte op;
    union {
        byte _padding[7];

        struct {
            u32 amount;
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
            byte size;
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

        struct {
            byte reg_destination;
            byte reg_source;
        } mov_reg_to_reg;

        struct {
            byte reg_op;
            byte reg_result;
        } unary_reg_to_reg;

        struct {
            byte reg_op;
        } call;
    } as;
};

#define REGISTER_COUNT 128
#define CALL_STACK_COUNT 1024

typedef struct function_t function_t;
struct function_t {
    string_t file_path;
    struct {
        texloc_t *items;
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
    call_frame_t call_frames[UINT8_MAX];
    size_t call_frame_count;
    call_frame_t call_frame;
    word_t registers[REGISTER_COUNT];
    bool halted;
};


void vm_init(vm_t *vm);

void vm_set_entry_point(vm_t *vm, function_t *entry_point);
void vm_step(vm_t *vm);

#endif

#ifdef VM_IMPLEMENTATION
#include <math.h>

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
}

void vm_set_entry_point(vm_t *vm, function_t *entry_point) {
    vm->halted = false;
    vm->call_frame.function = entry_point;
    vm->call_frame.pc = 0;
}

void vm_step(vm_t *vm) {
#define IP_ADV(amount) (vm->call_frame.pc += amount)
#define IP_DCR(amount) (vm->call_frame.pc -= amount)
#define MEMORY (vm->call_frame.function->memory)

    instruction_t in = vm->call_frame.function->code.items[vm->call_frame.pc];
    op_code_t op = (op_code_t)in.op;
    switch(op) {
        case OP_NOP: IP_ADV(1); break;

        case OP_JMP: {
            u32 jmp_amount = in.as.jmp.amount;
            IP_ADV(jmp_amount);
            break;
        }
        
        case OP_LOOP: {
            u32 jump_amount = in.as.jmp.amount;
            IP_DCR(jump_amount);
            break;
        }

        case OP_JMP_IF_REG_CONDITION: {
            byte reg = in.as.jmp.condition_reg;
            bool check_for = in.as.jmp.check_for;
            bool is_true = (check_for == vm->registers[reg].as.u);
            if (is_true) {
                u32 jmp_amount = in.as.jmp.amount;
                IP_ADV(jmp_amount);
            } else {
                IP_ADV(1);
            }
            break;
        }

        case OP_MOVI8_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            i8 value = *((i8*)(MEMORY->data + memaddr));
            vm->registers[reg].as.i = value;

            IP_ADV(1);
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

        case OP_MOVI16_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            i16 value = *((i16*)(MEMORY->data + memaddr));
            vm->registers[reg].as.i = value;

            IP_ADV(1);
            break;
        }

        case OP_MOVU16_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            u16 value = *((u16*)(MEMORY->data + memaddr));
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

        case OP_MOV_REG_TO_REG: {
            byte reg_dest = in.as.mov_reg_to_reg.reg_destination;
            byte reg_source = in.as.mov_reg_to_reg.reg_source;
            vm->registers[reg_dest] = vm->registers[reg_source];

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

        // conversion to unsigned for wrapped operation on overflow but converted back to integer after (c is ub for signed overflow)
        #define case_bini_reg_reg(name, op, type) case OP_##name##_REG_REG: {\
            i64 a = vm->registers[in.as.bin_reg_to_reg.reg_op1].as.type; \
            i64 b = vm->registers[in.as.bin_reg_to_reg.reg_op2].as.type; \
            byte c = in.as.bin_reg_to_reg.reg_result; \
            num_size_t sz = in.as.bin_reg_to_reg.size; \
            i64 result = 0; \
            switch (sz) {\
            case NUM_SIZE_8: {\
                result = op##type##8_(a, b);\
                break;\
            }\
            case NUM_SIZE_16: {\
                result = op##type##16_(a, b);\
                break;\
            }\
            case NUM_SIZE_32: {\
                result = op##type##32_(a, b);\
                break;\
            }\
            case NUM_SIZE_64: {\
                result = op##type##64_(a, b);\
                break;\
            }\
            }\
            vm->registers[c].as.type = result;\
            IP_ADV(1); \
            break; \
        }\
        break

        case_bini_reg_reg(ADDI, add, i);
        case_bini_reg_reg(SUBI, sub, i);
        case_bini_reg_reg(MULI, mul, i);
        case_bini_reg_reg(DIVI, mul, i);
        case_bini_reg_reg(MODI, rem, i);
        case_bini_reg_reg(REMI, rem, i);

        case_bini_reg_reg(ADDU, add, u);
        case_bini_reg_reg(SUBU, sub, u);
        case_bini_reg_reg(MULU, mul, u);
        case_bini_reg_reg(DIVU, mul, u);
        case_bini_reg_reg(MODU, rem, u);
        case_bini_reg_reg(REMU, rem, u);

        #undef case_bini_reg_reg

        #define case_bind_reg_reg(name, op) case OP_##name##_REG_REG: { \
            f64 a = vm->registers[in.as.bin_reg_to_reg.reg_op1].as.d; \
            f64 b = vm->registers[in.as.bin_reg_to_reg.reg_op2].as.d; \
            byte c = in.as.bin_reg_to_reg.reg_result; \
            num_size_t sz = in.as.bin_reg_to_reg.size; \
            f64 result = 0.0; \
            switch (sz) { \
            case NUM_SIZE_32: { \
                result = op##f_(a, b); \
                break; \
            } \
            case NUM_SIZE_64: { \
                result = op##d_(a, b); \
                break; \
            } \
            default: UNREACHABLE(); break; \
            } \
            vm->registers[c].as.d = result; \
            IP_ADV(1); \
            break; \
        } break

        case_bind_reg_reg(ADDD, add);
        case_bind_reg_reg(SUBD, sub);
        case_bind_reg_reg(MULD, mul);
        case_bind_reg_reg(DIVD, div);
        case_bind_reg_reg(MODD, mod);
        case_bind_reg_reg(REMD, rem);

        #undef case_bind_reg_reg

        #define case_binc_reg_reg(name, v, op) case OP_##name##_REG_REG: { \
            byte a = in.as.bin_reg_to_reg.reg_op1; \
            byte b = in.as.bin_reg_to_reg.reg_op2; \
            byte c = in.as.bin_reg_to_reg.reg_result; \
            vm->registers[c].as.u = vm->registers[a].as.v op vm->registers[b].as.v; \
            IP_ADV(1); \
            break; \
        } break

        case_binc_reg_reg(GTI, i, >);
        case_binc_reg_reg(GEI, i, >=);
        case_binc_reg_reg(LTI, i, <);
        case_binc_reg_reg(LEI, i, <=);
        case_binc_reg_reg(EQI, i, ==);
        case_binc_reg_reg(NQI, i, !=);

        case_binc_reg_reg(GTU, u, >);
        case_binc_reg_reg(GEU, u, >=);
        case_binc_reg_reg(LTU, u, <);
        case_binc_reg_reg(LEU, u, <=);
        case_binc_reg_reg(EQU, u, ==);
        case_binc_reg_reg(NQU, u, !=);

        case_binc_reg_reg(GTD, d, >);
        case_binc_reg_reg(GED, d, >=);
        case_binc_reg_reg(LTD, d, <);
        case_binc_reg_reg(LED, d, <=);
        case_binc_reg_reg(EQD, d, ==);
        case_binc_reg_reg(NQD, d, !=);

        #undef case_binc_reg_reg

        #define case_unary_reg_reg(name, m, op) case OP_##name: {\
            byte a = in.as.unary_reg_to_reg.reg_op;\
            byte result = in.as.unary_reg_to_reg.reg_result;\
            vm->registers[result].as.m = op(vm->registers[a].as.m);\
            IP_ADV(1); \
        } break

        case_unary_reg_reg(NOT, u, !);

        case_unary_reg_reg(NEGATEI, i, -);
        case_unary_reg_reg(NEGATED, d, -);

        case_unary_reg_reg(INCREMENTD, d, ++);
        case_unary_reg_reg(DECREMENTD, d, --);
        case_unary_reg_reg(INCREMENTI, i, ++);
        case_unary_reg_reg(DECREMENTI, i, --);
        case_unary_reg_reg(INCREMENTU, u, ++);
        case_unary_reg_reg(DECREMENTU, u, --);

        #undef case_unary_reg_reg

        case OP_CALL: {
            void *ptr = vm->registers[in.as.call.reg_op].as.p;
            function_t *func = (function_t*)ptr;
            call_frame_t call_frame = {0};
            call_frame.function = func;
            call_frame.pc = 0;

            vm->call_frames[vm->call_frame_count] = vm->call_frame;
            vm->call_frame_count++;
            vm->call_frame = call_frame;
            break;
        }

        case OP_RETURN: {
            if (vm->call_frame_count > 0) {
                --vm->call_frame_count;
                vm->call_frame = vm->call_frames[vm->call_frame_count];
                IP_ADV(1);
            } else {
                vm->halted = true;
            }
            break;
        }
    }

#undef MEMORY
#undef IP_DCR
#undef IP_ADV
}

#undef VM_IMPLEMENTATION
#endif
