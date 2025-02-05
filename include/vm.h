#ifndef VM_H_
#define VM_H_

#include "def.h"
#include "parser.h"

typedef enum reg_t reg_t;
enum reg_t {
    REG_NULL = 0,
    REG_RESULT = 1, // 256 bytes for memory on stack

    REG_TMP = 33,
    REG_STACK_BOTTOM = 34,
    REG_STACK_FRAME = 35,
};

typedef void (*write_function_t)(const char* chars);

typedef enum op_code_t op_code_t;
enum op_code_t {
    OP_NOP,

    OP_MOVU8_MEM_TO_REG,
    OP_MOVU16_MEM_TO_REG,
    OP_MOVU32_MEM_TO_REG,
    OP_MOVF32_MEM_TO_REG,
    OP_MOVWORD_MEM_TO_REG,

    OP_MOV_REG_TO_REG,

    OP_MOVU8_REG_TO_REGMEM,
    OP_MOVU16_REG_TO_REGMEM,
    OP_MOVU32_REG_TO_REGMEM,
    OP_MOVF32_REG_TO_REGMEM,
    OP_MOVWORD_REG_TO_REGMEM,

    OP_MOVU8_REGMEM_TO_REG,
    OP_MOVU16_REGMEM_TO_REG,
    OP_MOVU32_REGMEM_TO_REG,
    OP_MOVF32_REGMEM_TO_REG,
    OP_MOVWORD_REGMEM_TO_REG,

    OP_CAST_D2F,
    OP_CAST_D2UL,
    OP_CAST_D2L,

    OP_CAST_UL2UB,
    OP_CAST_UL2US,
    OP_CAST_UL2U,
    OP_CAST_UL2L,
    OP_CAST_UL2F,
    OP_CAST_UL2D,

    OP_CAST_L2B,
    OP_CAST_L2S,
    OP_CAST_L2I,
    OP_CAST_L2UL,
    OP_CAST_L2F,
    OP_CAST_L2D,

    OP_WIDEN,

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

    OP_MOV_REGMEM_TO_REGADDR,

    OP_MOVU8_REG_TO_REGADDR,
    OP_MOVU16_REG_TO_REGADDR,
    OP_MOVU32_REG_TO_REGADDR,
    OP_MOVF32_REG_TO_REGADDR,
    OP_MOVWORD_REG_TO_REGADDR,

    OP_MOVU8_REGADDR_TO_REG,
    OP_MOVU16_REGADDR_TO_REG,
    OP_MOVU32_REGADDR_TO_REG,
    OP_MOVF32_REGADDR_TO_REG,
    OP_MOVWORD_REGADDR_TO_REG,

    OP_INTRINSIC_CALL,
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
            byte reg_destination;
            byte reg_source;
        } mov_reg_to_reg;

        struct {
            byte reg_op;
            byte reg_result;
        } unary_reg_to_reg;

        struct {
            byte reg_op;
            byte reg_result_size;
            byte reg_arg_bottom_memaddr;
            byte reg_result;
        } call;

        struct {
            byte reg_op;
            byte reg_result;
            byte size_bytes; // for widening
        } casting;
    } as;
};

#define REGISTER_COUNT 128
#define CALL_STACK_COUNT 1024

typedef struct function_t function_t;
struct function_t {
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

function_t *new_function(memarr_t *memory, arena_t *arena);
void function_init(function_t *function, memarr_t *memory, arena_t *arena);
bool function_is_compiled(function_t *function);

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
void vm_fresh_run(vm_t *vm, function_t *entry_point);

typedef struct env_t env_t;
struct env_t {
    vm_t *vm;
    arena_t *arena;
    memarr_t *memory;
};

#endif

#ifdef VM_IMPLEMENTATION
#include <math.h>

function_t *new_function(memarr_t *memory, arena_t *arena) {
    function_t *function = arena_alloc(arena, sizeof(function_t));
    *function = (function_t){0};
    function_init(function, memory, arena);
    return function;
}

void function_init(function_t *function, memarr_t *memory, arena_t *arena) {
    function->memory = memory;
    function->locations.allocator = arena;
    function->code.allocator = arena;
}

bool function_is_compiled(function_t *function) {
    return function->code.capacity > 0 && function->memory != NULL;
}

void vm_init(vm_t *vm) {
    *vm = (vm_t){0};
}

void vm_set_entry_point(vm_t *vm, function_t *entry_point) {
    vm->halted = false;
    vm->call_frame.function = entry_point;
    vm->call_frame.pc = 0;
}

void vm_fresh_run(vm_t *vm, function_t *entry_point) {
    vm_set_entry_point(vm, entry_point);
    until (vm->halted) vm_step(vm);
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

        case OP_MOVU8_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            byte value = MEMORY->data[memaddr];
            vm->registers[reg].as.u = value;

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

        case OP_MOVU32_MEM_TO_REG: {
            memaddr_t memaddr = in.as.mov_mem_to_reg.mem_address;
            byte reg = in.as.mov_mem_to_reg.reg_result;
            u32 value = *((u32*)(MEMORY->data + memaddr));
            vm->registers[reg].as.u = value;

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

        #define case_op_mov_reg_to_regmem(name, type, q) case OP_MOV##name##_REG_TO_REGMEM: { \
            byte src = in.as.mov_reg_to_reg.reg_source; \
            byte dst_regmem = in.as.mov_reg_to_reg.reg_destination; \
            memaddr_t dst_mem = vm->registers[dst_regmem].as.u; \
            type value = (type)vm->registers[src].as.q; \
            *(type*)(MEMORY->data + dst_mem) = value; \
            IP_ADV(1); \
        } break;

        case_op_mov_reg_to_regmem(U8, u8, u);
        case_op_mov_reg_to_regmem(U16, u16, u);
        case_op_mov_reg_to_regmem(U32, u32, u);
        case_op_mov_reg_to_regmem(F32, f32, d);

        #undef case_op_reg_to_regmem

        case OP_MOVWORD_REG_TO_REGMEM: {
            byte reg = in.as.mov_reg_to_reg.reg_source;
            word_t value = vm->registers[reg];

            byte regmem = in.as.mov_reg_to_reg.reg_destination;
            memaddr_t memaddr = (u32)vm->registers[regmem].as.u;

            memcpy(MEMORY->data + memaddr, &value, sizeof(word_t));

            IP_ADV(1);
            break;
        }

        #define case_op_mov_regmem_to_reg(name, type, q) case OP_MOV##name##_REGMEM_TO_REG: { \
            byte src_regmem = in.as.mov_reg_to_reg.reg_source; \
            memaddr_t src_mem = vm->registers[src_regmem].as.u; \
            byte dst = in.as.mov_reg_to_reg.reg_destination; \
            vm->registers[dst].as.q = *(type*)(MEMORY->data + src_mem); \
            IP_ADV(1); \
        } break;

        case_op_mov_regmem_to_reg(U8, u8, u);
        case_op_mov_regmem_to_reg(U16, u16, u);
        case_op_mov_regmem_to_reg(U32, u32, u);
        case_op_mov_regmem_to_reg(F32, f32, d);

        #undef case_op_mov_regmem_reg

        case OP_MOVWORD_REGMEM_TO_REG: {
            byte regmem = in.as.mov_reg_to_reg.reg_source;
            memaddr_t memaddr = vm->registers[regmem].as.u;

            byte reg = in.as.mov_reg_to_reg.reg_destination;

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

        #define do_cast(d, s, dtype) do { \
            byte result = in.as.casting.reg_result; \
            byte op = in.as.casting.reg_op; \
            vm->registers[result].as.d = cast(dtype, vm->registers[op].as.s); \
            IP_ADV(1); \
        } while(false); break

        case OP_CAST_D2UL: do_cast(u, d, u64);
        case OP_CAST_D2L: do_cast(s, d, s64);
        case OP_CAST_D2F: do_cast(d, d, f32);

        case OP_CAST_UL2UB: do_cast(u, u, u8);
        case OP_CAST_UL2US: do_cast(u, u, u16);
        case OP_CAST_UL2U: do_cast(u, u, u32);
        case OP_CAST_UL2L: do_cast(s, u, s64);
        case OP_CAST_UL2F: do_cast(d, u, f32);
        case OP_CAST_UL2D: do_cast(d, u, f64);

        case OP_CAST_L2B: do_cast(s, s, s8);
        case OP_CAST_L2S: do_cast(s, s, s16);
        case OP_CAST_L2I: do_cast(s, s, s32);
        case OP_CAST_L2UL: do_cast(u, s, u64);
        case OP_CAST_L2F: do_cast(d, s, f32);
        case OP_CAST_L2D: do_cast(d, s, f64);

        #undef do_cast

        case OP_WIDEN: {
            const u64 masks[] = {0x0000000F, 0x000000FF, 0x00000FFF, 0x0000FFFF, 0x000FFFFF, 0x00FFFFFF, 0x0FFFFFFF, 0xFFFFFFFF};
            u8 size_bytes = in.as.casting.size_bytes;
            ASSERT(size_bytes < WORD_SIZE, "this is only for small things");

            u64 mask = masks[size_bytes];
            vm->registers[in.as.casting.reg_result].as.u = vm->registers[in.as.casting.reg_op].as.u&mask;

            IP_ADV(1);
            break;
        }

        // conversion to unsigned for wrapped operation on overflow but converted back to integer after (c is ub for signed overflow)
        #define case_bini_reg_reg(name, op, type) case OP_##name##_REG_REG: {\
            s64 a = vm->registers[in.as.bin_reg_to_reg.reg_op1].as.type; \
            s64 b = vm->registers[in.as.bin_reg_to_reg.reg_op2].as.type; \
            byte c = in.as.bin_reg_to_reg.reg_result; \
            num_size_t sz = in.as.bin_reg_to_reg.size; \
            s64 result = 0; \
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

        case_bini_reg_reg(ADDI, add, s);
        case_bini_reg_reg(SUBI, sub, s);
        case_bini_reg_reg(MULI, mul, s);
        case_bini_reg_reg(DIVI, div, s);
        case_bini_reg_reg(MODI, rem, s);
        case_bini_reg_reg(REMI, rem, s);

        case_bini_reg_reg(ADDU, add, u);
        case_bini_reg_reg(SUBU, sub, u);
        case_bini_reg_reg(MULU, mul, u);
        case_bini_reg_reg(DIVU, div, u);
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

        case_binc_reg_reg(GTI, s, >);
        case_binc_reg_reg(GEI, s, >=);
        case_binc_reg_reg(LTI, s, <);
        case_binc_reg_reg(LEI, s, <=);
        case_binc_reg_reg(EQI, s, ==);
        case_binc_reg_reg(NQI, s, !=);

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

        case_unary_reg_reg(NEGATEI, s, -);
        case_unary_reg_reg(NEGATED, d, -);

        case_unary_reg_reg(INCREMENTD, d, ++);
        case_unary_reg_reg(DECREMENTD, d, --);
        case_unary_reg_reg(INCREMENTI, s, ++);
        case_unary_reg_reg(DECREMENTI, s, --);
        case_unary_reg_reg(INCREMENTU, u, ++);
        case_unary_reg_reg(DECREMENTU, u, --);

        #undef case_unary_reg_reg

        case OP_MOV_REGMEM_TO_REGADDR: {
            byte src_mem = in.as.mov_reg_to_reg.reg_source;
            byte dest_addr = in.as.mov_reg_to_reg.reg_destination;

            memaddr_t memaddr = vm->registers[src_mem].as.u;

            vm->registers[dest_addr].as.p = (MEMORY->data + memaddr);

            IP_ADV(1);
            break;
        }

        #define case_mov_regaddr_to_reg(name, q, type) case OP_MOV##name##_REGADDR_TO_REG: { \
            byte regaddr = in.as.mov_reg_to_reg.reg_source; \
            byte reg_dest = in.as.mov_reg_to_reg.reg_destination; \
            vm->registers[reg_dest].as.q = (*((type*)(vm->registers[regaddr].as.p))); \
            IP_ADV(1); \
        } break

        case_mov_regaddr_to_reg(U8, u, u8);
        case_mov_regaddr_to_reg(U16, u, u16);
        case_mov_regaddr_to_reg(U32, u, u32);
        case_mov_regaddr_to_reg(F32, d, f32);

        #undef case_mov_regaddr_to_reg

        case OP_MOVWORD_REGADDR_TO_REG: {
            byte regaddr = in.as.mov_reg_to_reg.reg_source;
            byte reg_dest = in.as.mov_reg_to_reg.reg_destination;
            memcpy(&vm->registers[reg_dest], vm->registers[regaddr].as.p, sizeof(word_t));
            IP_ADV(1);
            break;
        }

        #define case_mov_reg_to_regaddr(name, q, type) case OP_MOV##name##_REG_TO_REGADDR: { \
            byte reg_src = in.as.mov_reg_to_reg.reg_source; \
            byte regaddr = in.as.mov_reg_to_reg.reg_destination; \
            *((type*)(vm->registers[regaddr].as.p)) = (type)vm->registers[reg_src].as.q; \
            IP_ADV(1); \
        } break

        case_mov_reg_to_regaddr(U8, u, u8);
        case_mov_reg_to_regaddr(U16, u, u16);
        case_mov_reg_to_regaddr(U32, u, u32);
        case_mov_reg_to_regaddr(F32, d, f32);

        #undef case_mov_reg_to_regaddr

        case OP_MOVWORD_REG_TO_REGADDR: {
            byte reg_src = in.as.mov_reg_to_reg.reg_source;
            byte regaddr = in.as.mov_reg_to_reg.reg_destination;
            memcpy(vm->registers[regaddr].as.p, &vm->registers[reg_src], sizeof(word_t));
            IP_ADV(1);
            break;
        }

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

        case OP_INTRINSIC_CALL: {
            void *ptr = vm->registers[in.as.call.reg_op].as.p;
            u64 result_size = vm->registers[in.as.call.reg_result_size].as.u;
            void* args_address_bottom = MEMORY->data + vm->registers[in.as.call.reg_arg_bottom_memaddr].as.u;

            size_t size = b2w(result_size)*WORD_SIZE;
            if (size < WORD_SIZE) size = WORD_SIZE;

            u8 result[size];
            memset(result, 0, size);

            intrinsic_fn_t fn = (intrinsic_fn_t)ptr;
            if (result_size > WORD_SIZE) TODO("not implemented yet");

            fn(args_address_bottom, result);

            vm->registers[in.as.call.reg_result] = *(word_t*)result;

            IP_ADV(1);
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
