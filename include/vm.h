#ifndef VM_H_
#define VM_H_

#include "def.h"
#include "parser.h"
#include "intrinsics.h"

typedef void (*write_function_t)(const char* chars);

typedef enum op_code_t op_code_t;
enum op_code_t {
    OP_NOP,

    OP_MOV_REG_TO_REG,

    OP_B2F,
    OP_S2F,
    OP_I2F,
    OP_B2D,
    OP_S2D,
    OP_I2D,

    OP_UB2F,
    OP_US2F,
    OP_U2F,
    OP_UB2D,
    OP_US2D,
    OP_U2D,

    OP_D2F,
    OP_D2UL,
    OP_D2L,

    OP_UL2UB,
    OP_UL2US,
    OP_UL2U,
    OP_UL2L,
    OP_UL2F,
    OP_UL2D,

    OP_L2B,
    OP_L2S,
    OP_L2I,
    OP_L2UL,
    OP_L2F,
    OP_L2D,

    OP_SUBU_IM,
    OP_ADDU_IM,

    OP_ADDI,
    OP_SUBI,
    OP_MULI,
    OP_DIVI,
    OP_REMI,
    OP_MODI,

    OP_ADDU,
    OP_SUBU,
    OP_MULU,
    OP_DIVU,
    OP_REMU,
    OP_MODU,

    OP_ADDD,
    OP_SUBD,
    OP_MULD,
    OP_DIVD,
    OP_REMD,
    OP_MODD,

    OP_GTD,
    OP_GED,
    OP_LTD,
    OP_LED,
    OP_EQD,
    OP_NQD,

    OP_GTI,
    OP_GEI,
    OP_LTI,
    OP_LEI,
    OP_EQI,
    OP_NQI,

    OP_GTU,
    OP_GEU,
    OP_LTU,
    OP_LEU,
    OP_EQU,
    OP_NQU,

    OP_JMP_IF_COND,
    OP_JMP,
    OP_LOOP,

    OP_NOT,

    OP_NEGATEI,
    OP_NEGATED,

    OP_MOVU8_REG_TO_ADDR,
    OP_MOVU16_REG_TO_ADDR,
    OP_MOVU32_REG_TO_ADDR,

    OP_MOVF32_REG_TO_ADDR,
    OP_MOVWORD_REG_TO_ADDR,

    OP_MOVU8_ADDR_TO_REG,
    OP_MOVU16_ADDR_TO_REG,
    OP_MOVU32_ADDR_TO_REG,

    OP_MOVS8_ADDR_TO_REG,
    OP_MOVS16_ADDR_TO_REG,
    OP_MOVS32_ADDR_TO_REG,

    OP_MOVF32_ADDR_TO_REG,
    OP_MOVWORD_ADDR_TO_REG,

    OP_LOAD_ADDR,
    OP_LOAD_REG_ADDR,

    OP_INTRINSIC_CALL,
    OP_CALL,
    OP_RETURN,
};

// ORIN = orso instruction
#define ORIN_UINTARG_MAX UINT32_MAX
static_assert(ORIN_UINTARG_MAX <= UINT32_MAX && ORIN_UINTARG_MAX < SIZE_T_MAX, "must be smaller than size_t_max and uint32_max");

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
            u32 mem_address;
        } mov_mem_to_reg;

        struct {
            byte reg_destination;
            byte reg_source;
            u32 byte_offset;
        } mov_reg_to_reg;

        struct {
            byte reg_op;
            byte reg_result;
        } unary_reg_to_reg;

        struct {
            byte reg_op;
            byte reg_result_addr;
            byte reg_arg_bottom_memaddr;
            byte reg_result;
        } call;

        struct {
            byte reg_op;
            byte reg_result;
            byte size_bytes; // for widening
        } casting;

        struct {
            u32 memaddr;
            byte reg_dest;
        } load_addr;
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

    arena_t *arena;
    memarr_t *program_mem;

    function_t *global_init_func;
    bool halted;
};

void vm_init(vm_t *vm);

void vm_set_entry_point(vm_t *vm, function_t *entry_point);
void vm_step(vm_t *vm);
void vm_global_init(vm_t *vm);
void vm_fresh_run(vm_t *vm, function_t *entry_point);

vm_t *vm_default(arena_t *arena);

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

void vm_global_init(vm_t *vm) {
    vm_fresh_run(vm, vm->global_init_func);
}

void vm_fresh_run(vm_t *vm, function_t *entry_point) {
    vm_set_entry_point(vm, entry_point);
    until (vm->halted) vm_step(vm);
}

vm_t *vm_default(arena_t *arena) {
    memarr_t *memory = arena_alloc(arena, sizeof(memarr_t));
    *memory = (memarr_t){0};

    memarr_init(memory, megabytes(2.5));
    size_t stack_size = (size_t)megabytes(0.5);
    memory->count = stack_size;
    memset(memory->data, 0, stack_size);

    vm_t *vm = arena_alloc(arena, sizeof(vm_t));
    vm_init(vm);
    vm->registers[REG_STACK_FRAME].as.p = (memory->data + stack_size);
    vm->registers[REG_STACK_BOTTOM].as.p = (memory->data + stack_size);

    vm->global_init_func = new_function(memory, arena);

    vm->arena = arena;
    vm->program_mem = memory;

    return vm;
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

        case OP_JMP_IF_COND: {
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

        case OP_LOAD_ADDR: {
            byte reg_dest = in.as.load_addr.reg_dest;
            u32 index = in.as.load_addr.memaddr;

            vm->registers[reg_dest].as.p = MEMORY->data + index;
            IP_ADV(1);
            break;
        }

        case OP_LOAD_REG_ADDR: {
            byte reg_dest = in.as.load_addr.reg_dest;
            byte reg_to_get_addr_of = (byte)in.as.load_addr.memaddr;

            vm->registers[reg_dest].as.p = &vm->registers[reg_to_get_addr_of];
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

        case OP_ADDU_IM: {
            byte s = in.as.binu_reg_immediate.reg_operand;
            u32 immediate = in.as.binu_reg_immediate.immediate;

            byte d = in.as.binu_reg_immediate.reg_result;

            vm->registers[d].as.u = vm->registers[s].as.u + immediate;

            IP_ADV(1);
            break;
        }

        case OP_SUBU_IM: {
            byte s = in.as.binu_reg_immediate.reg_operand;
            u32 immediate = in.as.binu_reg_immediate.immediate;

            byte d = in.as.binu_reg_immediate.reg_result;

            vm->registers[d].as.u = vm->registers[s].as.u - immediate;

            IP_ADV(1);
            break;
        }

        #define do_cast(d, s, dtype, stype) do { \
            byte result = in.as.casting.reg_result; \
            byte op = in.as.casting.reg_op; \
            vm->registers[result].as.d = cast(dtype, (stype)vm->registers[op].as.s); \
            IP_ADV(1); \
        } while(false); break

        case OP_B2D: do_cast(d, s, f64, s8);
        case OP_S2D: do_cast(d, s, f64, s16);
        case OP_I2D: do_cast(d, s, f64, s32);
        case OP_UB2D: do_cast(d, s, f64, u8);
        case OP_US2D: do_cast(d, s, f64, u16);
        case OP_U2D: do_cast(d, s, f64, u32);

        case OP_B2F: do_cast(d, s, f32, s8);
        case OP_S2F: do_cast(d, s, f32, s16);
        case OP_I2F: do_cast(d, s, f32, s32);
        case OP_UB2F: do_cast(d, s, f32, u8);
        case OP_US2F: do_cast(d, s, f32, u16);
        case OP_U2F: do_cast(d, s, f32, u32);

        case OP_D2F: do_cast(d, d, f32, f64);
        case OP_D2UL: do_cast(u, d, u64, f64);
        case OP_D2L: do_cast(s, d, s64, f64);

        case OP_UL2UB: do_cast(u, u, u8, u64);
        case OP_UL2US: do_cast(u, u, u16, u64);
        case OP_UL2U: do_cast(u, u, u32, u64);
        case OP_UL2L: do_cast(s, u, s64, u64);
        case OP_UL2F: do_cast(d, u, f32, u64);
        case OP_UL2D: do_cast(d, u, f64, u64);

        case OP_L2B: do_cast(s, s, s8, s64);
        case OP_L2S: do_cast(s, s, s16, s64);
        case OP_L2I: do_cast(s, s, s32, s64);
        case OP_L2UL: do_cast(u, s, u64, s64);
        case OP_L2F: do_cast(d, s, f32, s64);
        case OP_L2D: do_cast(d, s, f64, s64);

        #undef do_cast

        // conversion to unsigned for wrapped operation on overflow but converted back to integer after (c is ub for signed overflow)
        #define case_bini_reg_reg(name, op, type) case OP_##name: {\
            type##64 a = vm->registers[in.as.bin_reg_to_reg.reg_op1].as.type; \
            type##64 b = vm->registers[in.as.bin_reg_to_reg.reg_op2].as.type; \
            byte c = in.as.bin_reg_to_reg.reg_result; \
            num_size_t sz = in.as.bin_reg_to_reg.size; \
            type##64 result = 0; \
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
        case_bini_reg_reg(MODI, mod, s);
        case_bini_reg_reg(REMI, rem, s);

        case_bini_reg_reg(ADDU, add, u);
        case_bini_reg_reg(SUBU, sub, u);
        case_bini_reg_reg(MULU, mul, u);
        case_bini_reg_reg(DIVU, div, u);
        case_bini_reg_reg(MODU, mod, u);
        case_bini_reg_reg(REMU, rem, u);

        #undef case_bini_reg_reg

        #define case_bind_reg_reg(name, op) case OP_##name: { \
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

        #define case_binc_reg_reg(name, v, op) case OP_##name: { \
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

        #undef case_unary_reg_reg

        #define case_mov_regaddr_to_reg(name, q, type, mask) case OP_MOV##name##_ADDR_TO_REG: { \
            byte regaddr = in.as.mov_reg_to_reg.reg_source; \
            byte reg_dest = in.as.mov_reg_to_reg.reg_destination; \
            u32 offset = in.as.mov_reg_to_reg.byte_offset; \
            vm->registers[reg_dest].as.q = (*((type*)(vm->registers[regaddr].as.p + offset))); \
            IP_ADV(1); \
        } break
        
        case_mov_regaddr_to_reg(U8, u, u8, 0xF);
        case_mov_regaddr_to_reg(U16, u, u16, 0xFF);
        case_mov_regaddr_to_reg(U32, u, u32, 0xFFFF);

        case_mov_regaddr_to_reg(S8, s, s8, 0xF);
        case_mov_regaddr_to_reg(S16, s, s16, 0xFF);
        case_mov_regaddr_to_reg(S32, s, s32, 0xFFFF);

        case_mov_regaddr_to_reg(F32, d, f32, 0xFFFF);

        #undef case_mov_regaddr_to_reg

        case OP_MOVWORD_ADDR_TO_REG: {
            byte regaddr = in.as.mov_reg_to_reg.reg_source;
            byte reg_dest = in.as.mov_reg_to_reg.reg_destination;
            u32 offset = in.as.mov_reg_to_reg.byte_offset;
            memcpy(&vm->registers[reg_dest], vm->registers[regaddr].as.p + offset, sizeof(word_t));
            IP_ADV(1);
            break;
        }

        #define case_mov_reg_to_regaddr(name, q, type) case OP_MOV##name##_REG_TO_ADDR: { \
            byte reg_src = in.as.mov_reg_to_reg.reg_source; \
            byte regaddr = in.as.mov_reg_to_reg.reg_destination; \
            u32 offset = in.as.mov_reg_to_reg.byte_offset; \
            *((type*)(vm->registers[regaddr].as.p + offset)) = (type)vm->registers[reg_src].as.q; \
            IP_ADV(1); \
        } break

        case_mov_reg_to_regaddr(U8, u, u8);
        case_mov_reg_to_regaddr(U16, u, u16);
        case_mov_reg_to_regaddr(U32, u, u32);
        case_mov_reg_to_regaddr(F32, d, f32);

        #undef case_mov_reg_to_regaddr

        case OP_MOVWORD_REG_TO_ADDR: {
            byte reg_src = in.as.mov_reg_to_reg.reg_source;
            byte regaddr = in.as.mov_reg_to_reg.reg_destination;
            u32 offset = in.as.mov_reg_to_reg.byte_offset;
            memcpy(vm->registers[regaddr].as.p + offset, &vm->registers[reg_src], sizeof(word_t));
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
            void *result_addr = vm->registers[in.as.call.reg_result_addr].as.p;
            void* args_address_bottom = vm->registers[in.as.call.reg_arg_bottom_memaddr].as.p;

            intrinsic_fn_t fn = (intrinsic_fn_t)ptr;

            fn(args_address_bottom, result_addr);

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
