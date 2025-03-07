#ifndef CODEGEN_H_
#define CODEGENH_

#include "parser.h"
#include "vm.h"
#include "error.h"

bool compile_program(vm_t *vm, ast_t *ast);

void gen_funcdef(ast_t *ast, env_t *env, ast_node_t *funcdef, error_function_t error_fn);
bool compile_modules(ast_t *ast, error_function_t error_fn, memarr_t *program_memory, arena_t *funcarea, function_t *global_init_func);
bool compile_expr_to_function(function_t *function, ast_t *ast, ast_node_t *expr, error_function_t error_fn, memarr_t *program_memory, arena_t *function_arena);

#endif

#ifdef CODEGEN_IMPLEMENTATION

typedef struct local_t local_t;
struct local_t {
    size_t stack_location;
    ast_node_t *ref_decl;
};

static int ptr_hash__(void *id) {
    return kh_int64_hash_func((khint64_t)id);
}

static int ptr_eq__(void *a, void *b) {
    return a == b;
}

define_table(ptr2sz, void*, size_t, ptr_hash__, ptr_eq__);

typedef struct gen_t gen_t;
struct gen_t {
    error_function_t error_fn;
    ast_t *ast;
    size_t stack_size;
    arena_t *gen_arena;
    arena_t *program_arena;
    memarr_t *program_memory;

    struct {
        local_t *items;
        size_t count;
        size_t capacity;
        arena_t *allocator;
    } locals;

    table_t(ptr2sz) *decl2index;

    bool had_error;
    bool breached_stack_limit;
};

static void gen_error(gen_t *gen, error_t error) {
    gen->had_error = true;
    if (gen->error_fn) gen->error_fn(gen->ast, error);
}

static void emit_instruction(function_t *function, texloc_t location, instruction_t instruction) {
    array_push(&function->code, instruction);
    array_push(&function->locations, location);
}

typedef enum reg_mov_type_t reg_mov_type_t;
enum reg_mov_type_t {
    REG_MOV_TYPE_ADDR_TO_REG,
    REG_MOV_TYPE_REG_TO_ADDR,
    REG_MOV_TYPE_REG_TO_REG,
};

typedef enum reg_mov_size_t reg_mov_size_t;
enum reg_mov_size_t {
    REG_MOV_SIZE_0,
    REG_MOV_SIZE_U8,
    REG_MOV_SIZE_U16,
    REG_MOV_SIZE_U32,
    REG_MOV_SIZE_F32,
    REG_MOV_SIZE_WORD,
    REG_MOV_SIZE_MULTIWORD_ADDR,
};

static void emit_reg_to_reg(function_t *function, texloc_t loc, reg_t reg_dst, reg_t reg_src) {
    instruction_t in = {0};
    in.op = OP_MOV_REG_TO_REG;
    in.as.mov_reg_to_reg.reg_destination = reg_dst;
    in.as.mov_reg_to_reg.reg_source = reg_src;

    emit_instruction(function, loc, in);
}

static void emit_load_reg_addr(function_t *function, texloc_t loc, reg_t reg_dst, reg_t reg_to_get_addr_of) {
    instruction_t in = {0};
    in.op = OP_LOAD_REG_ADDR;
    in.as.load_addr.memaddr = reg_to_get_addr_of;
    in.as.load_addr.reg_dest = reg_dst;

    emit_instruction(function, loc, in);
}

static void emit_addr_to_reg(gen_t *gen, function_t *function, texloc_t loc, reg_mov_size_t mov_size, reg_t reg_dest, reg_t reg_src, size_t offset) {
    if (offset > UINT32_MAX) {
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_OFFSET_TOO_LARGE));
        return;
    }

    instruction_t in = {0};
    switch (mov_size) {
    case REG_MOV_SIZE_0: return;
    case REG_MOV_SIZE_U8: in.op = OP_MOVU8_ADDR_TO_REG; break;
    case REG_MOV_SIZE_U16: in.op = OP_MOVU16_ADDR_TO_REG; break;
    case REG_MOV_SIZE_U32: in.op = OP_MOVU32_ADDR_TO_REG; break;
    case REG_MOV_SIZE_F32: in.op = OP_MOVF32_ADDR_TO_REG; break;
    case REG_MOV_SIZE_WORD: in.op = OP_MOVWORD_ADDR_TO_REG; break;
    case REG_MOV_SIZE_MULTIWORD_ADDR: in.op = OP_MOVWORD_ADDR_TO_REG; break;
    }

    in.as.mov_reg_to_reg.reg_destination = reg_dest;
    in.as.mov_reg_to_reg.reg_source = reg_src;
    in.as.mov_reg_to_reg.byte_offset = (u32)offset;

    emit_instruction(function, loc, in);
}

static void emit_reg_to_addr(gen_t *gen, function_t *function, texloc_t loc, reg_mov_size_t mov_size, reg_t reg_dest, reg_t reg_src, size_t offset) {
    if (offset > UINT32_MAX) {
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_OFFSET_TOO_LARGE));
        return;
    }

    instruction_t in = {0};
    switch (mov_size) {
    case REG_MOV_SIZE_0: return;
    case REG_MOV_SIZE_U8: in.op = OP_MOVU8_REG_TO_ADDR; break;
    case REG_MOV_SIZE_U16: in.op = OP_MOVU16_REG_TO_ADDR; break;
    case REG_MOV_SIZE_U32: in.op = OP_MOVU32_REG_TO_ADDR; break;
    case REG_MOV_SIZE_F32: in.op = OP_MOVF32_REG_TO_ADDR; break;
    case REG_MOV_SIZE_WORD: in.op = OP_MOVWORD_REG_TO_ADDR; break;
    case REG_MOV_SIZE_MULTIWORD_ADDR: in.op = OP_MOVWORD_REG_TO_ADDR; break;
    }

    in.as.mov_reg_to_reg.reg_destination = reg_dest;
    in.as.mov_reg_to_reg.reg_source = reg_src;
    in.as.mov_reg_to_reg.byte_offset = (u32)offset;

    emit_instruction(function, loc, in);
}


static void emit_binu_reg_im(function_t *function, texloc_t loc, byte reg_dest, byte reg_op, u32 immediate, char plus_or_minus) {
    instruction_t in = {0};
    in.op = plus_or_minus == '-' ? OP_SUBU_IM : OP_ADDU_IM;
    in.as.binu_reg_immediate.immediate = immediate;
    in.as.binu_reg_immediate.reg_operand = reg_op;
    in.as.binu_reg_immediate.reg_result = reg_dest;

    emit_instruction(function, loc, in);
}

static reg_mov_size_t type2movsize(gen_t *gen, type_t t) {
    typedata_t *td = type2typedata(&gen->ast->type_set.types, t);
    switch (td->kind) {
    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_FLOAT: return td->size == NUM_SIZE_32 ? REG_MOV_SIZE_F32 : REG_MOV_SIZE_WORD;
        case NUM_TYPE_SIGNED:
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: return REG_MOV_SIZE_U8;
            case NUM_SIZE_16: return REG_MOV_SIZE_U16;
            case NUM_SIZE_32: return REG_MOV_SIZE_U32;
            case NUM_SIZE_64: return REG_MOV_SIZE_WORD;
            }
            break;
        }
        }
        break;
    }

    case TYPE_VOID: return REG_MOV_SIZE_0;
    case TYPE_BOOL: return REG_MOV_SIZE_U8;

    case TYPE_STRING:
    case TYPE_TYPE:
    case TYPE_FUNCTION:
    case TYPE_INTRINSIC_FUNCTION:
    case TYPE_POINTER:
    case TYPE_ARRAY:
    case TYPE_STRUCT: return REG_MOV_SIZE_WORD;


    case TYPE_COUNT:
    case TYPE_UNREACHABLE:
    case TYPE_INFERRED_FUNCTION:
    case TYPE_UNRESOLVED:
    case TYPE_INVALID: UNREACHABLE(); return REG_MOV_SIZE_WORD;
    }
}

static void emit_multiword_addr_to_addr(gen_t *gen, function_t *function, texloc_t loc, reg_t reg_dst, reg_t reg_src, reg_t reg_tmp, size_t size_bytes) {
    size_t words = b2w(size_bytes);
    for (size_t i = 0; i < words; ++i) {
        emit_addr_to_reg(gen, function, loc, REG_MOV_SIZE_WORD, reg_tmp, reg_src, i*WORD_SIZE);
        emit_reg_to_addr(gen, function, loc, REG_MOV_SIZE_WORD, reg_dst, reg_tmp, i*WORD_SIZE);
    }
}

// assumes that if type is too large for a register, then reg source pointers to a generic address
static void emit_push_reg(gen_t *gen, texloc_t loc, function_t *function, reg_t reg_source, reg_mov_size_t mov_size, size_t size_bytes) {
    if (mov_size == REG_MOV_SIZE_0) return;

    size_t size_in_words = b2w(size_bytes)*WORD_SIZE;
    gen->stack_size += (mov_size == REG_MOV_SIZE_MULTIWORD_ADDR) ? size_in_words : WORD_SIZE;

    if (size_bytes > UINT32_MAX) {
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_DATA_TOO_LARGE_TO_PUSH_TO_STACK));
        return;
    }

    if (gen->stack_size > UINT32_MAX && !gen->breached_stack_limit) {
        gen->breached_stack_limit = true;
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_STACK_SIZE_GROWS_LARGER_THAN_UINT32_MAX));
        return;
    }

    if (mov_size == REG_MOV_SIZE_MULTIWORD_ADDR) {
        // move stack pointer down to leave enough space for data
        emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, size_in_words, '-');

        emit_multiword_addr_to_addr(gen, function, loc, REG_STACK_BOTTOM, reg_source, REG_TMP2, size_bytes);
    } else {
        emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, (u32)WORD_SIZE, '-');
        emit_reg_to_addr(gen, function, loc, mov_size, REG_STACK_BOTTOM, reg_source, 0);
    }
}

static void emit_pop_to_reg(gen_t *gen, texloc_t loc, function_t *function, reg_t reg_destination, type_t type) {
    if (TYPE_IS_VOID(type)) return;

    gen->stack_size -= sizeof(word_t);

    emit_addr_to_reg(gen, function, loc, type2movsize(gen, type), reg_destination, REG_STACK_BOTTOM, 0);
    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, (u32)sizeof(word_t), '+');
}

static void emit_popn_bytes(gen_t *gen, function_t *function, u32 pop_size_bytes, texloc_t loc, bool decrement_stack) {
    if (pop_size_bytes == 0) return; // noop

    if (decrement_stack) {
        gen->stack_size -= pop_size_bytes;
    }

    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, pop_size_bytes, '+');
}

static void emit_bin_op(texloc_t loc, function_t *function, token_type_t token_type, typedata_t *type_info, reg_t op1, reg_t op2, reg_t result) {
    instruction_t instruction = {0};

    switch (type_info->kind) {
    case TYPE_TYPE:
    case TYPE_BOOL:
    case TYPE_FUNCTION: {
        switch (token_type) {
            case TOKEN_PLUS:
            case TOKEN_MINUS: 
            case TOKEN_SLASH:
            case TOKEN_STAR:
            case TOKEN_PERCENT:
            case TOKEN_PERCENT_PERCENT: UNREACHABLE(); break;

            case TOKEN_GREATER:
            case TOKEN_GREATER_EQUAL:
            case TOKEN_LESS:
            case TOKEN_LESS_EQUAL: UNREACHABLE(); break;

            case TOKEN_EQUAL_EQUAL: instruction.op = OP_EQU; break;
            case TOKEN_BANG_EQUAL: instruction.op = OP_NQU; break;

            default: UNREACHABLE(); break;
        }
    }

    case TYPE_NUMBER: {
        switch (token_type) {
            case TOKEN_PLUS: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_ADDD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_ADDI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_ADDU; break;
                }
                break;
            }

            case TOKEN_MINUS: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_SUBD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_SUBI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_SUBU; break;
                }
                break;
            }

            case TOKEN_STAR: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_MULD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_MULI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_MULU; break;
                }
                break;
            }
            
            case TOKEN_SLASH: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_DIVD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_DIVI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_DIVU; break;
                }
                break;
            }

            case TOKEN_PERCENT: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_MODD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_MODI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_MODU; break;
                }
                break;
            }

            case TOKEN_PERCENT_PERCENT: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_REMD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_REMI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_REMU; break;
                }
                break;
            }

            case TOKEN_GREATER: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_GTD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_GTI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_GTU; break;
                }
                break;
            }

            case TOKEN_GREATER_EQUAL: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_GED; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_GEI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_GEU; break;
                }
                break;
            }

            case TOKEN_LESS:{
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_LTD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_LTI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_LTU; break;
                }
                break;
            }

            case TOKEN_LESS_EQUAL: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_LED; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_LEI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_LEU; break;
                }
                break;
            }

            case TOKEN_EQUAL_EQUAL: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_EQD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_EQI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_EQU; break;
                }
                break;
            }

            case TOKEN_BANG_EQUAL: {
                switch (type_info->as.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_NQD; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_NQI; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_NQU; break;
                }
                break;
            }

            default:
                UNREACHABLE();
        }
        break;
    }
    
    default: UNREACHABLE(); break;
    }

    instruction.as.bin_reg_to_reg.reg_op1 = (byte)op1;
    instruction.as.bin_reg_to_reg.reg_op2 = (byte)op2;
    instruction.as.bin_reg_to_reg.reg_result = (byte)result;
    instruction.as.bin_reg_to_reg.size = (byte)type_info->size;

    emit_instruction(function, loc, instruction);
}

static void emit_unary(gen_t *gen, texloc_t loc, function_t *function, token_type_t token_type, type_t type, reg_t op1, reg_t result) {
    typedata_t *td = type2typedata(&gen->ast->type_set.types, type);

    switch (token_type) {
        case TOKEN_NOT: {
            instruction_t in = {0};
            in.as.unary_reg_to_reg.reg_op = op1;
            in.as.unary_reg_to_reg.reg_result = result;

            switch (td->kind) {
                case TYPE_BOOL: in.op = OP_NOT; break;
                default: UNREACHABLE();
            }

            emit_instruction(function, loc, in);
            break;
        }

        case TOKEN_MINUS: {
            instruction_t in = {0};
            in.as.unary_reg_to_reg.reg_op = op1;
            in.as.unary_reg_to_reg.reg_result = result;

            switch (td->kind) {
                case TYPE_NUMBER: {
                    switch (td->as.num) {
                        case NUM_TYPE_FLOAT: in.op = OP_NEGATED; break;
                        case NUM_TYPE_SIGNED: in.op = OP_NEGATEI; break;
                        case NUM_TYPE_UNSIGNED: UNREACHABLE(); break;
                    }
                    break;
                }

                default: UNREACHABLE();
            }

            emit_instruction(function, loc, in);
            break;
        }

        case TOKEN_STAR: {
            type_t t = type;
            ASSERT(td->kind == TYPE_POINTER, "only allowed on pointer types");
            t = td->as.ptr.type;
            emit_addr_to_reg(gen, function, loc, type2movsize(gen, t), result, op1, 0);
            break;
        }

        default: UNREACHABLE();
    }
}

// call convention is to pass a reg with a function_t* to the call instruction arguments
static void emit_call(function_t *function, reg_t reg, texloc_t loc) {
    instruction_t in = {0};
    in.op = OP_CALL;
    in.as.call.reg_op = (byte)reg;
    emit_instruction(function, loc, in);
}

static void emit_intrinsic_call(function_t *function, reg_t callee, reg_t reg_arg_bottom_memaddr, reg_t reg_result_size, texloc_t loc) {
    instruction_t in = {0};
    in.op = OP_INTRINSIC_CALL;
    in.as.call.reg_op = (byte)callee;
    in.as.call.reg_arg_bottom_memaddr = reg_arg_bottom_memaddr;
    in.as.call.reg_result = REG_RESULT;
    in.as.call.reg_result_size = reg_result_size;

    emit_instruction(function, loc, in);
}

static void emit_return(texloc_t loc, function_t *function) {
    instruction_t instruction = {0};
    instruction.op = OP_RETURN;

    emit_instruction(function, loc, instruction);
}

static void gen_pop_until_stack_point(gen_t *gen, function_t *function, texloc_t location, size_t stack_point, bool update_gen) {
    ASSERT(stack_point <= gen->stack_size, "stack point cannot be larger than current stack size");
    size_t pop_amount = 0;
    for (size_t i = gen->locals.count; i > 0; --i) {
        size_t i_ = i-1;
        local_t *local = &gen->locals.items[i_];
        if (local->stack_location <= stack_point) {
            break;
        }
        ++pop_amount;
    }

    if (update_gen) gen->locals.count -= pop_amount;

    size_t pop_size_bytes = gen->stack_size - stack_point;
    ASSERT(pop_size_bytes % WORD_SIZE == 0, "stuff being popped off stack should be word aligned");

    emit_popn_bytes(gen, function, pop_size_bytes, location, update_gen);
}

static void gen_return(gen_t *gen, texloc_t loc, function_t *function) {
    gen_pop_until_stack_point(gen, function, loc, 0, false);
    emit_return(loc, function);
}

static void gen_expression(gen_t *gen, function_t *function, ast_node_t *expression);

static size_t gen_jmp_if_reg(function_t *function, texloc_t location, reg_t condition_reg, bool jmp_condition) {
    instruction_t instruction = {0};
    instruction.op = OP_JMP_IF_COND;
    instruction.as.jmp.amount = 0;
    instruction.as.jmp.condition_reg = condition_reg;
    instruction.as.jmp.check_for = jmp_condition;

    size_t index = function->code.count;
    emit_instruction(function, location, instruction);
    return index;
}

static void gen_patch_jmp(gen_t *gen, function_t *function, size_t index) {
    size_t amount = function->code.count - index;
    if (amount > UINT32_MAX) {
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_JMP_TOO_LARGE));
        return;
    }

    instruction_t *instruction = &function->code.items[index];
    instruction->as.jmp.amount = (u32)amount;
}

static void add_constant(gen_t *gen, function_t *function, texloc_t loc, void *data, size_t size, reg_t reg_destination_ptr) {
    size_t index = memarr_push(function->memory, data, size);
    if (index > UINT_MAX) {
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG));
        return;
    }

    instruction_t in = {0};
    in.op = OP_LOAD_ADDR;
    in.as.load_addr.memaddr = (u32)index;
    in.as.load_addr.reg_dest = reg_destination_ptr;

    emit_instruction(function, loc, in);
}

static void gen_constant(gen_t *gen, texloc_t location, function_t *function, void *data, type_t type) {
    typedata_t *td = type2typedata(&gen->ast->type_set.types, type);

    add_constant(gen, function, location, data, td->size, REG_RESULT);

    if (td->size <= WORD_SIZE) {
        emit_addr_to_reg(gen, function, location, type2movsize(gen, type), REG_RESULT, REG_RESULT, 0);
    }
}

static void gen_binary(gen_t *gen, function_t *function, ast_node_t *binary) {
    token_type_t op = binary->operator.type;
    if (operator_is_arithmetic(op) || operator_is_comparing(op) || operator_is_equating(op)) {
        ast_node_t *lhs = an_lhs(binary);
        gen_expression(gen, function, lhs);

        emit_push_reg(gen, token_end_loc(&lhs->end), function, REG_RESULT, type2movsize(gen, lhs->value_type), 0);

        ast_node_t *rhs = an_rhs(binary);
        gen_expression(gen, function, rhs);

        typedata_t *lhstd = type2typedata(&gen->ast->type_set.types, lhs->value_type);
        typedata_t *rhstd = type2typedata(&gen->ast->type_set.types, rhs->value_type);
        typedata_t *optd = lhstd;
        {
            if (lhstd->kind == TYPE_POINTER || rhstd->kind == TYPE_POINTER) {
                optd = type2typedata(&gen->ast->type_set.types, gen->ast->type_set.u64_);
            }

            // do the implicit multiplication required for ptr arithmetic
            if (lhstd->kind == TYPE_POINTER && typeid_eq(rhs->value_type, gen->ast->type_set.ptrdiff_t_)) {
                texloc_t loc = token_end_loc(&rhs->end);
                emit_push_reg(gen, loc, function, REG_RESULT, type2movsize(gen, rhs->value_type), 0);

                typedata_t *lhsinnertd = type2typedata(&gen->ast->type_set.types, lhstd->as.ptr.type);

                size_t factor_size = lhsinnertd->size > 0 ? lhsinnertd->size : sizeof(u8);

                gen_constant(gen, loc, function, &factor_size, gen->ast->type_set.size_t_);

                emit_pop_to_reg(gen, loc, function, REG_TMP, rhs->value_type);

                emit_bin_op(loc, function, TOKEN_STAR, rhstd, REG_TMP, REG_RESULT, REG_RESULT);
            }
        }

        emit_pop_to_reg(gen, token_end_loc(&rhs->end), function, REG_TMP, lhs->value_type);

        emit_bin_op(token_end_loc(&binary->end), function, binary->operator.type, optd, REG_TMP, REG_RESULT, REG_RESULT);

    } else if (operator_is_logical(op)) {
        ast_node_t *lhs = an_lhs(binary);
        gen_expression(gen, function, lhs);

        bool jmp_condition = (binary->operator.type == TOKEN_AND) ? false : true;
        size_t and_or_jmp = gen_jmp_if_reg(function, token_end_loc(&lhs->end), REG_RESULT, jmp_condition);

        ast_node_t *rhs = an_rhs(binary);
        gen_expression(gen, function, rhs);
        
        gen_patch_jmp(gen, function, and_or_jmp);
    } else {
        UNREACHABLE();
    }
}

static void gen_local_addr(texloc_t loc, function_t *function, local_t *local, reg_t dest) {
    emit_binu_reg_im(function, loc, dest, REG_STACK_FRAME, (u32)local->stack_location, '-');
}

static local_t *find_local(gen_t *gen, ast_node_t *ref_decl) {
    for (size_t i = gen->locals.count; i >= 1; --i) {
        size_t i_ = i-1;
        local_t *local = &gen->locals.items[i_];
        if (local->ref_decl == ref_decl) {
            return local;
        }
    }

    UNREACHABLE();
    return NULL;
}

size_t get_inner_data_type(ast_t *ast, type_t type) {
    typedata_t *td = ast_type2td(ast, type);

    switch (td->kind) {
    case TYPE_ARRAY: {
        typedata_t *inner = ast_type2td(ast, td->as.arr.type);
        return inner->size;
    }

    case TYPE_POINTER: {
        typedata_t *inner = ast_type2td(ast, td->as.ptr.type);
        return inner->size;
    }

    default: UNREACHABLE();
    }
}

static void gen_item_access_array_addr(gen_t *gen, function_t *function, texloc_t loc, reg_t reg_dst, reg_t reg_lvalue, ast_node_t *accessor, type_t inner_lvalue_type) {
        // push lvalue
    emit_push_reg(gen, loc, function, reg_lvalue, REG_MOV_SIZE_WORD, 0);

    gen_expression(gen, function, accessor);

    type_t accessor_type = accessor->value_type;

    // push offset
    reg_mov_size_t accessor_mov_size = type2movsize(gen, accessor_type);

    emit_push_reg(gen, loc, function, REG_RESULT, accessor_mov_size, 0);

    {
        size_t inner_data_size = get_inner_data_type(gen->ast, inner_lvalue_type);
        // size of td size in result reg
        gen_constant(gen, loc, function, &inner_data_size, gen->ast->type_set.u64_);

        typedata_t *td = ast_type2td(gen->ast, gen->ast->type_set.u64_);
        // pop offset and multiply by td size
        emit_pop_to_reg(gen, loc, function, REG_TMP, accessor_type);
        emit_bin_op(loc, function, TOKEN_STAR, td, REG_TMP, REG_RESULT, REG_RESULT);

        // pop lvalue and add it with offset
        emit_pop_to_reg(gen, loc, function, REG_TMP, gen->ast->type_set.u64_);
        emit_bin_op(loc, function, TOKEN_PLUS, td, REG_TMP, REG_RESULT, reg_dst);
    }
}

static void gen_lvalue(gen_t *gen, function_t *function, ast_node_t *lvalue) {
    // special case if no lvalue
    // generate the expression, put it on the stack, and take the address of the stack position
    // is responsibility of caller to pop items off the stack if necessary afterwards
    if (an_is_none(lvalue->lvalue_node)) {
        UNREACHABLE(); // todo
        return;
    }

    lvalue = lvalue->lvalue_node;

    switch (lvalue->node_type) {
    case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
        ast_node_t *ref_decl = lvalue->ref_decl;

        local_t *local = find_local(gen, ref_decl);

        gen_local_addr(lvalue->end.loc, function, local, REG_RESULT);
        break;
    }

    case AST_NODE_TYPE_EXPRESSION_UNARY: {
        ASSERT(lvalue->operator.type == TOKEN_STAR, "must");

        // gen address
        ast_node_t *addr_node = an_operand(lvalue);
        gen_expression(gen, function, addr_node);
        break;
    }

    case AST_NODE_TYPE_EXPRESSION_ITEM_ACCESS: {
        ast_node_t *accessee = an_item_accessee(lvalue);

        ast_node_t *inner_lvalue = accessee->lvalue_node;

        gen_lvalue(gen, function, inner_lvalue);

        texloc_t end = token_end_loc(&lvalue->end);

        ast_node_t *accessor = an_item_accessor(lvalue);
        gen_item_access_array_addr(gen, function, end, REG_RESULT, REG_RESULT, accessor, inner_lvalue->value_type);
        break;
    }

    default: UNREACHABLE(); break;
    }

}

static void gen_unary(gen_t *gen, function_t *function, ast_node_t *unary) {
    ast_node_t *expr = an_expression(unary);

    if (unary->operator.type == TOKEN_AMPERSAND) {
        gen_lvalue(gen, function, an_operand(unary)->lvalue_node);
    } else {
        gen_expression(gen, function, expr);

        emit_unary(gen, token_end_loc(&unary->end), function, unary->operator.type, expr->value_type, REG_RESULT, REG_RESULT);
    }
}

static void gen_expr_val(gen_t *gen, function_t *function, ast_node_t *expression, ast_node_val_t val_override) {
    if (TYPE_IS_VOID(expression->value_type)) {
        // nop
        return;
    }

    ASSERT(expression->expr_val.is_concrete || val_override.is_concrete, "must contain concrete value");

    typedata_t *type_info = type2typedata(&gen->ast->type_set.types, expression->value_type);

    // this ensures the data is always indexable
    if (function->memory->count+type_info->size >= function->memory->capacity) {
        gen_error(gen, make_error_node(ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG, expression));
    }

    ast_node_val_t val_ = expression->expr_val;
    if (val_override.is_concrete) val_ = val_override;

    word_t data = val_.word;
    if (type_info->size > WORD_SIZE) {
        gen_constant(gen, expression->start.loc, function, data.as.p, expression->value_type);
        emit_push_reg(gen, expression->start.loc, function, REG_RESULT, REG_MOV_SIZE_MULTIWORD_ADDR, type_info->size);
    } else {
        switch (type_info->kind) {
        case TYPE_BOOL: {
            u8 val = (u8)data.as.u;
            gen_constant(gen, expression->start.loc, function, &val, expression->value_type);
        }

        case TYPE_NUMBER: {
            switch (type_info->as.num) {
            case NUM_TYPE_FLOAT: {
                switch ((num_size_t)type_info->size) {
                case NUM_SIZE_8:
                case NUM_SIZE_16: UNREACHABLE(); break;

                case NUM_SIZE_32: {
                    f32 val = (f32)data.as.d;
                    gen_constant(gen, expression->start.loc, function, &val, expression->value_type);
                    break;
                }

                case NUM_SIZE_64: {
                    f64 val = (f64)data.as.d;
                    gen_constant(gen, expression->start.loc, function, &val, expression->value_type);
                    break;
                }
                }
                break;
            }

            case NUM_TYPE_SIGNED:
            case NUM_TYPE_UNSIGNED: {
                switch ((num_size_t)type_info->size) {
                case NUM_SIZE_8: {
                    u8 val = (u8)data.as.u;
                    gen_constant(gen, expression->start.loc, function, &val, expression->value_type);
                    break;
                }
                case NUM_SIZE_16: {
                    u16 val = (u16)data.as.u;
                    gen_constant(gen, expression->start.loc, function, &val, expression->value_type);
                    break;
                }

                case NUM_SIZE_32: {
                    u32 val = (u32)data.as.u;
                    gen_constant(gen, expression->start.loc, function, &val, expression->value_type);
                    break;
                }

                case NUM_SIZE_64: {
                    u64 val = (u64)data.as.u;
                    gen_constant(gen, expression->start.loc, function, &val, expression->value_type);
                    break;
                }
                }
                break;
            }
            }
            break;
        }

        case TYPE_POINTER:
        case TYPE_TYPE:
        case TYPE_FUNCTION:
        case TYPE_INTRINSIC_FUNCTION:
        case TYPE_ARRAY:
        case TYPE_STRUCT: {
            gen_constant(gen, expression->start.loc, function, &data, expression->value_type);
            break;
        }

        case TYPE_STRING: UNREACHABLE(); break;
        
        case TYPE_VOID:
        case TYPE_UNREACHABLE:
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_INFERRED_FUNCTION:
        case TYPE_COUNT: UNREACHABLE(); break;
        }
    }
}

static void gen_add_local(gen_t *gen, ast_node_t *declaration, size_t stack_location) {
    local_t local;
    local.ref_decl = declaration;
    local.stack_location = stack_location;

    array_push(&gen->locals, local);
}

static void gen_local_decldef(gen_t *gen, function_t *function, ast_node_t *declaration) {
    gen_expression(gen, function, an_decl_expr(declaration));

    typedata_t *td = type2typedata(&gen->ast->type_set.types, declaration->value_type);

    if (td->size <= WORD_SIZE) {
        emit_push_reg(gen, token_end_loc(&declaration->end), function, REG_RESULT, type2movsize(gen, declaration->value_type), 0);
    }

    size_t local_location = gen->stack_size;
    gen_add_local(gen, declaration, local_location);
}

static size_t gen_global_decldef(gen_t *gen, ast_node_t *decldef) {
    typedata_t *td = type2typedata(&gen->ast->type_set.types, decldef->value_type);
    u8 data[td->size];
    memset(data, 0, td->size);
    size_t global_index = memarr_push(gen->program_memory, &data, td->size);

    ASSERT(decldef->identifier.view.length != 0, "cannot be an implicit def");

    table_put(ptr2sz, gen->decl2index, decldef, global_index);

    return global_index;
}

static size_t gen_stack_point(gen_t *gen) {
    return gen->stack_size;
}

static void gen_statement(gen_t *gen, function_t *function, ast_node_t *expr) {
    size_t stack_point = gen_stack_point(gen);
    gen_expression(gen, function, expr);
    gen_pop_until_stack_point(gen, function, token_end_loc(&expr->end), stack_point, true);
}

static void gen_local_decl(gen_t *gen, function_t *function, ast_node_t *decl) {
    switch (decl->node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            if (decl->is_mutable) {
                gen_local_decldef(gen, function, decl);
            } else {
                // todo
            }
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            ast_node_t *expression = an_expression(decl);
            gen_statement(gen, function, expression);
            break;
        }

        default: UNREACHABLE();
    }
}

static void gen_block_decls(gen_t *gen, function_t *function, ast_node_t *block) {
    for (size_t i = 0; i < block->children.count; ++i) {
        ast_node_t *declaration = block->children.items[i];
        gen_local_decl(gen, function, declaration);
    }
}

static void gen_global_addr(texloc_t loc, function_t *function, size_t index, reg_t reg_dest) {
    instruction_t in = {0};
    in.op = OP_LOAD_ADDR;
    in.as.load_addr.reg_dest = reg_dest;
    in.as.load_addr.memaddr = index;
    emit_instruction(function, loc, in);
}

static void gen_def_value(gen_t *gen, function_t *function, ast_node_t *def) {
    size_t global_index;
    if (table_get(ptr2sz, gen->decl2index, def->ref_decl, &global_index)) {
        gen_global_addr(token_end_loc(&def->end), function, global_index, REG_RESULT);

        emit_addr_to_reg(gen, function, token_end_loc(&def->end), type2movsize(gen, def->value_type), REG_RESULT, REG_RESULT, 0);
    } else {
        local_t *local = find_local(gen, def->ref_decl);

        gen_local_addr(def->start.loc, function, local, REG_RESULT);

        texloc_t end = token_end_loc(&def->end);

        typedata_t *td = ast_type2td(gen->ast, def->value_type);
        if (td->size > WORD_SIZE) {
            emit_push_reg(gen, end, function, REG_RESULT, REG_MOV_SIZE_MULTIWORD_ADDR, td->size);
            emit_reg_to_reg(function, end, REG_RESULT, REG_STACK_BOTTOM);
        } else {
            emit_addr_to_reg(gen, function, end, type2movsize(gen, def->value_type), REG_RESULT, REG_RESULT, 0);
        }
    }
}

static size_t gen_jmp(function_t *function, texloc_t location) {
    instruction_t instruction = {0};
    instruction.op = OP_JMP;
    instruction.as.jmp.amount = 0;

    size_t index = function->code.count;
    emit_instruction(function, location, instruction);
    return index;
}

static void gen_loop(gen_t *gen, function_t *function, texloc_t location, u32 loop_index) {
    size_t amount = function->code.count - loop_index;
    if (amount > UINT32_MAX) {
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_JMP_TOO_LARGE));
        return;
    }
    instruction_t in = {0};
    in.op = OP_LOOP;
    in.as.jmp.amount = (u32)amount;
    emit_instruction(function, location, in);
}

static void gen_patch_jmps(gen_t *gen, function_t *function, ast_node_t *expr, token_type_t jmp_type) {
    for (size_t i = 0; i < expr->jmp_nodes.count; ++i) {
        ast_node_t *jmp_node = expr->jmp_nodes.items[i];
        if (jmp_node->start.type != jmp_type) continue;
        size_t code_jmp_index = jmp_node->vm_jmp_index;
        gen_patch_jmp(gen, function, code_jmp_index);
    }
}

static size_t gen_condition(gen_t *gen, ast_node_t *branch, function_t *function) {
    emit_push_reg(gen, branch->start.loc, function, REG_RESULT, REG_MOV_SIZE_WORD, 0);

    ast_node_t *condition = an_condition(branch);
    gen_expression(gen, function, condition);

    emit_reg_to_reg(function, token_end_loc(&condition->end), REG_TMP, REG_RESULT);

    emit_pop_to_reg(gen, token_end_loc(&condition->end), function, REG_RESULT, gen->ast->type_set.u64_);

    // then index
    return gen_jmp_if_reg(function, token_end_loc(&condition->end), REG_TMP, branch->condition_negated ? true : false);
}

static void gen_branching(gen_t *gen, function_t *function, ast_node_t *branch) {
    size_t stack_point = gen_stack_point(gen);

    if (branch->branch_type == BRANCH_TYPE_FOR && an_is_notnone(an_for_decl(branch))) {
        gen_local_decl(gen, function, an_for_decl(branch));
    }

    branch->vm_stack_point = gen_stack_point(gen);

    switch (branch->branch_type) {
        case BRANCH_TYPE_DO: {
            gen_expression(gen, function, an_then(branch));
            gen_patch_jmps(gen, function, branch, TOKEN_CONTINUE);

            gen_expression(gen, function, an_else(branch));
            gen_patch_jmps(gen, function, branch, TOKEN_BREAK);
            break;
        }
        case BRANCH_TYPE_IFTHEN: {
            size_t then_index = gen_condition(gen, branch, function);
            gen_expression(gen, function, an_then(branch));

            size_t else_index = gen_jmp(function, token_end_loc(&an_then(branch)->end));

            gen_patch_jmp(gen, function, then_index);

            gen_expression(gen, function, an_else(branch));

            gen_patch_jmp(gen, function, else_index);
            break;
        }

        case BRANCH_TYPE_FOR:
        case BRANCH_TYPE_LOOPING: {
            size_t loop_index = function->code.count;
            size_t then_index = gen_condition(gen, branch, function);
            gen_expression(gen, function, an_then(branch));

            gen_patch_jmps(gen, function, branch, TOKEN_CONTINUE);

            if (branch->branch_type == BRANCH_TYPE_FOR && an_is_notnone(an_for_incr(branch))) {
                gen_statement(gen, function, an_for_incr(branch));
            }

            gen_loop(gen, function, token_end_loc(&an_then(branch)->end), loop_index);

            size_t else_index = gen_jmp(function, token_end_loc(&an_then(branch)->end));

            gen_patch_jmp(gen, function, then_index);

            gen_expression(gen, function, an_else(branch));
            gen_patch_jmps(gen, function, branch, TOKEN_BREAK);
            gen_patch_jmp(gen, function, else_index);
            break;
        }
    }

    gen_pop_until_stack_point(gen, function, token_end_loc(&branch->end), stack_point, true);
}

static void gen_assignment(gen_t *gen, function_t *function, ast_node_t *assignment) {
    ast_node_t *lhs = an_lhs(assignment);
    gen_lvalue(gen, function, lhs);

    emit_push_reg(gen, token_end_loc(&lhs->end), function, REG_RESULT, REG_MOV_SIZE_WORD, 0);
    // emit_push_reg(gen, (texloc_t){.line=1000}, function, REG_RESULT, REG_MOV_SIZE_WORD, 0);

    ast_node_t *rhs = an_rhs(assignment);
    gen_expression(gen, function, rhs);

    emit_pop_to_reg(gen, token_end_loc(&rhs->end), function, REG_TMP, gen->ast->type_set.u64_);

    emit_reg_to_addr(gen, function, token_end_loc(&assignment->end),
            type2movsize(gen, assignment->value_type), REG_TMP, REG_RESULT, 0);

    emit_addr_to_reg(gen, function, token_end_loc(&assignment->end),
            type2movsize(gen, assignment->value_type), REG_RESULT, REG_TMP, 0);
}


static void gen_jmp_expr(gen_t *gen, function_t *function, ast_node_t *jmp_expr) {
    gen_expression(gen, function, an_expression(jmp_expr));

    typedata_t *td = ast_type2td(gen->ast, an_expression(jmp_expr)->value_type);

    switch (jmp_expr->start.type) {
        case TOKEN_CONTINUE:
        case TOKEN_BREAK: {
            size_t stack_point = jmp_expr->jmp_out_scope_node->vm_stack_point;
            if (td->size > WORD_SIZE) {
                stack_point += b2w(td->size)*WORD_SIZE;

                emit_binu_reg_im(function, token_end_loc(&jmp_expr->end), REG_TMP, REG_STACK_FRAME, stack_point, '-');
                emit_multiword_addr_to_addr(gen, function, token_end_loc(&jmp_expr->end), REG_TMP, REG_RESULT, REG_TMP2, td->size);
            }

            gen_pop_until_stack_point(gen, function, token_end_loc(&jmp_expr->end), stack_point, false);
            jmp_expr->vm_jmp_index = gen_jmp(function, token_end_loc(&jmp_expr->end));
            break;
        }

        case TOKEN_RETURN: {
            if (td->size > WORD_SIZE) {
                emit_binu_reg_im(function, token_end_loc(&jmp_expr->end), REG_TMP, REG_STACK_FRAME, WORD_SIZE, '+');
                emit_multiword_addr_to_addr(gen, function, token_end_loc(&jmp_expr->end), REG_TMP, REG_RESULT, REG_TMP2, td->size);
                emit_reg_to_reg(function, token_end_loc(&jmp_expr->end), REG_RESULT, REG_TMP);
            }

            gen_return(gen, token_end_loc(&jmp_expr->end), function);
            break;
        }

        default: UNREACHABLE();

    }
}

static gen_t make_gen(ast_t *ast, memarr_t *program_memory, error_function_t error_fn, arena_t *gen_arena, arena_t *program_arena) {
    gen_t gen = {0};
    gen.ast = ast;
    gen.error_fn = error_fn;
    gen.gen_arena = gen_arena;
    gen.locals.allocator = gen_arena;
    gen.program_arena = program_arena;
    gen.program_memory = program_memory;
    gen.breached_stack_limit = false;
    gen.decl2index = table_new(ptr2sz, gen_arena);

    return gen;
}

void gen_funcdef(ast_t *ast, env_t *env, ast_node_t *funcdef, error_function_t error_fn) {
    ASSERT(funcdef->expr_val.is_concrete, "should have something there");
    function_t *function = (function_t*)funcdef->expr_val.word.as.p;
    if (function_is_compiled(function)) return;

    tmp_arena_t *tmp = allocator_borrow();
    gen_t gen = make_gen(ast, env->memory, error_fn, tmp->allocator, env->arena);

    for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
        ast_node_t *arg = funcdef->children.items[i];
        typedata_t *type_info = type2typedata(&gen.ast->type_set.types, arg->value_type);
        gen.stack_size += b2w(type_info->size)*WORD_SIZE;
        gen_add_local(&gen, arg, gen.stack_size);
    }

    ast_node_t *block = an_func_def_block(funcdef);
    gen_block_decls(&gen, function, block);

    gen_return(&gen, token_end_loc(&funcdef->end), function);

    allocator_return(tmp);
}

static void emit_reserve_stack_space(gen_t *gen, texloc_t loc, function_t *function, size_t size_bytes) {
    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, size_bytes, '-');
    gen->stack_size += size_bytes;
}

static void gen_call(gen_t *gen, function_t *function, ast_node_t *call, bool is_intrinsic) {
    typedata_t *call_td = ast_type2td(gen->ast,  call->value_type);
    if (call_td->size > WORD_SIZE) {
        emit_reserve_stack_space(gen, call->start.loc, function,  call_td->size);
    }

    size_t stack_point = gen_stack_point(gen);

    // store stack frame
    emit_push_reg(gen, call->start.loc, function, REG_STACK_FRAME, REG_MOV_SIZE_WORD, 0);

    // place arguments on stack for call
    size_t argument_size_words = 0;
    unless (TYPE_IS_VOID(call->children.items[an_call_arg_start(call)]->value_type)) {
        for (size_t i = an_call_arg_start(call); i < an_call_arg_end(call); ++i) {
            ast_node_t *arg = call->children.items[i];
            typedata_t *td = type2typedata(&gen->ast->type_set.types, arg->value_type);
            gen_expression(gen, function, arg);
            if (td->size <= WORD_SIZE) {
                emit_push_reg(gen, token_end_loc(&arg->end), function, REG_RESULT, type2movsize(gen, arg->value_type), 0);
            }
            argument_size_words += b2w(td->size);
        }
    }

    // puts size of result on stack if intrinsic
    if (is_intrinsic) {
        typedata_t *resulttd = type2typedata(&gen->ast->type_set.types, call->value_type);
        gen_constant(gen, an_callee(call)->start.loc, function, &resulttd->size, gen->ast->type_set.size_t_);
        emit_push_reg(gen, an_callee(call)->start.loc, function, REG_RESULT, REG_MOV_SIZE_WORD, 0);
    }

    // prepare callee for call by putting it in the result register
    gen_expression(gen, function, an_callee(call));

    if (is_intrinsic) {
        emit_pop_to_reg(gen, token_end_loc(&call->end), function, REG_TMP, gen->ast->type_set.size_t_);
        emit_intrinsic_call(function, REG_RESULT, REG_STACK_BOTTOM, REG_TMP, token_end_loc(&call->end));

        gen_pop_until_stack_point(gen, function, token_end_loc(&call->end), stack_point, true);

    } else {
        // replace stack frame
        emit_binu_reg_im(function, call->start.loc, REG_STACK_FRAME, REG_STACK_BOTTOM, argument_size_words*WORD_SIZE, '+');

        emit_call(function, REG_RESULT, token_end_loc(&call->end));

        // call consumes arguments
        gen->stack_size -= argument_size_words*WORD_SIZE;

        // restore stack frame
        emit_pop_to_reg(gen, token_end_loc(&call->end), function, REG_STACK_FRAME, gen->ast->type_set.u64_);
    }
}

static void gen_bcall(gen_t *gen, function_t *function, ast_node_t *call) {
    size_t arg1_index = an_bcall_arg_start(call);
    for (size_t i = arg1_index; i < an_bcall_arg_end(call); ++i) {
        ast_node_t *arg = call->children.items[i];
        gen_expression(gen, function, arg);
    }

    switch (call->identifier.type) {
    case TOKEN_TYPEOF: {
        ast_node_val_t val = call->children.items[arg1_index]->expr_val;
        gen_expr_val(gen, function, call, val);
        break;
    }

    case TOKEN_SIZEOF: {
        typedata_t *td = type2typedata(&gen->ast->type_set.types, call->children.items[arg1_index]->expr_val.word.as.t);
        ast_node_val_t val = ast_node_val_word(WORDU(td->size));
        gen_expr_val(gen, function, call, val);
        break;
    }

    default: UNREACHABLE(); break;
    }
}

static void emit_cast(gen_t *gen, function_t *function, type_t dest, type_t source, texloc_t loc) {
    typedata_t *desttd = type2typedata(&gen->ast->type_set.types, dest);
    typedata_t *sourcetd = type2typedata(&gen->ast->type_set.types, source);

    if (desttd->kind == TYPE_NUMBER && sourcetd->kind == TYPE_NUMBER) {
        ASSERT(desttd->kind == TYPE_NUMBER && desttd->kind == sourcetd->kind, "must both be number types for now");

        #define EMIT_CAST(dst_sz, ...) do { if (desttd->size == (dst_sz)) { \
            op_code_t ins[] = {__VA_ARGS__}; \
            size_t amount = sizeof((op_code_t[]){__VA_ARGS__})/sizeof(op_code_t); \
            for (size_t i = 0; i < amount; ++i) { \
                instruction_t in = {0}; \
                in.op = ins[i]; \
                in.as.casting.reg_op = REG_RESULT; \
                in.as.casting.reg_result = REG_RESULT; \
                emit_instruction(function, loc, in); \
            } \
        }} while(false)

        // registers only hold 64bit numbers, so all lower bit types are automatically widened when put into a register

        if (desttd->as.num == NUM_TYPE_SIGNED && sourcetd->as.num == NUM_TYPE_SIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_L2B);
            EMIT_CAST(NUM_SIZE_16, OP_L2S);
            EMIT_CAST(NUM_SIZE_32, OP_L2I);
        } else if (desttd->as.num == NUM_TYPE_UNSIGNED && sourcetd->as.num == NUM_TYPE_SIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_L2UL, OP_UL2UB);
            EMIT_CAST(NUM_SIZE_16, OP_L2UL, OP_UL2US);
            EMIT_CAST(NUM_SIZE_32, OP_L2UL, OP_UL2U);
            EMIT_CAST(NUM_SIZE_64, OP_L2UL);
        } else if (desttd->as.num == NUM_TYPE_FLOAT && sourcetd->as.num == NUM_TYPE_SIGNED) {
            switch ((num_size_t)sourcetd->size) {
                case NUM_SIZE_8: {
                    EMIT_CAST(NUM_SIZE_32, OP_B2F);
                    EMIT_CAST(NUM_SIZE_64, OP_B2D);
                    break;
                }

                case NUM_SIZE_16: {
                    EMIT_CAST(NUM_SIZE_32, OP_S2F);
                    EMIT_CAST(NUM_SIZE_64, OP_S2D);
                    break;
                }

                case NUM_SIZE_32: {
                    EMIT_CAST(NUM_SIZE_32, OP_I2F);
                    EMIT_CAST(NUM_SIZE_64, OP_I2D);
                    break;
                }
                case NUM_SIZE_64: {
                    EMIT_CAST(NUM_SIZE_32, OP_L2F);
                    EMIT_CAST(NUM_SIZE_64, OP_L2D);
                    break;
                }
            }
        } else if (desttd->as.num == NUM_TYPE_UNSIGNED && sourcetd->as.num == NUM_TYPE_UNSIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_UL2UB);
            EMIT_CAST(NUM_SIZE_16, OP_UL2US);
            EMIT_CAST(NUM_SIZE_32, OP_UL2U);
        } else if (desttd->as.num == NUM_TYPE_SIGNED && sourcetd->as.num == NUM_TYPE_UNSIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_UL2L, OP_L2B);
            EMIT_CAST(NUM_SIZE_16, OP_UL2L, OP_L2S);
            EMIT_CAST(NUM_SIZE_32, OP_UL2L, OP_L2I);
            EMIT_CAST(NUM_SIZE_64, OP_UL2L);
        } else if (desttd->as.num == NUM_TYPE_FLOAT && sourcetd->as.num == NUM_TYPE_UNSIGNED) {
            switch ((num_size_t)sourcetd->size) {
                case NUM_SIZE_8: {
                    EMIT_CAST(NUM_SIZE_32, OP_UB2F);
                    EMIT_CAST(NUM_SIZE_64, OP_UB2D);
                    break;
                }

                case NUM_SIZE_16: {
                    EMIT_CAST(NUM_SIZE_32, OP_US2F);
                    EMIT_CAST(NUM_SIZE_64, OP_US2D);
                    break;
                }

                case NUM_SIZE_32: {
                    EMIT_CAST(NUM_SIZE_32, OP_U2F);
                    EMIT_CAST(NUM_SIZE_64, OP_U2D);
                    break;
                }
                case NUM_SIZE_64: {
                    EMIT_CAST(NUM_SIZE_32, OP_UL2F);
                    EMIT_CAST(NUM_SIZE_64, OP_UL2D);
                    break;
                }
            }
        } else if (desttd->as.num == NUM_TYPE_FLOAT && sourcetd->as.num == NUM_TYPE_FLOAT) {
            EMIT_CAST(NUM_SIZE_32, OP_D2F);
        } else if (desttd->as.num == NUM_TYPE_UNSIGNED && sourcetd->as.num == NUM_TYPE_FLOAT) {
            EMIT_CAST(NUM_SIZE_8, OP_D2UL, OP_UL2UB);
            EMIT_CAST(NUM_SIZE_16, OP_D2UL, OP_UL2US);
            EMIT_CAST(NUM_SIZE_32, OP_D2UL, OP_UL2U);
            EMIT_CAST(NUM_SIZE_64, OP_D2UL);
        } else if (desttd->as.num == NUM_TYPE_SIGNED && sourcetd->as.num == NUM_TYPE_FLOAT) {
            EMIT_CAST(NUM_SIZE_8, OP_D2L, OP_L2B);
            EMIT_CAST(NUM_SIZE_16, OP_D2L, OP_L2S);
            EMIT_CAST(NUM_SIZE_32, OP_D2L, OP_L2I);
            EMIT_CAST(NUM_SIZE_64, OP_D2L);
        }
        #undef EMIT_CAST

    } else {
        ASSERT(desttd->size == sourcetd->size, "this at least must hold");
        // nop necessary to cast both are 64bit pointers so they can be interpreted the same
    }

}

static void gen_cast(gen_t *gen, function_t *function, ast_node_t *cast) {
    ast_node_t *expr = an_cast_expr(cast);
    gen_expression(gen, function, expr);

    emit_cast(gen, function, cast->value_type, expr->value_type, token_end_loc(&cast->end));
}

static void gen_item_access(gen_t *gen, function_t *function, ast_node_t *item_access) {
    ast_node_t *accessee = an_item_accessee(item_access);
    typedata_t *accessee_td = ast_type2td(gen->ast, accessee->value_type);
    

    if (an_is_notnone(item_access->lvalue_node)) {
        ASSERT(accessee_td->kind == TYPE_ARRAY, "only array type for now");

        gen_lvalue(gen, function, item_access->lvalue_node);

        // move data from reg result addr to result reg
        emit_addr_to_reg(gen, function, token_end_loc(&item_access->end), type2movsize(gen, item_access->value_type), REG_RESULT, REG_RESULT, 0);
    } else {
        ASSERT(accessee_td->kind == TYPE_ARRAY, "only array type for now");

        typedata_t *item_access_td = ast_type2td(gen->ast, item_access->value_type);
        size_t result_stack_point = gen->stack_size + b2w(item_access_td->size)*WORD_SIZE;

        gen_expression(gen, function, accessee);

        texloc_t start = token_implicit_at_start(accessee->start).loc;
        if (accessee_td->size <= WORD_SIZE) {
            emit_push_reg(gen, start, function, REG_RESULT, type2movsize(gen, accessee->value_type), 0);
            emit_reg_to_reg(function, start, REG_RESULT, REG_STACK_BOTTOM);
        }


        ast_node_t *accessor = an_item_accessor(item_access);

        gen_item_access_array_addr(gen, function, token_end_loc(&accessor->start), REG_RESULT, REG_RESULT, accessor, accessee->value_type);

        texloc_t end = token_end_loc(&item_access->end);
        emit_binu_reg_im(function, end, REG_TMP, REG_STACK_FRAME, result_stack_point, '-');

        if (item_access_td->size > WORD_SIZE) {
            emit_multiword_addr_to_addr(gen, function, end, REG_RESULT, REG_TMP, REG_TMP2, item_access_td->size);
        } else {
            emit_addr_to_reg(gen, function, end, type2movsize(gen, item_access->value_type), REG_RESULT, REG_RESULT, 0);
            emit_reg_to_addr(gen, function, end, type2movsize(gen, item_access->value_type), REG_TMP, REG_RESULT, 0);
        }

        gen_pop_until_stack_point(gen, function, end, result_stack_point, true);

        if (item_access_td->size <= WORD_SIZE) {
            emit_pop_to_reg(gen, end, function, REG_RESULT, item_access->value_type);
        } else {
            emit_reg_to_reg(function, end, REG_RESULT, REG_STACK_BOTTOM);
        }
    }
}

static void gen_initializer_list(gen_t *gen, function_t *function, ast_node_t *list) {
    typedata_t *list_type_td = ast_type2td(gen->ast, list->value_type);

    switch (list_type_td->kind) {
        case TYPE_ARRAY: {
            emit_reserve_stack_space(gen, list->start.loc, function, b2w(list_type_td->size)*WORD_SIZE);

            for (size_t i = an_list_start(list); i < an_list_end(list); ++i) {
                size_t i_ = i - an_list_start(list);
                ast_node_t *arg = list->children.items[i];

                gen_expression(gen, function, arg);
                typedata_t *arg_td = ast_type2td(gen->ast, arg->value_type);
                if (arg_td->size > WORD_SIZE) {
                    emit_popn_bytes(gen, function, b2w(arg_td->size)*WORD_SIZE, token_end_loc(&arg->end), true);
                    // emit_addr_to_reg(gen, function, token_end_loc(&arg->end), type2movsize(gen, arg->value_type), REG_TMP, REG_RESULT, i_*arg_td->size);
                    // emit_reg_to_addr(gen, function, token_end_loc(&arg->end), type2movsize(gen, arg->value_type), REG_STACK_BOTTOM, REG_TMP, i_*arg_td->size);

                    emit_binu_reg_im(function, token_end_loc(&arg->end), REG_TMP, REG_STACK_BOTTOM, arg_td->size*i_, '+');

                    emit_multiword_addr_to_addr(gen, function, token_end_loc(&arg->end), REG_TMP, REG_RESULT, REG_TMP2, arg_td->size);

                } else {
                    emit_reg_to_addr(gen, function, token_end_loc(&arg->end), type2movsize(gen, arg->value_type), REG_STACK_BOTTOM, REG_RESULT, i_*arg_td->size);
                }
            }

            if (list_type_td->size > WORD_SIZE) {
                emit_reg_to_reg(function, token_end_loc(&list->end), REG_RESULT, REG_STACK_BOTTOM);
            } else {
                emit_addr_to_reg(gen, function, token_end_loc(&list->end), REG_MOV_SIZE_WORD, REG_RESULT, REG_STACK_BOTTOM, 0);
                emit_popn_bytes(gen, function, b2w(list_type_td->size)*WORD_SIZE, list->start.loc, true);
            }
            break;
        }

        default: UNREACHABLE(); break;
    }
}

static void gen_block(gen_t *gen, function_t *function, ast_node_t *block) {
    size_t stack_point = gen_stack_point(gen);
    typedata_t *td = ast_type2td(gen->ast, block->value_type);
    if (td->size > WORD_SIZE) {
        stack_point += b2w(td->size)*WORD_SIZE;
    }
    gen_block_decls(gen, function, block);
    gen_pop_until_stack_point(gen, function, token_end_loc(&block->end), stack_point, true);
}

static void gen_expression(gen_t *gen, function_t *function, ast_node_t *expression) {
    ASSERT(ast_node_type_is_expression(expression->node_type), "must be expression");

    if (expression->expr_val.is_concrete) {
        gen_expr_val(gen, function, expression, ast_node_val_nil());
        return;
    }

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: UNREACHABLE(); break;

        case AST_NODE_TYPE_EXPRESSION_NIL: {
            ASSERT(TYPE_IS_VOID(expression->value_type), "this should be folded already if not void");
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            gen_binary(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            gen_unary(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            gen_expression(gen, function, an_operand(expression));
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: {
            UNREACHABLE(); // todo
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            gen_block(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            gen_def_value(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            gen_branching(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            gen_assignment(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            gen_jmp_expr(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            typedata_t *calleetd = type2typedata(&gen->ast->type_set.types, an_callee(expression)->value_type);
            bool is_intrinsic = (calleetd->kind == TYPE_INTRINSIC_FUNCTION);
            gen_call(gen, function, expression, is_intrinsic);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: {
            gen_bcall(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            gen_cast(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ITEM_ACCESS: {
            gen_item_access(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
            gen_initializer_list(gen, function, expression);
            break;
        }


        // should be resolved at compile time
        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: UNREACHABLE(); break;

        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE:
        case AST_NODE_TYPE_EXPRESSION_DOT:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_NONE:
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_MODULE: ASSERT(false, "not implemented");
    }
}

static void gen_module(gen_t *gen, ast_node_t *module, function_t *init_func) {
    for (size_t i = 0; i < module->children.count; ++i) {
        ast_node_t *decldef = module->children.items[i];
        size_t index = gen_global_decldef(gen, decldef);

        ast_node_t *type_expr = an_decl_type(decldef);
        gen_global_addr(token_end_loc(&type_expr->end), init_func, index, REG_RESULT);
        emit_push_reg(gen, token_end_loc(&type_expr->end), init_func, REG_RESULT, REG_MOV_SIZE_WORD, 0);

        ast_node_t *expr = an_decl_expr(decldef);
        gen_expression(gen, init_func, expr);

        emit_pop_to_reg(gen, token_end_loc(&expr->end), init_func, REG_TMP, gen->ast->type_set.size_t_);

        emit_reg_to_addr(gen, init_func, token_end_loc(&expr->end), type2movsize(gen, expr->value_type), REG_TMP, REG_RESULT, 0);
    }
}

bool compile_modules(ast_t *ast, error_function_t error_fn, memarr_t *program_memory, arena_t *program_arena, function_t *global_init_func) {
    arena_t arena = {0};

    gen_t gen = make_gen(ast, program_memory, error_fn, &arena, program_arena);

    ast_node_t *module;
    kh_foreach_value(ast->moduleid2node, module, gen_module(&gen, module, global_init_func));

    emit_return(token_end_loc(&module->end), global_init_func);

    return !gen.had_error;
}

bool compile_expr_to_function(function_t *function, ast_t *ast, ast_node_t *expr, error_function_t error_fn, memarr_t *program_memory, arena_t *program_arena) {
    arena_t arena = {0};

    gen_t gen = make_gen(ast, program_memory, error_fn, &arena, program_arena);

    gen_expression(&gen, function, expr);

    emit_return(token_end_loc(&expr->end), function);

    arena_free(&arena);
    
    return gen.had_error;
}

bool compile_program(vm_t *vm, ast_t *ast) {
    UNUSED(vm);
    UNUSED(ast);
    return false;
    // tmp_arena_t *tmp = allocator_borrow();

    // ast_node_t *root = ast->root;
    // assert(root->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK);

    // ast_node_t *main_node = NULL;
    // string_view_t main_name = cstr2sv("main");
    // for (size_t i = 0; i < root->as.block.count; ++i) {
    //     ast_node_t *node = root->as.block.items[i];
    //     assert(node->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION);

    //     string_view_t identifier = token2sv(node->as.declaration.identifier);
    //     if (sv_eq(identifier, main_name)) {
    //         main_node = node;
    //         break;
    //     }
    // }

    // if (!main_node) {
    //     return false;
    // }

    // ast_node_t *initial_expression = main_node->as.declaration.initial_value_expression;
    // if (initial_expression->value_index < 0) {
    //     return false;
    // }

    // if (!type_is_function(ast->type_set.types, initial_expression->value_type)) {
    //     return false;
    // }

    // // TODO: ignoring the parameters for main

    // assert(initial_expression->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION);

    // //size_t instruction_index = builder.code.count;

    // gen_t gen = {0};
    // gen.ast = ast;
    // gen.error_fn = error_fn;
    // gen_block(&gen, initial_expression->as.function.block);

    // allocator_return(tmp);

    // return true;
}

#undef CODEGEN_IMPLEMENTATION
#endif
