#ifndef CODEGEN_H_
#define CODEGENH_

#include "parser.h"
#include "vm.h"
#include "error.h"
#include "slot.h"

bool compile_program(vm_t *vm, ast_t *ast);

void gen_function_def(ast_t *ast, env_t *env, ast_node_t *funcdef, error_function_t error_fn);
bool compile_expr_to_function(function_t *function, ast_t *ast, ast_node_t *expr, error_function_t error_fn, arena_t *function_arena);

#endif

#ifdef CODEGEN_IMPLEMENTATION

typedef struct local_t local_t;
struct local_t {
    size_t stack_location;
    ast_node_t *ref_decl;
};

typedef struct gen_t gen_t;
struct gen_t {
    error_function_t error_fn;
    ast_t *ast;
    size_t stack_size;
    arena_t *gen_arena;
    arena_t *program_arena;

    struct {
        local_t *items;
        size_t count;
        size_t capacity;
        arena_t *allocator;
    } locals;

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

static void emit_read_memory_to_reg(texloc_t location, function_t *function, reg_t destination, memaddr_t address, typedata_t *type_info) {
    ASSERT(type_info->size <= WORD_SIZE, "only word-sized values or smaller can go into registers");

    if (type_info->size == 0) {
        // nop
        return;
    }

    instruction_t instruction;
    instruction.op = OP_MOVWORD_MEM_TO_REG;

    switch (type_info->kind) {
        case TYPE_BOOL: {
            instruction.op = OP_MOVU8_MEM_TO_REG;
            break;
        }

        case TYPE_POINTER:
        case TYPE_TYPE:
        case TYPE_FUNCTION: {
            if (type_info->size != WORD_SIZE) {
                TODO("other than numbers, type should be solely handled by size");
            }

            instruction.op = OP_MOVWORD_MEM_TO_REG;
            break;
        }

        case TYPE_NUMBER: {
            switch ((num_size_t)type_info->size) {
                case NUM_SIZE_8: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: UNREACHABLE(); break;
                        case NUM_TYPE_SIGNED:
                        case NUM_TYPE_UNSIGNED: instruction.op = OP_MOVU8_MEM_TO_REG; break;
                    }
                    break;
                }

                case NUM_SIZE_16: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: UNREACHABLE(); break;
                        case NUM_TYPE_SIGNED:
                        case NUM_TYPE_UNSIGNED: instruction.op = OP_MOVU16_MEM_TO_REG; break;
                    }
                    break;
                }

                case NUM_SIZE_32: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: instruction.op = OP_MOVF32_MEM_TO_REG; break;
                        case NUM_TYPE_SIGNED:
                        case NUM_TYPE_UNSIGNED: instruction.op = OP_MOVU32_MEM_TO_REG; break;
                    }
                    break;
                }

                case NUM_SIZE_64: break;

                default: UNREACHABLE();
            }
            break;
        }

        default: UNREACHABLE();
    }

    instruction.as.mov_mem_to_reg.mem_address = address;
    instruction.as.mov_mem_to_reg.reg_result = destination;

    emit_instruction(function, location, instruction);
}

typedef enum reg_mov_type_t reg_mov_type_t;
enum reg_mov_type_t {
    REG_MOV_TYPE_MEM_TO_REG,
    REG_MOV_TYPE_REG_TO_MEM,
    REG_MOV_TYPE_ADDR_TO_REG,
    REG_MOV_TYPE_REG_TO_ADDR,
    REG_MOV_TYPE_REG_TO_REG,
};

static void emit_mov_reg_to_reg(gen_t *gen, function_t *function, texloc_t loc, type_t type, reg_mov_type_t mov_type, reg_t dest, reg_t src) {
    typedata_t *td = type2typedata(&gen->ast->type_set.types, type);
    if (td->kind == TYPE_VOID) return; // noop

    op_code_t op_u8;
    op_code_t op_u16;
    op_code_t op_u32;
    op_code_t op_f32;
    op_code_t op_word;

    switch (mov_type) {
    case REG_MOV_TYPE_ADDR_TO_REG: {
        op_u8 = OP_MOVU8_REGADDR_TO_REG;
        op_u16 = OP_MOVU16_REGADDR_TO_REG;
        op_u32 = OP_MOVU32_REGADDR_TO_REG;
        op_f32 = OP_MOVF32_REGADDR_TO_REG;
        op_word = OP_MOVWORD_REGADDR_TO_REG;
        break;
    }

    case REG_MOV_TYPE_REG_TO_ADDR: {
        op_u8 = OP_MOVU8_REG_TO_REGADDR;
        op_u16 = OP_MOVU16_REG_TO_REGADDR;
        op_u32 = OP_MOVU32_REG_TO_REGADDR;
        op_f32 = OP_MOVF32_REG_TO_REGADDR;
        op_word = OP_MOVWORD_REG_TO_REGADDR;
        break;
    }

    case REG_MOV_TYPE_MEM_TO_REG: {
        op_u8 = OP_MOVU8_REGMEM_TO_REG;
        op_u16 = OP_MOVU16_REGMEM_TO_REG;
        op_u32 = OP_MOVU32_REGMEM_TO_REG;
        op_f32 = OP_MOVF32_REGMEM_TO_REG;
        op_word = OP_MOVWORD_REGMEM_TO_REG;
        break;
    }

    case REG_MOV_TYPE_REG_TO_MEM: {
        op_u8 = OP_MOVU8_REG_TO_REGMEM;
        op_u16 = OP_MOVU16_REG_TO_REGMEM;
        op_u32 = OP_MOVU32_REG_TO_REGMEM;
        op_f32 = OP_MOVF32_REG_TO_REGMEM;
        op_word = OP_MOVWORD_REG_TO_REGMEM;
        break;
    }

    case REG_MOV_TYPE_REG_TO_REG: {
        op_u8 = OP_MOV_REG_TO_REG;
        op_u16 = OP_MOV_REG_TO_REG;
        op_u32 = OP_MOV_REG_TO_REG;
        op_f32 = OP_MOV_REG_TO_REG;
        op_word = OP_MOV_REG_TO_REG;
        break;
    }
    }

    op_code_t mov_op = op_word;

    switch (td->kind) {
    case TYPE_NUMBER: {
        switch (td->data.num) {
        case NUM_TYPE_SIGNED:
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: mov_op = op_u8; break;
            case NUM_SIZE_16: mov_op = op_u16; break;
            case NUM_SIZE_32: mov_op = op_u32; break;
            case NUM_SIZE_64: mov_op = op_word; break;
            }
            break;
        }

        case NUM_TYPE_FLOAT: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8:
            case NUM_SIZE_16: UNREACHABLE(); break;
            case NUM_SIZE_32: mov_op = op_f32; break;
            case NUM_SIZE_64: mov_op = op_word; break;
            }
            break;
        }
        }
        break;
    }
    case TYPE_BOOL: mov_op = op_u8; break;

    case TYPE_FUNCTION:
    case TYPE_TYPE:
    case TYPE_POINTER: mov_op = op_word; break;

    case TYPE_NATIVE_FUNCTION:
    case TYPE_STRING: UNREACHABLE(); break;
    case TYPE_STRUCT: UNREACHABLE(); /*todo*/ break;

    case TYPE_VOID:
    case TYPE_UNRESOLVED:
    case TYPE_UNREACHABLE:
    case TYPE_INVALID:
    case TYPE_COUNT: UNREACHABLE(); break;
    }


    
    instruction_t in = {0};
    in.op = mov_op;
    in.as.mov_reg_to_reg.reg_destination = dest;
    in.as.mov_reg_to_reg.reg_source = src;

    emit_instruction(function, loc, in);
}


static void emit_binu_reg_im(function_t *function, texloc_t loc, byte reg_dest, byte reg_op, u32 immediate, char plus_or_minus) {
    instruction_t in = {0};
    in.op = plus_or_minus == '-' ? OP_SUBU_REG_IM32 : OP_ADDU_REG_IM32;
    in.as.binu_reg_immediate.immediate = immediate;
    in.as.binu_reg_immediate.reg_operand = reg_op;
    in.as.binu_reg_immediate.reg_result = reg_dest;

    emit_instruction(function, loc, in);
}

static void emit_push_reg(gen_t *gen, texloc_t loc, function_t *function, reg_t reg_source, type_t type) {
    gen->stack_size += sizeof(word_t);

    if (gen->stack_size > UINT32_MAX && !gen->breached_stack_limit) {
        gen->breached_stack_limit = true;
        gen_error(gen, make_error_no_args(ERROR_CODEGEN_STACK_SIZE_GROWS_LARGER_THAN_UINT32_MAX));
        
    }

    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, (u32)sizeof(word_t), '-');

    emit_mov_reg_to_reg(gen, function, loc, type, REG_MOV_TYPE_REG_TO_MEM, REG_STACK_BOTTOM, reg_source);
}

static void emit_pop_to_reg(gen_t *gen, texloc_t loc, function_t *function, reg_t reg_destination, type_t type) {
    gen->stack_size -= sizeof(word_t);

    emit_mov_reg_to_reg(gen, function, loc, type, REG_MOV_TYPE_MEM_TO_REG, reg_destination, REG_STACK_BOTTOM);
    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, (u32)sizeof(word_t), '+');
}

static void emit_popn_bytes(gen_t *gen, function_t *function, u32 pop_size_bytes, texloc_t loc, bool decrement_stack) {
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

            case TOKEN_EQUAL_EQUAL: instruction.op = OP_EQU_REG_REG; break;
            case TOKEN_BANG_EQUAL: instruction.op = OP_NQU_REG_REG; break;

            default: UNREACHABLE(); break;
        }
    }

    case TYPE_NUMBER: {
        switch (token_type) {
            case TOKEN_PLUS: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_ADDD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_ADDI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_ADDU_REG_REG; break;
                }
                break;
            }

            case TOKEN_MINUS: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_SUBD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_SUBI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_SUBU_REG_REG; break;
                }
                break;
            }

            case TOKEN_STAR: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_MULD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_MULI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_MULU_REG_REG; break;
                }
                break;
            }
            
            case TOKEN_SLASH: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_DIVD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_DIVI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_DIVU_REG_REG; break;
                }
                break;
            }

            case TOKEN_PERCENT: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_MODD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_MODI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_MODU_REG_REG; break;
                }
                break;
            }

            case TOKEN_PERCENT_PERCENT: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_REMD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_REMI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_REMU_REG_REG; break;
                }
                break;
            }

            case TOKEN_GREATER: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_GTD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_GTI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_GTU_REG_REG; break;
                }
                break;
            }

            case TOKEN_GREATER_EQUAL: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_GED_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_GEI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_GEU_REG_REG; break;
                }
                break;
            }

            case TOKEN_LESS:{
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_LTD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_LTI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_LTU_REG_REG; break;
                }
                break;
            }

            case TOKEN_LESS_EQUAL: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_LED_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_LEI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_LEU_REG_REG; break;
                }
                break;
            }

            case TOKEN_EQUAL_EQUAL: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_EQD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_EQI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_EQU_REG_REG; break;
                }
                break;
            }

            case TOKEN_BANG_EQUAL: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: instruction.op = OP_NQD_REG_REG; break;
                    case NUM_TYPE_SIGNED: instruction.op = OP_NQI_REG_REG; break;
                    case NUM_TYPE_UNSIGNED: instruction.op = OP_NQU_REG_REG; break;
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
    typedata_t *type_info = type2typedata(&gen->ast->type_set.types, type);

    switch (token_type) {
        case TOKEN_NOT: {
            instruction_t in = {0};
            in.as.unary_reg_to_reg.reg_op = op1;
            in.as.unary_reg_to_reg.reg_result = result;

            switch (type_info->kind) {
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

            switch (type_info->kind) {
                case TYPE_NUMBER: {
                    switch (type_info->data.num) {
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
            emit_mov_reg_to_reg(gen, function, loc, type, REG_MOV_TYPE_ADDR_TO_REG, result, op1);
            break;
        }

        default: UNREACHABLE();
    }
}

static void emit_mem_to_addr(texloc_t loc, function_t *function, reg_t destination, reg_t source) {
    instruction_t in = {0};
    in.op = OP_MOV_REGMEM_TO_REGADDR;
    in.as.mov_reg_to_reg.reg_destination = (byte)destination;
    in.as.mov_reg_to_reg.reg_source = (byte)source;

    emit_instruction(function, loc, in);
}

// call convention is to pass a reg with a function_t* to the call instruction arguments
static void emit_call(function_t *function, reg_t reg, texloc_t loc) {
    instruction_t in = {0};
    in.op = OP_CALL;
    in.as.call.reg_op = (byte)reg;
    emit_instruction(function, loc, in);
}

static void emit_return(texloc_t loc, function_t *function) {
    instruction_t instruction = {0};
    instruction.op = OP_RETURN;

    emit_instruction(function, loc, instruction);
}

static void gen_expression(gen_t *gen, function_t *function, ast_node_t *expression);

static size_t gen_jmp_if_reg(function_t *function, texloc_t location, reg_t condition_reg, bool jmp_condition) {
    instruction_t instruction = {0};
    instruction.op = OP_JMP_IF_REG_CONDITION;
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

static void gen_binary(gen_t *gen, function_t *function, ast_node_t *binary) {
    token_type_t op = binary->operator.type;
    if (operator_is_arithmetic(op) || operator_is_comparing(op) || operator_is_equating(op)) {
        ast_node_t *lhs = an_lhs(binary);
        gen_expression(gen, function, lhs);

        emit_push_reg(gen, token_end_location(&lhs->end), function, REG_RESULT, lhs->value_type);

        ast_node_t *rhs = an_rhs(binary);
        gen_expression(gen, function, rhs);

        emit_pop_to_reg(gen, token_end_location(&rhs->end), function, REG_TMP, lhs->value_type);

        typedata_t *expr_type_info = type2typedata(&gen->ast->type_set.types, an_lhs(binary)->value_type);

        emit_bin_op(token_end_location(&binary->end), function, binary->operator.type, expr_type_info, REG_TMP, REG_RESULT, REG_RESULT);
    } else if (operator_is_logical(op)) {
        ast_node_t *lhs = an_lhs(binary);
        gen_expression(gen, function, lhs);

        bool jmp_condition = (binary->operator.type == TOKEN_AND) ? false : true;
        size_t and_or_jmp = gen_jmp_if_reg(function, token_end_location(&lhs->end), REG_RESULT, jmp_condition);

        ast_node_t *rhs = an_rhs(binary);
        gen_expression(gen, function, rhs);
        
        gen_patch_jmp(gen, function, and_or_jmp);
    } else {
        UNREACHABLE();
    }
}

static void gen_local_memaddr(gen_t *gen, texloc_t loc, function_t *function, local_t *local, reg_t dest) {
    UNUSED(gen);
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

static void gen_unary(gen_t *gen, function_t *function, ast_node_t *unary) {
    ast_node_t *expr = an_expression(unary);

    if (unary->operator.type == TOKEN_AMPERSAND) {
        ast_node_t *ref_decl = an_operand(unary)->lvalue_node->ref_decl;
        local_t *local = find_local(gen, ref_decl);

        gen_local_memaddr(gen, token_end_location(&unary->end), function, local, REG_RESULT);

        emit_mem_to_addr(token_end_location(&unary->end), function, REG_RESULT, REG_RESULT);
    } else {
        gen_expression(gen, function, expr);

        emit_unary(gen, token_end_location(&unary->end), function, unary->operator.type, expr->value_type, REG_RESULT, REG_RESULT);
    }
}

static value_index_t add_constant(function_t *function, void *data, size_t size) {
    size_t index = memarr_push(function->memory, data, size);
    return value_index_(index);
}

static void gen_constant(texloc_t location, function_t *function, void *data, typedata_t *type_info) {
    value_index_t value_index = add_constant(function, data, type_info->size);

    if (type_info->size <= WORD_SIZE) {
        emit_read_memory_to_reg(location, function, REG_RESULT, value_index.index, type_info);
    } else {
        ASSERT(false, "todo");
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

    ast_node_val_t val = expression->expr_val;
    if (val_override.is_concrete) val = val_override;

    if (type_info->size > WORD_SIZE) {
        // todo
        UNREACHABLE();
    } else {
        word_t data = val.word;
        gen_constant(expression->start.loc, function, &data, type_info);
    }
}

static void gen_add_local(gen_t *gen, ast_node_t *declaration, size_t stack_location) {
    local_t local;
    local.ref_decl = declaration;
    local.stack_location = stack_location;

    array_push(&gen->locals, local);
}

static void gen_local_decl_def(gen_t *gen, function_t *function, ast_node_t *declaration) {
    gen_expression(gen, function, an_decl_expr(declaration));
    emit_push_reg(gen, token_end_location(&declaration->end), function, REG_RESULT, declaration->value_type);

    size_t local_location = gen->stack_size;
    gen_add_local(gen, declaration, local_location);
}

static void gen_decl(gen_t *gen, function_t *function, ast_node_t *decl, bool is_global) {
    if (is_global) {
        switch (decl->node_type) {
            case AST_NODE_TYPE_DECLARATION_DEFINITION: {
                // todo
                UNREACHABLE();
                break;
            }
            default: UNREACHABLE();
        }
    } else {
        switch (decl->node_type) {
            case AST_NODE_TYPE_DECLARATION_DEFINITION: {
                gen_local_decl_def(gen, function, decl);
                break;
            }

            case AST_NODE_TYPE_DECLARATION_STATEMENT: {
                ast_node_t *expression = an_expression(decl);
                gen_expression(gen, function, expression);
                break;
            }

            default: UNREACHABLE();
        }
    }
}

static void gen_block(gen_t *gen, function_t *function, ast_node_t *block) {
    for (size_t i = 0; i < block->children.count; ++i) {
        ast_node_t *declaration = block->children.items[i];
        gen_decl(gen, function, declaration, false);
    }
}

static size_t gen_stack_point(gen_t *gen) {
    return gen->stack_size;
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

static void gen_def_value(gen_t *gen, function_t *function, ast_node_t *def) {
    local_t *local = find_local(gen, def->ref_decl);

    gen_local_memaddr(gen, def->start.loc, function, local, REG_RESULT);

    texloc_t end = token_end_location(&def->end);
    emit_mov_reg_to_reg(gen, function, end, def->value_type, REG_MOV_TYPE_MEM_TO_REG, REG_RESULT, REG_RESULT);
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
    emit_push_reg(gen, branch->start.loc, function, REG_RESULT, gen->ast->type_set.u64_);

    ast_node_t *condition = an_condition(branch);
    gen_expression(gen, function, condition);

    emit_mov_reg_to_reg(gen, function, token_end_location(&condition->end), condition->value_type, REG_MOV_TYPE_REG_TO_REG, REG_TMP, REG_RESULT);

    emit_pop_to_reg(gen, token_end_location(&condition->end), function, REG_RESULT, gen->ast->type_set.u64_);

    // then index
    return gen_jmp_if_reg(function, token_end_location(&condition->end), REG_TMP, branch->condition_negated ? true : false);
}

static void gen_branching(gen_t *gen, function_t *function, ast_node_t *branch) {
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

            size_t else_index = gen_jmp(function, token_end_location(&an_then(branch)->end));

            gen_patch_jmp(gen, function, then_index);

            gen_expression(gen, function, an_else(branch));

            gen_patch_jmp(gen, function, else_index);
            break;
        }
        case BRANCH_TYPE_LOOPING: {
            size_t loop_index = function->code.count;
            size_t then_index = gen_condition(gen, branch, function);
            gen_expression(gen, function, an_then(branch));

            gen_patch_jmps(gen, function, branch, TOKEN_CONTINUE);
            gen_loop(gen, function, token_end_location(&an_then(branch)->end), loop_index);

            size_t else_index = gen_jmp(function, token_end_location(&an_then(branch)->end));

            gen_patch_jmp(gen, function, then_index);

            gen_expression(gen, function, an_else(branch));
            gen_patch_jmps(gen, function, branch, TOKEN_BREAK);
            gen_patch_jmp(gen, function, else_index);
            break;
        }
    }
}
static void gen_assignment(gen_t *gen, function_t *function, ast_node_t *assignment) {
    ast_node_t *rhs = an_rhs(assignment);
    gen_expression(gen, function, rhs);

    ast_node_t *lhs = an_lhs(assignment);
    if (lhs->lvalue_node->node_type == AST_NODE_TYPE_EXPRESSION_DEF_VALUE) {
        ast_node_t *ref_decl = lhs->lvalue_node->ref_decl;

        local_t *local = find_local(gen, ref_decl);

        gen_local_memaddr(gen, assignment->end.loc, function, local, REG_TMP);

        emit_mov_reg_to_reg(gen, function, token_end_location(&assignment->end),
                assignment->value_type, REG_MOV_TYPE_REG_TO_MEM, REG_TMP, REG_RESULT);

    } else if (lhs->lvalue_node->node_type == AST_NODE_TYPE_EXPRESSION_UNARY) {
        ASSERT(lhs->operator.type == TOKEN_STAR, "must be dereferencing");

        emit_push_reg(gen, rhs->end.loc, function, REG_RESULT, gen->ast->type_set.u64_);

        // gen address
        ast_node_t *addr_node = an_operand(lhs->lvalue_node);
        gen_expression(gen, function, addr_node);

        texloc_t end = token_end_location(&assignment->end);
        emit_pop_to_reg(gen, end, function, REG_TMP, gen->ast->type_set.u64_);

        emit_mov_reg_to_reg(gen, function, end,
                assignment->value_type, REG_MOV_TYPE_REG_TO_ADDR, REG_RESULT, REG_TMP);
    } else {
        UNREACHABLE();
    }
}


static void gen_jmp_expr(gen_t *gen, function_t *function, ast_node_t *jmp_expr) {
    gen_expression(gen, function, an_expression(jmp_expr));
    switch (jmp_expr->start.type) {
        case TOKEN_CONTINUE:
        case TOKEN_BREAK: {
            size_t stack_point = jmp_expr->jmp_out_scope_node->vm_stack_point;
            gen_pop_until_stack_point(gen, function, token_end_location(&jmp_expr->end), stack_point, false);
            jmp_expr->vm_jmp_index = gen_jmp(function, token_end_location(&jmp_expr->end));
            break;
        }

        case TOKEN_RETURN: {
            gen_pop_until_stack_point(gen, function, token_end_location(&jmp_expr->end), 0, false);
            emit_return(token_end_location(&jmp_expr->end), function);
            break;
        }

        default: UNREACHABLE();

    }
}

static gen_t make_gen(ast_t *ast, error_function_t error_fn, arena_t *gen_arena, arena_t *function_arena) {
    gen_t gen = {0};
    gen.ast = ast;
    gen.error_fn = error_fn;
    gen.gen_arena = gen_arena;
    gen.locals.allocator = gen_arena;
    gen.program_arena = function_arena;
    gen.breached_stack_limit = false;

    return gen;
}

void gen_function_def(ast_t *ast, env_t *env, ast_node_t *funcdef, error_function_t error_fn) {
    ASSERT(funcdef->expr_val.is_concrete, "should have something there");
    function_t *function = (function_t*)funcdef->expr_val.word.as.p;
    if (function_is_compiled(function)) return;

    tmp_arena_t *tmp = allocator_borrow();
    gen_t gen = make_gen(ast, error_fn, tmp->allocator, env->arena);

    for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
        ast_node_t *arg = funcdef->children.items[i];
        typedata_t *type_info = type2typedata(&gen.ast->type_set.types, arg->value_type);
        gen.stack_size += bytes_to_words(type_info->size)*WORD_SIZE;
        gen_add_local(&gen, arg, gen.stack_size);
    }

    ast_node_t *block = an_func_def_block(funcdef);
    gen_block(&gen, function, block);

    allocator_return(tmp);
}

static void gen_call(gen_t *gen, function_t *function, ast_node_t *call) {
    // store stack frame
    emit_push_reg(gen, call->start.loc, function, REG_STACK_FRAME, gen->ast->type_set.u64_);

    // place arguments on stack for call
    size_t argument_size_words = 0;
    unless (TYPE_IS_VOID(call->children.items[an_call_arg_start(call)]->value_type)) {
        for (size_t i = an_call_arg_start(call); i < an_call_arg_end(call); ++i) {
            ast_node_t *arg = call->children.items[i];
            typedata_t *td = type2typedata(&gen->ast->type_set.types, arg->value_type);
            gen_expression(gen, function, arg);
            emit_push_reg(gen, token_end_location(&arg->end), function, REG_RESULT, arg->value_type);
            argument_size_words += bytes_to_words(td->size);
        }
    }

    // prepare callee for call by putting it in the result register
    gen_expression(gen, function, an_callee(call));

    // replace stack frame
    emit_binu_reg_im(function, call->start.loc, REG_STACK_FRAME, REG_STACK_BOTTOM, argument_size_words*WORD_SIZE, '+');

    emit_call(function, REG_RESULT, token_end_location(&call->end));

    // call consumes arguments
    gen->stack_size -= argument_size_words*WORD_SIZE;

    // restore stack frame
    emit_pop_to_reg(gen, token_end_location(&call->end), function, REG_STACK_FRAME, gen->ast->type_set.u64_);
}

static void gen_bcall(gen_t *gen, function_t *function, ast_node_t *call) {
    for (size_t i = an_bcall_arg_start(call); i < an_bcall_arg_end(call); ++i) {
        ast_node_t *arg = call->children.items[i];
        gen_expression(gen, function, arg);
    }

    switch (call->identifier.type) {
    case TOKEN_TYPEOF: {
        ast_node_val_t val = call->children.items[0]->expr_val;
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

        if (desttd->data.num == NUM_TYPE_SIGNED && sourcetd->data.num == NUM_TYPE_SIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_CAST_L2B);
            EMIT_CAST(NUM_SIZE_16, OP_CAST_L2S);
            EMIT_CAST(NUM_SIZE_32, OP_CAST_L2I);
        } else if (desttd->data.num == NUM_TYPE_UNSIGNED && sourcetd->data.num == NUM_TYPE_SIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_CAST_L2UL, OP_CAST_UL2UB);
            EMIT_CAST(NUM_SIZE_16, OP_CAST_L2UL, OP_CAST_UL2US);
            EMIT_CAST(NUM_SIZE_32, OP_CAST_L2UL, OP_CAST_UL2U);
            EMIT_CAST(NUM_SIZE_64, OP_CAST_L2UL);
        } else if (desttd->data.num == NUM_TYPE_FLOAT && sourcetd->data.num == NUM_TYPE_SIGNED) {
            EMIT_CAST(NUM_SIZE_32, OP_CAST_L2F);
            EMIT_CAST(NUM_SIZE_64, OP_CAST_L2D);
        } else if (desttd->data.num == NUM_TYPE_UNSIGNED && sourcetd->data.num == NUM_TYPE_UNSIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_CAST_UL2UB);
            EMIT_CAST(NUM_SIZE_16, OP_CAST_UL2US);
            EMIT_CAST(NUM_SIZE_32, OP_CAST_UL2U);
        } else if (desttd->data.num == NUM_TYPE_SIGNED && sourcetd->data.num == NUM_TYPE_UNSIGNED) {
            EMIT_CAST(NUM_SIZE_8, OP_CAST_UL2L, OP_CAST_L2B);
            EMIT_CAST(NUM_SIZE_16, OP_CAST_UL2L, OP_CAST_L2S);
            EMIT_CAST(NUM_SIZE_32, OP_CAST_UL2L, OP_CAST_L2I);
            EMIT_CAST(NUM_SIZE_64, OP_CAST_UL2L);
        } else if (desttd->data.num == NUM_TYPE_FLOAT && sourcetd->data.num == NUM_TYPE_UNSIGNED) {
            EMIT_CAST(NUM_SIZE_32, OP_CAST_UL2F);
            EMIT_CAST(NUM_SIZE_64, OP_CAST_UL2D);
        } else if (desttd->data.num == NUM_TYPE_FLOAT && sourcetd->data.num == NUM_TYPE_FLOAT) {
            EMIT_CAST(NUM_SIZE_32, OP_CAST_D2F);
        } else if (desttd->data.num == NUM_TYPE_UNSIGNED && sourcetd->data.num == NUM_TYPE_FLOAT) {
            EMIT_CAST(NUM_SIZE_8, OP_CAST_D2UL, OP_CAST_UL2UB);
            EMIT_CAST(NUM_SIZE_16, OP_CAST_D2UL, OP_CAST_UL2US);
            EMIT_CAST(NUM_SIZE_32, OP_CAST_D2UL, OP_CAST_UL2U);
            EMIT_CAST(NUM_SIZE_64, OP_CAST_D2UL);
        } else if (desttd->data.num == NUM_TYPE_SIGNED && sourcetd->data.num == NUM_TYPE_FLOAT) {
            EMIT_CAST(NUM_SIZE_8, OP_CAST_D2L, OP_CAST_L2B);
            EMIT_CAST(NUM_SIZE_16, OP_CAST_D2L, OP_CAST_L2S);
            EMIT_CAST(NUM_SIZE_32, OP_CAST_D2L, OP_CAST_L2I);
            EMIT_CAST(NUM_SIZE_64, OP_CAST_D2L);
        }
        #undef EMIT_CAST

    } else {
        ASSERT(desttd->size == sourcetd->size, "this at least must hold");
        // nop necessary to cast both are 64bit pointers so they can be interpreted the same
    }

}

static void gen_cast(gen_t *gen, function_t *function, ast_node_t *cast) {
    ast_node_t *expr = an_rhs(cast);
    gen_expression(gen, function, expr);

    emit_cast(gen, function, cast->value_type, expr->value_type, token_end_location(&cast->end));
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

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            size_t stack_point = gen_stack_point(gen);
            gen_block(gen, function, expression);
            gen_pop_until_stack_point(gen, function, token_end_location(&expression->end), stack_point, true);
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
            gen_call(gen, function, expression);
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


        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE:
        case AST_NODE_TYPE_EXPRESSION_DOT:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_NONE:
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_MODULE: ASSERT(false, "not implemented");
    }
}

bool compile_expr_to_function(function_t *function, ast_t *ast, ast_node_t *expr, error_function_t error_fn, arena_t *function_arena) {
    arena_t arena = {0};

    gen_t gen = make_gen(ast, error_fn, &arena, function_arena);

    gen_expression(&gen, function, expr);

    emit_return(token_end_location(&expr->end), function);
    
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
