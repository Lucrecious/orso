#ifndef CODEGEN_H_
#define CODEGENH_

#include "parser.h"
#include "vm.h"
#include "error.h"
#include "slot.h"

bool compile_program(vm_t *vm, ast_t *ast);

bool compile_expr_to_function(function_t *function, ast_t *expr_ast, error_function_t error_fn, arena_t *function_arena);

#endif

#ifdef CODEGEN_IMPLEMENTATION

typedef struct local_t local_t;
struct local_t {
    size_t stack_location;
    ast_node_t *ref_decl;
    size_t scope_level;
};

typedef struct gen_t gen_t;
struct gen_t {
    error_function_t error_fn;
    ast_t *ast;
    size_t stack_size;
    size_t scope_level;
    arena_t *gen_arena;
    arena_t *function_arena;

    struct {
        local_t *items;
        size_t count;
        size_t capacity;
        arena_t *allocator;
    } locals;

    bool had_error;
    bool breached_stack_limit;
};

typedef enum reg_t reg_t;
enum reg_t {
    REG_NULL = 0,
    REG_RESULT = 1, // 256 bytes for memory on stack

    REG_TMP = 33,
    REG_STACK_BOTTOM = 34,
    REG_STACK_FRAME = 35,
};

declare_table(str2cf, string_t, call_frame_t)

static void gen_error(gen_t *gen, error_t error) {
    gen->had_error = true;
    if (gen->error_fn) gen->error_fn(gen->ast, error);
}

static void emit_instruction(function_t *function, texloc_t location, instruction_t instruction) {
    array_push(&function->code, instruction);
    array_push(&function->locations, location);
}

static void emit_read_memory_to_reg(texloc_t location, function_t *function, reg_t destination, memaddr_t address, type_info_t *type_info) {
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

        case TYPE_FUNCTION: {
            instruction.op = OP_MOVWORD_MEM_TO_REG; break;
            break;
        }

        case TYPE_NUMBER: {
            switch (type_info->size) {
                case 1: UNREACHABLE(); break;
                case 2: UNREACHABLE(); break;

                case 4: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: instruction.op = OP_MOVF32_MEM_TO_REG; break;
                        case NUM_TYPE_SIGNED: instruction.op = OP_MOVF32_MEM_TO_REG; break;
                        case NUM_TYPE_UNSIGNED: instruction.op = OP_MOVU32_MEM_TO_REG; break;
                    }
                    break;
                }

                case 8: instruction.op = OP_MOVWORD_MEM_TO_REG; break;

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

static void emit_binu_reg_im(function_t *function, texloc_t loc, byte reg_dest, byte reg_op, u32 immediate, char plus_or_minus) {
    instruction_t in = {0};
    in.op = plus_or_minus == '-' ? OP_SUBU_REG_IM32 : OP_ADDU_REG_IM32;
    in.as.binu_reg_immediate.immediate = immediate;
    in.as.binu_reg_immediate.reg_operand = reg_op;
    in.as.binu_reg_immediate.reg_result = reg_dest;

    emit_instruction(function, loc, in);
}

void emit_push_wordreg(gen_t *gen, texloc_t loc, function_t *function, reg_t reg_source) {
    gen->stack_size += sizeof(word_t);

    if (gen->stack_size > UINT32_MAX && !gen->breached_stack_limit) {
        gen->breached_stack_limit = true;
        gen_error(gen, make_error(ERROR_CODEGEN_STACK_SIZE_GROWS_LARGER_THAN_UINT32_MAX));
        
    }

    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, (u32)sizeof(word_t), '-');

    instruction_t instruction = {0};
    {
        instruction.op = OP_MOVWORD_REG_TO_REGMEM;
        instruction.as.mov_reg_to_regmem.reg_source = reg_source;
        instruction.as.mov_reg_to_regmem.regmem_destination = REG_STACK_BOTTOM;

        emit_instruction(function, loc, instruction);
    }
}

void emit_pop_to_wordreg(gen_t *gen, texloc_t loc, function_t *function, reg_t reg_destination) {
    gen->stack_size -= sizeof(word_t);
    instruction_t instruction = {0};

    {
        instruction.op = OP_MOVWORD_REGMEM_TO_REG;
        instruction.as.mov_regmem_to_reg.regmem_source = REG_STACK_BOTTOM;
        instruction.as.mov_regmem_to_reg.reg_destination = reg_destination;

        emit_instruction(function, loc, instruction);
    }

    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, (u32)sizeof(word_t), '+');
}

static void emit_popn_words(gen_t *gen, function_t *function, u32 pop_size_words, texloc_t loc) {
    gen->stack_size -= pop_size_words*WORD_SIZE;

    emit_binu_reg_im(function, loc, REG_STACK_BOTTOM, REG_STACK_BOTTOM, pop_size_words*WORD_SIZE, '+');
}

static void emit_bin_arithmetic(texloc_t loc, function_t *function, token_type_t token_type, type_info_t *type_info, reg_t op1, reg_t op2, reg_t result) {
    instruction_t instruction = {0};
    ASSERT(type_info->kind == TYPE_NUMBER, "for now only numbers");

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

    instruction.as.bin_reg_to_reg.reg_op1 = (byte)op1;
    instruction.as.bin_reg_to_reg.reg_op2 = (byte)op2;
    instruction.as.bin_reg_to_reg.reg_result = (byte)result;

    emit_instruction(function, loc, instruction);
}

static void emit_unary(texloc_t loc, function_t *function, token_type_t token_type, type_info_t *type_info, reg_t op1, reg_t result) {
    instruction_t in = {0};
    in.as.unary_reg_to_reg.reg_op = op1;
    in.as.unary_reg_to_reg.reg_result = result;

    switch (token_type) {
        case TOKEN_NOT: {
            switch (type_info->kind) {
                case TYPE_BOOL: in.op = OP_NOT; break;
                default: UNREACHABLE();
            }
            break;
        }

        case TOKEN_MINUS: {
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
            break;
        }

        case TOKEN_PLUS_PLUS: {
            switch (type_info->kind) {
                case TYPE_NUMBER: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: in.op = OP_INCREMENTD; break;
                        case NUM_TYPE_SIGNED: in.op = OP_INCREMENTI; break;
                        case NUM_TYPE_UNSIGNED: in.op = OP_INCREMENTU; break;
                    }
                    break;
                }

                default: UNREACHABLE();
            }
            break;
        }

        case TOKEN_MINUS_MINUS: {
            switch (type_info->kind) {
                case TYPE_NUMBER: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: in.op = OP_DECREMENTD; break;
                        case NUM_TYPE_SIGNED: in.op = OP_DECREMENTI; break;
                        case NUM_TYPE_UNSIGNED: in.op = OP_DECREMENTU; break;
                    }
                    break;
                }

                default: UNREACHABLE();
            }
            break;
        }

        default: UNREACHABLE();
    }

    emit_instruction(function, loc, in);
}

static void emit_return(texloc_t loc, function_t *function) {
    instruction_t instruction = {0};
    instruction.op = OP_RETURN;

    emit_instruction(function, loc, instruction);
}

static void emit_mov_reg_to_reg(function_t *function, texloc_t location, byte reg_dest, byte reg_src) {
    instruction_t in = {0};
    in.op = OP_MOV_REG_TO_REG;
    in.as.mov_reg_to_reg.reg_destination = reg_dest;
    in.as.mov_reg_to_reg.reg_source = reg_src;

    emit_instruction(function, location, in);
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
        gen_error(gen, make_error(ERROR_CODEGEN_JMP_TOO_LARGE));
        return;
    }

    instruction_t *instruction = &function->code.items[index];
    instruction->as.jmp.amount = (u32)amount;
}

static void gen_binary(gen_t *gen, function_t *function, ast_node_t *binary) {
    switch (binary->operator.type) {
        case TOKEN_EQUAL_EQUAL:
        case TOKEN_BANG_EQUAL:
        case TOKEN_GREATER:
        case TOKEN_GREATER_EQUAL:
        case TOKEN_LESS:
        case TOKEN_LESS_EQUAL:

        case TOKEN_PERCENT_PERCENT:
        case TOKEN_PERCENT:
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_STAR:
        case TOKEN_SLASH: {
            ast_node_t *lhs = an_lhs(binary);
            gen_expression(gen, function, lhs);

            emit_push_wordreg(gen, token_end_location(&lhs->end), function, REG_RESULT);

            ast_node_t *rhs = an_rhs(binary);
            gen_expression(gen, function, rhs);

            emit_pop_to_wordreg(gen, token_end_location(&rhs->end), function, REG_TMP);

            type_info_t *expr_type_info = get_type_info(&gen->ast->type_set.types, an_lhs(binary)->value_type);

            emit_bin_arithmetic(token_end_location(&binary->end), function, binary->operator.type, expr_type_info, REG_TMP, REG_RESULT, REG_RESULT);
            break;
        }

        case TOKEN_AND:
        case TOKEN_OR: {
            ast_node_t *lhs = an_lhs(binary);
            gen_expression(gen, function, lhs);

            bool jmp_condition = (binary->operator.type == TOKEN_AND) ? false : true;
            size_t and_or_jmp = gen_jmp_if_reg(function, token_end_location(&lhs->end), REG_RESULT, jmp_condition);

            ast_node_t *rhs = an_rhs(binary);
            gen_expression(gen, function, rhs);
            
            gen_patch_jmp(gen, function, and_or_jmp);
            break;
        }

        default: UNREACHABLE(); break;
    }
}

static void gen_unary(gen_t *gen, function_t *function, ast_node_t *unary) {
    ast_node_t *expr = an_expression(unary);
    gen_expression(gen, function, expr);

    type_info_t *expr_type_info = get_type_info(&gen->ast->type_set.types, expr->value_type);

    emit_unary(token_end_location(&unary->end), function, unary->operator.type, expr_type_info, REG_RESULT, REG_RESULT);
}

static value_index_t add_constant(function_t *function, void *data, size_t size) {
    size_t index = memarr_push(function->memory, data, size);
    return value_index_(index);
}

static void gen_constant(texloc_t location, function_t *function, void *data, type_info_t *type_info) {
    value_index_t value_index = add_constant(function, data, type_info->size);

    if (type_info->size <= WORD_SIZE) {
        emit_read_memory_to_reg(location, function, REG_RESULT, value_index.index, type_info);
    } else {
        ASSERT(false, "todo");
    }
}

static void gen_folded_value(gen_t *gen, function_t *function, ast_node_t *expression) {
    if (TYPE_IS_VOID(expression->value_type)) {
        // nop
        return;
    }
    ASSERT(expression->value_index.exists, "must contain concrete value");

    type_info_t *type_info = get_type_info(&gen->ast->type_set.types, expression->value_type);

    // this ensures the data is always indexable
    if (function->memory->count+type_info->size >= function->memory->capacity) {
        gen_error(gen, make_error_node(ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG, expression));
    }

    void *data = memarr_get_ptr(&gen->ast->constants, expression->value_index);
    gen_constant(expression->start.loc, function, data, type_info);
}

static void gen_add_local(gen_t *gen, ast_node_t *declaration, size_t stack_location) {
    local_t local;
    local.ref_decl = declaration;
    local.stack_location = stack_location;
    local.scope_level = gen->scope_level;

    array_push(&gen->locals, local);
}

static void gen_local_decl_def(gen_t *gen, function_t *function, ast_node_t *declaration) {
    gen_expression(gen, function, an_decl_expr(declaration));
    emit_push_wordreg(gen, token_end_location(&declaration->end), function, REG_RESULT);

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

static void push_scope(gen_t *gen) {
    ++gen->scope_level;
}

static void gen_pop_scope(gen_t *gen, function_t *function, texloc_t location) {
    size_t truncate_to = gen->stack_size;
    size_t pop_amount = 0;
    for (size_t i = gen->locals.count; i > 0; --i) {
        size_t i_ = i-1;
        local_t *local = &gen->locals.items[i_];
        truncate_to = local->stack_location;
        if (local->scope_level < gen->scope_level) {
            break;
        }
        ++pop_amount;
    }

    gen->locals.count -= pop_amount;
    --gen->scope_level;

    size_t pop_size_bytes = gen->stack_size - truncate_to;
    ASSERT(pop_size_bytes % WORD_SIZE == 0, "stuff being popped off stack should be word aligned");

    size_t pop_size_words = (pop_size_bytes + (WORD_SIZE-1))/WORD_SIZE;
    emit_popn_words(gen, function, pop_size_words, location);
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

static void gen_def_value(gen_t *gen, function_t *function, ast_node_t *def) {
    local_t *local = find_local(gen, def->ref_decl);

    emit_binu_reg_im(function, def->start.loc, REG_RESULT, REG_STACK_FRAME, (u32)local->stack_location, '-');

    instruction_t instruction = {0};
    {
        instruction.op = OP_MOVWORD_REGMEM_TO_REG;
        instruction.as.mov_regmem_to_reg.regmem_source = REG_RESULT;
        instruction.as.mov_regmem_to_reg.reg_destination = REG_RESULT;

        emit_instruction(function, token_end_location(&def->end), instruction);
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
        gen_error(gen, make_error(ERROR_CODEGEN_JMP_TOO_LARGE));
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
    emit_push_wordreg(gen, branch->start.loc, function, REG_RESULT);

    gen_expression(gen, function, an_condition(branch));

    emit_mov_reg_to_reg(function, token_end_location(&an_condition(branch)->end), REG_TMP, REG_RESULT);

    emit_pop_to_wordreg(gen, token_end_location(&an_condition(branch)->end), function, REG_RESULT);

    // then index
    return gen_jmp_if_reg(function, token_end_location(&an_condition(branch)->end), REG_TMP, branch->condition_negated ? true : false);

}

static void gen_branching(gen_t *gen, function_t *function, ast_node_t *branch) {
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
    gen_expression(gen, function, an_rhs(assignment));

    ast_node_t *ref_decl = an_lhs(assignment)->ref_decl;

    local_t *local = find_local(gen, ref_decl);

    emit_binu_reg_im(function, assignment->end.loc, REG_TMP, REG_STACK_FRAME, (u32)local->stack_location, '-');

    instruction_t in = {0};
    {
        in.op = OP_MOVWORD_REG_TO_REGMEM;
        in.as.mov_reg_to_regmem.reg_source = REG_RESULT;
        in.as.mov_reg_to_regmem.regmem_destination = REG_TMP;

        emit_instruction(function, token_end_location(&assignment->end), in);
    }
}


static void gen_jmp_expr(gen_t *gen, function_t *function, ast_node_t *jmp_expr) {
    gen_expression(gen, function, an_expression(jmp_expr));
    switch (jmp_expr->start.type) {
        case TOKEN_CONTINUE:
        case TOKEN_BREAK: {
            jmp_expr->vm_jmp_index = gen_jmp(function, token_end_location(&jmp_expr->end));
            break;
        }

        case TOKEN_RETURN: {
            emit_popn_words(gen, function, bytes_to_words(gen->stack_size), token_end_location(&jmp_expr->end));
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
    gen.function_arena = function_arena;
    gen.breached_stack_limit = false;

    return gen;
}

static void gen_function_def(gen_t *parent_gen, function_t *parent_function, ast_node_t *function_def) {
    function_t *function = new_function(parent_function->file_path, parent_function->memory, parent_gen->function_arena);

    gen_t gen = make_gen(parent_gen->ast, parent_gen->error_fn, parent_gen->locals.allocator, parent_gen->function_arena);

    for (size_t i = an_func_def_arg_start(function_def); i < an_func_def_arg_end(function_def); ++i) {
        ast_node_t *arg = function_def->children.items[i];
        type_info_t *type_info = get_type_info(&gen.ast->type_set.types, arg->value_type);
        gen.stack_size += type_info->size;
        gen_add_local(&gen, arg, gen.stack_size);
    }

    gen_block(&gen, function, an_func_def_block(function_def));

    type_info_t *func_type_info = get_type_info(&parent_gen->ast->type_set.types, function_def->value_type);
    gen_constant(function_def->end.loc, parent_function, &function, func_type_info);
}

static void gen_call(gen_t *gen, function_t *function, ast_node_t *call) {
    // store stack frame
    emit_push_wordreg(gen, call->start.loc, function, REG_STACK_FRAME);

    // place arguments on stack for call
    size_t argument_size_words = 0;
    unless (TYPE_IS_VOID(call->children.items[an_call_arg_start(call)]->value_type)) {
        for (size_t i = an_call_arg_start(call); i < an_call_arg_end(call); ++i) {
            ast_node_t *arg = call->children.items[i];
            gen_expression(gen, function, arg);
            emit_push_wordreg(gen, token_end_location(&arg->end), function, REG_RESULT);
            ++argument_size_words;
        }
    }

    gen_expression(gen, function, an_callee(call));

    // replace stack frame
    emit_binu_reg_im(function, call->start.loc, REG_STACK_FRAME, REG_STACK_BOTTOM, argument_size_words*WORD_SIZE, '+');

    // call convention is to pass a reg with a function_t* to the call instruction arguments
    instruction_t in = {0};
    in.op = OP_CALL;
    in.as.call.reg_op = REG_RESULT;
    emit_instruction(function, token_end_location(&call->end), in);

    // call consumes arguments
    gen->stack_size -= argument_size_words*WORD_SIZE;

    // pop arguments and restore stack frame
    {
        emit_pop_to_wordreg(gen, token_end_location(&call->end), function, REG_STACK_FRAME);
    }
}

static void gen_expression(gen_t *gen, function_t *function, ast_node_t *expression) {
    ASSERT(ast_node_type_is_expression(expression->node_type), "must be expression");

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_NIL:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            gen_folded_value(gen, function, expression);
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
            push_scope(gen);
            gen_block(gen, function, expression);
            gen_pop_scope(gen, function, token_end_location(&expression->end));
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

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            gen_function_def(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            gen_call(gen, function, expression);
            break;
        }


        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
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

bool compile_expr_to_function(function_t *function, ast_t *ast, error_function_t error_fn, arena_t *function_arena) {
    arena_t arena = {0};

    gen_t gen = make_gen(ast, error_fn, &arena, function_arena);

    gen_expression(&gen, function, ast->root);

    emit_return(token_end_location(&ast->root->end), function);
    
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
