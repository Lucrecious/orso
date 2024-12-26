#ifndef CODEGEN_H_
#define CODEGENH_

#include "parser.h"
#include "vm.h"
#include "error.h"

bool compile_program(vm_t *vm, ast_t *ast);

bool compile_expr_to_function(function_t *function, ast_t *expr_ast);

#endif

#ifdef CODEGEN_IMPLEMENTATION

#define memarr_get_ptr(arr, value_index) ((arr)->data)+((value_index).index)

typedef struct gen_t gen_t;
struct gen_t {
    error_function_t error_fn;
    ast_t *ast;

    bool had_error;
};

typedef enum reg_t reg_t;
enum reg_t {
    REG_NULL = 0,
    REG_OPERAND1,
    REG_OPERAND2,

    REG_STACK_BOTTOM,
};

declare_table(str2cf, string_t, call_frame_t)

static string_view_t token2sv(token_t t) {
    return (string_view_t) {
        .data = t.start,
        .length = t.length,
    };
}

static void emit_read_memory_to_reg(function_t *function, reg_t destination, memaddr_t address, type_info_t *type_info) {
    ASSERT(type_info->size <= WORD_SIZE, "only word-sized values or smaller can go into registers");

    instruction_t instruction;
    instruction.op = OP_MOVWORD_MEM_TO_REG;

    if (type_info->kind == TYPE_BOOL) {
        instruction.op = OP_MOVU8_MEM_TO_REG;

    } else if (type_info->kind == TYPE_NUMBER) {
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
    } else {
        UNREACHABLE();
    }

    instruction.as.mov_mem_to_reg.mem_address = address;
    instruction.as.mov_mem_to_reg.reg_result = destination;

    array_push(&function->code, instruction);
}

void emit_push_wordreg(function_t *function, reg_t reg_source) {
    instruction_t instruction = {0};

    {
        instruction.op = OP_SUBU_REG_IM32;
        instruction.as.bin_regu_immediateu.reg_operand = REG_STACK_BOTTOM;
        instruction.as.bin_regu_immediateu.reg_result = REG_STACK_BOTTOM;
        instruction.as.bin_regu_immediateu.immediate = (u32)sizeof(word_t);

        array_push(&function->code, instruction);
    }

    {
        instruction.op = OP_MOVWORD_REG_TO_REGMEM;
        instruction.as.mov_reg_to_regmem.reg_source = reg_source;
        instruction.as.mov_reg_to_regmem.regmem_destination = REG_STACK_BOTTOM;

        array_push(&function->code, instruction);
    }
}

void emit_pop_to_wordreg(function_t *function, reg_t reg_destination) {
    instruction_t instruction = {0};

    {
        instruction.op = OP_MOVWORD_REGMEM_TO_REG;
        instruction.as.mov_regmem_to_reg.regmem_source = REG_STACK_BOTTOM;
        instruction.as.mov_regmem_to_reg.reg_destination = reg_destination;

        array_push(&function->code, instruction);
    }

    {
        instruction.op = OP_ADDU_REG_IM32;
        instruction.as.bin_regu_immediateu.reg_operand = REG_STACK_BOTTOM;
        instruction.as.bin_regu_immediateu.reg_result = REG_STACK_BOTTOM;
        instruction.as.bin_regu_immediateu.immediate = (u32)sizeof(word_t);

        array_push(&function->code, instruction);
    }
}

static void emit_binary(function_t *function, token_type_t token_type, type_info_t *type_info, reg_t op1, reg_t op2, reg_t result) {
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

        default:
            UNREACHABLE();
    }

    instruction.as.bin_reg_to_reg.reg_op1 = (byte)op1;
    instruction.as.bin_reg_to_reg.reg_op2 = (byte)op2;
    instruction.as.bin_reg_to_reg.reg_result = (byte)result;

    array_push(&function->code, instruction);
}

static void emit_return(function_t *function) {
    instruction_t instruction = {0};
    instruction.op = OP_RETURN;

    array_push(&function->code, instruction);
}

static void gen_expression(gen_t *gen, function_t *function, ast_node_t *expression);

static void gen_binary(gen_t *gen, function_t *function, ast_node_t *binary) {
    ast_node_t *lhs = an_lhs(binary);
    gen_expression(gen, function, lhs);

    emit_push_wordreg(function, REG_OPERAND1);

    ast_node_t *rhs = an_rhs(binary);
    gen_expression(gen, function, rhs);

    emit_pop_to_wordreg(function, REG_OPERAND2);

    type_info_t *expr_type_info = get_type_info(&gen->ast->type_set.types, binary->value_type);

    emit_binary(function, binary->operator.type, expr_type_info, REG_OPERAND2, REG_OPERAND1, REG_OPERAND1);
}

static value_index_t add_constant(function_t *function, void *data, size_t size) {
    size_t index;
    unless (memarr_push(function->memory, data, size, &index)) {
        return value_index_nil();
    }

    return value_index_(index);
}

static void gen_constant(function_t *function, void *data, type_info_t *type_info) {
    value_index_t value_index = add_constant(function, data, type_info->size);

    if (type_info->size <= WORD_SIZE) {
        emit_read_memory_to_reg(function, REG_OPERAND1, value_index.index, type_info);
    } else {
        ASSERT(false, "todo");
    }
}

static void gen_primary(gen_t *gen, function_t *function, ast_node_t *primary) {
    ASSERT(primary->value_index.exists, "must contain concrete value");

    type_info_t *type_info = get_type_info(&gen->ast->type_set.types, primary->value_type);

    // this ensures the data is always indexable
    if (function->memory->count+type_info->size >= function->memory->capacity) {
        if (gen->error_fn) {
            gen->error_fn((error_t){
                .type=ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG,
                .region_type=ERROR_REGION_TYPE_RANGE,
                .region={.range={.start=primary->start, .end=primary->end}}
            });
            return;
        }
    }

    void *data = memarr_get_ptr(&gen->ast->constants, primary->value_index);
    gen_constant(function, data, type_info);
}

static void gen_expression(gen_t *gen, function_t *function, ast_node_t *expression) {
    ASSERT(ast_node_type_is_expression(expression->node_type), "must be expression");

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            gen_primary(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            gen_binary(gen, function, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            gen_expression(gen, function, an_operand(expression));
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case AST_NODE_TYPE_EXPRESSION_BLOCK:
        case AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case AST_NODE_TYPE_EXPRESSION_CALL:
        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case AST_NODE_TYPE_EXPRESSION_DOT:
        case AST_NODE_TYPE_EXPRESSION_ENTITY:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER:
        case AST_NODE_TYPE_EXPRESSION_UNARY:
        case AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case AST_NODE_TYPE_STATEMENT_RETURN:
        case AST_NODE_TYPE_UNDEFINED:
        case AST_NODE_TYPE_DECLARATION:
        case AST_NODE_TYPE_EXPRESSION_PRINT:
        case AST_NODE_TYPE_EXPRESSION_PRINT_EXPR:
        case AST_NODE_TYPE_EXPRESSION_STATEMENT: ASSERT(false, "not implemented");
    }
}

static void gen_block(gen_t *gen, function_t *function, ast_node_t *block) {
    ASSERT(block->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK, "must be block");

    for (size_t i = 0; i < block->as.block.count; ++i) {
        ast_node_t *node = block->as.block.items[i];
        ASSERT(node->node_type == AST_NODE_TYPE_STATEMENT_EXPRESSION, "only allowing statement expressions right now");

        gen_expression(gen, function, an_operand(node));
    }
}

static void gen_return(gen_t *gen, function_t *function, type_t type) {
    UNUSED(gen);
    UNUSED(type);
    emit_return(function);
}

static void error_fn(error_t error) {
    UNUSED(error);
}

bool compile_expr_to_function(function_t *function, ast_t *ast) {
    gen_t gen = {0};
    gen.ast = ast;
    gen.error_fn = error_fn;

    gen_expression(&gen, function, ast->root);
    gen_return(&gen, function, ast->root->value_type);
    
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
    //     assert(node->node_type == AST_NODE_TYPE_DECLARATION);

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

#undef memarr_get_ptr
#undef CODEGEN_IMPLEMENTATION
#endif
