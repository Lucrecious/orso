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
    instruction.op = OP_READWORD_MEM_TO_REG;

    if (type_is_number(type_info, true) && type_info->size < WORD_SIZE) {
        switch (type_info->kind) {
            case TYPE_BOOL: {
                instruction.op = OP_READU8_MEM_TO_REG;
                break;
            }

            case TYPE_INT32: {
                instruction.op = OP_READI32_MEM_TO_REG;
                break;
            }

            case TYPE_FLOAT32: {
                instruction.op = OP_READF32_MEM_TO_REG;
                break;
            }

            case TYPE_INVALID:
            case TYPE_UNDEFINED:
            case TYPE_UNRESOLVED:
            case TYPE_VOID:
            case TYPE_INT64:
            case TYPE_FLOAT64:
            case TYPE_STRING:
            case TYPE_SYMBOL:
            case TYPE_TYPE:
            case TYPE_FUNCTION:
            case TYPE_NATIVE_FUNCTION:
            case TYPE_POINTER:
            case TYPE_UNION:
            case TYPE_STRUCT:
            case TYPE_COUNT: UNREACHABLE(); break;
        }

    }

    instruction.as.read_memory_to_reg.memory_address = address;
    instruction.as.read_memory_to_reg.reg_result = destination;

    array_push(&function->code, instruction);
}

static void gen_binary(function_t *function, ast_node_t *binary) {
    UNUSED(function);
    UNUSED(binary);
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
            gen_binary(function, expression);
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
        case AST_NODE_TYPE_EXPRESSION_GROUPING:
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

        gen_expression(gen, function, node->as.expression);
    }
}

static void gen_return(gen_t *gen, function_t *function, type_t type) {
    UNUSED(gen);
    UNUSED(function);
    UNUSED(type);
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
