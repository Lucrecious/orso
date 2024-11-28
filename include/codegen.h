#ifndef CODEGEN_H_
#define CODEGENH_

#include "parser.h"
#include "vm.h"

bool compile_program(vm_t *vm, ast_t *ast);

#endif

#ifdef CODEGEN_IMPLEMENTATION

typedef enum reg_t reg_t;
enum reg_t {
    REG_NULL = 0,
    REG_OPERAND1,
};

typedef struct arena_array_t arena_array_t;
struct arena_array_t {
    arena_t data;
    size_t count;
};

static size_t arena_array_push(arena_array_t *array, void *data, size_t size) {
    void *space = arena_alloc(&array->data, size);
    memcpy(space, data, size);

    size_t new_count = 0;
    size_t index = 0;
    
    // find index
    {
        Region *last_region = array->data.end;
        ASSERT((space >= (void*)last_region->data) && (space < ((void*)last_region->data+last_region->count)), "expected that the reserved space is in the last region");

        size_t region_index = (byte*)space - (byte*)last_region->data;

        Region *r = array->data.begin;
        while (r) {
            new_count += r->count;
            r = r->next;
        }

        // remove over counting since last region is not filled yet
        index = new_count;

        // remove over counting
        index -= last_region->count;
        index += region_index;
    }

    array->count = new_count;

    return index;
}

typedef struct builder_t builder_t;
struct builder_t {
    ast_t *ast;
    error_function_t error_fn;

    struct {
        instruction_t *items;
        size_t count;
        size_t capacity;
        arena_t *allocator;
    } instructions;
    arena_array_t memory;
};

static string_view_t token2sv(token_t t) {
    return (string_view_t) {
        .data = t.start,
        .length = t.length,
    };
}

static void builder2vm(vm_t *vm, builder_t *builder) {
    ASSERT(vm->memory == NULL, "vm is expected to be zeroed out right now");

    vm_init(vm, builder->instructions.count, builder->memory.count);

    Region *r = builder->memory.data.begin;
    size_t index = 0;
    while (r) {
        memcpy(vm->memory+index, r->data, r->count);
        r = r->next;
        index += r->count;
    }

    memcpy(vm->program, builder->instructions.items, builder->instructions.count*sizeof(instruction_t));
}

static void emit_read_memory_to_reg(builder_t *builder, reg_t destination, memaddr_t address, type_info_t *type_info) {
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

    array_push(&builder->instructions, instruction);
}

static void gen_binary(builder_t *builder, ast_node_t *binary) {
    UNUSED(builder);
    UNUSED(binary);
}

static u32 add_constant(builder_t *builder, void *data, size_t size) {
    size_t index = arena_array_push(&builder->memory, data, size);
    ASSERT(index < UINT32_MAX, "index cannot be higher than this, we must have checked this already");
    return (u32)index;
}

static void gen_constant(builder_t *builder, void *data, type_info_t *type_info) {
    u32 index = add_constant(builder, data, type_info->size);

    if (type_info->size <= WORD_SIZE) {
        emit_read_memory_to_reg(builder, REG_OPERAND1, index, type_info);
    } else {
        ASSERT(false, "todo");
    }
}

static void gen_primary(builder_t *builder, ast_node_t *primary) {
    ASSERT(primary->value_index >= 0, "must contain concrete value");

    type_info_t *type_info = get_type_info(&builder->ast->type_set.types, primary->value_type);

    // this ensures the data is always indexable
    if (builder->memory.count+(type_info->size*2) > UINT32_MAX) {
        if (builder->error_fn) {
            builder->error_fn((error_t){
                .type=ERROR_CODEGEN_MEMORY_SIZE_TOO_BIG,
                .region_type=ERROR_REGION_TYPE_RANGE,
                .region={.range={.start=primary->start, .end=primary->end}}
            });
            return;
        }
    }

    void *data = &builder->ast->folded_constants.items[primary->value_index];
    gen_constant(builder, data, type_info);
}

static void gen_expression(builder_t *builder, ast_node_t *expression) {
    ASSERT(ast_node_type_is_expression(expression->node_type), "must be expression");

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            gen_primary(builder, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            gen_binary(builder, expression);
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

static void gen_block(builder_t *builder, ast_node_t *block) {
    ASSERT(block->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK, "must be block");

    for (size_t i = 0; i < block->as.block.count; ++i) {
        ast_node_t *node = block->as.block.items[i];
        ASSERT(node->node_type == AST_NODE_TYPE_STATEMENT_EXPRESSION, "only allowing statement expressions right now");

        gen_expression(builder, node->as.expression);
    }
}

bool compile_program(vm_t *vm, ast_t *ast) {
    tmp_arena_t *tmp = allocator_borrow();

    ast_node_t *root = ast->root;
    assert(root->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK);

    ast_node_t *main_node = NULL;
    string_view_t main_name = cstr2sv("main");
    for (size_t i = 0; i < root->as.block.count; ++i) {
        ast_node_t *node = root->as.block.items[i];
        assert(node->node_type == AST_NODE_TYPE_DECLARATION);

        string_view_t identifier = token2sv(node->as.declaration.identifier);
        if (sv_eq(identifier, main_name)) {
            main_node = node;
            break;
        }
    }

    if (!main_node) {
        return false;
    }

    ast_node_t *initial_expression = main_node->as.declaration.initial_value_expression;
    if (initial_expression->value_index < 0) {
        return false;
    }

    if (!type_is_function(ast->type_set.types, initial_expression->value_type)) {
        return false;
    }

    // TODO: ignoring the parameters for main

    assert(initial_expression->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION);

    //size_t instruction_index = builder.code.count;

    builder_t builder = {0};
    gen_block(&builder, initial_expression->as.function.block);

    builder2vm(vm, &builder);

    allocator_return(tmp);

    return true;
}

#endif
