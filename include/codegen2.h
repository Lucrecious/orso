#ifndef CODEGEN2_H_
#define CODEGEN2_H_

#include "parser.h"
#include "vm2.h"

bool generate_code_for_vm2(vm2_t *vm, ast_t *ast);

#endif

#ifdef CODEGEN2_IMPLEMENTATION

typedef struct vm2_builder_t vm2_builder_t;
struct vm2_builder_t {
    size_t stack_size;
    size_t memory_size;
};

static vm2_builder_t default_builder(void) {
    vm2_builder_t builder = {0};
    builder.memory_size = megabytes(10);
    builder.stack_size = megabytes(1);

    return builder;
}

static string_view_t token2sv(token_t t) {
    return (string_view_t) {
        .data = t.start,
        .length = t.length,
    };
}

bool generate_code_for_vm2(vm2_t *vm, ast_t *ast) {
    UNUSED(vm);

    tmp_arena_t *tmp = allocator_borrow();

    // vm2_builder_t builder = default_builder();

    ast_node_t *root = ast->root;
    assert(root->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK);

    ast_node_t *main_node = NULL;
    string_view_t main_name = cstr2sv("main");
    for (size_t i = 0; i < root->data.block.count; ++i) {
        ast_node_t *node = root->data.block.items[i];
        assert(node->node_type == AST_NODE_TYPE_DECLARATION);

        string_view_t identifier = token2sv(node->data.declaration.identifier);
        if (sv_eq(identifier, main_name)) {
            main_node = node;
            break;
        }
    }

    if (!main_node) {
        return false;
    }

    ast_node_t *initial_expression = main_node->data.declaration.initial_value_expression;
    if (initial_expression->value_index < 0) {
        return false;
    }

    if (!type_is_function(ast->type_set.types, initial_expression->value_type)) {
        return false;
    }

    // TODO: ignoring the parameters for main

    assert(initial_expression->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION);

    printfln("ast node value %d", main_node->value_index);

    allocator_return(tmp);

    return true;
}

#endif
