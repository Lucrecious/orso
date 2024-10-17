#include "interpreter.h"

#include "common.h"
#include "chunk.h"
#include "codegen.h"

#include <stdio.h>

slot_t *orso_call_function(vm_t *vm, function_t *function, error_function_t error_fn) {
    vm_push_object(vm, (OrsoObject*)function);
    vm_call(vm, function);

    vm_interpret(vm, error_fn);

    // skip the function local and land on the return type
    // caller is responsible for knowing the type of the return value
    return vm->stack;
}

function_t *compile_ast(vm_t *vm, ast_t *ast) {
    ASSERT(ast->resolved, "must be resolved");

    function_t *main_function = generate_code(vm, ast);

    return main_function;
}

void run_source(vm_t* vm, const char* source, error_function_t error_fn) {
    ast_t ast;
    ast_init(&ast, &vm->symbols);

    analyzer_t analyzer;
    analyzer_init(&analyzer, vm->write_fn, error_fn);

    if (!parse(&ast, source, error_fn)) {
        orso_ast_free(&ast);
        return;
    }

    bool resolved = resolve_ast(&analyzer, &ast);
    if (!resolved) {
        orso_ast_free(&ast);
        return;
    }

    function_t *main_function = compile_ast(vm, &ast);

    if (!main_function) {
        printf("something failed. TODO: better error messages..\n");
        analyzer.had_error = false;
        return;
    }

    // TODO: find a better way to set this
    vm->type_set = &ast.type_set;

    orso_call_function(vm, main_function, error_fn);
}
