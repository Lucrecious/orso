#include "interpreter.h"

#include "common.h"
#include "chunk.h"
#include "codegen.h"

#include <stdio.h>

OrsoSlot* orso_call_function(OrsoVM* vm, OrsoFunction* function, OrsoErrorFunction error_fn) {
    orso_vm_push_object(vm, (OrsoObject*)function);
    orso_vm_call(vm, function);

    orso_vm_interpret(vm, error_fn);

    // skip the function local and land on the return type
    // caller is responsible for knowing the type of the return value
    return vm->stack;
}

OrsoFunction* orso_compile_ast(OrsoVM* vm, OrsoAST* ast) {
    ASSERT(ast->resolved, "must be resolved");

    OrsoFunction* main_function = orso_generate_code(vm, ast);

    return main_function;
}

void orso_run_source(OrsoVM* vm, const char* source, OrsoErrorFunction error_fn) {
    OrsoAST ast;
    orso_ast_init(&ast, &vm->symbols);

    OrsoStaticAnalyzer analyzer;
    orso_static_analyzer_init(&analyzer, vm->write_fn, error_fn);

    if (!orso_parse(&ast, source, error_fn)) {
        orso_ast_free(&ast);
        return;
    }

    #ifdef DEBUG_PRINT
        orso_ast_print(&ast, "unresolved");
    #endif

    bool resolved = orso_resolve_ast(&analyzer, &ast);
    if (!resolved) {
        orso_ast_free(&ast);
        return;
    }

    #ifdef DEBUG_PRINT
        orso_ast_print(&ast, "resolved");
    #endif


    OrsoFunction* main_function = orso_compile_ast(vm, &ast);

    //orso_ast_free(&ast);

    if (!main_function) {
        analyzer.had_error = false;
        return;
    }

    OrsoSlot stack_slots[256];
    vm->stack = stack_slots;
    vm->stack_top = vm->stack;

    orso_call_function(vm, main_function, error_fn);

    vm->stack = NULL;
    vm->stack_top = NULL;
}
