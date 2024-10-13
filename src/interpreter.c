#include "interpreter.h"

#include "common.h"
#include "chunk.h"
#include "codegen.h"

#include <stdio.h>

OrsoSlot* orso_call_function(vm_t* vm, OrsoFunction* function, OrsoErrorFunction error_fn) {
    orso_vm_push_object(vm, (OrsoObject*)function);
    orso_vm_call(vm, function);

    orso_vm_interpret(vm, error_fn);

    // skip the function local and land on the return type
    // caller is responsible for knowing the type of the return value
    return vm->stack;
}

OrsoFunction* orso_compile_ast(vm_t* vm, OrsoAST* ast) {
    ASSERT(ast->resolved, "must be resolved");

    OrsoFunction* main_function = orso_generate_code(vm, ast);

    return main_function;
}

void orso_run_source(vm_t* vm, const char* source, OrsoErrorFunction error_fn) {
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
        printf("something failed. TODO: better error messages..\n");
        analyzer.had_error = false;
        return;
    }

    // TODO: find a better way to set this
    vm->type_set = &ast.type_set;

    orso_call_function(vm, main_function, error_fn);
}
