#include "interpreter.h"

#include "common.h"
#include "chunk.h"
#include "codegen.h"

#include <stdio.h>

void orso_interpreter_init(OrsoInterpreter* interpreter, OrsoWriteFunction write_fn, OrsoErrorFunction error_fn) {
    orso_vm_init(&interpreter->vm, write_fn);
    orso_static_analyzer_init(&interpreter->static_analyzer, error_fn);
    interpreter->error_fn = error_fn;
}

void orso_interpreter_free(OrsoInterpreter* interpreter) {
    orso_vm_free(&interpreter->vm);
    orso_static_analyzer_free(&interpreter->static_analyzer);
    interpreter->error_fn = NULL;
}

static OrsoFunction* compile(const char* source, OrsoVM* vm, OrsoStaticAnalyzer* analyzer, OrsoErrorFunction error_fn) {
    OrsoAST ast;
    orso_ast_init(&ast);

    analyzer->panic_mode = false;

    if (!orso_parse(&ast, source, error_fn)) {
        orso_ast_free(&ast);
        return NULL;
    }

#ifdef DEBUG_PRINT
    orso_ast_print(&ast, "unresolved");
#endif

    bool resolved = orso_resolve_ast_types(analyzer, &ast);

#ifdef DEBUG_PRINT
    orso_ast_print(&ast, "resolved");
#endif

    OrsoFunction* main_function = NULL;
    if (resolved) {
        main_function = orso_generate_code(vm, &ast, &analyzer->type_set);
    }

    orso_ast_free(&ast);

    return main_function;
}

static void interpret_continuous(OrsoVM* vm, OrsoStaticAnalyzer* analyzer, const char* source, OrsoErrorFunction error_fn) {
    OrsoFunction* main_function = compile(source, vm, analyzer, error_fn);

    if (!main_function) {
        analyzer->had_error = false;
        return;
    }

    vm->stack = ORSO_ALLOCATE_N(OrsoSlot, 256);
    vm->stack_top = vm->stack;

    vm->object_stack = ORSO_ALLOCATE_N(OrsoGCValueIndex, 256);
    vm->object_stack_top = vm->object_stack;

    orso_vm_push_object(vm, (OrsoObject*)main_function);
    orso_vm_call(vm, main_function);

    orso_vm_interpret(vm, error_fn);

    free(vm->stack);
    vm->stack = NULL;

    free(vm->object_stack);
    vm->object_stack = NULL;
    
    vm->stack_top = NULL;
    vm->object_stack_top = NULL;
}

void orso_interpreter_run(OrsoInterpreter* interpreter, const char* source) {
    interpret_continuous(&interpreter->vm, &interpreter->static_analyzer, source, interpreter->error_fn);
}
