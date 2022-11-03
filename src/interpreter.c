#include "interpreter.h"

#include "common.h"
#include "chunk.h"
#include "codegen.h"

#include <stdio.h>

void orso_interpreter_init(OrsoInterpreter* interpreter, OrsoErrorFunction error_fn) {
    orso_vm_init(&interpreter->vm);
    orso_static_analyzer_init(&interpreter->static_analyzer, &interpreter->vm.gc, &interpreter->vm.symbols, error_fn);
    interpreter->error_fn = error_fn;
}

void orso_interpreter_free(OrsoInterpreter* interpreter) {
    orso_vm_free(&interpreter->vm);
    orso_static_analyzer_free(&interpreter->static_analyzer);
}

static bool compile(const char* source, OrsoVM* vm, OrsoStaticAnalyzer* analyzer, OrsoErrorFunction error_fn) {
    OrsoAST ast;
    orso_ast_init(&ast);

    analyzer->panic_mode = false;

    if (!orso_parse(&ast, source, error_fn)) {
        orso_ast_free(&ast);
        return false;
    }

#ifdef DEBUG_PRINT
    orso_ast_print(&ast, "unresolved");
#endif

    bool resolved = orso_resolve_ast_types(analyzer, &ast);

#ifdef DEBUG_PRINT
    orso_ast_print(&ast, "resolved");
#endif

    bool succeeded = resolved;

    if (succeeded) {
        succeeded = orso_generate_code(vm, &ast, vm->chunk);
    }

    orso_ast_free(&ast);

    return succeeded;
}

static void interpret_continuous(OrsoVM* vm, OrsoStaticAnalyzer* analyzer, const char* source, OrsoErrorFunction error_fn) {
    Chunk chunk;
    chunk_init(&chunk);
    chunk.max_stack_size = 256;

    vm->chunk = &chunk;

    if (!compile(source, vm, analyzer, error_fn)) {
        analyzer->had_error = false;
        return;
    }

    vm->stack = ORSO_ALLOCATE_N(OrsoSlot, chunk.max_stack_size);
    vm->stack_top = vm->stack;

    vm->ip = vm->chunk->code;

    orso_vm_interpret(vm, error_fn);

    free(vm->stack);
    chunk_free(&chunk);
    vm->stack_top = NULL;
}

void orso_interpreter_run(OrsoInterpreter* interpreter, const char* source) {
    interpret_continuous(&interpreter->vm, &interpreter->static_analyzer, source, interpreter->error_fn);
}