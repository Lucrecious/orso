#ifndef INTERPRETER_H_
#define INTERPRETER_H_

#include "static_analyzer.h"
#include "virtual_machine.h"
#include "codegen.h"

function_t *compile_ast(vm_t *vm, ast_t *ast);

// leaves return value on the stack and returns its stack position (since it can be any size)
slot_t *orso_call_function(vm_t *vm, function_t *function, error_function_t error_fn);

void vm_run_source(vm_t *vm, cstr_t file_path, cstr_t source, error_function_t error_fn);

#endif
