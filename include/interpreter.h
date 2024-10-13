#ifndef INTERPRETER_H_
#define INTERPRETER_H_

#include "static_analyzer.h"
#include "virtual_machine.h"
#include "codegen.h"

function_t* orso_compile_ast(vm_t* vm, OrsoAST* ast);

// leaves return value on the stack and returns its stack position (since it can be any size)
slot_t* orso_call_function(vm_t* vm, function_t* function, OrsoErrorFunction error_fn);

void orso_run_source(vm_t* vm, const char* source, OrsoErrorFunction error_fn);

#endif
