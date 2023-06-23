#ifndef INTERPRETER_H_
#define INTERPRETER_H_

#include "static_analyzer.h"
#include "virtual_machine.h"
#include "codegen.h"

OrsoFunction* orso_compile_ast(OrsoVM* vm, OrsoAST* ast);

// leaves return value on the stack and returns its stack position (since it can be any size)
OrsoSlot* orso_call_function(OrsoVM* vm, OrsoFunction* function, OrsoErrorFunction error_fn);

void orso_run_source(OrsoVM* vm, const char* source, OrsoErrorFunction error_fn);

#endif
