#ifndef INTERPRETER_H_
#define INTERPRETER_H_

#include "static_analyzer.h"
#include "virtual_machine.h"

typedef struct OrsoInterpreter {
    OrsoStaticAnalyzer static_analyzer;
    OrsoVM vm;
    OrsoErrorFunction error_fn;
} OrsoInterpreter;

void orso_interpreter_init(OrsoInterpreter* interpreter, OrsoWriteFunction write_fn, OrsoErrorFunction error_fn);
void orso_interpreter_free(OrsoInterpreter* interpreter);

void orso_interpreter_run(OrsoInterpreter* interpreter, const char* source);

#endif