#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "value.h"

typedef enum {
    ORSO_INTERPRET_OK,
    ORSO_INTERPRET_COMPILE_ERROR,
    ORSO_INTERPRET_RUNTIME_ERROR,
} InterpretResult;

typedef struct OrsoVM {
    Chunk* chunk;
    uint8_t* ip;

    OrsoValue* stack;
    OrsoValue* stack_top;
} OrsoVM;

void orso_vm_init(OrsoVM* vm);
void orso_vm_free(OrsoVM* vm);

InterpretResult orso_interpret(OrsoVM* vm, const char* source);

#endif