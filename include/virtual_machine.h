#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "common.h"
#include "value.h"

typedef struct OrsoVM {
    Chunk* chunk;
    uint8_t* ip;

    OrsoValue* stack;
    OrsoValue* stack_top;
} OrsoVM;

void orso_vm_init(OrsoVM* vm);
void orso_vm_free(OrsoVM* vm);

void orso_interpret(OrsoVM* vm, const char* source, OrsoErrorFunction error_function);

#endif