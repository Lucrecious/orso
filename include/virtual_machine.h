#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "symbol_table.h"

typedef struct OrsoVM {
    Chunk* chunk;
    OrsoInstruction* ip;

    OrsoSymbolTable symbol_table;
    OrsoSymbolTable globals;
    
    OrsoSlot* stack;
    OrsoSlot* stack_top;
} OrsoVM;

void orso_vm_init(OrsoVM* vm);
void orso_vm_free(OrsoVM* vm);

void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn);

#endif