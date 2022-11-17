#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "symbol_table.h"

typedef struct OrsoVM {
    OrsoWriteFunction write_fn;
    Chunk* chunk;
    OrsoInstruction* ip;

    OrsoSymbolTable symbols;
    OrsoSymbolTable stack_globals;
    OrsoSymbolTable object_globals;
    OrsoGarbageCollector gc;

    // Address to actual stack
    OrsoSlot** object_stack;
    OrsoSlot** object_stack_top;
    
    OrsoSlot* stack;
    OrsoSlot* stack_top;
} OrsoVM;

void orso_vm_init(OrsoVM* vm, OrsoWriteFunction write_fn);
void orso_vm_free(OrsoVM* vm);

void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn);

#endif