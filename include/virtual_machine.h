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

    struct {
        OrsoSymbolTable name_to_index;

        // TODO: figure a way for union
        i32* gc_values_indices; // addresses to values or union object (see TODO above)
        OrsoSlot* values;
    } globals;

    OrsoSymbolTable symbols;
    OrsoGarbageCollector gc;
    
    // TODO: Make offsets instead beacuse changing stack might cuz to be invalidated.
    // This is only for gcing anyways, which happens incrementally, so it should be cheap 
    // enough to do the double lookup (lookup into this array, and then look up on stack)
    OrsoSlot** object_stack; // addresses to stack
    OrsoSlot** object_stack_top;
    
    OrsoSlot* stack;
    OrsoSlot* stack_top;
} OrsoVM;

void orso_vm_init(OrsoVM* vm, OrsoWriteFunction write_fn);
void orso_vm_free(OrsoVM* vm);

void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn);

#endif