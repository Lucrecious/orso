#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "symbol_table.h"

typedef struct OrsoGCValueIndex {
    u32 is_object : 1;
    u32 index : 31;
} OrsoGCValueIndex;

typedef struct OrsoVM {
    OrsoWriteFunction write_fn;
    Chunk* chunk;
    byte* ip;

    struct {
        OrsoSymbolTable name_to_index;

        // negative indices indicate a union object type that is holding a stack item
        // only union types with a gc type are included in this list
        OrsoGCValueIndex* gc_values_indices;
        OrsoSlot* values;
    } globals;

    OrsoSymbolTable symbols;
    OrsoGarbageCollector gc;

    i32 union_object_stack_count;
    i32* union_object_stack;
    
    OrsoObject** object_stack;
    OrsoObject** object_stack_top;
    
    OrsoSlot* stack;
    OrsoSlot* stack_top;
} OrsoVM;

void orso_vm_init(OrsoVM* vm, OrsoWriteFunction write_fn);
void orso_vm_free(OrsoVM* vm);

void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn);

#endif
