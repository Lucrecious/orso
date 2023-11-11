#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "symbol_table.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
    OrsoFunction* function;
    byte* ip;
    OrsoSlot* slots;
} CallFrame;

typedef struct OrsoVM {
    OrsoWriteFunction write_fn;

    OrsoTypeSet* type_set;

    CallFrame frames[FRAMES_MAX];
    i32 frame_count;

    struct {
        OrsoSymbolTable name_to_index;
#ifdef DEBUG_TRACE_EXECUTION
        OrsoType** types;
#endif
        OrsoSlot* values;
    } globals;

    OrsoSymbolTable symbols;

#ifdef DEBUG_TRACE_EXECUTION
    OrsoType** stack_types;
#endif
    
    OrsoSlot* stack;
    OrsoSlot* stack_top;
} OrsoVM;

void orso_vm_init(OrsoVM* vm, OrsoWriteFunction write_fn, i32 stack_size);
void orso_vm_free(OrsoVM* vm);

void orso_vm_call(OrsoVM* vm, OrsoFunction* function);
void orso_vm_push_object(OrsoVM* vm, OrsoObject* object);

void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn);

#endif
