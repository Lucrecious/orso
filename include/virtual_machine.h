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

typedef struct vm_t {
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
} vm_t;

void orso_vm_init(vm_t* vm, OrsoWriteFunction write_fn, i32 stack_size);
void orso_vm_free(vm_t* vm);

void orso_vm_call(vm_t* vm, OrsoFunction* function);
void orso_vm_push_object(vm_t* vm, OrsoObject* object);

void orso_vm_interpret(vm_t* vm, OrsoErrorFunction error_fn);

#endif
