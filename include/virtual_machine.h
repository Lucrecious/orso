#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "symbol_table.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct OrsoGCValueIndex {
    u32 is_object : 1;
    u32 index : 31;
} OrsoGCValueIndex;

typedef struct OrsoVMHook {
    bool is_object;
    union {
        OrsoObject* hook;
        u32 next_free_index;
    } value;
} OrsoVMHook;

typedef struct {
    OrsoFunction* function;
    byte* ip;
    OrsoSlot* slots;
    OrsoGCValueIndex* object_indices_cache;
} CallFrame;

typedef struct OrsoVM {
    OrsoWriteFunction write_fn;

    CallFrame frames[FRAMES_MAX];
    i32 frame_count;

    struct {
        OrsoSymbolTable name_to_index;

        // negative indices indicate a union object type that is holding a stack item
        // only union types with a gc type are included in this list
        OrsoGCValueIndex* gc_values_indices;
        OrsoSlot* values;
    } globals;

    OrsoSymbolTable symbols;
    OrsoGarbageCollector gc;

    OrsoGCValueIndex* object_stack;
    OrsoGCValueIndex* object_stack_top;
    
    OrsoSlot* stack;
    OrsoSlot* stack_top;

    i32 last_free_object_hook_index;
    OrsoVMHook* object_hooks;
    
} OrsoVM;

void orso_vm_init(OrsoVM* vm, OrsoWriteFunction write_fn);
void orso_vm_free(OrsoVM* vm);

void orso_vm_call(OrsoVM* vm, OrsoFunction* function);
void orso_vm_push_object(OrsoVM* vm, OrsoObject* object);

i32 orso_vm_add_hook(OrsoVM* vm,  OrsoObject* object);
void orso_vm_remove_hook(OrsoVM* vm, i32 index);

void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn);

#endif
