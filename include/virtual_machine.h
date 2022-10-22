#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "value.h"

typedef enum {
    SAVINE_INTERPRET_OK,
    SAVINE_INTERPRET_COMPILE_ERROR,
    SAVINE_INTERPRET_RUNTIME_ERROR,
} InterpretResult;

typedef struct SavineVM {
    Chunk* chunk;
    uint8_t* ip;

    SavineValue* stack;
    SavineValue* stack_top;
} SavineVM;

void savine_vm_init(SavineVM* vm);
void savine_vm_free(SavineVM* vm);

InterpretResult savine_interpret(SavineVM* vm, const char* source);

void FORCE_INLINE savine_vm_push(SavineVM* vm, SavineValue value);
SavineValue FORCE_INLINE savine_vm_pop(SavineVM* vm);

void FORCE_INLINE savine_vm_push_int(SavineVM* vm, i64 value);
i64 FORCE_INLINE savine_vm_pop_int(SavineVM* vm);

void FORCE_INLINE savine_vm_push_float(SavineVM* vm, f64 value);
f64 FORCE_INLINE savine_vm_pop_float(SavineVM* vm);

#endif