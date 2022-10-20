#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"

// must be less than 0xFFFF
typedef enum SavineType {
    SavineType_I32,
    SavineType_F64,
    SavineType_SIZE,
} SavineType;

typedef enum {
    SAVINE_INTERPRET_OK,
    SAVINE_INTERPRET_COMPILE_ERROR,
    SAVINE_INTERPRET_RUNTIME_ERROR,
} InterpretResult;

typedef struct SavineVM {
    Chunk* chunk;
    uint8_t* ip;
} SavineVM;

void savine_vm_init(SavineVM* vm);
void savine_vm_free(SavineVM* vm);

InterpretResult savine_interpret(SavineVM* vm, Chunk* chunk);

#endif