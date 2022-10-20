#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "def.h"
#include "opcodes.h"
#include "stringtable.h"

// must be less than 0xFFFF
typedef enum SavineType {
    SavineType_I32,
    SavineType_F64,
    SavineType_SIZE,
} SavineType;

typedef struct SavineVM {
    byte* code;
} SavineVM;

void savine_vm_init(SavineVM* vm);

void savine_vm_free(SavineVM* vm);

#endif