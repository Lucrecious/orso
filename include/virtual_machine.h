#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "def.h"
#include "opcodes.h"
#include "stringtable.h"

typedef struct SavineVirtualMachine {

    StringTable symbol_value_offsets;
    i16* symbol_values;
    OP* code;
} SavineVirtualMachine;

void savine_vm_init(SavineVirtualMachine* vm);

void savine_vm_free(SavineVirtualMachine* vm);

#endif