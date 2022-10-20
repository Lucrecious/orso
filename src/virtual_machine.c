#include "virtual_machine.h"

#include "sb.h"

void savine_vm_init(SavineVM* vm) {
    vm->code = NULL;
}

void savine_vm_free(SavineVM* vm) {
    sb_free(vm->code);
}

