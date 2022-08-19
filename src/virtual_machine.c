#include "virtual_machine.h"

#include "sb.h"

void savine_vm_init(SavineVirtualMachine* vm) {
    vm->code = NULL;
    vm->symbol_values = NULL;
    string_table_init(&vm->symbol_value_offsets);
}

void savine_vm_free(SavineVirtualMachine* vm) {
    sb_free(vm->code);
    sb_free(vm->symbol_values);
    string_table_free(&vm->symbol_value_offsets);
}