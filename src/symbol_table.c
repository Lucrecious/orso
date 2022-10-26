#include "symbol_table.h"

void orso_init_symbol(OrsoSymbolTable* symbol_table) {
    symbol_table->entries = NULL;
    symbol_table->count = 0;
    symbol_table->capacity = 0;
}

void orso_free_symbol(OrsoSymbolTable* symbol_table) {
    free(symbol_table->entries);
    orso_init_symbol(symbol_table);
}