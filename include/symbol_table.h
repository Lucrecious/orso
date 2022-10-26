#ifndef SYMBOL_TABLE_H_
#define SYMBOL_TABLE_H_

#include "def.h"
#include "object.h"
#include "type.h"

typedef struct OrsoSymbolTableEntry {
    OrsoSymbol* key;
    OrsoSlot slot;
} OrsoSymbolTableEntry;

typedef struct OrsoSymbolTable {
    i32 count;
    i32 capacity;
    OrsoSymbolTableEntry* entries;

} OrsoSymbolTable;

void orso_init_symbol(OrsoSymbolTable* symbol_table);
void orso_free_symbol(OrsoSymbolTable* symbol_table);

#endif