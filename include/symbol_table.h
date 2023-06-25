#ifndef SYMBOL_TABLE_H_
#define SYMBOL_TABLE_H_

#include "def.h"
#include "slot.h"

#define ORSO_SYMBOL_TABLE_MAX_LOAD 0.75

struct OrsoSymbol;

typedef struct OrsoSymbolTableEntry {
    struct OrsoSymbol* key;
    OrsoSlot value;
} OrsoSymbolTableEntry;

typedef struct OrsoSymbolTable {
    i32 count;
    i32 capacity;
    OrsoSymbolTableEntry* entries;
} OrsoSymbolTable;

void orso_symbol_table_init(OrsoSymbolTable* symbol_table);
void orso_symbol_table_free(OrsoSymbolTable* symbol_table);

bool orso_symbol_table_get(OrsoSymbolTable* table, struct OrsoSymbol* symbol, OrsoSlot* value);
bool orso_symbol_table_set(OrsoSymbolTable* table, struct OrsoSymbol* symbol, OrsoSlot value);
bool orso_symbol_table_remove(OrsoSymbolTable* table, struct OrsoSymbol* key);
void orso_symbol_table_add_all(OrsoSymbolTable* source, OrsoSymbolTable* destination);

struct OrsoSymbol* orso_symbol_table_find_cstrn(OrsoSymbolTable* table, const char* start, i32 length, u32 hash);

#endif
