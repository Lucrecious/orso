#ifndef SYMBOL_TABLE_H_
#define SYMBOL_TABLE_H_

#include "def.h"
#include "slot.h"

#define ORSO_SYMBOL_TABLE_MAX_LOAD 0.75

struct OrsoSymbol;

typedef struct OrsoSymbolTableEntry {
    struct OrsoSymbol* key;
    slot_t value;
} OrsoSymbolTableEntry;

typedef struct symbol_table_t {
    i32 count;
    i32 capacity;
    OrsoSymbolTableEntry* entries;
} symbol_table_t;

void orso_symbol_table_init(symbol_table_t* symbol_table);
void orso_symbol_table_free(symbol_table_t* symbol_table);

bool orso_symbol_table_get(symbol_table_t* table, struct OrsoSymbol* symbol, slot_t* value);
bool orso_symbol_table_set(symbol_table_t* table, struct OrsoSymbol* symbol, slot_t value);
bool orso_symbol_table_remove(symbol_table_t* table, struct OrsoSymbol* key);
void orso_symbol_table_add_all(symbol_table_t* source, symbol_table_t* destination);

struct OrsoSymbol* orso_symbol_table_find_cstrn(symbol_table_t* table, const char* start, i32 length, u32 hash);

#endif
