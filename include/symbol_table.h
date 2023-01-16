#ifndef SYMBOL_TABLE_H_
#define SYMBOL_TABLE_H_

#include "def.h"
#include "object.h"
#include "type.h"

#define ORSO_SYMBOL_TABLE_MAX_LOAD 0.75

typedef struct OrsoSymbolTableEntry {
    OrsoSymbol* key;
    OrsoSlot value;
} OrsoSymbolTableEntry;

typedef struct OrsoSymbolTable {
    i32 count;
    i32 capacity;
    OrsoSymbolTableEntry* entries;

} OrsoSymbolTable;

void orso_symbol_table_init(OrsoSymbolTable* symbol_table);
void orso_symbol_table_free(OrsoSymbolTable* symbol_table);

bool orso_symbol_table_get(OrsoSymbolTable* table, OrsoSymbol* symbol, OrsoSlot* value);
bool orso_symbol_table_set(OrsoSymbolTable* table, OrsoSymbol* symbol, OrsoSlot value);
bool orso_symbol_table_remove(OrsoSymbolTable* table, OrsoSymbol* key);
void orso_symbol_table_add_all(OrsoSymbolTable* source, OrsoSymbolTable* destination);

OrsoSymbol* orso_symbol_table_find_cstrn(OrsoSymbolTable* table, const char* start, i32 length, u32 hash);

FORCE_INLINE OrsoSymbol* orso_unmanaged_symbol_from_cstrn(const char* start, i32 length, OrsoSymbolTable* symbol_table) {
    u32 hash = orso_hash_cstrn(start, length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = ORSO_ALLOCATE_FLEX(OrsoSymbol, length + 1);
    symbol->object.type_kind = ORSO_TYPE_SYMBOL;
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';
    symbol->object.gc_header.next = symbol->object.gc_header.previous = NULL;

    OrsoSlot slot = ORSO_SLOT_I(0, ORSO_TYPE_NULL);
    orso_symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}

FORCE_INLINE void orso_unmanaged_symbol_free(OrsoSymbol* symbol) {
    free(symbol);
}

FORCE_INLINE OrsoSymbol* orso_new_symbol_from_cstrn(OrsoGarbageCollector* gc, const char* start, i32 length, OrsoSymbolTable* symbol_table) {
    u32 hash = orso_hash_cstrn(start, length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = ORSO_OBJECT_ALLOCATE_FLEX(gc, OrsoSymbol, ORSO_TYPE_SYMBOL, length + 1);
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';

    OrsoSlot slot = ORSO_SLOT_I(0, ORSO_TYPE_NULL);
    orso_symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}

#endif
