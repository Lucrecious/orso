#include "symbol_table.h"

#define GROW_CAPACITY(capacity) capacity == 0 ? 8 : capacity * 2

void orso_symbol_table_init(OrsoSymbolTable* table) {
    table->entries = NULL;
    table->count = 0;
    table->capacity = 0;
}

void orso_symbol_table_free(OrsoSymbolTable* table) {
    free(table->entries);
    orso_symbol_table_init(table);
}

static OrsoSymbolTableEntry* find_entry(OrsoSymbolTableEntry* entries, i32 capacity, OrsoSymbol* key) {
    u32 index = key->hash % capacity;
    OrsoSymbolTableEntry* tombstone = NULL;

    for (;;) {
        OrsoSymbolTableEntry* entry = &entries[index];

        if (entry->key == NULL) {
            if (entry->value.i == 0) {
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) {
                    tombstone  = entry;
                }
            }
        } else if (entry->key == key) {
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

static void adjust_capacity(OrsoSymbolTable* table, i32 capacity) {
    OrsoSymbolTableEntry* entries = ORSO_ALLOCATE_N(OrsoSymbolTableEntry, capacity);
    for (i32 i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value.i = 0;
#ifdef DEBUG_TRACE_EXECUTION
        entries[i].value.type = ORSO_TYPE_NULL;
#endif
    }

    table->count = 0;
    for (i32 i = 0; i < table->capacity; i++) {
        OrsoSymbolTableEntry* entry = &table->entries[i];
        if (entry->key == NULL) {
            continue;
        }

        OrsoSymbolTableEntry* destination = find_entry(entries, capacity, entry->key);
        destination->key = entry->key;
        destination->value = entry->value;
        table->count++;
    }

    free(table->entries);
    table->entries = entries;
    table->capacity = capacity;
}

bool orso_symbol_table_get(OrsoSymbolTable* table, OrsoSymbol* symbol, OrsoSlot* value) {
    if (table->count == 0) {
        return false;
    }

    OrsoSymbolTableEntry* entry = find_entry(table->entries, table->capacity, symbol);
    if (entry->key == NULL) {
        return false;
    }

    *value = entry->value;
    return true;
}

bool orso_symbol_table_set(OrsoSymbolTable* table, OrsoSymbol* key, OrsoSlot value) {
    if (table->count + 1 > table->capacity * ORSO_SYMBOL_TABLE_MAX_LOAD) {
        i32 capacity = GROW_CAPACITY(table->capacity);
        adjust_capacity(table, capacity);
    }

    OrsoSymbolTableEntry* entry = find_entry(table->entries, table->capacity, key);
    bool is_new_key = (entry->key == NULL);

    if (is_new_key && entry->value.i == 0) {
        table->count++;
    }

    entry->key = key;
    entry->value = value;

    return is_new_key;
}

bool orso_symbol_table_remove(OrsoSymbolTable* table, OrsoSymbol* key) {
    if (table->count == 0) {
        return false;
    }

    OrsoSymbolTableEntry* entry = find_entry(table->entries, table->capacity, key);
    if (entry == NULL) {
        return false;
    }

    entry->key = NULL;
    entry->value.i = 1;
}

void orso_symbol_table_add_all(OrsoSymbolTable* source, OrsoSymbolTable* destination) {
    for (i32 i = 0; i < source->capacity; i++) {
        OrsoSymbolTableEntry* entry = &source->entries[i];
        if (entry->key == NULL) {
            continue;
        }

        orso_symbol_table_set(destination, entry->key, entry->value);
    }
}

OrsoSymbol* orso_symbol_table_find_cstrn(OrsoSymbolTable* table, const char* start, i32 length, u32 hash) {
    if (table->count == 0) {
        return NULL;
    }

    u32 index = hash % table->capacity;
    for (;;) {
        OrsoSymbolTableEntry* entry = &table->entries[index++];

        if (entry->key == NULL) {
            if (entry->value.i == 0) {
                return NULL;
            }
        } else if (entry->key->length == length && entry->key->hash == hash
                && memcmp(entry->key->text, start, length) == 0) {
            return entry->key;
        }

        index = index % table->capacity;
    }
}

#undef GROW_CAPACITY