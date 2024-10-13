#include "symbol_table.h"

#include "type_set.h"
#include "object.h"

#define GROW_CAPACITY(capacity) capacity == 0 ? 8 : capacity * 2

void orso_symbol_table_init(symbol_table_t* table) {
    table->entries = NULL;
    table->count = 0;
    table->capacity = 0;
}

void orso_symbol_table_free(symbol_table_t* table) {
    free(table->entries);
    orso_symbol_table_init(table);
}

static OrsoSymbolTableEntry* find_entry(OrsoSymbolTableEntry* entries, i32 capacity, OrsoSymbol* key) {
    u32 index = key->hash & (capacity - 1);
    OrsoSymbolTableEntry* tombstone = NULL;

    for (;;) {
        OrsoSymbolTableEntry* entry = &entries[index];

        if (entry->key == NULL) {
            if (entry->value.as.i == 0) {
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) {
                    tombstone  = entry;
                }
            }
        } else if (entry->key == key) {
            return entry;
        }

        index = (index + 1) & (capacity - 1);
    }
}

static void adjust_capacity(symbol_table_t* table, i32 capacity) {
    OrsoSymbolTableEntry* entries = ORSO_ALLOCATE_N(OrsoSymbolTableEntry, capacity);
    for (i32 i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = ORSO_SLOT_I(0);
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

bool orso_symbol_table_get(symbol_table_t* table, OrsoSymbol* symbol, slot_t* value) {
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

bool orso_symbol_table_set(symbol_table_t* table, OrsoSymbol* key, slot_t value) {
    if (table->count + 1 > table->capacity * ORSO_SYMBOL_TABLE_MAX_LOAD) {
        i32 capacity = GROW_CAPACITY(table->capacity);
        adjust_capacity(table, capacity);
    }

    OrsoSymbolTableEntry* entry = find_entry(table->entries, table->capacity, key);
    bool is_new_key = (entry->key == NULL);

    if (is_new_key && entry->value.as.i == 0) {
        table->count++;
    }

    entry->key = key;
    entry->value = value;

    return is_new_key;
}

bool orso_symbol_table_remove(symbol_table_t* table, OrsoSymbol* key) {
    if (table->count == 0) {
        return false;
    }

    OrsoSymbolTableEntry* entry = find_entry(table->entries, table->capacity, key);
    if (entry == NULL) {
        return false;
    }

    entry->key = NULL;
    entry->value.as.i = 1;

    return true;
}

void orso_symbol_table_add_all(symbol_table_t* source, symbol_table_t* destination) {
    for (i32 i = 0; i < source->capacity; i++) {
        OrsoSymbolTableEntry* entry = &source->entries[i];
        if (entry->key == NULL) {
            continue;
        }

        orso_symbol_table_set(destination, entry->key, entry->value);
    }
}

OrsoSymbol* orso_symbol_table_find_cstrn(symbol_table_t* table, const char* start, i32 length, u32 hash) {
    if (table->count == 0) {
        return NULL;
    }

    u32 index = hash & (table->capacity - 1);
    for (;;) {
        OrsoSymbolTableEntry* entry = &table->entries[index++];

        if (entry->key == NULL) {
            if (entry->value.as.i == 0) {
                return NULL;
            }
        } else if (entry->key->length == length && entry->key->hash == hash
                && memcmp(entry->key->text, start, length) == 0) {
            return entry->key;
        }

        index = index & (table->capacity - 1);
    }
}

#undef GROW_CAPACITY
