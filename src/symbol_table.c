#include "symbol_table.h"

#include "type_set.h"
#include "object.h"

#define GROW_CAPACITY(capacity) capacity == 0 ? 8 : capacity * 2

void symbol_table_init(symbol_table_t* table, arena_t *allocator) {
    table->entries = NULL;
    table->count = 0;
    table->capacity = 0;
    table->allocator = allocator;
}

static symbol_table_entry_t* find_entry(symbol_table_entry_t* entries, i32 capacity, symbol_t* key) {
    u32 index = key->hash & (capacity - 1);
    symbol_table_entry_t* tombstone = NULL;

    for (;;) {
        symbol_table_entry_t* entry = &entries[index];

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
    symbol_table_entry_t* entries = arena_alloc(table->allocator, sizeof(symbol_table_entry_t)*capacity);
    for (i32 i = 0; i < capacity; i++) {
        entries[i].key = NULL;
        entries[i].value = SLOT_I(0);
    }

    table->count = 0;
    for (i32 i = 0; i < table->capacity; i++) {
        symbol_table_entry_t* entry = &table->entries[i];
        if (entry->key == NULL) {
            continue;
        }

        symbol_table_entry_t* destination = find_entry(entries, capacity, entry->key);
        destination->key = entry->key;
        destination->value = entry->value;
        table->count++;
    }

    table->entries = entries;
    table->capacity = capacity;
}

bool symbol_table_get(symbol_table_t* table, symbol_t* symbol, slot_t* value) {
    if (table->count == 0) {
        return false;
    }

    symbol_table_entry_t* entry = find_entry(table->entries, table->capacity, symbol);
    if (entry->key == NULL) {
        return false;
    }

    *value = entry->value;
    return true;
}

bool symbol_table_set(symbol_table_t* table, symbol_t* key, slot_t value) {
    if (table->count + 1 > table->capacity * ORSO_SYMBOL_TABLE_MAX_LOAD) {
        i32 capacity = GROW_CAPACITY(table->capacity);
        adjust_capacity(table, capacity);
    }

    symbol_table_entry_t* entry = find_entry(table->entries, table->capacity, key);
    bool is_new_key = (entry->key == NULL);

    if (is_new_key && entry->value.as.i == 0) {
        table->count++;
    }

    entry->key = key;
    entry->value = value;

    return is_new_key;
}

bool symbol_table_remove(symbol_table_t* table, symbol_t* key) {
    if (table->count == 0) {
        return false;
    }

    symbol_table_entry_t* entry = find_entry(table->entries, table->capacity, key);
    if (entry == NULL) {
        return false;
    }

    entry->key = NULL;
    entry->value.as.i = 1;

    return true;
}

void symbol_table_add_all(symbol_table_t* source, symbol_table_t* destination) {
    for (i32 i = 0; i < source->capacity; i++) {
        symbol_table_entry_t* entry = &source->entries[i];
        if (entry->key == NULL) {
            continue;
        }

        symbol_table_set(destination, entry->key, entry->value);
    }
}

symbol_t* symbol_table_find_cstrn(symbol_table_t* table, const char* start, i32 length, u32 hash) {
    if (table->count == 0) {
        return NULL;
    }

    u32 index = hash & (table->capacity - 1);
    for (;;) {
        symbol_table_entry_t* entry = &table->entries[index++];

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
