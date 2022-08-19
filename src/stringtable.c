#include "stringtable.h"

#include <stdlib.h>
#include <string.h>

#include "sb.h"

#define TABLE_MAX_LOAD 0.75

#define GROW_CAPACITY(x) (x) < 8 ? 8 : (x) * 2;

void string_table_init(StringTable* table) {
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void string_table_free(StringTable* table) {
    for (i32 i = 0; i < table->capacity; i++) {
        if (table->entries[i].key == NULL) {
            continue;
        }

        savine_string_free(table->entries[i].key);
        free(table->entries[i].key);
        table->entries[i].key = NULL;
    }

    free(table->entries);
    string_table_init(table);
}

static Entry* find_entry(Entry* entries, i32 capacity, SavineString* key) {
    i32 index = key->hash % capacity;
    Entry* tombstone = NULL;
    for (;;) {
        Entry* entry = &entries[index];
        if (entry->key == NULL) {
            if (!entry->value) {
                return tombstone != NULL ? tombstone : entry;
            } else {
                if (tombstone == NULL) {
                    tombstone = entry;
                }
            }
        } else if (entry->key == key) {
            return entry;
        }

        index += (index + 1) % capacity;
    }
}

static void adjust_capacity(StringTable* table, i32 capacity) {
    Entry* entries = (Entry*)malloc(sizeof(Entry) * capacity);
    for (i32 i = 0; i < capacity; i++) {
        entries->key = NULL;
        entries->value = false;
    }

    table->count = 0;
    for (i32 i = 0; i < table->capacity; i++) {
        Entry* entry = &table->entries[i];
        if (entry->key == NULL) {
            continue;
        }

        Entry* dest = find_entry(entries, capacity, entry->key);
        dest->key = entry->key;
        dest->value = entry->value;
        table->count++;
    }

    free(table->entries);

    table->entries = entries;
    table->capacity = capacity;
}

bool string_table_set(StringTable* table, SavineString* key, i64 value) {
    if (table->count + 1 > table->capacity * TABLE_MAX_LOAD) {
        i32 capacity = GROW_CAPACITY(table->capacity);
        adjust_capacity(table, capacity);
    }

    Entry* entry = find_entry(table->entries, table->capacity, key);
    bool is_new_key = entry->key == NULL;
    if (is_new_key && !entry->value) {
        table->count++;
    }

    entry->key = key;
    entry->value = value;
    return is_new_key;
}

bool string_table_get(StringTable* table, SavineString* key, i64* value) {
    if (table->count == 0) {
        return false;
    }

    Entry* entry = find_entry(table->entries, table->capacity, key);
    if (entry->key == NULL) {
        return false;
    }

    *value = entry->value;
    return true;
}

bool string_table_delete(StringTable* table, SavineString* key) {
    if (table->count == 0) {
        return false;
    }

    Entry* entry = find_entry(table->entries, table->capacity, key);
    if (entry == NULL) {
        return false;
    }

    entry->key = NULL;
    entry->value = true;
    return true;
}

void string_table_add_all(StringTable* src, StringTable* dst) {
    for (int i = 0; i < src->capacity; i++) {
        Entry* entry = &src->entries[i];
        if (entry->key != NULL) {
            string_table_set(dst, entry->key, entry->value);
        }
  }
}