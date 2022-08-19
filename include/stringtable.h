#ifndef STRING_TABLE_H_
#define STRING_TABLE_H_

#include "def.h"
#include "savine_string.h"

typedef struct Entry {
    SavineString* key;
    i64 value;
} Entry;

typedef struct StringTable {
    i32 count;
    i32 capacity;
    Entry* entries;
} StringTable;

void string_table_init(StringTable* table);

void string_table_free(StringTable* table);

bool string_table_set(StringTable* table, SavineString* key, i64 value);
bool string_table_get(StringTable* table, SavineString* key, i64* value);

bool string_table_delete(StringTable* table, SavineString* key);

void string_table_add_all(StringTable* src, StringTable* dst);

#endif