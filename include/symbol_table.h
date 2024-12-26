#ifndef SYMBOL_TABLE_H_
#define SYMBOL_TABLE_H_

#include "def.h"
#include "slot.h"

#include "arena.h"

#define ORSO_SYMBOL_TABLE_MAX_LOAD 0.75

struct symbol_t;

typedef struct symbol_table_entry_t {
    struct symbol_t* key;
    word_t value;
} symbol_table_entry_t;

typedef struct symbol_table_t {
    i32 count;
    i32 capacity;
    symbol_table_entry_t* entries;
    arena_t *allocator;
} symbol_table_t;

void symbol_table_init(symbol_table_t *symbol_table, arena_t *allocator);
bool symbol_table_get(symbol_table_t *table, struct symbol_t *symbol, word_t *value);
bool symbol_table_set(symbol_table_t *table, struct symbol_t *symbol, word_t value);
bool symbol_table_remove(symbol_table_t* table, struct symbol_t *key);
void symbol_table_add_all(symbol_table_t* source, symbol_table_t *destination);

struct symbol_t* symbol_table_find_cstrn(symbol_table_t *table, const char *start, i32 length, u32 hash);

#endif
