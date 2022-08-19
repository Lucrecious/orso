#include "def.h"
#include "stringtable.h"

StringTable interned;

SavineString* find_string(StringTable* table, const char* c_str, i32 length, i32 hash) {
    if (table->count == 0) {
        return NULL;
    }

    i32 index = hash % table->capacity;
    for (;;) {
        Entry* entry = &table->entries[index];
        if (entry->key == NULL) {
            if (!entry->value) {
                return NULL;
            } 
        } else if (entry->key->length == length && entry->key->hash == hash
            && memcmp(entry->key->str, c_str, length) == 0) {
            return entry->key;
        }

        index = index % table->capacity;
    }
}

void savine_string_init_interned() {
    string_table_init(&interned);
}

void savine_string_free_interned() {
    string_table_free(&interned);
}

char* c_str_stack_to_heap(const char* str) {
    i32 length = 0;
    while (str[length] != '\0') {
        length++;
    }

    char* new = (char*)malloc(sizeof(char) * (length + 1));
    memcpy(new, str, length);
    new[length] = '\0';

    return new;
}

static u32 hash(char* key, i32 length) {
    u32 hash = 2166136261u;
    for (i32 i = 0; i < length; i++) {
        hash ^= (uint8_t)key[i];
        hash *= 16777619;
    }

    return hash;
}

SavineString* c_strn_to_savine_string(char* c_str, i32 length) {
    u32 h = hash(c_str, length);
    SavineString* interned_str = find_string(&interned, c_str, length, h);
    if (interned_str != NULL) {
        free(c_str);

        return interned_str;
    }
    else {
        SavineString* string = (SavineString*)malloc(sizeof(SavineString));
        string->str = c_str;
        string->length = length;
        string->hash = h;
        string_table_set(&interned, string, false);

        return string;
    }

}

void savine_string_free(SavineString* string) {
    free(string->str);
    string->str = NULL;
}