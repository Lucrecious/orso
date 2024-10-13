#ifndef STRINGT_H_
#define STRINGT_H_

#include "stddef.h"
#include "arena.h"

typedef char* cstr_t;

typedef struct string_t {
    cstr_t cstr;
    size_t length;
} string_t;

string_t cstr2string(const cstr_t cstr, arena_t *allocator);

#endif

#ifdef STRINGT_IMPLEMENTATION

string_t cstr2string(const cstr_t cstr, arena_t *allocator) {
    size_t size = strlen(cstr);
    char *new_cstr = (char*)arena_alloc(allocator, (size + 1)*sizeof(char));
    strcpy(new_cstr, cstr);
    string_t s = { .cstr = new_cstr, .length = size };
    return s;
}

#endif