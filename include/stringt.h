#ifndef STRINGT_H_
#define STRINGT_H_

#include "stddef.h"
#include "arena.h"
#include "array.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

typedef char* cstr_t;

typedef struct string_t string_t;
struct string_t {
    cstr_t cstr;
    size_t length;
};

typedef struct string_builder_t string_builder_t;
struct string_builder_t {
    cstr_t items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

string_t cstrn2string(const cstr_t cstr, size_t n, arena_t *allocator);
string_t cstr2string(const cstr_t cstr, arena_t *allocator);
string_t string_format(const cstr_t format, arena_t *allocator, ...);

void sb_add_char(string_builder_t *builder, char c);
void sb_add_cstr(string_builder_t *builder, cstr_t cstr);
string_t sb_render(string_builder_t *builder, arena_t *allocator);

#define str(string_literal) (string_t){ .cstr = string_literal, .length = (sizeof(string_literal)/sizeof(char) - sizeof(char)) };

#endif

#ifdef STRINGT_IMPLEMENTATION

string_t cstrn2string(const cstr_t cstr, size_t n, arena_t *allocator) {
    char *new_cstr = (char*)arena_alloc(allocator, (n + 1)*sizeof(char));
    memcpy(new_cstr, cstr, n);
    new_cstr[n] = '\0';
    string_t s = { .cstr = new_cstr, .length = n };
    return s;
}

string_t cstr2string(const cstr_t cstr, arena_t *allocator) {
    size_t size = strlen(cstr);
    return cstrn2string(cstr, size, allocator);
}

string_t string_format(const cstr_t format, arena_t *allocator, ...) {
	va_list args;
    va_list args_copy;
	va_start(args, allocator);
    va_copy(args_copy, args);

	int size = vsnprintf(NULL, 0, format, args) + 1;

	char *buffer = (char*)arena_alloc(allocator, size * sizeof(char));

    va_end(args);

	vsnprintf(buffer, size, format, args_copy);

	va_end(args_copy);

	return (string_t){ .cstr = buffer, .length = size - 1 };
}

void sb_add_char(string_builder_t *builder, char c) {
    array_push(builder, c);
}

void sb_add_cstr(string_builder_t *builder, cstr_t cstr) {
    char *c = cstr;
    while (*c) {
        array_push(builder, *c);
        c = ++cstr;
    }
}

string_t sb_render(string_builder_t *builder, arena_t *allocator) {
    return cstrn2string(builder->items, builder->count, allocator);
}

#endif
