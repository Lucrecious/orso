#ifndef STRINGT_H_
#define STRINGT_H_

#include "stddef.h"
#include "arena.h"
#include "array.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

typedef struct string_t string_t;
struct string_t {
    cstr_t cstr;
    size_t length;
};

typedef struct string_view_t string_view_t;
struct string_view_t {
    cstr_t data;
    size_t length;
};

typedef struct strings_t strings_t;
struct strings_t {
    string_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct string_builder_t string_builder_t;
struct string_builder_t {
    char *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

#define cstr_eq(a, b) (strcmp(a, b) == 0)
#define string_eq(a, b) cstr_eq(a.cstr, b.cstr)
#define STRING_EMPTY (string_t){.cstr="", .length=0}

bool sv_eq(string_view_t a, string_view_t b);
string_t cstrn2string(cstr_t cstr, size_t n, arena_t *allocator);
string_t cstr2string(cstr_t cstr, arena_t *allocator);
string_t string_format(cstr_t format, arena_t *allocator, ...);
strings_t string_split(cstr_t cstr, cstr_t delimiters, arena_t *allocator);
string_t string_copy(string_t s, arena_t *allocator);

size_t string2size(string_t s);

s64 cstrn_to_u64(const char* text, s32 length);
f64 cstrn_to_f64(const char* text, s32 length);

#define cstr2sv(cstr) (string_view_t){.data=(cstr), .length=strlen(cstr)}
string_view_t string2sv(string_t string);
string_t sv2string(string_view_t sv, arena_t *allocator);
string_view_t sv_filename(string_view_t sv);
bool sv_ends_with(string_view_t sv, cstr_t cstr);

void sb_add_char(string_builder_t *sb, char c);
void sb_add_cstr(string_builder_t *sb, cstr_t cstr);
void sb_add_format(string_builder_t *sb, cstr_t format, ...);
string_t sb_render(string_builder_t *builder, arena_t *allocator);

#define str(lit) ((string_t){ .cstr = (lit), .length = (sizeof(lit)/sizeof(char) - sizeof(char)) })
#define lit2str(lit) str(lit)
#define lit2sv(lit) ((string_view_t){ .data = lit, .length = (sizeof(lit)/sizeof(char) - sizeof(char)) })

#endif

#ifdef STRINGT_IMPLEMENTATION

#include "tmp.h"

bool sv_eq(string_view_t a, string_view_t b) {
    if (a.length != b.length) return false;
    return strncmp(a.data, b.data, a.length) == 0;
}

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

strings_t string_split(cstr_t cstr, cstr_t delimiters, arena_t *allocator) {
    tmp_arena_t *tmp = allocator_borrow();
    string_builder_t sb = {.allocator=tmp->allocator};

    strings_t split = {.allocator=allocator};

    for (cstr_t c = cstr; *c; ++c) {
        bool split_here = false;
        for (cstr_t d = delimiters; *d; ++d) {
            if (*d == *c) {
                split_here = true;
                break;
            }
        }

        unless (split_here) {
            sb_add_char(&sb, *c);
        } else {
            if (sb.count > 0) {
                string_t s = sb_render(&sb, allocator);
                array_push(&split, s);
            }

            sb.count = 0;
        }
    }

    if (sb.count > 0) {
        string_t s = sb_render(&sb, allocator);
        array_push(&split, s);
    }

    allocator_return(tmp);

    return split;
}

string_t string_copy(string_t s, arena_t *allocator) {
    char *copy = arena_alloc(allocator, sizeof(char)*(s.length+1));
    strncpy(copy, s.cstr, s.length);
    copy[s.length] = '\0';

    string_t result = {.cstr=copy, .length=s.length};
    return result;
}

size_t string2size(string_t s) {
    size_t size = strtoul(s.cstr, NULL, 10);
    return size;
}

string_view_t string2sv(string_t string) {
    return (string_view_t){.data=string.cstr, .length=string.length};
}

string_t sv2string(string_view_t sv, arena_t *allocator) {
    char *s = arena_alloc(allocator, sizeof(char)*(sv.length+1));
    strncpy(s, sv.data, sv.length);
    s[sv.length] = '\0';

    string_t result = {.cstr=s, .length=sv.length};
    return result;
}

string_view_t sv_filename(string_view_t sv) {
    if (sv.length == 0) return sv;
    size_t begin = 0;

    for (size_t i = sv.length-1; i >= 0; --i) {
        if (sv.data[i] == '\\' || sv.data[i] == '/') {
            begin = i+1;
            break;
        }
    }

    return (string_view_t){.data=sv.data+begin, .length=sv.length-begin};
}

bool sv_ends_with(string_view_t sv, cstr_t cstr) {
    size_t len = strlen(cstr);
    if (len > sv.length) return false;
    if (len == 0) return true;

    for (size_t i = 0; i < len; ++i) {
        if (sv.data[sv.length - i - 1] != cstr[len - i - 1]) return false;
    }

    return true;
}

void sb_add_char(string_builder_t *builder, char c) {
    array_push(builder, c);
}

void sb_add_cstr(string_builder_t *builder, cstr_t cstr) {
    cstr_t c = cstr;
    while (*c) {
        array_push(builder, *c);
        c = ++cstr;
    }
}

void sb_add_format(string_builder_t *sb, cstr_t format, ...) {
	va_list args;
	va_start(args, format);

	int size = vsnprintf(NULL, 0, format, args);
	va_end(args);

	if (size <= 0) {
		return;
	}

    tmp_arena_t *tmp = allocator_borrow();

	char *buffer = arena_alloc(tmp->allocator, (size + 1));

	va_start(args, format);
	vsnprintf(buffer, size + 1, format, args);
	va_end(args);

	for (char *c = buffer; *c; ++c) {
		array_push(sb, *c);
	}

    allocator_return(tmp);
}

string_t sb_render(string_builder_t *builder, arena_t *allocator) {
    return cstrn2string(builder->items, builder->count, allocator);
}

s64 cstrn_to_u64(const char* text, s32 length) {
    s64 integer = 0;

    for (s32 i = 0; i < length; i++) {
        char digit = text[i];
        if (digit == '_') {
            continue;
        }

        s32 ones = text[i] - '0';
        if (ones < 0 || ones > 9) {
            break;
        }

        integer *= 10;
        integer += ones;
    }

    return integer;
}

f64 cstrn_to_f64(const char* text, s32 length) {
    f64 value = 0;
    f64 fact = 1;
    bool point_seen = false;

    for (char* c = (char*)text; c != (text + length); c++) {
        if (*c == '.') {
            point_seen = true;
            continue;
        }

        if (*c == '_') {
            continue;
        }

        s32 digit = *c - '0';
        if (digit < 0 || digit > 9) {
            break;
        }

        if (point_seen) {
            fact /= 10.0f;
        }

        value = value * 10.0f + (float)digit;
    }

    return value * fact;
}

#undef STRINGT_IMPLEMENTATION
#endif
