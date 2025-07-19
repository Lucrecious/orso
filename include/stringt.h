#ifndef STRINGT_H_
#define STRINGT_H_

#include "stddef.h"
#include "arena.h"
#include "array.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

typedef struct string_view_t string_view_t;
struct string_view_t {
    orcstr_t data;
    size_t length;
};

typedef struct strings_t strings_t;
struct strings_t {
    orstring_t *items;
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
orstring_t cstrn2string(orcstr_t cstr, size_t n, arena_t *allocator);
orstring_t cstr2string(orcstr_t cstr, arena_t *allocator);
orstring_t string_format(orcstr_t format, arena_t *allocator, ...);
strings_t string_split(orcstr_t cstr, orcstr_t delimiters, arena_t *allocator);
orstring_t string_copy(orstring_t s, arena_t *allocator);
orstring_t string_path_combine(orstring_t a, orstring_t b, arena_t *arena);

size_t string2size(orstring_t s);

oru64 cstrn_to_u64(const char* text, size_t length);
orf64 cstrn_to_f64(const char* text, size_t length);

#define cstr2sv(cstr) (string_view_t){.data=(cstr), .length=strlen(cstr)}
string_view_t string2sv(orstring_t string);
orstring_t sv2string(string_view_t sv, arena_t *allocator);
string_view_t sv_filename(string_view_t sv);
string_view_t sv_dir_no_separator(string_view_t sv);
bool sv_ends_with(string_view_t sv, orcstr_t cstr);
bool sv_starts_with(string_view_t sv, orcstr_t prefix);

void sb_add_char(string_builder_t *sb, char c);
void sb_add_cstr(string_builder_t *sb, orcstr_t cstr);
void sb_add_format(string_builder_t *sb, orcstr_t format, ...);
orstring_t sb_render(string_builder_t *builder, arena_t *allocator);
orstring_t bytes2alphanum(char *s, size_t length, arena_t *arena);
bool core_abspath(orstring_t relpath, arena_t *arena, orstring_t *result);
bool core_fileid(orstring_t absolute_path, string_builder_t *result);


#define str(lit) ((orstring_t){ .cstr = (lit), .length = (sizeof(lit)/sizeof(char) - sizeof(char)) })
#define lit2str(lit) str(lit)
#define lit2sv(lit) ((string_view_t){ .data = lit, .length = (sizeof(lit)/sizeof(char) - sizeof(char)) })

#endif

#ifdef STRINGT_IMPLEMENTATION

#include "tmp.h"

bool sv_eq(string_view_t a, string_view_t b) {
    if (a.length != b.length) return false;
    return strncmp(a.data, b.data, a.length) == 0;
}

orstring_t cstrn2string(const orcstr_t cstr, size_t n, arena_t *allocator) {
    char *new_cstr = (char*)arena_alloc(allocator, (n + 1)*sizeof(char));
    memcpy(new_cstr, cstr, n);
    new_cstr[n] = '\0';
    orstring_t s = { .cstr = new_cstr, .length = n };
    return s;
}

orstring_t cstr2string(const orcstr_t cstr, arena_t *allocator) {
    size_t size = strlen(cstr);
    return cstrn2string(cstr, size, allocator);
}

orstring_t string_format(const orcstr_t format, arena_t *allocator, ...) {
	va_list args;
    va_list args_copy;
	va_start(args, allocator);
    va_copy(args_copy, args);

	int size = vsnprintf(NULL, 0, format, args) + 1;

	char *buffer = (char*)arena_alloc(allocator, (size < 0 ? 0 : (size_t)size) * sizeof(char));

    va_end(args);

	vsnprintf(buffer, (size_t)size, format, args_copy);

	va_end(args_copy);

	return (orstring_t){ .cstr = buffer, .length = (size_t)(size < 0 ? 0 : size - 1) };
}

#if WIN_32_
#define FS '\\'
#else
#define FS '/'
#endif
orstring_t string_path_combine(orstring_t a, orstring_t b, arena_t *arena) {
    string_view_t asv = string2sv(a);
    while (asv.length > 0 && asv.data[asv.length-1] == FS) {
        --asv.length;
    }

    string_view_t bsv = string2sv(b);
    while (bsv.length > 0 && (bsv.data[0] == FS || bsv.data[0] == '.')) {
        --bsv.length;
        ++bsv.data;
    }

    orstring_t result = string_format("%.*s/%.*s", arena, asv.length, asv.data, bsv.length, bsv.data);
    return result;
}
#undef FS

strings_t string_split(orcstr_t cstr, orcstr_t delimiters, arena_t *allocator) {
    tmp_arena_t *tmp = allocator_borrow();
    string_builder_t sb = {.allocator=tmp->allocator};

    strings_t split = {.allocator=allocator};

    for (orcstr_t c = cstr; *c; ++c) {
        bool split_here = false;
        for (orcstr_t d = delimiters; *d; ++d) {
            if (*d == *c) {
                split_here = true;
                break;
            }
        }

        unless (split_here) {
            sb_add_char(&sb, *c);
        } else {
            if (sb.count > 0) {
                orstring_t s = sb_render(&sb, allocator);
                array_push(&split, s);
            }

            sb.count = 0;
        }
    }

    if (sb.count > 0) {
        orstring_t s = sb_render(&sb, allocator);
        array_push(&split, s);
    }

    allocator_return(tmp);

    return split;
}

orstring_t string_copy(orstring_t s, arena_t *allocator) {
    char *copy = arena_alloc(allocator, sizeof(char)*(s.length+1));
    strncpy(copy, s.cstr, s.length);
    copy[s.length] = '\0';

    orstring_t result = {.cstr=copy, .length=s.length};
    return result;
}

size_t string2size(orstring_t s) {
    size_t size = strtoul(s.cstr, NULL, 10);
    return size;
}

string_view_t string2sv(orstring_t string) {
    return (string_view_t){.data=string.cstr, .length=string.length};
}

orstring_t sv2string(string_view_t sv, arena_t *allocator) {
    char *s = arena_alloc(allocator, sizeof(char)*(sv.length+1));
    strncpy(s, sv.data, sv.length);
    s[sv.length] = '\0';

    orstring_t result = {.cstr=s, .length=sv.length};
    return result;
}

string_view_t sv_no_ext(string_view_t sv) {
    if (sv.length == 0) return sv;
    size_t end = sv.length;

    for (size_t i = sv.length; i > 0; --i) {
        if (sv.data[i-1] == '.') {
            end = i-1;
            break;
        }
    }

    sv.length = end;
    return sv;
}

string_view_t sv_filename(string_view_t sv) {
    if (sv.length == 0) return sv;
    size_t begin = 0;

    for (size_t i = sv.length; i > 0; --i) {
        if (sv.data[i-1] == '\\' || sv.data[i-1] == '/') {
            begin = i;
            break;
        }
    }

    return (string_view_t){.data=sv.data+begin, .length=sv.length-begin};
}

string_view_t sv_dir_no_separator(string_view_t sv) {
    for (size_t i = sv.length; i > 0; --i) {
        if (sv.data[i] == '\\' || sv.data[i] == '/') {
            string_view_t dir = {
                .data = sv.data,
                .length = i,
            };
            return dir;
        }
    }

    return sv;
}

bool sv_ends_with(string_view_t sv, orcstr_t cstr) {
    size_t len = strlen(cstr);
    if (len > sv.length) return false;
    if (len == 0) return true;

    for (size_t i = 0; i < len; ++i) {
        if (sv.data[sv.length - i - 1] != cstr[len - i - 1]) return false;
    }

    return true;
}

bool sv_starts_with(string_view_t sv, orcstr_t prefix) {
    size_t s = strlen(prefix);
    if (s > sv.length) return false;

    bool same = (strncmp(sv.data, prefix, s) == 0);
    return same;
}

void sb_add_char(string_builder_t *builder, char c) {
    array_push(builder, c);
}

void sb_add_cstr(string_builder_t *builder, orcstr_t cstr) {
    orcstr_t c = cstr;
    while (*c) {
        array_push(builder, *c);
        c = ++cstr;
    }
}

void sb_add_format(string_builder_t *sb, orcstr_t format, ...) {
	va_list args;
	va_start(args, format);

	int size = vsnprintf(NULL, 0, format, args);
	va_end(args);

	if (size <= 0) {
		return;
	}

    tmp_arena_t *tmp = allocator_borrow();

	char *buffer = arena_alloc(tmp->allocator, (size_t)(size + 1));

	va_start(args, format);
	vsnprintf(buffer, (size_t)(size + 1), format, args);
	va_end(args);

	for (char *c = buffer; *c; ++c) {
		array_push(sb, *c);
	}

    allocator_return(tmp);
}

orstring_t sb_render(string_builder_t *builder, arena_t *allocator) {
    return cstrn2string(builder->items, builder->count, allocator);
}

char base64[64] = {
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_', '.',
};

orstring_t bytes2alphanum(char *s, size_t length, arena_t *arena) {
    tmp_arena_t *tmp = allocator_borrow();
    
    string_builder_t sb = {.allocator=tmp->allocator};

    sb_add_char(&sb, 'x');

    for (size_t i = 0; i < length; i += 3) {
        oru8 a = (oru8)s[i];
        oru8 b = (oru8)(i+1 < length ? s[i+1] : '_');
        oru8 c = (oru8)(i+2 < length ? s[i+2] : '_');
        ors32 combined = a | (b << 8) | (c << 16);

        char chars[4];

        chars[0] = (char)(combined & 0x3f);
        chars[1] = (char)((combined & 0xFC0) >> 6);
        chars[2] = (char)((combined & 0x3F000) >> 12);
        chars[3] = (char)((combined & 0xFC0000) >> 18);

        for (size_t i = 0; i < 4; ++i) {
            chars[i] = base64[(int)chars[i]];
            if (chars[i] == '.') {
                sb_add_cstr(&sb, "ab");
            } else {
                sb_add_char(&sb, chars[i]);
            }
        }
    }

    orstring_t ret = sb_render(&sb, arena);

    allocator_return(tmp);

    return ret;
}

#ifdef _WIN32

string_t core_abspath(string_t relpath, arena_t *arena) {
    #error "not implemented"
}

bool core_fileid(string_t absolute_path, string_builder_t *result) {
    #error "not implemented"
}

#else

#include <limits.h>
#include <sys/stat.h>

bool core_abspath(orstring_t relpath, arena_t *arena, orstring_t *result) {
    char resolved_path[PATH_MAX];
    if (realpath(relpath.cstr, resolved_path) == NULL) {
        *result = lit2str("");
        return false;
    }

    *result = cstr2string(resolved_path, arena);
    return true;
}

bool core_fileid(orstring_t absolute_path, string_builder_t *result) {
    struct stat sb;
    if (stat(absolute_path.cstr, &sb) == -1) return false;

    #define IDSIZE (sizeof(sb.st_dev)+sizeof(sb.st_ino))
    char bytes[IDSIZE];
    memcpy(bytes, &sb.st_dev, sizeof(sb.st_dev));
    memcpy(bytes+sizeof(sb.st_dev), &sb.st_ino, sizeof(sb.st_ino));

    for (size_t i = 0; i < IDSIZE; ++i) {
        char c = bytes[i];
        sb_add_char(result, c);
    }

    #undef IDSIZE

    return true;
}

#endif


oru64 cstrn_to_u64(const char* text, size_t length) {
    oru64 integer = 0;

    for (size_t i = 0; i < length; i++) {
        char digit = text[i];
        if (digit == '_') {
            continue;
        }

        int ones = text[i] - '0';
        if (ones < 0 || ones > 9) {
            break;
        }

        integer *= 10;
        integer += (oru64)ones;
    }

    return integer;
}

orf64 cstrn_to_f64(const char* text, size_t length) {
    orf64 value = 0;
    orf64 fact = 1;
    bool point_seen = false;

    for (char* c = (char*)text; c != (text + length); c++) {
        if (*c == '.') {
            point_seen = true;
            continue;
        }

        if (*c == '_') {
            continue;
        }

        ors32 digit = *c - '0';
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
