#ifndef OBJECT_H_
#define OBJECT_H_

#include "def.h"
#include "type_set.h"
#include "slot.h"
#include "arena.h"

typedef struct object_t {
    type_t type;
} object_t;

typedef struct OrsoString {
    object_t object;
    i32 length;
    char text[];
} OrsoString;

typedef void (*native_function_interface_t)(word_t *arguments, word_t *result);

typedef struct native_function_t {
    object_t object;
    type_t signature;
    native_function_interface_t function;
} native_function_t;

FORCE_INLINE u32 hash_cstrn(const char *start, i32 length) {
    u32 hash = 2166136261u;
    for (i32 i = 0; i < length; i++) {
        hash ^= (byte)start[i];
        hash *= 16777619;
    }

    return hash;
}

FORCE_INLINE bool string_equal(OrsoString *a, OrsoString *b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->text, b->text, a->length) == 0;
}

OrsoString *orso_string_concat(OrsoString *a, OrsoString *b, arena_t *allocator);

string_t bytes_to_string(byte *data, type_infos_t *types, type_t type, arena_t *allocator);

void copy_bytes_to_slots(void *destination, void *source, type_kind_t type_kind, u64 size_bytes);

native_function_t *orso_new_native_function(native_function_interface_t function, type_t type, arena_t *allocator);

i64 cstrn_to_i64(const char *text, i32 length);
f64 cstrn_to_f64(const char *text, i32 length);

#endif
