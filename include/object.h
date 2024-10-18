#ifndef OBJECT_H_
#define OBJECT_H_

#include "chunk.h"
#include "def.h"
#include "type_set.h"
#include "slot.h"
#include "symbol_table.h"
#include "arena.h"

typedef struct object_t {
    type_t *type;
} object_t;

typedef struct OrsoString {
    object_t object;
    i32 length;
    char text[];
} OrsoString;

typedef struct symbol_t {
    object_t object;
    i32 length;
    u32 hash;
    char text[];
} symbol_t;

typedef struct function_t {
    object_t object;
    chunk_t chunk;
    type_t *signature;
    symbol_t *binded_name;
} function_t;

typedef struct struct_t {
    slot_t *slots;
} struct_t;

typedef void (*native_function_interface_t)(slot_t *arguments, slot_t *result);

typedef struct native_function_t {
    object_t object;
    type_t* signature;
    native_function_interface_t function;
} native_function_t;

void* orso_object_reallocate(object_t *pointer, type_t *type, size_t old_size, size_t new_size);

#define ORSO_OBJECT_ALLOCATE_N(TYPE, ORSO_TYPE, N) (TYPE*)orso_object_reallocate(NULL, ORSO_TYPE, 0, sizeof(TYPE) * N)
#define ORSO_OBJECT_ALLOCATE(TYPE, ORSO_TYPE) ORSO_OBJECT_ALLOCATE_N(TYPE, ORSO_TYPE, 1)
#define ORSO_OBJECT_ALLOCATE_FLEX(TYPE, ORSO_TYPE, N) (TYPE*)orso_object_reallocate(NULL, ORSO_TYPE, 0, sizeof(TYPE) + N)

FORCE_INLINE u32 orso_hash_cstrn(const char *start, i32 length) {
    u32 hash = 2166136261u;
    for (i32 i = 0; i < length; i++) {
        hash ^= (byte)start[i];
        hash *= 16777619;
    }

    return hash;
}

FORCE_INLINE bool orso_string_equal(OrsoString *a, OrsoString *b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->text, b->text, a->length) == 0;
}

OrsoString *orso_string_concat(OrsoString *a, OrsoString *b);

string_t slot_to_string(slot_t *slot, type_t *type, arena_t *allocator);

OrsoString *orso_slot_to_string(slot_t *slot, type_t *type);

OrsoString *orso_new_string_from_cstrn(const char *start, i32 length);

function_t *orso_new_function(arena_t *allocator);
bool is_function_compiled(function_t *function);

native_function_t *orso_new_native_function(native_function_interface_t function, type_t *type);

struct_t *orso_new_struct(void);

i64 cstrn_to_i64(const char *text, i32 length);
f64 cstrn_to_f64(const char *text, i32 length);

symbol_t *orso_unmanaged_symbol_from_cstrn(const char *start, i32 length, symbol_table_t *symbol_table);

FORCE_INLINE void orso_unmanaged_symbol_free(symbol_t *symbol) {
    free(symbol);
}

symbol_t *orso_new_symbol_from_cstrn(const char *start, i32 length, symbol_table_t *symbol_table);

#endif
