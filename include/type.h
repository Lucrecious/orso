#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "def.h"
#include "lexer.h"
#include "stringt.h"

typedef struct type_t type_t;
struct type_t {
    u64 i;
};

typedef struct types_t types_t;
struct types_t {
    type_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct type_infos_t type_infos_t;
typedef struct type_info_t type_info_t;

typedef enum type_kind_t {
    TYPE_INVALID = 0,      // error type
    TYPE_UNDEFINED,        // used for blocks with returns
    TYPE_UNRESOLVED,       // unresolved (not undefined, not error, not defined)
    TYPE_VOID,             // null
    TYPE_BOOL,             // true false
    TYPE_INT32,            // [-2^32, 2^32 - 1]
    TYPE_INT64,            // [-2^64, 2^64 - 1]
    TYPE_FLOAT32,          // single precision IEEE 754 float
    TYPE_FLOAT64,          // double precision IEEE 754 float
    TYPE_STRING,           // "anything in here"
    TYPE_SYMBOL,           // 'anything in here'
    TYPE_TYPE,             // i32, void, type, () -> void, etc
    TYPE_FUNCTION,         // (type1, type2, ..., typen) -> return_type OR (foo := 0, bar := "") -> return_type
    TYPE_NATIVE_FUNCTION,  // (type1, type2, ..., typen) -> return_type
    TYPE_POINTER,          // &type
    TYPE_UNION,            // type1|type2|...|typen
    TYPE_STRUCT,           // used for both anonymous and named

    TYPE_COUNT,
} type_kind_t;

typedef struct struct_field_t {
    char* name;
    type_t type;

    // not relevant for hashing
    i32 offset;
} struct_field_t;

typedef struct struct_constant_t {
    char *name;
    type_t type;
} struct_constant_t;

struct type_infos_t {
    type_info_t **items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

struct type_info_t {
    type_kind_t kind;
    union {
        struct {
            types_t types;
        } union_;

        struct {
            type_t return_type;
            types_t argument_types;
        } function;

        struct {
            // null if anonymous
            char *name;

            i32 field_count;
            struct_field_t *fields;

            i32 constant_count;
            struct_constant_t *constants;

            // not relevant for hashing
            i32 total_bytes;
        } struct_;

        struct {
            type_t type;
        } pointer;
    } data;
};

#define TYPE_IS_VOID(TYPE) ((TYPE).i == TYPE_VOID)
#define TYPE_IS_TYPE(TYPE) ((TYPE).i == TYPE_TYPE)
#define TYPE_IS_INVALID(TYPE) ((TYPE).i == TYPE_INVALID)
#define TYPE_IS_UNDEFINED(TYPE) ((TYPE).i == TYPE_UNDEFINED)
#define TYPE_IS_UNRESOLVED(TYPE) ((TYPE).i == TYPE_UNRESOLVED)

struct type_table_t;

bool struct_type_is_incomplete(type_info_t *type);

bool type_equal(type_info_t *a, type_info_t *b);

// orso_type_has_type_kind
bool union_type_has_type(type_info_t *type, type_t subtype);
bool union_type_contains_type(type_infos_t types, type_t union_, type_t type);

type_t type_merge(struct type_table_t *set, type_t a, type_t b);

bool type_is_float(type_info_t *type);
bool type_is_integer(type_info_t *type, bool include_bool);
bool type_is_number(type_info_t *type, bool include_bool);
FORCE_INLINE bool orso_type_is_unsigned_integer_type(type_info_t *type, bool include_bool){
    (void)type;
    (void)include_bool;
    return false;
}

size_t bytes_to_slots(i32 byte_count);

u32 type_size_bytes(type_info_t *type);

struct_field_t *type_struct_find_field(type_info_t *struct_, const char *name, size_t name_length);

size_t type_slot_count(type_info_t *type);

bool type_fits(type_info_t *storage_type, type_info_t *value_type);

string_t type_to_string(type_infos_t types, type_t type_id, arena_t *allocator);

bool orso_is_gc_type(type_info_t *type);

bool can_cast_implicit(type_infos_t types, type_t type_id_to_cast, type_t type_id);

#endif
