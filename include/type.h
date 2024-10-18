#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "def.h"
#include "lexer.h"
#include "stringt.h"

#define ORSO_UNION_NUM_MAX 4

typedef enum type_kind_t {
    ORSO_TYPE_INVALID = 0,      // error type
    ORSO_TYPE_UNDEFINED,        // used for blocks with returns
    ORSO_TYPE_UNRESOLVED,       // unresolved (not undefined, not error, not defined)
    ORSO_TYPE_VOID,             // null
    ORSO_TYPE_BOOL,             // true false
    ORSO_TYPE_INT32,            // [-2^32, 2^32 - 1]
    ORSO_TYPE_INT64,            // [-2^64, 2^64 - 1]
    ORSO_TYPE_FLOAT32,          // single precision IEEE 754 float
    ORSO_TYPE_FLOAT64,          // double precision IEEE 754 float
    ORSO_TYPE_STRING,           // "anything in here"
    ORSO_TYPE_SYMBOL,           // 'anything in here'
    ORSO_TYPE_TYPE,             // i32, void, type, () -> void, etc
    ORSO_TYPE_FUNCTION,         // (type1, type2, ..., typen) -> return_type OR (foo := 0, bar := "") -> return_type
    ORSO_TYPE_NATIVE_FUNCTION,  // (type1, type2, ..., typen) -> return_type
    ORSO_TYPE_POINTER,          // &type
    ORSO_TYPE_UNION,            // type1|type2|...|typen
    ORSO_TYPE_STRUCT,           // used for both anonymous and named
} type_kind_t;

typedef struct types_t types_t;
typedef struct type_t type_t;

typedef struct struct_field_t {
    char* name;
    type_t* type;

    // not relevant for hashing
    i32 offset;
} struct_field_t;

typedef struct struct_constant_t {
    char *name;
    type_t* type;
} struct_constant_t;

struct types_t {
    type_t **items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

struct type_t {
    type_kind_t kind;
    union {
        struct {
            types_t types;
        } union_;

        struct {
            type_t *return_type;
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
            type_t *type;
        } pointer;
    } data;
};

#define ORSO_TYPE_IS_UNION(TYPE) (TYPE->kind == ORSO_TYPE_UNION)
#define ORSO_TYPE_IS_FUNCTION(TYPE) (TYPE->kind == ORSO_TYPE_FUNCTION)
#define ORSO_TYPE_IS_NATIVE_FUNCTION(TYPE) (TYPE->kind == ORSO_TYPE_NATIVE_FUNCTION)
#define ORSO_TYPE_IS_STRUCT(TYPE) (TYPE->kind == ORSO_TYPE_STRUCT)
#define ORSO_TYPE_IS_POINTER(TYPE) (TYPE->kind == ORSO_TYPE_POINTER)
#define ORSO_TYPE_IS_INVALID(TYPE) (TYPE->kind == ORSO_TYPE_INVALID)
#define ORSO_TYPE_IS_UNDEFINED(TYPE) (TYPE->kind == ORSO_TYPE_UNDEFINED)
#define ORSO_TYPE_IS_UNRESOLVED(TYPE) (TYPE->kind == ORSO_TYPE_UNRESOLVED)

struct type_set_t;

bool orso_union_type_has_type(type_t *type, type_t *subtype);

bool orso_struct_type_is_incomplete(type_t *type);

bool orso_type_equal(type_t *a, type_t *b);

// orso_type_has_type_kind
bool orso_union_type_has_type(type_t *type, type_t *subtype);
bool orso_union_type_contains_type(type_t *union_, type_t *type);

type_t* orso_type_merge(struct type_set_t *set, type_t *a, type_t *b);

bool orso_type_is_float(type_t *type);
bool orso_type_is_integer(type_t *type, bool include_bool);
bool orso_type_is_number(type_t *type, bool include_bool);
FORCE_INLINE bool orso_type_is_unsigned_integer_type(type_t *type, bool include_bool){
    (void)type;
    (void)include_bool;
    return false;
}

// orso_has_float_type
bool orso_union_has_float(type_t *type);

bool orso_type_is_or_has_float(type_t *type);

// orso_has_integer_type
bool orso_union_has_integer(type_t *type, bool include_bool);

bool orso_type_is_or_has_integer(type_t *type, bool include_bool);

i32 orso_bytes_to_slots(i32 byte_count);

u32 orso_type_size_bytes(type_t *type);

struct_field_t *orso_type_struct_find_field(type_t *struct_, const char *name, size_t name_length);

bool orso_integer_fit(type_t *storage_type, type_t *value_type, bool include_bool);

i32 orso_type_slot_count(type_t *type);

bool orso_type_fits(type_t *storage_type, type_t *value_type);

string_t type_to_string(type_t *type, arena_t *allocator);

bool orso_is_gc_type(type_t *type);

bool can_cast_implicit(type_t *type_to_cast, type_t *type);

type_t *orso_binary_arithmetic_cast(type_t *a, type_t *b, token_type_t operation);

void orso_binary_comparison_casts(type_t *a, type_t *b, type_t **a_cast, type_t **b_cast);

void orso_binary_equality_casts(type_t *a, type_t *b, type_t **a_cast, type_t **b_cast);

//void generate_struct_layout()

#endif
