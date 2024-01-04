#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "def.h"
#include "lexer.h"

#define ORSO_UNION_NUM_MAX 4

typedef enum OrsoTypeKind {
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
} OrsoTypeKind;

struct OrsoType;
typedef struct OrsoType OrsoType;

typedef struct OrsoStructField {
    char* name;
    OrsoType* type;

    // not relevant for hashing
    i32 offset;
} OrsoStructField;

typedef struct OrsoStructConstant {
    char *name;
    OrsoType* type;
} OrsoStructConstant;

struct OrsoType {
    OrsoTypeKind kind;
    union {
        struct {
            i32 count;
            OrsoType** types;
        } union_;

        struct {
            OrsoType* return_type;
            i32 argument_count;
            OrsoType** argument_types;
        } function;

        struct {
            // null if anonymous
            char* name;

            i32 field_count;
            OrsoStructField* fields;

            i32 constant_count;
            OrsoStructConstant* constants;

            // not relevant for hashing
            i32 total_bytes;
        } struct_;

        struct {
            OrsoType* type;
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

struct OrsoTypeSet;

bool orso_union_type_has_type(OrsoType* type, OrsoType* subtype);

bool orso_struct_type_is_incomplete(OrsoType* type);

bool orso_type_equal(OrsoType* a, OrsoType* b);

// orso_type_has_type_kind
bool orso_union_type_has_type(OrsoType* type, OrsoType* subtype);
bool orso_union_type_contains_type(OrsoType* union_, OrsoType* type);

OrsoType* orso_type_merge(struct OrsoTypeSet* set, OrsoType* a, OrsoType* b);

bool orso_type_is_float(OrsoType* type);
bool orso_type_is_integer(OrsoType* type, bool include_bool);
bool orso_type_is_number(OrsoType* type, bool include_bool);
FORCE_INLINE bool orso_type_is_unsigned_integer_type(OrsoType* type, bool include_bool){
    (void)type;
    (void)include_bool;
    return false;
}

// orso_has_float_type
bool orso_union_has_float(OrsoType* type);

bool orso_type_is_or_has_float(OrsoType* type);

// orso_has_integer_type
bool orso_union_has_integer(OrsoType* type, bool include_bool);

bool orso_type_is_or_has_integer(OrsoType* type, bool include_bool);

i32 orso_bytes_to_slots(i32 byte_count);

u32 orso_type_size_bytes(OrsoType* type);

OrsoStructField* orso_type_struct_find_field(OrsoType* struct_, const char* name, size_t name_length);

bool orso_integer_fit(OrsoType* storage_type, OrsoType* value_type, bool include_bool);

i32 orso_type_slot_count(OrsoType* type);

bool orso_type_fits(OrsoType* storage_type, OrsoType* value_type);

i32 orso_type_to_cstrn(OrsoType* type, char* buffer, i32 n);

bool orso_is_gc_type(OrsoType* type);

OrsoType* orso_binary_arithmetic_cast(OrsoType* a, OrsoType* b, TokenType operation);

void orso_binary_comparison_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast);

void orso_binary_equality_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast);

//void generate_struct_layout()

#endif
