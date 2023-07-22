#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "def.h"
#include "lexer.h"

#define ORSO_UNION_NUM_MAX 4

typedef enum OrsoTypeKind {
    ORSO_TYPE_INVALID = 0,
    ORSO_TYPE_UNDEFINED, // used for blocks with returns
    ORSO_TYPE_UNRESOLVED,
    ORSO_TYPE_VOID,
    ORSO_TYPE_BOOL,
    ORSO_TYPE_INT32,
    ORSO_TYPE_INT64,
    ORSO_TYPE_FLOAT32,
    ORSO_TYPE_FLOAT64,
    ORSO_TYPE_STRING,
    ORSO_TYPE_SYMBOL,
    ORSO_TYPE_TYPE,
    ORSO_TYPE_FUNCTION,
    ORSO_TYPE_NATIVE_FUNCTION,
    ORSO_TYPE_PTR_OPAQUE,
    ORSO_TYPE_UNION,
    ORSO_TYPE_USER = 5000,
    // Aiming to allow for 60k custom types. This number must be less than 0xFFFF (largest u16)
    ORSO_TYPE_MAX = 65000,
} OrsoTypeKind;

typedef struct OrsoType {
    OrsoTypeKind kind;
    byte _space[4 + 8 * ORSO_UNION_NUM_MAX];
} OrsoType;

typedef struct OrsoUnionType {
    OrsoType type;
    i32 count;
    OrsoType* types[ORSO_UNION_NUM_MAX];
} OrsoUnionType;

typedef struct OrsoFunctionType {
    OrsoType type;
    OrsoType* return_type;
    i32 argument_count;
    OrsoType** argument_types;
} OrsoFunctionType;

#define ORSO_TYPE_IS_UNION(TYPE) (TYPE->kind == ORSO_TYPE_UNION)

struct OrsoTypeSet;

bool orso_union_type_has_type(OrsoUnionType const * type, OrsoType* subtype);

bool orso_type_equal(OrsoType* a, OrsoType* b);

// orso_type_has_type_kind
bool orso_union_type_has_type(OrsoUnionType const * type, OrsoType* subtype);

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
bool orso_union_has_float(OrsoUnionType* type);

bool orso_type_is_or_has_float(OrsoType* type);

// orso_has_integer_type
bool orso_union_has_integer(OrsoUnionType* type, bool include_bool);

bool orso_type_is_or_has_integer(OrsoType* type, bool include_bool);

i32 orso_type_bits(OrsoType* type_kind);

bool orso_integer_fit(OrsoType* storage_type, OrsoType* value_type, bool include_bool);

i32 orso_type_slot_count(OrsoType* type);

bool orso_type_fits(OrsoType* storage_type, OrsoType* value_type);

i32 orso_type_to_cstrn(OrsoType* type, char* buffer, i32 n);

bool orso_is_gc_type(OrsoType* type);

OrsoType* orso_binary_arithmetic_cast(OrsoType* a, OrsoType* b, TokenType operation);

void orso_binary_comparison_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast);

void orso_binary_equality_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast);

#endif
