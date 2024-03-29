#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "def.h"
#include "lexer.h"

#define ORSO_UNION_NUM_MAX 4

typedef enum OrsoTypeKind {
    ORSO_TYPE_INVALID = 0,
    ORSO_TYPE_UNRESOLVED = 1,
    ORSO_TYPE_VOID = 2,
    ORSO_TYPE_BOOL = 3,
    ORSO_TYPE_INT32 = 4,
    ORSO_TYPE_INT64 = 5,
    ORSO_TYPE_FLOAT32 = 6,
    ORSO_TYPE_FLOAT64 = 7,
    ORSO_TYPE_STRING = 8,
    ORSO_TYPE_SYMBOL = 9,
    ORSO_TYPE_TYPE = 10,
    ORSO_TYPE_FUNCTION = 11,
    ORSO_TYPE_UNION = 12,
    ORSO_TYPE_USER = 13,
    // Aiming to allow for 65k custom types. This number must be less than 0xFFFF (largest u16)
    ORSO_TYPE_MAX = 65012,
} OrsoTypeKind;

// typedef union OrsoType {
//     u64 one;
//     u16 union_[ORSO_UNION_NUM_MAX];
// } OrsoType;

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

typedef struct OrsoSlot {
#ifdef DEBUG_TRACE_EXECUTION
    OrsoType* type;
#endif
    union {
        i64 i;
        f64 f;
        ptr p;
        u64 u;
    } as;
} OrsoSlot;

struct OrsoTypeSet;

#define ORSO_SLOT_IS_FALSE(SLOT) (SLOT.as.i == 0)

#ifdef DEBUG_TRACE_EXECUTION
#define ORSO_SLOT_I(VALUE, TYPE) (OrsoSlot){ .as.i = VALUE, .type = TYPE }
#define ORSO_SLOT_U(VALUE, TYPE) (OrsoSlot){ .as.u = VALUE, .type = TYPE }
#define ORSO_SLOT_F(VALUE, TYPE) (OrsoSlot){ .as.f = VALUE, .type = TYPE }
#define ORSO_SLOT_P(VALUE, TYPE) (OrsoSlot){ .as.p = VALUE, .type = TYPE }
#else
#define ORSO_SLOT_I(VALUE, TYPE) (OrsoSlot){ .as.i = VALUE }
#define ORSO_SLOT_U(VALUE, TYPE) (OrsoSlot){ .as.u = VALUE }
#define ORSO_SLOT_F(VALUE, TYPE) (OrsoSlot){ .as.f = VALUE }
#define ORSO_SLOT_P(VALUE, TYPE) (OrsoSlot){ .as.p = VALUE }
#endif

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

bool FORCE_INLINE orso_slot_is_falsey(OrsoSlot slot) {
    return slot.as.u == 0;
}

i32 orso_type_bits(OrsoType* type_kind);

bool orso_integer_fit(OrsoType* storage_type, OrsoType* value_type, bool include_bool);

i32 orso_type_slot_count(OrsoType* type);

bool orso_type_fits(OrsoType* storage_type, OrsoType* value_type);

// orso_type_kind_to_cstr

i32 orso_type_to_cstrn(OrsoType* type, char* buffer, i32 n);

bool orso_is_gc_type(OrsoType* type);

OrsoType* orso_binary_arithmetic_cast(OrsoType* a, OrsoType* b, TokenType operation);

void orso_binary_comparison_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast);

void orso_binary_equality_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast);

#endif
