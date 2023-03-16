#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "def.h"
#include "lexer.h"

#define ORSO_UNION_NUM_MAX 4

typedef enum OrsoTypeType {
    ORSO_TYPE_TYPE_PTR,
    ORSO_TYPE_TYPE_STACK,
    ORSO_TYPE_TYPE_UNION,
} OrsoTypeType;

typedef enum OrsoTypeKind {
    ORSO_TYPE_INVALID = 0,
    ORSO_TYPE_UNRESOLVED = 1,
    ORSO_TYPE_NULL = 2,
    ORSO_TYPE_BOOL = 3,
    ORSO_TYPE_INT32 = 4,
    ORSO_TYPE_INT64 = 5,
    ORSO_TYPE_FLOAT32 = 6,
    ORSO_TYPE_FLOAT64 = 7,
    ORSO_TYPE_STRING = 8,
    ORSO_TYPE_SYMBOL = 9,
    ORSO_TYPE_TYPE = 11,
    ORSO_TYPE_USER = 12,
    // Aiming to allow for 65k custom types. This number must be less than 0xFFFF (largest u16)
    ORSO_TYPE_MAX = 65012,
} OrsoTypeKind;

typedef union OrsoType {
    u64 one;
    u16 union_[ORSO_UNION_NUM_MAX];
} OrsoType;

#define ORSO_TYPE_ONE(TYPE) (OrsoType){ .one = TYPE }

typedef struct OrsoSlot {
#ifdef DEBUG_TRACE_EXECUTION
    OrsoType type;
#endif
    union {
        i64 i;
        f64 f;
        ptr p;
        u64 u;
    } as;
} OrsoSlot;

#define ORSO_SLOT_IS_FALSE(SLOT) (SLOT.as.i == 0)

#ifndef DEBUG_TRACE_EXECUTION
#define ORSO_SLOT_I(VALUE, TYPE) (OrsoSlot){ .as.i = VALUE }
#define ORSO_SLOT_U(VALUE, TYPE) (OrsoSlot){ .as.u = VALUE }
#define ORSO_SLOT_F(VALUE, TYPE) (OrsoSlot){ .as.f = VALUE }
#define ORSO_SLOT_P(VALUE, TYPE) (OrsoSlot){ .as.p = VALUE }
#else
#define ORSO_SLOT_I(VALUE, TYPE) (OrsoSlot){ .as.i = VALUE, .type = TYPE }
#define ORSO_SLOT_U(VALUE, TYPE) (OrsoSlot){ .as.u = VALUE, .type = TYPE }
#define ORSO_SLOT_F(VALUE, TYPE) (OrsoSlot){ .as.f = VALUE, .type = TYPE }
#define ORSO_SLOT_P(VALUE, TYPE) (OrsoSlot){ .as.p = VALUE, .type = TYPE }
#endif

#define ORSO_TYPE_IS_UNION(TYPE) TYPE.one >= ORSO_TYPE_MAX
#define ORSO_TYPE_IS_SINGLE(TYPE) TYPE.one < ORSO_TYPE_MAX

FORCE_INLINE bool orso_type_has_kind(OrsoType type, OrsoTypeKind kind) {
    if (type.one == kind) {
        return true;
    }

    for (i32 i = 0; i < ORSO_UNION_NUM_MAX; i++) {
        if (type.union_[i] != kind) {
            continue;
        }

        return true;
    }

    return false;
}

FORCE_INLINE bool orso_type_add_kind(OrsoType* type, OrsoTypeKind kind) {
    ASSERT(!orso_type_has_kind(*type, kind), "type must not already have kind");

    for (i32 i = 0; i < ORSO_UNION_NUM_MAX; i++) {
        if (type->union_[i] == ORSO_TYPE_INVALID) {
            type->union_[i] = kind;
            return true;
        }
    }

    return false;
}

FORCE_INLINE OrsoType orso_type_merge(OrsoType type1, OrsoType type2) {
    if (type1.one == type2.one) {
        return type1;
    }

    for (i32 i = 0; i < ORSO_UNION_NUM_MAX; i++) {
        OrsoTypeKind type2_kind = type2.union_[i];
        if (type2_kind == ORSO_TYPE_INVALID) {
            continue;
        }

        if (orso_type_has_kind(type1, type2_kind)) {
            continue;
        }

        if (!orso_type_add_kind(&type1, type2_kind)) {
            return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
        }
    }

    return type1;
}

bool FORCE_INLINE orso_is_float_type_kind(OrsoTypeKind type_kind) {
    switch (type_kind) {
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE orso_is_integer_type_kind(OrsoTypeKind type_kind, bool include_bool) {
    switch (type_kind) {
        case ORSO_TYPE_BOOL: return include_bool ? true : false;
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE orso_is_unsigned_integer_type(OrsoTypeKind type_kind) {
    (void)type_kind;
    return false;
}

bool FORCE_INLINE orso_is_number_type_kind(OrsoTypeKind type_kind, bool include_bool) {
    return orso_is_float_type_kind(type_kind) || orso_is_integer_type_kind(type_kind, include_bool) || orso_is_unsigned_integer_type(type_kind);
}

bool FORCE_INLINE orso_slot_is_falsey(OrsoSlot slot) {
    return slot.as.u == 0;
}

i32 FORCE_INLINE orso_get_builtin_type_kind_bits(OrsoTypeKind type_kind) {
    switch (type_kind) {
        case ORSO_TYPE_NULL: return 0;

        case ORSO_TYPE_INT64:
        case ORSO_TYPE_FLOAT64: return 64;

        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_INT32: return 32;

        case ORSO_TYPE_STRING:
        case ORSO_TYPE_SYMBOL: return 64;

        case ORSO_TYPE_BOOL: return 1;

        case ORSO_TYPE_TYPE: return 64;

        // TODO: Add assert here should never go here
        case ORSO_TYPE_INVALID:
        case ORSO_TYPE_UNRESOLVED:
        default: return 0;
    }
}

FORCE_INLINE bool orso_integer_fit(OrsoType storage_type, OrsoType value_type, bool include_bool) {
    if (ORSO_TYPE_IS_UNION(storage_type)) {
        return false;
    }

    if (ORSO_TYPE_IS_UNION(value_type)) {
        return false;
    }

    if (!orso_is_integer_type_kind(storage_type.one, include_bool)) {
        return false;
    }

    if (!orso_is_integer_type_kind(value_type.one, include_bool)) {
        return false;
    }

    return orso_get_builtin_type_kind_bits(storage_type.one) >= orso_get_builtin_type_kind_bits(value_type.one);
}

FORCE_INLINE bool orso_type_fits(OrsoType storage_type, OrsoType value_type) {
    if (storage_type.one == value_type.one) {
        return true;
    }

    if (orso_integer_fit(storage_type, value_type, true)) {
        return true;
    }

    if (ORSO_TYPE_IS_UNION(storage_type)) {
        // value type must be contained within storage type
        for (i32 i = 0; i < ORSO_UNION_NUM_MAX; i++) {
            OrsoTypeKind kind = value_type.union_[i];
            if (kind == 0) {
                continue;
            }

            if (orso_type_has_kind(storage_type, kind)) {
                continue;
            }

            return false;
        }

        return true;
    }

    // storage type isn't union
    //    if the value type is union type, storage type single type cant hold union type
    //    if the value type is single type, then that case is already failed at the top
    return false;
}

const FORCE_INLINE char* orso_type_kind_to_cstr(OrsoTypeKind type_kind) {
    switch (type_kind) {
        case ORSO_TYPE_NULL: return "void";
        case ORSO_TYPE_BOOL: return "bool";
        case ORSO_TYPE_INT32: return "i32";
        case ORSO_TYPE_INT64: return "i64";
        case ORSO_TYPE_FLOAT32: return "f32";
        case ORSO_TYPE_FLOAT64: return "f64";
        case ORSO_TYPE_STRING: return "string";
        case ORSO_TYPE_SYMBOL: return "symbol";
        case ORSO_TYPE_TYPE: return "type";
        case ORSO_TYPE_UNRESOLVED: return "<unresolved>";
        case ORSO_TYPE_INVALID: return "<invalid>";
        default: return "<unknown>";
    }
}

FORCE_INLINE void orso_type_to_cstr(OrsoType type, char type_str[128]) {
    if (ORSO_TYPE_IS_SINGLE(type)) {
        sprintf(type_str, "%s", orso_type_kind_to_cstr(type.one));
    } else {
        char* next_type = type_str;
        next_type += sprintf(next_type, "%s", orso_type_kind_to_cstr(type.union_[0]));
        for (i32 i = 1; i < ORSO_UNION_NUM_MAX; i++) {
            OrsoTypeKind kind = type.union_[i];
            if (kind == 0) {
                continue;
            }

            next_type += sprintf(next_type, "|%s", orso_type_kind_to_cstr(kind));
        }
    }
}

bool FORCE_INLINE orso_is_gc_type(OrsoType type) {
    if (orso_type_has_kind(type, ORSO_TYPE_STRING)) {
        return true;
    }

    if (orso_type_has_kind(type, ORSO_TYPE_SYMBOL)) {
        return true;
    }

    return false;
}

OrsoType FORCE_INLINE orso_binary_arithmetic_cast(OrsoType a, OrsoType b, TokenType operation) {
    if (ORSO_TYPE_IS_UNION(a) || ORSO_TYPE_IS_UNION(b)) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    if (operation == TOKEN_PLUS && a.one == ORSO_TYPE_STRING && b.one == ORSO_TYPE_STRING) {
        return ORSO_TYPE_ONE(ORSO_TYPE_STRING);
    }

    if (!orso_is_number_type_kind(a.one, true)) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    if (!orso_is_number_type_kind(b.one, true)) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    i32 a_count = orso_get_builtin_type_kind_bits(a.one);
    i32 b_count = orso_get_builtin_type_kind_bits(b.one);

    if (a_count == 0 || b_count == 0) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    if (a.one == ORSO_TYPE_BOOL && b.one == ORSO_TYPE_BOOL) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INT32);
    }

    bool include_bool = true;
    bool is_same_integer_types = (orso_is_integer_type_kind(a.one, include_bool)) && (orso_is_integer_type_kind(b.one, include_bool));
    bool is_same_float_types = orso_is_float_type_kind(a.one) && orso_is_float_type_kind(b.one);

    if (is_same_integer_types || is_same_float_types) {
        return a_count > b_count ? a : b;
    }

    return a_count > 32 || b_count > 32 ? ORSO_TYPE_ONE(ORSO_TYPE_FLOAT64) : ORSO_TYPE_ONE(ORSO_TYPE_FLOAT32);
}

void FORCE_INLINE orso_binary_comparison_casts(OrsoType a, OrsoType b, OrsoType* a_cast, OrsoType* b_cast) {
    if (ORSO_TYPE_IS_UNION(a) || ORSO_TYPE_IS_UNION(b)) {
        *a_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
        *b_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
        return;
    }

    bool include_bool = false;
    if (orso_is_number_type_kind(a.one, include_bool) && orso_is_number_type_kind(b.one, include_bool)) {
        if (orso_is_integer_type_kind(a.one, include_bool) && orso_is_integer_type_kind(b.one, include_bool)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }
        
        if (orso_is_float_type_kind(a.one) && orso_is_float_type_kind(b.one)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }

        i32 a_count = orso_get_builtin_type_kind_bits(a.one);
        i32 b_count = orso_get_builtin_type_kind_bits(b.one);

        if (a_count <= 32 && b_count <= 32) {
            *a_cast = ORSO_TYPE_ONE(ORSO_TYPE_FLOAT32);
            *b_cast = ORSO_TYPE_ONE(ORSO_TYPE_FLOAT32);
            return;
        }

        *a_cast = ORSO_TYPE_ONE(ORSO_TYPE_FLOAT64);
        *b_cast = ORSO_TYPE_ONE(ORSO_TYPE_FLOAT64);
        return;
    }

    *a_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    *b_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
}

void FORCE_INLINE orso_binary_equality_casts(OrsoType a, OrsoType b, OrsoType* a_cast, OrsoType* b_cast) {
    if (ORSO_TYPE_IS_UNION(a) || ORSO_TYPE_IS_UNION(b)) {
        *a_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
        *b_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
        return;
    }

    bool include_bool = false;
    if (orso_is_number_type_kind(a.one, include_bool) && orso_is_number_type_kind(b.one, include_bool)) {
        orso_binary_comparison_casts(a, b, a_cast, b_cast);
        return;
    }

    if ((a.one == ORSO_TYPE_BOOL && b.one == ORSO_TYPE_BOOL)
    || (a.one == ORSO_TYPE_STRING && b.one == ORSO_TYPE_STRING)
    || (a.one == ORSO_TYPE_SYMBOL && b.one == ORSO_TYPE_SYMBOL)) {
        *a_cast = a;
        *b_cast = b;
        return;
    }

    *a_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    *b_cast = ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
}

#endif
