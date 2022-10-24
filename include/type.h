#ifndef TYPE_H_
#define TYPE_H_

#include "def.h"

typedef enum OrsoType {
    ORSO_TYPE_NULL,
    ORSO_TYPE_BOOL,
    ORSO_TYPE_INT32,
    ORSO_TYPE_INT64,
    ORSO_TYPE_FLOAT32,
    ORSO_TYPE_FLOAT64,
    ORSO_TYPE_STRING,
    ORSO_TYPE_UNRESOLVED,
    ORSO_TYPE_INVALID,
    ORSO_TYPE_MAX,
} OrsoType;

const FORCE_INLINE char* orso_type_to_cstr(OrsoType type) {
    switch (type) {
        case ORSO_TYPE_NULL: return "null";
        case ORSO_TYPE_BOOL: return "bool";
        case ORSO_TYPE_INT32: return "i32";
        case ORSO_TYPE_INT64: return "i64";
        case ORSO_TYPE_FLOAT32: return "f32";
        case ORSO_TYPE_FLOAT64: return "f64";
        case ORSO_TYPE_STRING: return "string";
        case ORSO_TYPE_UNRESOLVED: return "<unresolved>";
        case ORSO_TYPE_INVALID: return "<invalid>";
        default: return "<unknown>";
    }
}

bool FORCE_INLINE orso_is_float_type(OrsoType type) {
    switch (type) {
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE orso_is_integer_type(OrsoType type, bool include_bool) {
    switch (type) {
        case ORSO_TYPE_BOOL: return include_bool ? true : false;
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE orso_is_unsigned_integer_type(OrsoType type) {
    return false;
}

bool FORCE_INLINE orso_is_number_type(OrsoType type, bool include_bool) {
    return orso_is_float_type(type) || orso_is_integer_type(type, include_bool) || orso_is_unsigned_integer_type(type);
}

i32 FORCE_INLINE orso_number_and_bool_type_bit_count(OrsoType number_type) {
    switch (number_type) {
        case ORSO_TYPE_INT64:
        case ORSO_TYPE_FLOAT64: return 64;

        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_INT32: return 32;

        case ORSO_TYPE_BOOL: return 1;

        default: return 0;
    }
}

OrsoType FORCE_INLINE orso_binary_arithmetic_cast(OrsoType a, OrsoType b) {
    i32 a_count = orso_number_and_bool_type_bit_count(a);
    i32 b_count = orso_number_and_bool_type_bit_count(b);

    if (a_count == 0 || b_count == 0) {
        return ORSO_TYPE_INVALID;
    }

    if (a == ORSO_TYPE_BOOL && b == ORSO_TYPE_BOOL) {
        return ORSO_TYPE_INT32;
    }

    bool include_bool = true;
    bool is_same_integer_types = (orso_is_integer_type(a, include_bool)) && (orso_is_integer_type(b, include_bool));
    bool is_same_float_types = orso_is_float_type(a) && orso_is_float_type(b);

    if (is_same_integer_types || is_same_float_types) {
        return a_count > b_count ? a : b;
    }

    return a_count > 32 || b_count > 32 ? ORSO_TYPE_FLOAT64 : ORSO_TYPE_FLOAT32;
}

void FORCE_INLINE orso_binary_comparison_casts(OrsoType a, OrsoType b, OrsoType* a_cast, OrsoType* b_cast) {
    bool include_bool = false;
    if (orso_is_number_type(a, include_bool) && orso_is_number_type(b, include_bool)) {
        if (orso_is_integer_type(a, include_bool) && orso_is_integer_type(b, include_bool)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }
        
        if (orso_is_float_type(a) && orso_is_float_type(b)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }

        i32 a_count = orso_number_and_bool_type_bit_count(a);
        i32 b_count = orso_number_and_bool_type_bit_count(b);

        if (a_count <= 32 && b_count <= 32) {
            *a_cast = ORSO_TYPE_FLOAT32;
            *b_cast = ORSO_TYPE_FLOAT32;
            return;
        }

        *a_cast = ORSO_TYPE_FLOAT64;
        *b_cast = ORSO_TYPE_FLOAT64;
        return;
    }

    *a_cast = ORSO_TYPE_INVALID;
    *b_cast = ORSO_TYPE_INVALID;
}

void FORCE_INLINE orso_binary_equality_casts(OrsoType a, OrsoType b, OrsoType* a_cast, OrsoType* b_cast) {
    bool include_bool = false;
    if (orso_is_number_type(a, include_bool) && orso_is_number_type(b, include_bool)) {
        orso_binary_comparison_casts(a, b, a_cast, b_cast);
        return;
    }

    if ((a == ORSO_TYPE_BOOL && b == ORSO_TYPE_BOOL)
    || (a == ORSO_TYPE_STRING && b == ORSO_TYPE_STRING)) {
        *a_cast = a;
        *b_cast = b;
        return;
    }

    *a_cast = ORSO_TYPE_INVALID;
    *b_cast = ORSO_TYPE_INVALID;
}

#endif