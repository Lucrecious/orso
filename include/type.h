#ifndef TYPE_H_
#define TYPE_H_

#include "def.h"

typedef enum SavineType {
    SAVINE_TYPE_NIL,
    SAVINE_TYPE_BOOL,
    SAVINE_TYPE_INT32,
    SAVINE_TYPE_INT64,
    SAVINE_TYPE_FLOAT32,
    SAVINE_TYPE_FLOAT64,
    SAVINE_TYPE_UNRESOLVED,
    SAVINE_TYPE_INVALID,
    SAVINE_TYPE_MAX,
} SavineType;

bool FORCE_INLINE savine_is_float_type(SavineType type) {
    switch (type) {
        case SAVINE_TYPE_FLOAT32:
        case SAVINE_TYPE_FLOAT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE savine_is_integer_type(SavineType type, bool include_bool) {
    switch (type) {
        case SAVINE_TYPE_BOOL: return include_bool ? true : false;
        case SAVINE_TYPE_INT32:
        case SAVINE_TYPE_INT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE savine_is_unsigned_integer_type(SavineType type) {
    return false;
}

bool FORCE_INLINE savine_is_number_type(SavineType type, bool include_bool) {
    return savine_is_float_type(type) || savine_is_integer_type(type, include_bool) || savine_is_unsigned_integer_type(type);
}

i32 FORCE_INLINE savine_number_and_bool_type_bit_count(SavineType number_type) {
    switch (number_type) {
        case SAVINE_TYPE_INT64:
        case SAVINE_TYPE_FLOAT64: return 64;

        case SAVINE_TYPE_FLOAT32:
        case SAVINE_TYPE_INT32: return 32;

        case SAVINE_TYPE_BOOL: return 1;

        default: return 0;
    }
}

SavineType FORCE_INLINE savine_binary_arithmetic_cast(SavineType a, SavineType b) {
    i32 a_count = savine_number_and_bool_type_bit_count(a);
    i32 b_count = savine_number_and_bool_type_bit_count(b);

    if (a_count == 0 || b_count == 0) {
        return SAVINE_TYPE_INVALID;
    }

    if (a == SAVINE_TYPE_BOOL && b == SAVINE_TYPE_BOOL) {
        return SAVINE_TYPE_INT32;
    }

    bool include_bool = true;
    bool is_same_integer_types = (savine_is_integer_type(a, include_bool)) && (savine_is_integer_type(b, include_bool));
    bool is_same_float_types = savine_is_float_type(a) && savine_is_float_type(b);

    if (is_same_integer_types || is_same_float_types) {
        return a_count > b_count ? a : b;
    }

    return a_count > 32 || b_count > 32 ? SAVINE_TYPE_FLOAT64 : SAVINE_TYPE_FLOAT32;
}

void FORCE_INLINE savine_binary_comparison_casts(SavineType a, SavineType b, SavineType* a_cast, SavineType* b_cast) {
    bool include_bool = false;
    if (savine_is_number_type(a, include_bool) && savine_is_number_type(b, include_bool)) {
        if (savine_is_integer_type(a, include_bool) && savine_is_integer_type(b, include_bool)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }
        
        if (savine_is_float_type(a) && savine_is_float_type(b)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }

        i32 a_count = savine_number_and_bool_type_bit_count(a);
        i32 b_count = savine_number_and_bool_type_bit_count(b);

        if (a_count <= 32 && b_count <= 32) {
            *a_cast = SAVINE_TYPE_FLOAT32;
            *b_cast = SAVINE_TYPE_FLOAT32;
            return;
        }

        *a_cast = SAVINE_TYPE_FLOAT64;
        *b_cast = SAVINE_TYPE_FLOAT64;
        return;
    }

    *a_cast = SAVINE_TYPE_INVALID;
    *b_cast = SAVINE_TYPE_INVALID;
}

void FORCE_INLINE savine_binary_equality_casts(SavineType a, SavineType b, SavineType* a_cast, SavineType* b_cast) {
    bool include_bool = false;
    if (savine_is_number_type(a, include_bool) && savine_is_number_type(b, include_bool)) {
        savine_binary_comparison_casts(a, b, a_cast, b_cast);
        return;
    }

    if (a == SAVINE_TYPE_BOOL && b == SAVINE_TYPE_BOOL) {
        *a_cast = a;
        *b_cast = b;
        return;
    }

    *a_cast = SAVINE_TYPE_INVALID;
    *b_cast = SAVINE_TYPE_INVALID;
}

#endif