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

} SavineType;

bool FORCE_INLINE savine_is_float_type(SavineType type) {
    switch (type) {
        case SAVINE_TYPE_FLOAT32:
        case SAVINE_TYPE_FLOAT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE savine_is_integer_type(SavineType type) {
    switch (type) {
        case SAVINE_TYPE_BOOL:
        case SAVINE_TYPE_INT32:
        case SAVINE_TYPE_INT64: return true;
        default: return false;
    }
}

bool FORCE_INLINE savine_is_unsigned_integer_type(SavineType type) {
    return false;
}

bool FORCE_INLINE savine_is_number_type(SavineType type) {
    return savine_is_float_type(type) || savine_is_integer_type(type) || savine_is_unsigned_integer_type(type);
}


#endif