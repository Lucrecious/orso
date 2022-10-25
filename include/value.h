#ifndef VALUE_H_
#define VALUE_H_

#include "def.h"

typedef struct OrsoObject {
    i32 ref_count;
} OrsoObject;

typedef struct OrsoString {
    OrsoObject obj;
    i32 length;
    char text[];
} OrsoString;

typedef struct OrsoValue {
    union {
        i64 as_int;
        f64 as_float;
    };
} OrsoValue;

FORCE_INLINE OrsoString* orso_new_string_from_cstrn(const char* start, i32 length) {
    OrsoString* string = ALLOCATE_FLEX(OrsoString, length + 1);
    string->length = length;
    memcpy(string->text, start, length);
    string->text[length] = '\0';

    return string;
}

FORCE_INLINE bool orso_string_equal(OrsoString* a, OrsoString* b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->text, b->text, a->length) == 0;
}

FORCE_INLINE OrsoString* orso_string_concat(OrsoString* a, OrsoString* b) {
    OrsoString* string = ALLOCATE_FLEX(OrsoString, a->length + b->length + 1);
    string->length = a->length + b->length;
    memcpy(string->text, a->text, a->length);
    memcpy(string->text + a->length, b->text, b->length);
    string->text[a->length + b->length] = '\0';

    return string;
}

FORCE_INLINE i64 cstrn_to_i64(const char* text, i32 length) {
    i64 integer = 0;

    for (i32 i = 0; i < length; i++) {
        char digit = text[i];
        if (digit == '_') {
            continue;
        }

        i32 ones = text[i] - '0';
        if (ones < 0 || ones > 9) {
            continue;
        }

        integer *= 10;
        integer += ones;
    }

    return integer;
}

FORCE_INLINE f64 cstrn_to_f64(const char* text, i32 length) {
    f64 value = 0;
    f64 fact = 1;
    bool point_seen = false;

    for (char* c = text; c != (text + length); c++) {
        if (*c == '.') {
            point_seen = true;
            continue;
        }

        if (*c == '_') {
            continue;
        }

        i32 digit = *c - '0';
        if (digit < 0 || digit > 9) {
            continue;
        }

        if (point_seen) {
            fact /= 10.0f;
        }

        value = value * 10.0f + (float)digit;
    }

    return value * fact;
}

void print_value(OrsoValue value);

#endif