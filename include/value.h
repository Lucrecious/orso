#ifndef VALUE_H_
#define VALUE_H_

#include "def.h"

typedef struct OrsoValue {
    union {
        i64 as_int;
        f64 as_float;
    };
} OrsoValue;

void print_value(OrsoValue value);

#endif