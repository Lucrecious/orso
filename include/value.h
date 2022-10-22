#ifndef VALUE_H_
#define VALUE_H_

#include "def.h"

typedef struct SavineValue {
    union {
        i64 as_int;
        f64 as_float;
    };
} SavineValue;

void print_value(SavineValue value);

#endif