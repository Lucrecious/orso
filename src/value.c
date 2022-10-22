#include "value.h"

#include <stdio.h>

void print_value(SavineValue value) {
    printf("'%d', '%.2f'", value.as_int, value.as_float);
}