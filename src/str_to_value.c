#include "str_to_value.h"

i32 cstr_to_i32(const char* text, i32 length) {
    i32 integer = 0;

    for (i32 i = 0; i < length; i++) {
        char digit = text[i];
        if (digit == '_') {
            continue;
        }

        byte ones = text[i] - '0';
        integer *= 10;
        integer += ones;
    }

    return integer;
}