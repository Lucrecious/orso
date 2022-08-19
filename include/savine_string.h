#ifndef SAVINE_STRING_H_
#define SAVINE_STRING_H_

#include "def.h"

typedef struct SavineString {
    i32 length;
    u32 hash;
    char* str;
} SavineString;


char* c_str_stack_to_heap(const char* str);

void savine_string_init_interned();
void savine_string_free_interned();

SavineString* c_strn_to_savine_string(char* c_str, i32 length);

void savine_string_free(SavineString* string);

#endif
