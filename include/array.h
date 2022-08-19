#ifndef ARRAY_H_
#define ARRAY_H_

#include "def.h"

typedef struct SavineArray {
    i32 element_size_bytes;
    i32 size;
    byte* items;
} SavineArray;

void savine_array_new(SavineArray* array, i32 element_size_bytes, i32 size);
void savine_array_free(SavineArray* array);
void savine_array_insert(SavineArray* array, i32 index, void* bytes);
void savine_array_get(SavineArray* array, i32 index, void* out_bytes);
void savine_array_get_as(SavineArray* array, i32 index, void* out_bytes, i32 element_size);
void savine_array_insert_as(SavineArray* array, i32 index, void* bytes, i32 element_size);

#endif
