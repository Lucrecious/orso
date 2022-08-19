#include "array.h"

#include <stdlib.h>
#include <string.h>

#include "mathutils.h"

void savine_array_new(SavineArray* array, i32 element_size_bytes, i32 size) {
    array->items = (byte*)malloc(size * element_size_bytes);
    for (i32 i = 0; i < array->size; i++) {
        array->items[i] = 0;
    }

    array->size = size;
    array->element_size_bytes = element_size_bytes;
}

void savine_array_free(SavineArray* array) {
    free(array->items);
}

void savine_array_insert(SavineArray* array, i32 index, void* bytes) {
    i32 actual_index = index * array->element_size_bytes;
    memcpy(array->items + actual_index, bytes, array->element_size_bytes);
}

void savine_array_get(SavineArray* array, i32 index, void* out_bytes) {
    i32 actual_index = index * array->element_size_bytes;
    memcpy(out_bytes, array->items + actual_index, array->element_size_bytes);
}

void savine_array_insert_as(SavineArray* array, i32 index, void* bytes, i32 element_size) {
    i32 actual_index = index * array->element_size_bytes;
    memcpy(array->items + actual_index, bytes, imin(element_size, array->element_size_bytes));
}

void savine_array_get_as(SavineArray* array, i32 index, void* out_bytes, i32 element_size) {
    i32 actual_index = index * array->element_size_bytes;
    memcpy(out_bytes, array->items + actual_index, imin(element_size, array->element_size_bytes));
}
