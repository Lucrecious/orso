#ifndef MEMARR_H_
#define MEMARR_H_

#include "def.h"

typedef struct memarr_t memarr_t;
struct memarr_t {
    byte *data;
    size_t capacity;
    size_t count;
    arena_t arena;
};

void memarr_init(memarr_t *arr, size_t capacity);
void memarr_free(memarr_t *arr);
size_t memarr_push(memarr_t *arr, void *data, size_t size_bytes);
bool memarr_get(memarr_t *arr, size_t index, size_t size_bytes, void *out_result);

#define memarr_get_ptr(arr, value_index) ((arr)->data)+((value_index).index)

#endif

#ifdef MEMARR_IMPLEMENTATION

void memarr_init(memarr_t *arr, size_t capacity) {
    zero(&arr->arena, arena_t);
    arr->count = 0;
    arr->capacity = capacity;
    arr->data = arena_alloc(&arr->arena, capacity);
}

void memarr_free(memarr_t *arr) {
    free(arr->data);
    *arr = (memarr_t){0};
}

size_t memarr_push(memarr_t *arr, void *data, size_t size_bytes) {
    size_t word_sized = (size_bytes + sizeof(uintptr_t) - 1)/sizeof(uintptr_t);
    size_t reserved_size = word_sized*sizeof(uintptr_t);

    while ((arr->count + reserved_size) >= arr->capacity) {
        arr->data = arena_realloc(&arr->arena, arr->data, arr->capacity, arr->capacity*2);
        arr->capacity = arr->capacity*2;
    }

    size_t result = arr->count;
    memcpy(&arr->data[arr->count], data, size_bytes);
    arr->count += reserved_size;

    return result;
}

bool memarr_get(memarr_t *arr, size_t index, size_t size_bytes, void *out_result) {
    if (index >= arr->count) return false;
    if ((index + size_bytes) > arr->count) return false;

    memcpy(out_result, arr->data+index, size_bytes);
    return true;
}

#undef MEMARR_IMPLEMENTATION
#endif

