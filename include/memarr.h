#ifndef MEMARR_H_
#define MEMARR_H_

#include "def.h"

typedef struct memarr_t memarr_t;
struct memarr_t {
    byte *data;
    size_t capacity;
    size_t count;
};

void memarr_init(memarr_t *arr, size_t capacity);
void memarr_free(memarr_t *arr);
bool memarr_push(memarr_t *arr, void *data, size_t size_bytes, size_t *out_index);
bool memarr_get(memarr_t *arr, size_t index, size_t size_bytes, void *out_result);

#endif

#ifdef MEMARR_IMPLEMENTATION

void memarr_init(memarr_t *arr, size_t capacity) {
    arr->count = 0;
    arr->capacity = capacity;
    arr->data = (byte*)malloc(capacity);
}

void memarr_free(memarr_t *arr) {
    free(arr->data);
    *arr = (memarr_t){0};
}

bool memarr_push(memarr_t *arr, void *data, size_t size_bytes, size_t *out_index) {
    size_t word_sized = (size_bytes + sizeof(uintptr_t) - 1)/sizeof(uintptr_t);
    size_t reserved_size = word_sized*sizeof(uintptr_t);

    if ((arr->count + reserved_size) >= arr->capacity) {
        return false;
    }

    *out_index = arr->count;
    memcpy(&arr->data[arr->count], data, size_bytes);
    arr->count += reserved_size;
    return true;
}

bool memarr_get(memarr_t *arr, size_t index, size_t size_bytes, void *out_result) {
    if (index >= arr->count) return false;
    if ((index + size_bytes) >= arr->count) return false;

    memcpy(out_result, arr->data+index, size_bytes);
    return true;
}

#undef MEMARR_IMPLEMENTATION
#endif

