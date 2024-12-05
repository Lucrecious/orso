#ifndef ARRAY_H_
#define ARRAY_H_

#include "stddef.h"
#include "arena.h"
#include "def.h"

#define array_push(array, item) do { \
    if ((array)->count >= (array)->capacity) {\
        size_t old_capacity = (array)->capacity; \
        (array)->capacity = (array)->capacity == 0 ? 8 : (array)->capacity*2; \
        (array)->items = arena_realloc( \
            (array)->allocator, \
            (array)->items, \
            old_capacity*sizeof(*(array)->items), \
            (array)->capacity*sizeof(*(array)->items)); \
    } \
    (array)->items[(array)->count++] = (item); \
} while(0)

typedef struct bytes_t bytes_t;
struct bytes_t {
    byte *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

#endif
