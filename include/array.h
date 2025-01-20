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

#define array_insert(array, index, item) do { \
	if ((array)->count >= (array)->capacity) { \
		size_t old_capacity = (array)->capacity; \
		(array)->capacity = (array)->capacity == 0 ? 8 : (array)->capacity * 2; \
		(array)->items = arena_realloc( \
			(array)->allocator, \
			(array)->items, \
			old_capacity * sizeof(*(array)->items), \
			(array)->capacity * sizeof(*(array)->items)); \
	} \
	memmove( \
		&(array)->items[(index) + 1], \
		&(array)->items[index], \
		((array)->count - (index)) * sizeof(*(array)->items)); \
	(array)->items[index] = (item); \
	(array)->count++; \
} while (0)


#endif
