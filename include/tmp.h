#ifndef TMP_H_
#define TMP_H_

#include "arena.h"

typedef struct tmp_arena_t tmp_arena_t;
struct tmp_arena_t {
    arena_t *allocator;
    tmp_arena_t *next;
};

tmp_arena_t *allocator_borrow(void);
void allocator_return(tmp_arena_t *allocator);

#endif

#ifdef TMP_IMPLEMENTATION
#include "arena.h"

arena_t allocator = {0};
tmp_arena_t *available_arenas = NULL;

tmp_arena_t *allocator_borrow(void) {
    if (available_arenas== NULL) {
        arena_t *arena = (arena_t*)arena_alloc(&allocator, sizeof(arena_t));
        *arena =(arena_t){0};

        tmp_arena_t *tmp_arena = (tmp_arena_t*)arena_alloc(&allocator, sizeof(tmp_arena_t));
        *tmp_arena = (tmp_arena_t){0};
        tmp_arena->allocator = arena;

        available_arenas = tmp_arena;
    }

    tmp_arena_t *tmp_arena = available_arenas;
    available_arenas = tmp_arena->next;

    tmp_arena->next =  NULL;
    return tmp_arena;
}

void allocator_return(tmp_arena_t *tmp_arena) {
    arena_reset(tmp_arena->allocator);
    tmp_arena->next = NULL;

    if (available_arenas == NULL) {
        available_arenas = tmp_arena;
    } else {
        tmp_arena->next = available_arenas;
        available_arenas = tmp_arena;
    }
}

#undef TMP_IMPLEMENTATION
#endif

