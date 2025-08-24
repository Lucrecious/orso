#ifndef TMP_H_
#define TMP_H_

#include "arena.h"

typedef struct arena_link_t arena_link_t;
struct arena_link_t {
    arena_t *allocator;
    arena_link_t *next;
};

arena_t *allocator_borrow(void);
void allocator_return(arena_t *allocator);

#endif

#ifdef TMP_IMPLEMENTATION
#include "arena.h"

arena_t allocator = {0};
arena_link_t *available_arenas = NULL;
arena_link_t *link_pool = NULL;

arena_t *allocator_borrow(void) {
    if (available_arenas == NULL) {
        arena_t *arena = (arena_t*)arena_alloc(&allocator, sizeof(arena_t));
        *arena =(arena_t){0};

        arena_link_t *link = (arena_link_t*)arena_alloc(&allocator, sizeof(arena_link_t));
        *link = (arena_link_t){0};
        link->allocator = arena;

        available_arenas = link;
    }

    arena_link_t *link = available_arenas;
    available_arenas = link->next;
    
    link->next = link_pool;
    link_pool = link;

    arena_reset(link->allocator);
    return link->allocator;
}

void allocator_return(arena_t *arena) {
    MUST(link_pool);

    arena_link_t *link = link_pool;
    link_pool = link->next;

    link->allocator = arena;
    link->next = available_arenas;
    available_arenas = link;
}

#undef TMP_IMPLEMENTATION
#endif

