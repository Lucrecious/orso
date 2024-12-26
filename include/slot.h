#ifndef SLOT_H_
#define SLOT_H_

#include "def.h"
#include "type.h"

typedef struct word_t word_t;
struct word_t {
    union {
        i64 i;
        f64 d;
        ptr p;
        u64 u;
    } as;
};

#define WORD_SIZE sizeof(word_t)

#define WORDI(value) ((word_t){.as.i=(value)})
#define WORDU(value) ((word_t){.as.u=(value)})
#define WORDD(value) ((word_t){.as.d=(value)})
#define WORDP(value) ((word_t){.as.p=(value)})

#endif
