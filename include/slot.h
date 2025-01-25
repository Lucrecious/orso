#ifndef SLOT_H_
#define SLOT_H_

#include "def.h"
#include "type.h"

typedef struct word_t word_t;
struct word_t {
    union {
        s64 s;
        f64 d;
        ptr p;
        u64 u;
        type_t t;
    } as;
};

#define WORD_SIZE sizeof(word_t)

#define WORDI(value) ((word_t){.as.s=(value)})
#define WORDU(value) ((word_t){.as.u=(value)})
#define WORDD(value) ((word_t){.as.d=(value)})
#define WORDP(value) ((word_t){.as.p=(value)})
#define WORDT(value) ((word_t){.as.t=(value)})

#endif
