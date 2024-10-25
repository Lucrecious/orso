#ifndef SLOT_H_
#define SLOT_H_

#include "def.h"
#include "type.h"

typedef struct slot_t {
    union {
        i64 i;
        f64 f;
        ptr p;
        u64 u;
    } as;
} slot_t;

#define SLOT_IS_FALSE(SLOT) (SLOT.as.i == 0)

#define SLOT_I(VALUE) ((slot_t){ .as.i = VALUE })
#define SLOT_U(VALUE) ((slot_t){ .as.u = VALUE })
#define SLOT_F(VALUE) ((slot_t){ .as.f = VALUE })
#define SLOT_P(VALUE) ((slot_t){ .as.p = VALUE })

bool FORCE_INLINE SLOT_IS_FALSEy(slot_t slot) {
    return slot.as.u == 0;
}

#endif
