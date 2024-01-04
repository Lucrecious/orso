#ifndef SLOT_H_
#define SLOT_H_

#include "def.h"
#include "type.h"

typedef struct OrsoSlot {
    union {
        i64 i;
        f64 f;
        ptr p;
        u64 u;
    } as;
} OrsoSlot;

#define ORSO_SLOT_IS_FALSE(SLOT) (SLOT.as.i == 0)

#define ORSO_SLOT_I(VALUE) ((OrsoSlot){ .as.i = VALUE })
#define ORSO_SLOT_U(VALUE) ((OrsoSlot){ .as.u = VALUE })
#define ORSO_SLOT_F(VALUE) ((OrsoSlot){ .as.f = VALUE })
#define ORSO_SLOT_P(VALUE) ((OrsoSlot){ .as.p = VALUE })

bool FORCE_INLINE orso_slot_is_falsey(OrsoSlot slot) {
    return slot.as.u == 0;
}

#endif
