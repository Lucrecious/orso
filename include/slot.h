#ifndef SLOT_H_
#define SLOT_H_

#include "def.h"
#include "type.h"

typedef struct OrsoSlot {
#ifdef DEBUG_TRACE_EXECUTION
    OrsoType* type;
#endif
    union {
        i64 i;
        f64 f;
        ptr p;
        u64 u;
    } as;
} OrsoSlot;

#define ORSO_SLOT_IS_FALSE(SLOT) (SLOT.as.i == 0)

#ifdef DEBUG_TRACE_EXECUTION
#define ORSO_SLOT_I(VALUE, TYPE) (OrsoSlot){ .as.i = VALUE, .type = TYPE }
#define ORSO_SLOT_U(VALUE, TYPE) (OrsoSlot){ .as.u = VALUE, .type = TYPE }
#define ORSO_SLOT_F(VALUE, TYPE) (OrsoSlot){ .as.f = VALUE, .type = TYPE }
#define ORSO_SLOT_P(VALUE, TYPE) (OrsoSlot){ .as.p = VALUE, .type = TYPE }
#else
#define ORSO_SLOT_I(VALUE, TYPE) (OrsoSlot){ .as.i = VALUE }
#define ORSO_SLOT_U(VALUE, TYPE) (OrsoSlot){ .as.u = VALUE }
#define ORSO_SLOT_F(VALUE, TYPE) (OrsoSlot){ .as.f = VALUE }
#define ORSO_SLOT_P(VALUE, TYPE) (OrsoSlot){ .as.p = VALUE }
#endif

bool FORCE_INLINE orso_slot_is_falsey(OrsoSlot slot) {
    return slot.as.u == 0;
}

#endif
