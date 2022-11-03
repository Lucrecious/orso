#ifndef OBJECT_H_
#define OBJECT_H_

#include "def.h"
#include "garbage_collector.h"

typedef struct OrsoObject {
    OrsoGCHeader gc_header;
} OrsoObject;

typedef struct OrsoString {
    OrsoObject object;
    i32 length;
    char text[];
} OrsoString;

typedef struct OrsoSymbol {
    OrsoObject object;
    i32 length;
    u32 hash;
    char text[];
} OrsoSymbol;

void* orso_object_reallocate(OrsoGarbageCollector* gc, OrsoGCHeader* pointer, size_t old_size, size_t new_size);

#define ORSO_OBJECT_ALLOCATE_N(GC, TYPE, N) (TYPE*)orso_object_reallocate(GC, NULL, 0, sizeof(TYPE) * N)
#define ORSO_OBJECT_ALLOCATE(GC, TYPE) ORSO_OBJECT_ALLOCATE_N(GC, TYPE, 1)
#define ORSO_OBJECT_ALLOCATE_FLEX(GC, TYPE, N) (TYPE*)orso_object_reallocate(GC, NULL, 0, sizeof(TYPE) + N)
#define ORSO_OBJECT_FREE(GC, POINTER) orso_object_reallocate(GC, POINTER, sizeof(POINTER), 0)

FORCE_INLINE u32 orso_hash_cstrn(const char* start, i32 length);
FORCE_INLINE bool orso_string_equal(OrsoString* a, OrsoString* b);
FORCE_INLINE OrsoString* orso_string_concat(OrsoGarbageCollector* gc, OrsoString* a, OrsoString* b);

OrsoString* orso_new_string_from_cstrn(OrsoGarbageCollector* gc, const char* start, i32 length);
i64 cstrn_to_i64(const char* text, i32 length);
f64 cstrn_to_f64(const char* text, i32 length);

#endif