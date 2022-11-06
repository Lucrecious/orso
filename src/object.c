#include "object.h"

#ifdef DEBUG_GC_PRINT
#include <stdio.h>
#endif

void* orso_object_reallocate(OrsoGarbageCollector* gc, OrsoGCHeader* pointer, OrsoType type, size_t old_size, size_t new_size) {
    if (new_size == 0) {
        // Should only be here when called by GC
        free(pointer);
        return NULL;
    }

#ifdef DEBUG_GC_STRESS
    orso_gc_collect(gc);
#endif

    OrsoObject* result = realloc(pointer, new_size);
    if (result == NULL) {
        exit(1);
    }

    result->type = type;
#ifdef DEBUG_GC_PRINT
    printf("%p allocate %zu for %s\n", (void*)result, new_size, orso_type_to_cstr(result->type));
#endif

    orso_gc_register(gc, result);

    return result;
}

void orso_object_free(OrsoGarbageCollector* gc, OrsoObject* object) {
#ifdef DEBUG_GC_PRINT
    printf("%p free type %s\n", (void*)object, orso_type_to_cstr(((OrsoObject*)object)->type));
#endif
    switch (object->type) {
        case ORSO_TYPE_SYMBOL: {
            OrsoSymbol* symbol = (OrsoSymbol*)object;
            orso_object_reallocate(gc, symbol, ORSO_TYPE_SYMBOL, sizeof(OrsoSymbol) + symbol->length, 0);
            break;
        }
        case ORSO_TYPE_STRING: {
            OrsoString* string = (OrsoString*)object;
            orso_object_reallocate(gc, string, ORSO_TYPE_SYMBOL, sizeof(OrsoString) + string->length, 0);
        }
    }
}

OrsoString* orso_new_string_from_cstrn(OrsoGarbageCollector* gc, const char* start, i32 length) {
    OrsoString* string = ORSO_OBJECT_ALLOCATE_FLEX(gc, OrsoString, ORSO_TYPE_STRING, length + 1);
    string->length = length;
    memcpy(string->text, start, length);
    string->text[length] = '\0';

    return string;
}

u32 orso_hash_cstrn(const char* start, i32 length) {
    u32 hash = 2166136261u;
    for (i32 i = 0; i < length; i++) {
        hash ^= (byte)start[i];
        hash *= 16777619;
    }

    return hash;
}

bool orso_string_equal(OrsoString* a, OrsoString* b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->text, b->text, a->length) == 0;
}

OrsoString* orso_string_concat(OrsoGarbageCollector* gc, OrsoString* a, OrsoString* b) {
    OrsoString* string = ORSO_OBJECT_ALLOCATE_FLEX(gc, OrsoString, ORSO_TYPE_STRING, a->length + b->length + 1);
    string->length = a->length + b->length;
    memcpy(string->text, a->text, a->length);
    memcpy(string->text + a->length, b->text, b->length);
    string->text[a->length + b->length] = '\0';

    return string;
}

i64 cstrn_to_i64(const char* text, i32 length) {
    i64 integer = 0;

    for (i32 i = 0; i < length; i++) {
        char digit = text[i];
        if (digit == '_') {
            continue;
        }

        i32 ones = text[i] - '0';
        if (ones < 0 || ones > 9) {
            continue;
        }

        integer *= 10;
        integer += ones;
    }

    return integer;
}

f64 cstrn_to_f64(const char* text, i32 length) {
    f64 value = 0;
    f64 fact = 1;
    bool point_seen = false;

    for (char* c = text; c != (text + length); c++) {
        if (*c == '.') {
            point_seen = true;
            continue;
        }

        if (*c == '_') {
            continue;
        }

        i32 digit = *c - '0';
        if (digit < 0 || digit > 9) {
            continue;
        }

        if (point_seen) {
            fact /= 10.0f;
        }

        value = value * 10.0f + (float)digit;
    }

    return value * fact;
}