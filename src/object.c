#include "object.h"

#include "sb.h"
#include <stdio.h>
#include "type_set.h"

void* orso_object_reallocate(OrsoGarbageCollector* gc, OrsoGCHeader* pointer, OrsoType* type, size_t old_size, size_t new_size) {
    (void)old_size;
    (void)gc; // TODO: repalce gc with free/alloc and use ref counting for things I want to be considered "value" types

    if (new_size == 0) {
        // Should only be here when called by GC
        free(pointer);
        return NULL;
    }

#ifdef DEBUG_GC_STRESS
    //orso_gc_collect(gc);
#endif

    OrsoObject* result = realloc(pointer, new_size);
    if (result == NULL) {
        exit(1);
    }

    result->type = type;
#ifdef DEBUG_GC_PRINT
    char tmp_buffer[256];
    orso_type_to_cstrn(result->type, tmp_buffer, 256);
    printf("%p allocate %zu for %s\n", (void*)result, new_size, tmp_buffer);
#endif

    //orso_gc_register(gc, (OrsoGCHeader*)result);

    return result;
}

void orso_object_free(OrsoGarbageCollector* gc, OrsoObject* object) {
#ifdef DEBUG_GC_PRINT
    char type_buffer[256];
    orso_type_to_cstrn(object->type, type_buffer, 256);
    printf("%p free type %s\n", (void*)object, type_buffer);
#endif
    switch (object->type->kind) {
        case ORSO_TYPE_SYMBOL: {
            OrsoSymbol* symbol = (OrsoSymbol*)object;
            orso_object_reallocate(gc, (OrsoGCHeader*)symbol, &OrsoTypeSymbol, sizeof(OrsoSymbol) + symbol->length, 0);
            break;
        }
        case ORSO_TYPE_STRING: {
            OrsoString* string = (OrsoString*)object;
            orso_object_reallocate(gc, (OrsoGCHeader*)string, &OrsoTypeString, sizeof(OrsoString) + string->length, 0);
            break;
        }
        case ORSO_TYPE_FUNCTION: {
            OrsoFunction* function = (OrsoFunction*)object;
            chunk_free(&function->chunk);
            orso_object_reallocate(gc, (OrsoGCHeader*)function, (OrsoType*)&OrsoTypeEmptyFunction, sizeof(OrsoFunction), 0);
            break;
        }
        default: UNREACHABLE();
    }
}

OrsoString* orso_new_string_from_cstrn(OrsoGarbageCollector* gc, const char* start, i32 length) {
    OrsoString* string = ORSO_OBJECT_ALLOCATE_FLEX(gc, OrsoString, &OrsoTypeString, length + 1);
    string->length = length;
    memcpy(string->text, start, length);
    string->text[length] = '\0';

    return string;
}

char* cstrn_new(const char* start, i32 length) {
    char* cstr = ORSO_ALLOCATE_N(char, length + 1);
    memcpy(cstr, start, length);
    cstr[length] = '\0';

    return cstr;
}

char* orso_slot_to_new_cstrn(OrsoSlot slot, OrsoType* type) {
    switch (type->kind) {
        case ORSO_TYPE_BOOL: {
            if (ORSO_SLOT_IS_FALSE(slot)) {
                return cstrn_new("false", 5);
            } else {
                return cstrn_new("true", 4);
            }
        }

        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: {
            // Max characters for i64 is 19 + sign and \0
            char buffer[21];
            i32 length = sprintf(buffer, "%lld", slot.as.i);
            return cstrn_new(buffer, length);
        }

        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: {
            // Using Wren's num to string for this: https://github.com/wren-lang/wren/blob/main/src/vm/wren_value.c#L775
            char buffer[24];
            i32 length;
            if (slot.as.f == (i64)slot.as.f) {
                length = sprintf(buffer, "%.1f", slot.as.f);
            } else {
                length = sprintf(buffer, "%.14g", slot.as.f);
            }
            return cstrn_new(buffer, length);
        }

        case ORSO_TYPE_VOID: return cstrn_new("null", 4);

        case ORSO_TYPE_STRING: return cstrn_new(((OrsoString*)slot.as.p)->text, ((OrsoString*)slot.as.p)->length);

        case ORSO_TYPE_SYMBOL: {
            OrsoSymbol* symbol = (OrsoSymbol*)slot.as.p;
            // 2 single quotes
            // 1 \0
            char buffer[symbol->length + 3];
            sprintf(buffer, "'%s'", symbol->text);
            return cstrn_new(buffer, symbol->length + 3);
        }

        case ORSO_TYPE_FUNCTION: {
            OrsoFunction* function = (OrsoFunction*)slot.as.p;
            char buffer[500];
            i32 n = sprintf(buffer, "<(");

            char tmp_buffer[128];
            for (i32 i = 0; i < function->type->argument_count; i++) {
                orso_type_to_cstrn(function->type->argument_types[i], tmp_buffer, 128);

                n += sprintf(buffer + n, "%s%s", tmp_buffer,
                        i == function->type->argument_count - 1 ? "" : ", ");
            }

            orso_type_to_cstrn(function->type->return_type, tmp_buffer, 128);
            n += sprintf(buffer + n, ") -> %s>", tmp_buffer);
            
            buffer[n] = '\0';

            return cstrn_new(buffer, n);
        }

        case ORSO_TYPE_TYPE: {
            OrsoType* type = (OrsoType*)slot.as.p;

            char buffer[128];

            char tmp_buffer[128];
            orso_type_to_cstrn(type, tmp_buffer, 128);

            sprintf(buffer, "<%s>", tmp_buffer);

            return cstrn_new(buffer, strlen(buffer));
        }
        case ORSO_TYPE_UNRESOLVED: return cstrn_new("<unresolved>", 12);
        case ORSO_TYPE_INVALID: return cstrn_new("<invalid>", 9);

        case ORSO_TYPE_UNION:
        case ORSO_TYPE_USER:
        case ORSO_TYPE_MAX: UNREACHABLE(); return NULL;
    }
}

OrsoString* orso_slot_to_string(OrsoGarbageCollector* gc, OrsoSlot slot, OrsoType* type) {
    char* cstr = orso_slot_to_new_cstrn(slot, type);
    OrsoString* string = orso_new_string_from_cstrn(gc, cstr, strlen(cstr));
    free(cstr);

    return string;
}

OrsoString* orso_string_concat(OrsoGarbageCollector* gc, OrsoString* a, OrsoString* b) {
    OrsoString* string = ORSO_OBJECT_ALLOCATE_FLEX(gc, OrsoString, &OrsoTypeString, a->length + b->length + 1);
    string->length = a->length + b->length;
    memcpy(string->text, a->text, a->length);
    memcpy(string->text + a->length, b->text, b->length);
    string->text[a->length + b->length] = '\0';

    return string;
}

OrsoFunction* orso_new_function(OrsoGarbageCollector* gc) {
    OrsoFunction* function = ORSO_OBJECT_ALLOCATE(gc, OrsoFunction, (OrsoType * const)&OrsoTypeEmptyFunction);
    function->type = &OrsoTypeEmptyFunction;
    chunk_init(&function->chunk);

    return function;
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

    for (char* c = (char*)text; c != (text + length); c++) {
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
