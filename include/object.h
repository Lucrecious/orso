#ifndef OBJECT_H_
#define OBJECT_H_

#include "chunk.h"
#include "def.h"
#include "type_set.h"
#include "slot.h"
#include "symbol_table.h"

typedef struct OrsoObject {
    OrsoType* type;
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

typedef struct OrsoFunction {
    OrsoObject object;
    Chunk chunk;
    OrsoType* signature;
    OrsoSymbol* binded_name;
} OrsoFunction;

typedef struct OrsoStruct {
    OrsoSlot* slots;
} OrsoStruct;

typedef void (*NativeFunction)(OrsoSlot* arguments, OrsoSlot* result);

typedef struct OrsoNativeFunction {
    OrsoObject object;
    OrsoType* signature;
    NativeFunction function;
} OrsoNativeFunction;

void* orso_object_reallocate(OrsoObject* pointer, OrsoType* type, size_t old_size, size_t new_size);
void orso_object_free(OrsoObject* pointer);

#define ORSO_OBJECT_ALLOCATE_N(TYPE, ORSO_TYPE, N) (TYPE*)orso_object_reallocate(NULL, ORSO_TYPE, 0, sizeof(TYPE) * N)
#define ORSO_OBJECT_ALLOCATE(TYPE, ORSO_TYPE) ORSO_OBJECT_ALLOCATE_N(TYPE, ORSO_TYPE, 1)
#define ORSO_OBJECT_ALLOCATE_FLEX(TYPE, ORSO_TYPE, N) (TYPE*)orso_object_reallocate(NULL, ORSO_TYPE, 0, sizeof(TYPE) + N)

FORCE_INLINE u32 orso_hash_cstrn(const char* start, i32 length) {
    u32 hash = 2166136261u;
    for (i32 i = 0; i < length; i++) {
        hash ^= (byte)start[i];
        hash *= 16777619;
    }

    return hash;
}

FORCE_INLINE bool orso_string_equal(OrsoString* a, OrsoString* b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->text, b->text, a->length) == 0;
}

OrsoString* orso_string_concat(OrsoString* a, OrsoString* b);

// TODO: move to a better file
char* orso_slot_to_new_cstrn(OrsoSlot* slot, OrsoType* type);

OrsoString* orso_slot_to_string(OrsoSlot* slot, OrsoType* type);

OrsoString* orso_new_string_from_cstrn(const char* start, i32 length);

OrsoFunction* orso_new_function(void);
bool is_function_compiled(OrsoFunction* function);

OrsoNativeFunction* orso_new_native_function(NativeFunction function, OrsoType* type);

OrsoStruct* orso_new_struct(void);

i64 cstrn_to_i64(const char* text, i32 length);
f64 cstrn_to_f64(const char* text, i32 length);

OrsoSymbol* orso_unmanaged_symbol_from_cstrn(const char* start, i32 length, OrsoSymbolTable* symbol_table);

FORCE_INLINE void orso_unmanaged_symbol_free(OrsoSymbol* symbol) {
    free(symbol);
}

OrsoSymbol* orso_new_symbol_from_cstrn(const char* start, i32 length, OrsoSymbolTable* symbol_table);

#endif
