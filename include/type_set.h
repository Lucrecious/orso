#ifndef TYPE_SET_H_
#define TYPE_SET_H_

#include "def.h"
#include "type.h"

typedef struct OrsoTypeSet {
    i32 count;
    i32 capacity;
    OrsoType** entries;
    OrsoType** heap;
} OrsoTypeSet;

extern OrsoType OrsoTypeVoid;
extern OrsoType OrsoTypeBool;
extern OrsoType OrsoTypeInteger32;
extern OrsoType OrsoTypeInteger64;
extern OrsoType OrsoTypeFloat32;
extern OrsoType OrsoTypeFloat64;
extern OrsoType OrsoTypeString;
extern OrsoType OrsoTypeSymbol;
extern OrsoType OrsoTypeType;
extern OrsoType OrsoTypeInvalid;
extern OrsoType OrsoTypeUnresolved;
extern OrsoType OrsoTypeUndefined;

extern OrsoFunctionType OrsoTypeEmptyFunction;

void orso_type_set_init(OrsoTypeSet* set);
void orso_type_set_free(OrsoTypeSet* set);

OrsoType* orso_type_set_fetch_union(OrsoTypeSet* set, OrsoType** types, i32 count);
OrsoType* orso_type_set_fetch_function(OrsoTypeSet* set, OrsoType* return_type, OrsoType** arguments, i32 argument_count);
OrsoType* orso_type_set_fetch_native_function(OrsoTypeSet* set, OrsoType* return_type, OrsoType** arguments, i32 argument_count);

#endif
