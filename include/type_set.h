#ifndef TYPE_SET_H_
#define TYPE_SET_H_

#include "def.h"
#include "type.h"
#include "khash.h"

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

extern OrsoType OrsoTypeEmptyFunction;

void orso_type_set_init(OrsoTypeSet* set);
void orso_type_set_free(OrsoTypeSet* set);

OrsoType* orso_type_set_fetch_union(OrsoTypeSet* set, OrsoType** types, i32 count);

OrsoType* orso_type_set_fetch_function(
    OrsoTypeSet* set,
    OrsoType* return_type,
    OrsoType** arguments, i32 argument_count);

OrsoType* orso_type_set_fetch_native_function(
    OrsoTypeSet* set,
    OrsoType* return_type,
    OrsoType** arguments, i32 argument_count);

OrsoType* orso_type_set_fetch_anonymous_struct(
    OrsoTypeSet* set,
    i32 field_count, OrsoStructField* fields,
    i32 constant_count, OrsoStructConstant* constants);

OrsoType* orso_type_set_fetch_pointer(OrsoTypeSet* set, OrsoType* type);

OrsoType* orso_type_create_struct(OrsoTypeSet* set, char* name, i32 name_length, OrsoType* anonymous_struct);

OrsoType* orso_type_unique_incomplete_struct_type(OrsoTypeSet* set);

void orso_named_struct_copy_data_from_completed_struct_type(OrsoType* incomplete_named_struct, OrsoType* complete_anonymous_struct);

#endif
