#ifndef TYPE_SET_H_
#define TYPE_SET_H_

#include "def.h"
#include "type.h"
#include "arena.h"

typedef struct type_set_t {
    i32 count;
    i32 capacity;
    type_t** entries;
    arena_t *allocator;
} type_set_t;

extern type_t OrsoTypeVoid;
extern type_t OrsoTypeBool;
extern type_t OrsoTypeInteger32;
extern type_t OrsoTypeInteger64;
extern type_t OrsoTypeFloat32;
extern type_t OrsoTypeFloat64;
extern type_t OrsoTypeString;
extern type_t OrsoTypeSymbol;
extern type_t OrsoTypeType;
extern type_t OrsoTypeInvalid;
extern type_t OrsoTypeUnresolved;
extern type_t OrsoTypeUndefined;

extern type_t OrsoTypeEmptyFunction;

void type_set_init(type_set_t *set, arena_t *allocator);

type_t *orso_type_set_fetch_union(type_set_t *set, types_t types);

type_t *orso_type_set_fetch_function(
    type_set_t *set,
    type_t *return_type,
    types_t arguments);

type_t *orso_type_set_fetch_native_function(
    type_set_t* set,
    type_t *return_type,
    types_t arguments);

type_t *orso_type_set_fetch_anonymous_struct(
    type_set_t *set,
    i32 field_count, struct_field_t *fields,
    i32 constant_count, struct_constant_t *constants);

type_t *orso_type_set_fetch_pointer(type_set_t *set, type_t *type);

type_t *orso_type_create_struct(type_set_t *set, char *name, i32 name_length, type_t* anonymous_struct);

type_t *orso_type_unique_incomplete_struct_type(type_set_t *set);

void orso_named_struct_copy_data_from_completed_struct_type(type_t *incomplete_named_struct, type_t *complete_anonymous_struct);

#endif
