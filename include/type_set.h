#ifndef TYPE_SET_H_
#define TYPE_SET_H_

#include "def.h"
#include "type.h"
#include "arena.h"
#include "table.h"

typedef struct type_id_t type_id_t;
struct type_id_t {
    u64 i;
};

#define typeid(INDEX) (type_id_t){.i=INDEX}
#define typeid_eq(ID1, ID2) ((ID1).i == (ID2).i)

declare_table(type2u64, type_t*, type_id_t);

typedef struct type_table_t {
    types_t types;
    table_t(type2u64) *types2index;
    arena_t *allocator;
} type_table_t;

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

void type_set_init(type_table_t *set, arena_t *allocator);

inline type_t *get_type_info(types_t *types, type_id_t type_id) {
    return types->items[type_id.i];
}

inline bool type_is_union(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_UNION;
}

inline bool type_is_function(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_FUNCTION;
}

inline bool type_is_native_function(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_NATIVE_FUNCTION;
}

inline bool type_is_struct(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_STRUCT;
}

inline bool type_is_pointer(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_POINTER;
}

type_id_t type_set_fetch_union(type_table_t *set, type_ids_t types);

type_id_t type_set_fetch_function(
    type_table_t *set,
    type_id_t return_type,
    type_ids_t arguments);

type_id_t type_set_fetch_native_function(
    type_table_t* set,
    type_id_t return_type,
    type_ids_t arguments);

type_id_t type_set_fetch_anonymous_struct(
    type_table_t *set,
    i32 field_count, struct_field_t *fields,
    i32 constant_count, struct_constant_t *constants);

type_id_t type_set_fetch_pointer(type_table_t *set, type_id_t type);

type_id_t type_create_struct(type_table_t *set, char *name, i32 name_length, type_id_t anonymous_struct);

type_id_t type_unique_incomplete_struct_type(type_table_t *set);

void named_struct_copy_data_from_completed_struct_type(type_table_t *set, type_id_t incomplete_named_struct, type_id_t complete_anonymous_struct);

#endif
