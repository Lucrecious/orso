#ifndef TYPE_SET_H_
#define TYPE_SET_H_

#include "def.h"
#include "type.h"
#include "arena.h"
#include "table.h"

#define typeid(INDEX) (type_t){.i=INDEX}
#define typeid_eq(ID1, ID2) ((ID1).i == (ID2).i)

declare_table(type2u64, type_info_t*, type_t);

typedef struct type_table_t {
    type_infos_t types;
    table_t(type2u64) *types2index;
    arena_t *allocator;
} type_table_t;

void type_set_init(type_table_t *set, arena_t *allocator);

type_info_t *get_type_info(type_infos_t *types, type_t type);
bool type_is_union(type_infos_t types, type_t type);
bool type_is_function(type_infos_t types, type_t type);
bool type_is_native_function(type_infos_t types, type_t type);
bool type_is_struct(type_infos_t types, type_t type);
bool type_is_pointer(type_infos_t types, type_t type);

type_t type_set_fetch_union(type_table_t *set, types_t types);

type_t type_set_fetch_function(
    type_table_t *set,
    type_t return_type,
    types_t arguments);

type_t type_set_fetch_native_function(
    type_table_t* set,
    type_t return_type,
    types_t arguments);

type_t type_set_fetch_anonymous_struct(
    type_table_t *set,
    i32 field_count, struct_field_t *fields,
    i32 constant_count, struct_constant_t *constants);

type_t type_set_fetch_pointer(type_table_t *set, type_t type);

type_t type_create_struct(type_table_t *set, char *name, i32 name_length, type_info_t *anonymous_struct);

type_t type_unique_incomplete_struct_type(type_table_t *set);

void named_struct_copy_data_from_completed_struct_type(type_table_t *set, type_t incomplete_named_struct, type_t complete_anonymous_struct);

#endif
