#ifndef TYPE_SET_H_
#define TYPE_SET_H_

#include "def.h"
#include "type.h"
#include "arena.h"
#include "table.h"

declare_table(type2u64, typedata_t*, type_t);

typedef struct type_table_t type_table_t;
struct type_table_t {
    typedatas_t types;
    table_t(type2u64) *types2index;
    arena_t *allocator;

    type_t void_;
    type_t bool_;

    type_t s8_;
    type_t u8_;

    type_t s16_;
    type_t u16_;

    type_t s32_;
    type_t u32_;

    type_t u64_;
    type_t s64_;

    type_t f32_;
    type_t f64_;

    type_t int_;
    type_t uint_;
    type_t size_t_;

    type_t ptrdiff_t_;

    type_t string_;
    type_t symbol_;
    type_t type_;
    type_t empty_function_;
};

void type_set_init(type_table_t *set, arena_t *allocator);

typedata_t *type2typedata(typedatas_t *types, type_t type);
bool type_is_function(typedatas_t types, type_t type);
bool type_is_intrinsic_function(typedatas_t types, type_t type);
bool type_is_struct(typedatas_t types, type_t type);
bool type_is_pointer(typedatas_t types, type_t type);

type_t type_set_fetch_function(
    type_table_t *set,
    type_t return_type,
    types_t arguments);

type_t type_set_fetch_intrinsic_function(
    type_table_t* set,
    type_t function_type);

type_t type_set_fetch_anonymous_struct(
    type_table_t *set,
    s32 field_count, struct_field_t *fields,
    s32 constant_count, struct_constant_t *constants);

type_t type_set_fetch_pointer(type_table_t *set, type_t type);

type_t type_set_fetch_array(type_table_t *set, type_t value_type, bool sized_at_runtime, size_t size);

type_t type_create_struct(type_table_t *set, cstr_t name, s32 name_length, typedata_t *anonymous_struct);

type_t type_unique_incomplete_struct_type(type_table_t *set);

void named_struct_copy_data_from_completed_struct_type(type_table_t *set, type_t incomplete_named_struct, type_t complete_anonymous_struct);

#endif
