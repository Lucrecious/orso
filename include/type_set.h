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

type_t type_set_fetch_pointer(type_table_t *set, type_t type);

type_t type_set_fetch_array(type_table_t *set, type_t value_type, size_t size);

type_t type_set_fetch_anonymous_incomplete_struct(type_table_t *set);
void type_set_invalid_struct(type_table_t *set, type_t incomplete_type);
void type_set_complete_struct(type_table_t *set, type_t incomplete_struct, struct_fields_t fields);

#endif
