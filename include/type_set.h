#ifndef TYPE_SET_H_
#define TYPE_SET_H_

#include "def.h"
#include "type.h"
#include "arena.h"
#include "table.h"

declare_table(type2u64, typedata_t*, ortype_t);

typedef struct type_table_t type_table_t;
struct type_table_t {
    typedatas_t types;
    table_t(type2u64) *types2index;
    arena_t *allocator;

    ortype_t void_;
    ortype_t bool_;

    ortype_t char_;
    ortype_t schar_;
    ortype_t uchar_;

    ortype_t s8_;
    ortype_t u8_;

    ortype_t s16_;
    ortype_t u16_;

    ortype_t s32_;
    ortype_t u32_;

    ortype_t u64_;
    ortype_t s64_;

    ortype_t f32_;
    ortype_t f64_;

    ortype_t int_;
    ortype_t uint_;
    ortype_t size_t_;

    ortype_t ptrdiff_t_;

    ortype_t str8_t_;
    ortype_t symbol_;
    ortype_t type_;
};

void type_set_init(type_table_t *set, arena_t *allocator);

typedata_t *type2typedata(typedatas_t *types, ortype_t type);
bool type_is_function(typedatas_t types, ortype_t type);
bool type_is_intrinsic_function(typedatas_t types, ortype_t type);
bool type_is_struct(typedatas_t types, ortype_t type);
bool type_is_pointer(typedatas_t types, ortype_t type);

ortype_t type_set_fetch_function(
    type_table_t *set,
    ortype_t return_type,
    types_t arguments);

ortype_t type_set_fetch_intrinsic_function(
    type_table_t* set,
    ortype_t function_type);

ortype_t type_set_fetch_pointer(type_table_t *set, ortype_t type);

ortype_t type_set_fetch_array(type_table_t *set, ortype_t value_type, size_t size);

ortype_t type_set_fetch_anonymous_incomplete_struct(type_table_t *set);
void type_set_invalid_struct(type_table_t *set, ortype_t incomplete_type);
void type_set_complete_struct(type_table_t *set, ortype_t incomplete_struct, struct_fields_t fields, struct_fields_t consts);

void type_set_attach_params_to_struct_type(type_table_t *set, ortype_t struct_type, struct_fields_t params);

#endif
