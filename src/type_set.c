#include "type_set.h"

#include <stdlib.h>

#define ORSO_TYPE_SET_MAX_LOAD 0.75
#define GROW_CAPACITY(capacity) capacity == 0 ? 8 : capacity * 2

#define ALLOC(TYPE) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE))
#define ALLOC_N(TYPE, N) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE)*N)

typedata_t *function_type_new(type_table_t *set, types_t arguments, type_t return_type, bool is_native) {
    typedata_t *function_type = ALLOC(typedata_t);
    function_type->kind = is_native ? TYPE_INTRINSIC_FUNCTION : TYPE_FUNCTION;
    function_type->as.function.argument_types = (types_t){.allocator=set->allocator};
    for (size_t i = 0; i < arguments.count; ++i) {
        array_push(&function_type->as.function.argument_types, arguments.items[i]);
    }

    function_type->as.function.return_type = return_type;

    function_type->size = sizeof(void*);
    function_type->alignment = sizeof(void*);

    return function_type;
}

typedata_t *pointer_type_new(type_table_t *set, type_t type) {
    typedata_t *pointer = ALLOC(typedata_t);
    pointer->kind = TYPE_POINTER;
    pointer->as.ptr.type = type;
    pointer->size = sizeof(void*);
    pointer->alignment = sizeof(void*);
    pointer->capabilities = TYPE_CAP_NONE;

    return pointer;
}

void array_type_new(type_table_t *set, size_t size, type_t type, typedata_t *result) {
    typedata_t *itemtd = set->types.items[type.i];
    result->kind = TYPE_ARRAY;
    result->as.arr.type = type;
    result->as.arr.count = size;
    result->alignment = itemtd->alignment;
    result->size = td_align(itemtd->size, itemtd->alignment)*size;
    result->capabilities = (itemtd->capabilities & TYPE_CAP_ARITHMETIC);
}

void incomplete_struct_type_init(type_table_t *set, struct_fields_t fields, struct_fields_t constants, typedata_t *result) {
    result->kind = TYPE_STRUCT;
    result->as.struct_.constants = constants;

    size_t current_alignment = 0;
    size_t current_size = 0;
    bool all_arithmetic = true;
    for (size_t i = 0; i < fields.count; ++i) {
        struct_field_t field = fields.items[i];
        typedata_t *fieldtd = type2typedata(&set->types, field.type);
        size_t offset = td_align(current_size, fieldtd->alignment);

        fields.items[i].offset = offset;

        current_size = offset + fieldtd->size;
        current_alignment = current_alignment < fieldtd->alignment ? fieldtd->alignment : current_alignment;

        if ((fieldtd->capabilities&TYPE_CAP_ARITHMETIC) == 0) {
            all_arithmetic = false;
        }
    }

    if (fields.count == 0) all_arithmetic = false;

    result->as.struct_.fields = fields;

    result->size = (size_t)current_size;
    result->alignment = current_alignment;
    result->capabilities = all_arithmetic ? TYPE_CAP_ARITHMETIC : TYPE_CAP_NONE;
    result->as.struct_.status = STRUCT_STATUS_INCOMPLETE;
}

struct_fields_t fields_copy(struct_fields_t fields, arena_t *arena) {
    struct_fields_t copy = {.allocator=arena};
    
    for (size_t i = 0; i < fields.count; ++i) {
        struct_field_t field = fields.items[i];
        field.name = string_copy(field.name, arena);
        array_push(&copy, field);
    }

    return copy;
}

typedata_t *type_copy_new(type_table_t *set, typedata_t *type) {
    if (type->kind == TYPE_FUNCTION || type->kind == TYPE_INTRINSIC_FUNCTION) {
        return (typedata_t*)function_type_new(
            set,
            type->as.function.argument_types,
            type->as.function.return_type,
            type->kind == TYPE_INTRINSIC_FUNCTION
        );
}

    if (type->kind == TYPE_STRUCT) {
        typedata_t *td = ALLOC(typedata_t);
        struct_fields_t fields = fields_copy(type->as.struct_.fields, set->allocator);
        struct_fields_t constants = fields_copy(type->as.struct_.constants, set->allocator);
        incomplete_struct_type_init(set, fields, constants, td);
        return td;
    }

    if (type->kind == TYPE_POINTER) {
        return pointer_type_new(
            set,
            type->as.ptr.type);
    }

    if (type->kind == TYPE_ARRAY) {
        typedata_t *td = ALLOC(typedata_t);
        array_type_new(set, type->as.arr.count, type->as.arr.type, td);
        return td;
    }

    UNREACHABLE();
    return NULL;
}

static type_t track_type(type_table_t *set, typedata_t *type) {
    array_push(&set->types, type);
    table_put(type2u64, set->types2index, type, typeid(set->types.count-1));
    return typeid(set->types.count-1);
}

static type_t add_type_no_track(type_table_t *set, typedata_t *type) {
    array_push(&set->types, type);
    return typeid(set->types.count-1);
}

void type_set_init(type_table_t* set, arena_t *allocator) {
    set->allocator = allocator;
    set->types2index = table_new(type2u64, allocator);
    set->types = (typedatas_t){.allocator=allocator};

    static typedata_t type_invalid = {.name=lit2str("<invalid>"), .kind=TYPE_INVALID, .size=0, .alignment=0};
    static typedata_t type_unresolved = {.name=lit2str("<unresolved>"), .kind=TYPE_UNRESOLVED, .size=0, .alignment=0};
    static typedata_t type_inferred_funcdef = {.name=lit2str("<inferred funcdef>"), .kind=TYPE_INFERRED_FUNCTION, .size=0, .alignment=0};
    static typedata_t type_param_struct = {.name=lit2str("<param struct>"), .kind=TYPE_PARAM_STRUCT, .size=0, .alignment=0};
    static typedata_t type_unreachable = {.name=lit2str("<unreachable>"), .kind=TYPE_UNREACHABLE, .size=0, .alignment=0};

    static typedata_t type_void = {.name=lit2str("void"), .kind=TYPE_VOID, .size=0, .alignment=0, .capabilities=TYPE_CAP_NONE};
    static typedata_t type_bool = {.name=lit2str("bool"), .kind=TYPE_BOOL, .size=NUM_SIZE_8, .alignment=NUM_SIZE_8, .capabilities=TYPE_CAP_LOGICAL};
    static typedata_t type_str8 = {.name=lit2str("str8_t"), .kind=TYPE_STRING, .size=0, .alignment=0, .capabilities=TYPE_CAP_NONE};
    static typedata_t type_f32 = {.name=lit2str("f32"), .kind=TYPE_NUMBER, .size=NUM_SIZE_32, .alignment=NUM_SIZE_32, .as.num = NUM_TYPE_FLOAT, .capabilities=TYPE_CAP_NUMBER};
    static typedata_t type_f64 = {.name=lit2str("f64"), .kind=TYPE_NUMBER, .size=NUM_SIZE_64, .alignment=NUM_SIZE_64, .as.num = NUM_TYPE_FLOAT, .capabilities=TYPE_CAP_NUMBER};

    static typedata_t type_s8 = {.name=lit2str("s8"), .kind=TYPE_NUMBER, .size=NUM_SIZE_8, .alignment=NUM_SIZE_8, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_NUMBER)};
    static typedata_t type_u8 = {.name=lit2str("u8"), .kind=TYPE_NUMBER, .size=NUM_SIZE_8, .alignment=NUM_SIZE_8, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_NUMBER)};

    static typedata_t type_s16 = {.name=lit2str("s16"), .kind=TYPE_NUMBER, .size=NUM_SIZE_16, .alignment=NUM_SIZE_16, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_NUMBER)};
    static typedata_t type_u16 = {.name=lit2str("u16"), .kind=TYPE_NUMBER, .size=NUM_SIZE_16, .alignment=NUM_SIZE_16, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_NUMBER)};

    static typedata_t type_s32 = {.name=lit2str("s32"), .kind=TYPE_NUMBER, .size=NUM_SIZE_32, .alignment=NUM_SIZE_32, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_NUMBER)};
    static typedata_t type_u32 = {.name=lit2str("u32"), .kind=TYPE_NUMBER, .size=NUM_SIZE_32, .alignment=NUM_SIZE_32, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_NUMBER)};

    static typedata_t type_s64 = {.name=lit2str("s64"), .kind=TYPE_NUMBER, .size=NUM_SIZE_64, .alignment=NUM_SIZE_64, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_NUMBER)};
    static typedata_t type_u64 = {.name=lit2str("u64"), .kind=TYPE_NUMBER, .size=NUM_SIZE_64, .alignment=NUM_SIZE_64, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_NUMBER)};

    static typedata_t type_int = {.name=lit2str("int"), .kind=TYPE_NUMBER, .size=sizeof(int), .alignment=sizeof(int), .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_NUMBER)};
    static typedata_t type_uint = {.name=lit2str("uint"), .kind=TYPE_NUMBER, .size=sizeof(unsigned int), .alignment=sizeof(unsigned int), .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_NUMBER)};
    static typedata_t type_size_t = {.name=lit2str("size_t"), .kind=TYPE_NUMBER, .size=sizeof(size_t), .alignment=sizeof(size_t), .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_NUMBER)};
    static typedata_t type_ptrdiff_t = {.name=lit2str("s64"), .kind=TYPE_NUMBER, .size=sizeof(s64), .alignment=sizeof(s64), .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_NUMBER)};

    static typedata_t type_type = {.name=lit2str("type"), .kind=TYPE_TYPE, .size=sizeof(type_t), .alignment=sizeof(type_t), .capabilities=(TYPE_CAP_NONE)};
    static typedata_t empty_function = {.name=lit2str(""), .kind = TYPE_FUNCTION, .size = sizeof(void*), .alignment=sizeof(void*), .as.function.return_type = typeid(TYPE_VOID), .capabilities=(TYPE_CAP_NONE)};

    type_t invalid = typeid(set->types.count);
    array_push(&set->types, &type_invalid);

    type_t unresolved = typeid(set->types.count);
    array_push(&set->types, &type_unresolved);

    type_t inferred_funcdef = typeid(set->types.count);
    array_push(&set->types, &type_inferred_funcdef);

    type_t param_struct_ = typeid(set->types.count);
    array_push(&set->types, &type_param_struct);

    type_t unreachable = typeid(set->types.count);
    array_push(&set->types, &type_unreachable);

    set->void_ = typeid(set->types.count);
    array_push(&set->types, &type_void);

    set->bool_ = typeid(set->types.count);
    array_push(&set->types, &type_bool);

    set->str8_t_ = typeid(set->types.count);
    array_push(&set->types, &type_str8);

    type_t type_ = typeid(set->types.count);
    array_push(&set->types, &type_type);

    set->f32_ = typeid(set->types.count);
    array_push(&set->types, &type_f32);

    set->f64_ = typeid(set->types.count);
    array_push(&set->types, &type_f64);

    set->s8_ = typeid(set->types.count);
    array_push(&set->types, &type_s8);

    set->u8_ = typeid(set->types.count);
    array_push(&set->types, &type_u8);

    set->s16_ = typeid(set->types.count);
    array_push(&set->types, &type_s16);

    set->u16_ = typeid(set->types.count);
    array_push(&set->types, &type_u16);

    set->s32_ = typeid(set->types.count);
    array_push(&set->types, &type_s32);

    set->u32_ = typeid(set->types.count);
    array_push(&set->types, &type_u32);

    set->s64_ = typeid(set->types.count);
    array_push(&set->types, &type_s64);

    set->u64_ = typeid(set->types.count);
    array_push(&set->types, &type_u64);

    set->int_ = typeid(set->types.count);
    array_push(&set->types, &type_int);

    set->uint_ = typeid(set->types.count);
    array_push(&set->types, &type_uint);

    set->size_t_ = typeid(set->types.count);
    array_push(&set->types, &type_size_t);

    set->ptrdiff_t_ = typeid(set->types.count);
    array_push(&set->types, &type_ptrdiff_t);

    set->empty_function_ = typeid(set->types.count);
    array_push(&set->types, &empty_function);
    
    ASSERT(invalid.i == TYPE_INVALID, "must be same as type invalid");
    ASSERT(unresolved.i == TYPE_UNRESOLVED, "must be same as type unresolved");
    ASSERT(inferred_funcdef.i == TYPE_INFERRED_FUNCTION, "must be same as type inferred func def");
    ASSERT(param_struct_.i == TYPE_PARAM_STRUCT, "must be same as type param struct");
    ASSERT(unreachable.i == TYPE_UNREACHABLE, "must be same as type unreachable");
    ASSERT(set->void_.i == TYPE_VOID, "must be same as type void");
    ASSERT(set->bool_.i == TYPE_BOOL, "must be same as type bool");
    ASSERT(set->str8_t_.i == TYPE_STRING, "must be same as type string");
    ASSERT(type_.i == TYPE_TYPE, "must be same as type type");
}

typedata_t *type2typedata(typedatas_t *types, type_t type) {
    return types->items[type.i];
}

bool type_is_function(typedatas_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_FUNCTION;
}

bool type_is_intrinsic_function(typedatas_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_INTRINSIC_FUNCTION;
}

bool type_is_struct(typedatas_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_STRUCT;
}

bool type_is_pointer(typedatas_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_POINTER;
}

static khint_t hash_type(typedata_t *type) {
#define ADD_HASH(HASH, APPEND) HASH ^= APPEND; HASH *= 16777619

    u32 hash = 2166136261u;
    ADD_HASH(hash, type->kind);

    if (type->kind == TYPE_FUNCTION) {
        ADD_HASH(hash, type->as.function.argument_types.count);

        for (size_t i = 0; i < type->as.function.argument_types.count; ++i) {
            ADD_HASH(hash, (u64)(type->as.function.argument_types.items[i].i));
        }

        ADD_HASH(hash, (u64)(type->as.function.return_type.i));
    } else if (type->kind == TYPE_STRUCT) {
        ASSERT(type->as.struct_.name_or_null == NULL, "only anonymous structs are hashed");

        ADD_HASH(hash, (u32)type->as.struct_.fields.count);

        for (size_t i = 0; i < type->as.struct_.fields.count; i++) {
            string_t name = type->as.struct_.fields.items[i].name;
            for (size_t i = 0; i < name.length; i++) {
                ADD_HASH(hash, (u32)name.cstr[i]);
            }
            
            ADD_HASH(hash, (u64)(type->as.struct_.fields.items[i].type.i));
        }
    } else if (type->kind == TYPE_POINTER) {
        ADD_HASH(hash, (u64)(type->as.ptr.type.i));
    } else if (type->kind == TYPE_ARRAY) {
        ADD_HASH(hash, (u64)(type->as.arr.type.i));
        ADD_HASH(hash, (u64)(type->as.arr.count));
    }

    return hash;

#undef ADD_HASH
}

implement_table(type2u64, typedata_t*, type_t, hash_type, type_equal);

type_t type_set_fetch_pointer(type_table_t* set, type_t inner_type) {
    typedata_t pointer_type = {
        .kind = TYPE_POINTER,
        .as.ptr.type = inner_type,
        .size = sizeof(void*),
        .capabilities = TYPE_CAP_NONE
    };

    type_t type;
    if (table_get(type2u64, set->types2index, &pointer_type, &type)) {
        return type;
    }

    typedata_t *type_info = type_copy_new(set, &pointer_type);
    type = track_type(set, type_info);

    return type;
}

type_t type_set_fetch_array(type_table_t *set, type_t value_type, size_t size) {
    typedata_t array_type;
    array_type_new(set, size, value_type, &array_type);

    type_t type;
    if (table_get(type2u64, set->types2index, &array_type, &type)) {
        return type;
    }

    typedata_t *type_info = type_copy_new(set, &array_type);
    type = track_type(set, type_info);

    return type;
}

type_t type_set_fetch_anonymous_incomplete_struct(type_table_t *set) {
    typedata_t struct_type = {0};
    incomplete_struct_type_init(set, (struct_fields_t){0}, (struct_fields_t){0}, &struct_type);

    typedata_t *typeinfo = type_copy_new(set, &struct_type);
    typeinfo->as.struct_.status = STRUCT_STATUS_INCOMPLETE;
    type_t type = add_type_no_track(set, typeinfo);
    return type;
}

void type_set_invalid_struct(type_table_t *set, type_t incomplete_type) {
    typedata_t *td = type2typedata(&set->types, incomplete_type);
    MUST(td->kind == TYPE_STRUCT);
    td->as.struct_.status = STRUCT_STATUS_INVALID;
}

void type_set_complete_struct(type_table_t *set, type_t incomplete_type, struct_fields_t fields, struct_fields_t consts) {
    typedata_t *td = type2typedata(&set->types, incomplete_type);
    MUST(td->kind == TYPE_STRUCT && td->as.struct_.status == STRUCT_STATUS_INCOMPLETE);

    fields = fields_copy(fields, set->allocator);
    consts = fields_copy(consts, set->allocator);

    incomplete_struct_type_init(set, fields, consts, td);
    td->as.struct_.status = STRUCT_STATUS_COMPLETE;
}

type_t type_set_fetch_function(type_table_t *set, type_t return_type, types_t arguments) {
    typedata_t function_type = {
        .kind = TYPE_FUNCTION,
        .as.function.argument_types = arguments,
        .as.function.return_type = return_type,
        .size = sizeof(void*),
    };

    type_t type;
    if (table_get(type2u64, set->types2index, &function_type, &type)) {
        return type;
    }

    typedata_t *type_info = type_copy_new(set, (typedata_t*)&function_type);
    type = track_type(set, type_info);

    return type;
}

type_t type_set_fetch_intrinsic_function(type_table_t *set, type_t function_type) {
    typedata_t *td = type2typedata(&set->types, function_type);
    typedata_t functd = *td;
    functd.kind = TYPE_INTRINSIC_FUNCTION;

    type_t type;
    if (table_get(type2u64, set->types2index, &functd, &type)) {
        return type;
    }

    typedata_t *type_info = type_copy_new(set, &functd);
    type = track_type(set, type_info);

    return type;
}

