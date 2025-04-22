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

// only anonymous structs can be looked up in the type set
typedata_t *struct_type_new(type_table_t *set, struct_field_t *fields, s32 field_count, struct_constant_t *constants, s32 constant_count, s32 total_size) {
    MUST(false); // todo

    typedata_t* struct_type = ALLOC(typedata_t);
    struct_type->kind = TYPE_STRUCT;

    struct_type->as.struct_.name = NULL;

    struct_type->as.struct_.field_count = field_count;
    struct_type->as.struct_.fields = NULL;
    struct_type->as.struct_.constant_count = constant_count;
    struct_type->as.struct_.constants = NULL;
    struct_type->size = (size_t)total_size;

    if (field_count > 0) {
        struct_type->as.struct_.fields = ALLOC_N(struct_field_t, (size_t)field_count);

        for (s32 i = 0; i < field_count; i++) {
            struct_type->as.struct_.fields[i] = fields[i];

            size_t length = strlen(fields[i].name);
            char* name = ALLOC_N(char, length + 1);
            memcpy(name, fields[i].name, length);
            name[length] = '\0';

            struct_type->as.struct_.fields[i].name = name;
        }
    }

    if (constant_count > 0) {
        struct_type->as.struct_.constants = ALLOC_N(struct_constant_t, (size_t)constant_count);

        for (s32 i = 0; i < constant_count; i++) {
            struct_type->as.struct_.constants[i] = constants[i];

            size_t length = strlen(constants[i].name);
            char* name = ALLOC_N(char, length + 1);
            memcpy(name, constants[i].name, length);
            name[length] = '\0';

            struct_type->as.struct_.constants[i].name = name;
        }
    }

    return struct_type;
}

typedata_t *pointer_type_new(type_table_t *set, type_t type) {
    typedata_t *pointer = ALLOC(typedata_t);
    pointer->kind = TYPE_POINTER;
    pointer->as.ptr.type = type;
    pointer->size = sizeof(void*);
    pointer->alignment = sizeof(void*);
    pointer->capabilities = TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE;

    return pointer;
}

void array_type_new(type_table_t *set, size_t size, type_t type, typedata_t *result) {
    typedata_t *itemtd = set->types.items[type.i];
    result->kind = TYPE_ARRAY;
    result->as.arr.type = type;
    result->as.arr.count = size;
    result->alignment = itemtd->alignment;
    result->size = td_align(itemtd->size, itemtd->alignment)*size;
    result->capabilities = TYPE_CAP_NONE;
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
        return struct_type_new(
            set,
            type->as.struct_.fields,
            type->as.struct_.field_count,
            type->as.struct_.constants,
            type->as.struct_.constant_count,
            (s32)type->size);
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

void type_set_init(type_table_t* set, arena_t *allocator) {
    set->allocator = allocator;
    set->types2index = table_new(type2u64, allocator);
    set->types = (typedatas_t){.allocator=allocator};

    static typedata_t type_invalid = {.name=lit2str("<invalid>"), .kind=TYPE_INVALID, .size=0, .alignment=0};
    static typedata_t type_unresolved = {.name=lit2str("<unresolved>"), .kind=TYPE_UNRESOLVED, .size=0, .alignment=0};
    static typedata_t type_inferred_funcdef = {.name=lit2str("<inferred funcdef>"), .kind=TYPE_INFERRED_FUNCTION, .size=0, .alignment=0};
    static typedata_t type_unreachable = {.name=lit2str("<unreachable>"), .kind=TYPE_UNREACHABLE, .size=0, .alignment=0};

    static typedata_t type_void = {.name=lit2str("void"), .kind=TYPE_VOID, .size=0, .alignment=0, .capabilities=TYPE_CAP_NONE};
    static typedata_t type_bool = {.name=lit2str("bool"), .kind=TYPE_BOOL, .size=NUM_SIZE_8, .alignment=NUM_SIZE_8, .capabilities=TYPE_CAP_LOGICAL};
    static typedata_t type_f32 = {.name=lit2str("f32"), .kind=TYPE_NUMBER, .size=NUM_SIZE_32, .alignment=NUM_SIZE_32, .as.num = NUM_TYPE_FLOAT, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_f64 = {.name=lit2str("f64"), .kind=TYPE_NUMBER, .size=NUM_SIZE_64, .alignment=NUM_SIZE_64, .as.num = NUM_TYPE_FLOAT, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};

    static typedata_t type_s8 = {.name=lit2str("s8"), .kind=TYPE_NUMBER, .size=NUM_SIZE_8, .alignment=NUM_SIZE_8, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_u8 = {.name=lit2str("u8"), .kind=TYPE_NUMBER, .size=NUM_SIZE_8, .alignment=NUM_SIZE_8, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};

    static typedata_t type_s16 = {.name=lit2str("s16"), .kind=TYPE_NUMBER, .size=NUM_SIZE_16, .alignment=NUM_SIZE_16, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_u16 = {.name=lit2str("u16"), .kind=TYPE_NUMBER, .size=NUM_SIZE_16, .alignment=NUM_SIZE_16, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};

    static typedata_t type_s32 = {.name=lit2str("s32"), .kind=TYPE_NUMBER, .size=NUM_SIZE_32, .alignment=NUM_SIZE_32, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_u32 = {.name=lit2str("u32"), .kind=TYPE_NUMBER, .size=NUM_SIZE_32, .alignment=NUM_SIZE_32, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};

    static typedata_t type_s64 = {.name=lit2str("s64"), .kind=TYPE_NUMBER, .size=NUM_SIZE_64, .alignment=NUM_SIZE_64, .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_u64 = {.name=lit2str("u64"), .kind=TYPE_NUMBER, .size=NUM_SIZE_64, .alignment=NUM_SIZE_64, .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};

    static typedata_t type_int = {.name=lit2str("int"), .kind=TYPE_NUMBER, .size=sizeof(int), .alignment=sizeof(int), .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_uint = {.name=lit2str("uint"), .kind=TYPE_NUMBER, .size=sizeof(unsigned int), .alignment=sizeof(unsigned int), .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_size_t = {.name=lit2str("size_t"), .kind=TYPE_NUMBER, .size=sizeof(size_t), .alignment=sizeof(size_t), .as.num = NUM_TYPE_UNSIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};
    static typedata_t type_ptrdiff_t = {.name=lit2str("s64"), .kind=TYPE_NUMBER, .size=sizeof(s64), .alignment=sizeof(s64), .as.num = NUM_TYPE_SIGNED, .capabilities=(TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE)};

    static typedata_t type_string = {.name=lit2str("string"), .kind=TYPE_STRING, .size=sizeof(void*), .alignment=sizeof(void*), .capabilities=(TYPE_CAP_COMPARABLE)};
    static typedata_t type_type = {.name=lit2str("type"), .kind=TYPE_TYPE, .size=sizeof(type_t), .alignment=sizeof(type_t), .capabilities=(TYPE_CAP_NONE)};
    static typedata_t empty_function = {.name=lit2str(""), .kind = TYPE_FUNCTION, .size = sizeof(void*), .alignment=sizeof(void*), .as.function.return_type = typeid(TYPE_VOID), .capabilities=(TYPE_CAP_NONE)};

    type_t invalid = typeid(set->types.count);
    array_push(&set->types, &type_invalid);

    type_t unresolved = typeid(set->types.count);
    array_push(&set->types, &type_unresolved);

    type_t inferred_funcdef = typeid(set->types.count);
    array_push(&set->types, &type_inferred_funcdef);

    type_t unreachable = typeid(set->types.count);
    array_push(&set->types, &type_unreachable);

    set->void_ = typeid(set->types.count);
    array_push(&set->types, &type_void);

    set->bool_ = typeid(set->types.count);
    array_push(&set->types, &type_bool);

    type_t string_ = typeid(set->types.count);
    array_push(&set->types, &type_string);

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
    ASSERT(inferred_funcdef.i == TYPE_INFERRED_FUNCTION, "must be same as type unresolved");
    ASSERT(unreachable.i == TYPE_UNREACHABLE, "must be same as type unreachable");
    ASSERT(set->void_.i == TYPE_VOID, "must be same as type void");
    ASSERT(set->bool_.i == TYPE_BOOL, "must be same as type bool");
    ASSERT(string_.i == TYPE_STRING, "must be same as type string");
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
        ASSERT(type->as.struct_.name == NULL, "only anonymous structs are hashed");
        ASSERT(type->as.struct_.constant_count == 0, "only anonymous structs without constants can be hashed");

        ADD_HASH(hash, (u32)type->as.struct_.field_count);

        for (s32 i = 0; i < type->as.struct_.field_count; i++) {
            char* name = type->as.struct_.fields[i].name;
            size_t length = strlen(name);
            for (size_t i = 0; i < length; i++) {
                ADD_HASH(hash, (u32)name[i]);
            }
            
            ADD_HASH(hash, (u64)(type->as.struct_.fields[i].type.i));
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
        .capabilities = TYPE_CAP_ARITHMETIC|TYPE_CAP_COMPARABLE
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

type_t type_set_fetch_anonymous_struct(type_table_t *set, s32 field_count, struct_field_t *fields, s32 constant_count, struct_constant_t* constants) {
    typedata_t struct_type = {
        .kind = TYPE_STRUCT,
        .as.struct_.field_count = field_count,
        .as.struct_.fields = fields,

        .as.struct_.constant_count = constant_count,
        .as.struct_.constants = constants,

        .size = 0,
    };

    typedata_t *type_info;
    type_t type;
    if (constant_count == 0) {
        if (table_get(type2u64, set->types2index, &struct_type, &type)) {
            return type;
        }

        type_info = type_copy_new(set, &struct_type);
        type = track_type(set, type_info);
    } else {
        // anonymous structs with constants must be unique
        type_info = type_copy_new(set, &struct_type);
        array_push(&set->types, &struct_type);
        type = typeid(set->types.count-1);
    }

    // put the layout in a hash table separate from type
    // it should be impossible for sizing to be infinitely recursive because the types should have resolved
    // properly for this struct type to be created
    if (field_count > 0) {
        type_info->as.struct_.fields[0].offset = 0;
        for (s32 i = 1; i < field_count; ++i) {
            s32 previous_offset = type_info->as.struct_.fields[i - 1].offset;
            type_t previous_type = fields[i - 1].type;
            typedata_t *previous_type_info = type2typedata(&set->types, previous_type);

            size_t bytes = b2w(previous_type_info->size) * WORD_SIZE;
            type_info->as.struct_.fields[i].offset = (s32)((size_t)previous_offset + bytes);
        }

        type_t field_type= type_info->as.struct_.fields[field_count-1].type;
        typedata_t *field_type_info = type2typedata(&set->types, field_type);
        size_t size_of_final = b2w(field_type_info->size) * WORD_SIZE;
        size_t total_size = (size_t)type_info->as.struct_.fields[field_count - 1].offset + size_of_final;

        type_info->size = total_size;
    }

    return type; 
}

type_t type_create_struct(type_table_t *set, cstr_t name, s32 name_length, typedata_t *anonymous_struct) {
    ASSERT(anonymous_struct->kind == TYPE_STRUCT && anonymous_struct->as.struct_.name == NULL, "can only create struct from anonymous struct");

    if (anonymous_struct->as.struct_.constant_count == 0) {
        typedata_t *new_type_info = type_copy_new(set, anonymous_struct);
        new_type_info->as.struct_.name = ALLOC_N(char, (size_t)(name_length + 1));
        memcpy(new_type_info->as.struct_.name, name, name_length);
        new_type_info->as.struct_.name[name_length] = '\0';

        array_push(&set->types, new_type_info);
        type_t new_type = typeid(set->types.count-1);

        return new_type;
    } else {
        // here we know that the anonymous struct is unique because it has constants, and those are never shared
        typedata_t *new_type_info = type_copy_new(set, anonymous_struct);

        // anonymous_struct->data.struct_ = new_type->data.struct_;

        // anonymous_struct->data.struct_.name = ALLOC_N(char, name_length + 1);
        // memcpy(anonymous_struct->data.struct_.name, name, name_length);
        // anonymous_struct->data.struct_.name[name_length] = '\0';

        array_push(&set->types, new_type_info);
        type_t new_type = typeid(set->types.count-1);

        return new_type;
    }
}

type_t type_unique_incomplete_struct_type(type_table_t *set) {
    typedata_t *new_type = struct_type_new(set, NULL, -1, NULL, -1, 0);
    array_push(&set->types, new_type);
    return typeid(set->types.count-1);
}

void named_struct_copy_data_from_completed_struct_type(type_table_t *set, type_t incomplete_named_struct_id, type_t complete_anonymous_struct_id) {
    typedata_t *incomplete_named_struct = set->types.items[incomplete_named_struct_id.i];
    typedata_t *complete_anonymous_struct = set->types.items[complete_anonymous_struct_id.i];

    typedata_t *copied_type = type_copy_new(NULL, complete_anonymous_struct);

    incomplete_named_struct->as.struct_.field_count = copied_type->as.struct_.field_count;
    incomplete_named_struct->as.struct_.fields = copied_type->as.struct_.fields;

    incomplete_named_struct->as.struct_.constant_count = copied_type->as.struct_.constant_count;
    incomplete_named_struct->as.struct_.constants = copied_type->as.struct_.constants;

    incomplete_named_struct->size = copied_type->size;
}
