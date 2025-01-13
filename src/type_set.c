#include "type_set.h"

#include <stdlib.h>
#include "slot.h"

#define ORSO_TYPE_SET_MAX_LOAD 0.75
#define GROW_CAPACITY(capacity) capacity == 0 ? 8 : capacity * 2

#define ALLOC(TYPE) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE))
#define ALLOC_N(TYPE, N) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE)*N)

typedata_t *function_type_new(type_table_t *set, types_t arguments, type_t return_type, bool is_native) {
    typedata_t *function_type = ALLOC(typedata_t);
    function_type->kind = is_native ? TYPE_NATIVE_FUNCTION : TYPE_FUNCTION;
    function_type->data.function.argument_types = (types_t){.allocator=set->allocator};
    for (size_t i = 0; i < arguments.count; ++i) {
        array_push(&function_type->data.function.argument_types, arguments.items[i]);
    }

    function_type->data.function.return_type = return_type;

    function_type->size = sizeof(void*);

    return function_type;
}

// only anonymous structs can be looked up in the type set
typedata_t *struct_type_new(type_table_t *set, struct_field_t *fields, i32 field_count, struct_constant_t *constants, i32 constant_count, i32 total_size) {
    typedata_t* struct_type = ALLOC(typedata_t);
    struct_type->kind = TYPE_STRUCT;

    struct_type->data.struct_.name = NULL;

    struct_type->data.struct_.field_count = field_count;
    struct_type->data.struct_.fields = NULL;
    struct_type->data.struct_.constant_count = constant_count;
    struct_type->data.struct_.constants = NULL;
    struct_type->size = (size_t)total_size;

    if (field_count > 0) {
        struct_type->data.struct_.fields = ALLOC_N(struct_field_t, field_count);

        for (i32 i = 0; i < field_count; i++) {
            struct_type->data.struct_.fields[i] = fields[i];

            i32 length = strlen(fields[i].name);
            char* name = ALLOC_N(char, length + 1);
            memcpy(name, fields[i].name, length);
            name[length] = '\0';

            struct_type->data.struct_.fields[i].name = name;
        }
    }

    if (constant_count > 0) {
        struct_type->data.struct_.constants = ALLOC_N(struct_constant_t, constant_count);

        for (i32 i = 0; i < constant_count; i++) {
            struct_type->data.struct_.constants[i] = constants[i];

            i32 length = strlen(constants[i].name);
            char* name = ALLOC_N(char, length + 1);
            memcpy(name, constants[i].name, length);
            name[length] = '\0';

            struct_type->data.struct_.constants[i].name = name;
        }
    }

    return struct_type;
}

typedata_t *pointer_type_new(type_table_t *set, type_t type) {
    typedata_t *pointer = ALLOC(typedata_t);
    pointer->kind = TYPE_POINTER;
    pointer->data.pointer.type = type;
    pointer->size = sizeof(void*);

    return pointer;
}

typedata_t *type_copy_new(type_table_t *set, typedata_t *type) {
    if (type->kind == TYPE_FUNCTION || type->kind == TYPE_NATIVE_FUNCTION) {
        return (typedata_t*)function_type_new(
            set,
            type->data.function.argument_types,
            type->data.function.return_type,
            type->kind == TYPE_NATIVE_FUNCTION
        );
    }

    if (type->kind == TYPE_STRUCT) {
        return struct_type_new(
            set,
            type->data.struct_.fields,
            type->data.struct_.field_count,
            type->data.struct_.constants,
            type->data.struct_.constant_count,
            type->size);
    }

    if (type->kind == TYPE_POINTER) {
        return pointer_type_new(
            set,
            type->data.pointer.type);
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
    set->types = (type_infos_t){.allocator=allocator};

    static typedata_t type_invalid = {.kind=TYPE_INVALID, .size=0};
    static typedata_t type_unresolved = {.kind=TYPE_UNRESOLVED, .size=0};
    static typedata_t type_undefined = {.kind=TYPE_UNDEFINED, .size=0};
    static typedata_t type_unreachable = {.kind=TYPE_UNREACHABLE, .size=0};

    static typedata_t type_void = {.kind=TYPE_VOID, .size=0};
    static typedata_t type_label = { .kind=TYPE_LABEL, .size=sizeof(u64) };
    static typedata_t type_bool = {.kind=TYPE_BOOL, .size=NUM_SIZE_BYTE};
    static typedata_t type_f32 = {.kind=TYPE_NUMBER, .size=NUM_SIZE_SINGLE, .data.num = NUM_TYPE_FLOAT};
    static typedata_t type_f64 = {.kind=TYPE_NUMBER, .size=NUM_SIZE_LONG, .data.num = NUM_TYPE_FLOAT};
    static typedata_t type_i32 = {.kind=TYPE_NUMBER, .size=NUM_SIZE_SINGLE, .data.num = NUM_TYPE_SIGNED};
    static typedata_t type_i64 = {.kind=TYPE_NUMBER, .size=NUM_SIZE_LONG, .data.num = NUM_TYPE_SIGNED};
    static typedata_t type_u64 = {.kind=TYPE_NUMBER, .size=NUM_SIZE_LONG, .data.num = NUM_TYPE_UNSIGNED};
    static typedata_t type_string = {.kind=TYPE_STRING, .size=sizeof(void*)};
    static typedata_t type_type = {.kind=TYPE_TYPE, .size=sizeof(type_t)};
    static typedata_t empty_function = {.kind = TYPE_FUNCTION, .size = sizeof(void*), .data.function.return_type = typeid(TYPE_VOID)};

    type_t invalid = typeid(set->types.count);
    array_push(&set->types, &type_invalid);

    type_t unresolved = typeid(set->types.count);
    array_push(&set->types, &type_unresolved);

    type_t undefined = typeid(set->types.count);
    array_push(&set->types, &type_undefined);

    type_t unreachable = typeid(set->types.count);
    array_push(&set->types, &type_unreachable);

    type_t label = typeid(set->types.count);
    array_push(&set->types, &type_label);

    type_t void_ = typeid(set->types.count);
    array_push(&set->types, &type_void);

    type_t bool_ = typeid(set->types.count);
    array_push(&set->types, &type_bool);

    type_t string_ = typeid(set->types.count);
    array_push(&set->types, &type_string);

    type_t type_ = typeid(set->types.count);
    array_push(&set->types, &type_type);

    set->f32_ = typeid(set->types.count);
    array_push(&set->types, &type_f32);

    set->f64_ = typeid(set->types.count);
    array_push(&set->types, &type_f64);

    set->i32_ = typeid(set->types.count);
    array_push(&set->types, &type_i32);

    set->i64_ = typeid(set->types.count);
    array_push(&set->types, &type_i64);

    set->u64_ = typeid(set->types.count);
    array_push(&set->types, &type_u64);

    set->empty_function_ = typeid(set->types.count);
    array_push(&set->types, &empty_function);
    
    ASSERT(invalid.i == TYPE_INVALID, "must be same as type invalid");
    ASSERT(unresolved.i == TYPE_UNRESOLVED, "must be same as type unresolved");
    ASSERT(undefined.i == TYPE_UNDEFINED, "must be same as type undefined");
    ASSERT(unreachable.i == TYPE_UNREACHABLE, "must be same as type unreachable");
    ASSERT(label.i == TYPE_LABEL, "must be same as type jmp location");
    ASSERT(void_.i == TYPE_VOID, "must be same as type void");
    ASSERT(bool_.i == TYPE_BOOL, "must be same as type bool");
    ASSERT(string_.i == TYPE_STRING, "must be same as type string");
    ASSERT(type_.i == TYPE_TYPE, "must be same as type type");
}

typedata_t *type2typedata(type_infos_t *types, type_t type) {
    return types->items[type.i];
}

bool type_is_function(type_infos_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_FUNCTION;
}

bool type_is_native_function(type_infos_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_NATIVE_FUNCTION;
}

bool type_is_struct(type_infos_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_STRUCT;
}

bool type_is_pointer(type_infos_t types, type_t type) {
    return type2typedata(&types, type)->kind == TYPE_POINTER;
}

static u64 hash_type(typedata_t *type) {
#define ADD_HASH(HASH, APPEND) HASH ^= APPEND; HASH *= 16777619

    u32 hash = 2166136261u;
    ADD_HASH(hash, type->kind);

    if (type->kind == TYPE_FUNCTION) {
        ADD_HASH(hash, type->data.function.argument_types.count);

        for (size_t i = 0; i < type->data.function.argument_types.count; ++i) {
            ADD_HASH(hash, (u64)(type->data.function.argument_types.items[i].i));
        }

        ADD_HASH(hash, (u64)(type->data.function.return_type.i));
    } else if (type->kind == TYPE_STRUCT) {
        ASSERT(type->data.struct_.name == NULL, "only anonymous structs are hashed");
        ASSERT(type->data.struct_.constant_count == 0, "only anonymous structs without constants can be hashed");

        ADD_HASH(hash, type->data.struct_.field_count);

        for (i32 i = 0; i < type->data.struct_.field_count; i++) {
            char* name = type->data.struct_.fields[i].name;
            i32 length = strlen(name);
            for (i32 i = 0; i < length; i++) {
                ADD_HASH(hash, name[i]);
            }
            
            ADD_HASH(hash, (u64)(type->data.struct_.fields[i].type.i));
        }
    } else if (type->kind == TYPE_POINTER) {
        ADD_HASH(hash, (u64)(type->data.pointer.type.i));
    }

    return hash;

#undef ADD_HASH
}

implement_table(type2u64, typedata_t*, type_t, hash_type, type_equal);

type_t type_set_fetch_pointer(type_table_t* set, type_t inner_type) {
    typedata_t pointer_type = {
        .kind = TYPE_POINTER,
        .data.pointer.type = inner_type,
        .size = sizeof(void*)
    };

    type_t type;
    if (table_get(type2u64, set->types2index, &pointer_type, &type)) {
        return type;
    }

    typedata_t *type_info = type_copy_new(set, &pointer_type);
    type = track_type(set, type_info);

    return type;
}

type_t type_set_fetch_function_(type_table_t *set, type_t return_type, types_t arguments, bool is_native) {
    typedata_t function_type = {
        .kind = is_native ? TYPE_NATIVE_FUNCTION : TYPE_FUNCTION,
        .data.function.argument_types = arguments,
        .data.function.return_type = return_type,
    };

    type_t type;
    if (table_get(type2u64, set->types2index, &function_type, &type)) {
        return type;
    }

    typedata_t *type_info = type_copy_new(set, (typedata_t*)&function_type);
    type = track_type(set, type_info);

    return type;
}

type_t type_set_fetch_function(type_table_t *set, type_t return_type, types_t arguments) {
    return type_set_fetch_function_(set, return_type, arguments, false);
}

type_t type_set_fetch_native_function(type_table_t *set, type_t return_type, types_t arguments) {
    return type_set_fetch_function_(set, return_type, arguments, true);
}

type_t type_set_fetch_anonymous_struct(type_table_t *set, i32 field_count, struct_field_t *fields, i32 constant_count, struct_constant_t* constants) {
    typedata_t struct_type = {
        .kind = TYPE_STRUCT,
        .data.struct_.field_count = field_count,
        .data.struct_.fields = fields,

        .data.struct_.constant_count = constant_count,
        .data.struct_.constants = constants,

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
        type_info->data.struct_.fields[0].offset = 0;
        for (i32 i = 1; i < field_count; ++i) {
            i32 previous_offset = type_info->data.struct_.fields[i - 1].offset;
            type_t previous_type = fields[i - 1].type;
            typedata_t *previous_type_info = type2typedata(&set->types, previous_type);

            i32 bytes = bytes_to_words(previous_type_info->size) * WORD_SIZE;
            type_info->data.struct_.fields[i].offset = previous_offset + bytes;
        }

        type_t field_type= type_info->data.struct_.fields[field_count-1].type;
        typedata_t *field_type_info = type2typedata(&set->types, field_type);
        i32 size_of_final = bytes_to_words(field_type_info->size) * WORD_SIZE;
        i32 total_size = type_info->data.struct_.fields[field_count - 1].offset + size_of_final;

        type_info->size = total_size;
    }

    return type; 
}

type_t type_create_struct(type_table_t *set, cstr_t name, i32 name_length, typedata_t *anonymous_struct) {
    ASSERT(anonymous_struct->kind == TYPE_STRUCT && anonymous_struct->data.struct_.name == NULL, "can only create struct from anonymous struct");

    if (anonymous_struct->data.struct_.constant_count == 0) {
        typedata_t *new_type_info = type_copy_new(set, anonymous_struct);
        new_type_info->data.struct_.name = ALLOC_N(char, name_length + 1);
        memcpy(new_type_info->data.struct_.name, name, name_length);
        new_type_info->data.struct_.name[name_length] = '\0';

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

    incomplete_named_struct->data.struct_.field_count = copied_type->data.struct_.field_count;
    incomplete_named_struct->data.struct_.fields = copied_type->data.struct_.fields;

    incomplete_named_struct->data.struct_.constant_count = copied_type->data.struct_.constant_count;
    incomplete_named_struct->data.struct_.constants = copied_type->data.struct_.constants;

    incomplete_named_struct->size = copied_type->size;
}
