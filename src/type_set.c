#include "type_set.h"

#include <stdlib.h>
#include "slot.h"

#define ORSO_TYPE_SET_MAX_LOAD 0.75
#define GROW_CAPACITY(capacity) capacity == 0 ? 8 : capacity * 2

type_t OrsoTypeVoid = (type_t) { .kind = TYPE_VOID };
type_t OrsoTypeBool = (type_t) { .kind = TYPE_BOOL };
type_t OrsoTypeInteger32 = (type_t) { .kind = TYPE_INT32 };
type_t OrsoTypeInteger64 = (type_t) { .kind = TYPE_INT64 };
type_t OrsoTypeFloat32 = (type_t){ .kind = TYPE_FLOAT32 };
type_t OrsoTypeFloat64 = (type_t){ .kind = TYPE_FLOAT64 };
type_t OrsoTypeString = (type_t) { .kind = TYPE_STRING };
type_t OrsoTypeSymbol = (type_t) { .kind = TYPE_SYMBOL };
type_t OrsoTypeType = (type_t) { .kind = TYPE_TYPE };
type_t OrsoTypeInvalid = (type_t){ .kind = TYPE_INVALID  };
type_t OrsoTypeUnresolved = (type_t) { .kind = TYPE_UNRESOLVED };
type_t OrsoTypeUndefined = (type_t) { .kind = TYPE_UNDEFINED };

#define ALLOC(TYPE) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE))
#define ALLOC_N(TYPE, N) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE)*N)

static type_t *union_type_new(type_table_t *set, type_ids_t type_ids) {
    type_t* union_type = ALLOC(type_t);
    union_type->kind = TYPE_UNION;
    union_type->data.union_.types = (type_ids_t){.allocator=set->allocator};
    for (size_t i = 0; i < union_type->data.union_.types.count; ++i) {
        array_push(&union_type->data.union_.types, type_ids.items[i]);
        
    }

    return union_type;
}

type_t *function_type_new(type_table_t *set, type_ids_t arguments, type_id_t return_type, bool is_native) {
    type_t *function_type = ALLOC(type_t);
    function_type->kind = is_native ? TYPE_NATIVE_FUNCTION : TYPE_FUNCTION;
    function_type->data.function.argument_types = (type_ids_t){.allocator=set->allocator};
    for (size_t i = 0; i < arguments.count; ++i) {
        array_push(&function_type->data.function.argument_types, arguments.items[i]);
    }

    function_type->data.function.return_type = return_type;

    return function_type;
}

// only anonymous structs can be looked up in the type set
type_t *struct_type_new(type_table_t *set, struct_field_t *fields, i32 field_count, struct_constant_t *constants, i32 constant_count, i32 total_size) {
    type_t* struct_type = ALLOC(type_t);
    struct_type->kind = TYPE_STRUCT;

    struct_type->data.struct_.name = NULL;

    struct_type->data.struct_.field_count = field_count;
    struct_type->data.struct_.fields = NULL;
    struct_type->data.struct_.constant_count = constant_count;
    struct_type->data.struct_.constants = NULL;
    struct_type->data.struct_.total_bytes = total_size;

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

type_t *pointer_type_new(type_table_t *set, type_id_t type_id) {
    type_t *pointer = ALLOC(type_t);
    pointer->kind = TYPE_POINTER;
    pointer->data.pointer.type = type_id;

    return pointer;
}

type_t *type_copy_new(type_table_t *set, type_t *type) {
    if (type->kind == TYPE_UNION) {
        return (type_t*)union_type_new(
            set,
            type->data.union_.types
        );
    }

    if (type->kind == TYPE_FUNCTION || type->kind == TYPE_NATIVE_FUNCTION) {
        return (type_t*)function_type_new(
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
            type->data.struct_.total_bytes);
    }

    if (type->kind == TYPE_POINTER) {
        return pointer_type_new(
            set,
            type->data.pointer.type);
    }

    UNREACHABLE();
    return NULL;
}

static type_id_t track_type(type_table_t *set, type_t *type) {
    array_push(&set->types, type);
    table_put(type2u64, set->types2index, type, typeid(set->types.count-1));
    return typeid(set->types.count-1);
}

void type_set_init(type_table_t* set, arena_t *allocator) {
    set->allocator = allocator;
    set->types2index = table_new(type2u64, allocator);
    set->types = (types_t){.allocator=allocator};

    for (size_t i = 0; i < TYPE_COUNT; ++i) {
        array_push(&set->types, NULL);
    }

    static type_t type_void = {.kind=TYPE_VOID};
    static type_t type_bool = {.kind=TYPE_BOOL};
    static type_t type_f32 = {.kind=TYPE_FLOAT32};
    static type_t type_f64 = {.kind=TYPE_FLOAT64};
    static type_t type_i32 = {.kind=TYPE_INT32};
    static type_t type_i64 = {.kind=TYPE_INT64};
    static type_t type_string = {.kind=TYPE_STRING};
    static type_t type_symbol = {.kind=TYPE_SYMBOL};
    static type_t type_invalid = {.kind=TYPE_INVALID};
    static type_t type_unresolved = {.kind=TYPE_UNRESOLVED};
    static type_t type_undefined = {.kind=TYPE_UNDEFINED};
    static type_t type_type = {.kind=TYPE_TYPE};
    static type_t empty_function = {.kind = TYPE_FUNCTION, .data.function.return_type = typeid(TYPE_VOID)};

    for (size_t i = 0; i < TYPE_COUNT; ++i) {
        switch ((type_kind_t)i) {
            case TYPE_VOID: set->types.items[i] = &type_void; break;
            case TYPE_BOOL: set->types.items[i] = &type_bool; break;
            case TYPE_FLOAT32: set->types.items[i] = &type_f32; break;
            case TYPE_FLOAT64: set->types.items[i] = &type_f64; break;
            case TYPE_INT32: set->types.items[i] = &type_i32; break;
            case TYPE_INT64: set->types.items[i] = &type_i64; break;
            case TYPE_STRING: set->types.items[i] = &type_string; break;
            case TYPE_SYMBOL: set->types.items[i] = &type_symbol; break;
            case TYPE_INVALID: set->types.items[i] = &type_invalid; break;
            case TYPE_UNRESOLVED: set->types.items[i] = &type_unresolved; break;
            case TYPE_UNDEFINED: set->types.items[i] = &type_undefined; break;
            case TYPE_TYPE: set->types.items[i] = &type_type; break;
            case TYPE_FUNCTION: set->types.items[i] = &empty_function; break;

            case TYPE_NATIVE_FUNCTION:
            case TYPE_POINTER:
            case TYPE_UNION:
            case TYPE_STRUCT:
            case TYPE_COUNT: set->types.items[i] = &type_invalid; break;
        }
    }
}

type_t *get_type_info(types_t *types, type_id_t type_id) {
    return types->items[type_id.i];
}

bool type_is_union(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_UNION;
}

bool type_is_function(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_FUNCTION;
}

bool type_is_native_function(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_NATIVE_FUNCTION;
}

bool type_is_struct(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_STRUCT;
}

bool type_is_pointer(types_t types, type_id_t type_id) {
    return types.items[type_id.i]->kind == TYPE_POINTER;
}

static u64 hash_type(type_t *type) {
#define ADD_HASH(HASH, APPEND) HASH ^= APPEND; HASH *= 16777619

    u32 hash = 2166136261u;
    ADD_HASH(hash, type->kind);

    if (type->kind == TYPE_UNION) {
        for (size_t i = 0; i < type->data.union_.types.count; ++i) {
            ADD_HASH(hash, (u64)(type->data.union_.types.items[i].i));
        }
    } else if (type->kind == TYPE_FUNCTION) {
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

implement_table(type2u64, type_t*, type_id_t, hash_type, type_equal);

static i32 type_compare(const void *a, const void *b) {
    type_id_t *type_a = *((type_id_t**)a);
    type_id_t *type_b = *((type_id_t**)b);
    return type_a->i == type_b->i ? 0 : (type_a->i < type_b->i ? -1 : 1);

}

type_id_t type_set_fetch_union(type_table_t *set, type_ids_t types) {
    type_t union_type = {
        .kind = TYPE_UNION,
        .data.union_.types = {.allocator=set->allocator},
    };

    for (size_t i = 0; i < types.count; i++) {
        array_push(&union_type.data.union_.types, types.items[i]);
    }

    qsort(union_type.data.union_.types.items, types.count, sizeof(type_id_t), type_compare);

    type_id_t index;
    if (table_get(type2u64, set->types2index, &union_type, &index)) {
        return index;
    }

    type_t *type = type_copy_new(set, &union_type);
    type_id_t type_id = track_type(set, type);

    return type_id;
}

type_id_t type_set_fetch_pointer(type_table_t* set, type_id_t inner_type_id) {
    type_t pointer_type = {
        .kind = TYPE_POINTER,
        .data.pointer.type = inner_type_id,
    };

    type_id_t type_id;
    if (table_get(type2u64, set->types2index, &pointer_type, &type_id)) {
        return type_id;
    }

    type_t *type = type_copy_new(set, &pointer_type);
    type_id = track_type(set, type);

    return type_id;
}

type_id_t type_set_fetch_function_(type_table_t *set, type_id_t return_type, type_ids_t arguments, bool is_native) {
    type_t function_type = {
        .kind = is_native ? TYPE_NATIVE_FUNCTION : TYPE_FUNCTION,
        .data.function.argument_types = arguments,
        .data.function.return_type = return_type,
    };

    type_id_t type_id;
    if (table_get(type2u64, set->types2index, &function_type, &type_id)) {
        return type_id;
    }

    type_t *type = type_copy_new(set, (type_t*)&function_type);
    type_id = track_type(set, type);

    return type_id;
}

type_id_t type_set_fetch_function(type_table_t *set, type_id_t return_type, type_ids_t arguments) {
    return type_set_fetch_function_(set, return_type, arguments, false);
}

type_id_t type_set_fetch_native_function(type_table_t *set, type_id_t return_type, type_ids_t arguments) {
    return type_set_fetch_function_(set, return_type, arguments, true);
}

type_id_t type_set_fetch_anonymous_struct(type_table_t *set, i32 field_count, struct_field_t *fields, i32 constant_count, struct_constant_t* constants) {
    type_t struct_type = {
        .kind = TYPE_STRUCT,
        .data.struct_.field_count = field_count,
        .data.struct_.fields = fields,

        .data.struct_.constant_count = constant_count,
        .data.struct_.constants = constants,

        .data.struct_.total_bytes = 0,
    };

    type_t *type;
    type_id_t type_id;
    if (constant_count == 0) {
        if (table_get(type2u64, set->types2index, &struct_type, &type_id)) {
            return type_id;
        }

        type = type_copy_new(set, &struct_type);
        type_id = track_type(set, type);
    } else {
        // anonymous structs with constants must be unique
        type = type_copy_new(set, &struct_type);
        array_push(&set->types, &struct_type);
        type_id = typeid(set->types.count-1);
    }

    // put the layout in a hash table separate from type
    // it should be impossible for sizing to be infinitely recursive because the types should have resolved
    // properly for this struct type to be created
    if (field_count > 0) {
        type->data.struct_.fields[0].offset = 0;
        for (i32 i = 1; i < field_count; ++i) {
            i32 previous_offset = type->data.struct_.fields[i - 1].offset;
            type_id_t previous_type_id = fields[i - 1].type;
            type_t *previous_type = set->types.items[previous_type_id.i];

            i32 bytes = bytes_to_slots(type_size_bytes(previous_type)) * sizeof(slot_t);
            type->data.struct_.fields[i].offset = previous_offset + bytes;
        }

        type_id_t field_type_id= type->data.struct_.fields[field_count-1].type;
        type_t *field_type = set->types.items[field_type_id.i];
        i32 size_of_final = bytes_to_slots(type_size_bytes(field_type)) * sizeof(slot_t);
        i32 total_size = type->data.struct_.fields[field_count - 1].offset + size_of_final;

        type->data.struct_.total_bytes = total_size;
    }

    return type_id; 
}

type_id_t type_create_struct(type_table_t *set, char *name, i32 name_length, type_t *anonymous_struct) {
    ASSERT(anonymous_struct->kind == TYPE_STRUCT && anonymous_struct->data.struct_.name == NULL, "can only create struct from anonymous struct");

    if (anonymous_struct->data.struct_.constant_count == 0) {
        type_t *new_type = type_copy_new(set, anonymous_struct);
        new_type->data.struct_.name = ALLOC_N(char, name_length + 1);
        memcpy(new_type->data.struct_.name, name, name_length);
        new_type->data.struct_.name[name_length] = '\0';

        array_push(&set->types, new_type);
        type_id_t new_type_id = typeid(set->types.count-1);

        return new_type_id;
    } else {
        // here we know that the anonymous struct is unique because it has constants, and those are never shared
        type_t *new_type = type_copy_new(set, anonymous_struct);

        // anonymous_struct->data.struct_ = new_type->data.struct_;

        // anonymous_struct->data.struct_.name = ALLOC_N(char, name_length + 1);
        // memcpy(anonymous_struct->data.struct_.name, name, name_length);
        // anonymous_struct->data.struct_.name[name_length] = '\0';

        array_push(&set->types, new_type);
        type_id_t new_type_id = typeid(set->types.count-1);

        return new_type_id;
    }
}

type_id_t type_unique_incomplete_struct_type(type_table_t *set) {
    type_t *new_type = struct_type_new(set, NULL, -1, NULL, -1, 0);
    array_push(&set->types, new_type);
    return typeid(set->types.count-1);
}

void named_struct_copy_data_from_completed_struct_type(type_table_t *set, type_id_t incomplete_named_struct_id, type_id_t complete_anonymous_struct_id) {
    type_t *incomplete_named_struct = set->types.items[incomplete_named_struct_id.i];
    type_t *complete_anonymous_struct = set->types.items[complete_anonymous_struct_id.i];

    type_t *copied_type = type_copy_new(NULL, complete_anonymous_struct);

    incomplete_named_struct->data.struct_.field_count = copied_type->data.struct_.field_count;
    incomplete_named_struct->data.struct_.fields = copied_type->data.struct_.fields;

    incomplete_named_struct->data.struct_.constant_count = copied_type->data.struct_.constant_count;
    incomplete_named_struct->data.struct_.constants = copied_type->data.struct_.constants;

    incomplete_named_struct->data.struct_.total_bytes = copied_type->data.struct_.total_bytes;
}
