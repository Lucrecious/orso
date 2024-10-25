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

type_t OrsoTypeEmptyFunction = (type_t) {
    .kind = TYPE_FUNCTION,
    .data.function.argument_types = {0},
    .data.function.return_type = &OrsoTypeVoid,
};

#define ALLOC(TYPE) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE))
#define ALLOC_N(TYPE, N) (TYPE*)arena_alloc(set->allocator, sizeof(TYPE)*N)

type_t *union_type_new(type_set_t *set, types_t types) {
    //ASSERT(count <= UNION_NUM_MAX, "cannot create union type with more than 4 types"); // TODO: Add this in later, need codegen and runtime errors first

    type_t* union_type = ALLOC(type_t);
    union_type->kind = TYPE_UNION;
    union_type->data.union_.types = (types_t){.allocator=set->allocator};
    for (size_t i = 0; i < union_type->data.union_.types.count; ++i) {
        array_push(&union_type->data.union_.types, types.items[i]);
        
    }

    return union_type;
}

type_t *function_type_new(type_set_t *set, types_t arguments, type_t *return_type, bool is_native) {
    type_t *function_type = ALLOC(type_t);
    function_type->kind = is_native ? TYPE_NATIVE_FUNCTION : TYPE_FUNCTION;
    function_type->data.function.argument_types = (types_t){.allocator=set->allocator};
    for (size_t i = 0; i < arguments.count; ++i) {
        array_push(&function_type->data.function.argument_types, arguments.items[i]);
    }

    function_type->data.function.return_type = return_type;

    return function_type;
}

// only anonymous structs can be looked up in the type set
type_t* struct_type_new(type_set_t* set, struct_field_t* fields, i32 field_count, struct_constant_t* constants, i32 constant_count, i32 total_size) {
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

type_t* pointer_type_new(type_set_t* set, type_t* type) {
    type_t* pointer = ALLOC(type_t);
    pointer->kind = TYPE_POINTER;
    pointer->data.pointer.type = type;

    return pointer;
}

type_t* type_copy_new(type_set_t* set, type_t* type) {
    if (TYPE_IS_UNION(type)) {
        return (type_t*)union_type_new(
            set,
            type->data.union_.types
        );
    }

    if (TYPE_IS_FUNCTION(type) || type->kind == TYPE_NATIVE_FUNCTION) {
        return (type_t*)function_type_new(
            set,
            type->data.function.argument_types,
            type->data.function.return_type,
            type->kind == TYPE_NATIVE_FUNCTION
        );
    }

    if (TYPE_IS_STRUCT(type)) {
        return struct_type_new(
            set,
            type->data.struct_.fields,
            type->data.struct_.field_count,
            type->data.struct_.constants,
            type->data.struct_.constant_count,
            type->data.struct_.total_bytes);
    }

    if (TYPE_IS_POINTER(type)) {
        return pointer_type_new(
            set,
            type->data.pointer.type);
    }

    UNREACHABLE();
    return NULL;
}

static bool add_type(type_set_t* set, type_t* type);

void type_set_init(type_set_t* set, arena_t *allocator) {
    set->capacity = 0;
    set->count = 0;
    set->entries = NULL;
    set->allocator = allocator;

    add_type(set, (type_t*)&OrsoTypeEmptyFunction);
}

static u32 hash_type(type_t* type) {
#define ADD_HASH(HASH, APPEND) HASH ^= APPEND; HASH *= 16777619

    u32 hash = 2166136261u;
    ADD_HASH(hash, type->kind);

    if (TYPE_IS_UNION(type)) {
        for (size_t i = 0; i < type->data.union_.types.count; ++i) {
            ADD_HASH(hash, (u64)(type->data.union_.types.items[i]));
        }
    } else if (TYPE_IS_FUNCTION(type)) {
        ADD_HASH(hash, type->data.function.argument_types.count);

        for (size_t i = 0; i < type->data.function.argument_types.count; ++i) {
            ADD_HASH(hash, (u64)(type->data.function.argument_types.items[i]));
        }

        ADD_HASH(hash, (u64)(type->data.function.return_type));
    } else if (TYPE_IS_STRUCT(type)) {
        ASSERT(type->data.struct_.name == NULL, "only anonymous structs are hashed");
        ASSERT(type->data.struct_.constant_count == 0, "only anonymous structs without constants can be hashed");

        ADD_HASH(hash, type->data.struct_.field_count);

        for (i32 i = 0; i < type->data.struct_.field_count; i++) {
            char* name = type->data.struct_.fields[i].name;
            i32 length = strlen(name);
            for (i32 i = 0; i < length; i++) {
                ADD_HASH(hash, name[i]);
            }
            
            ADD_HASH(hash, (u64)(type->data.struct_.fields[i].type));
        }
    } else if (TYPE_IS_POINTER(type)) {
        ADD_HASH(hash, (u64)(type->data.pointer.type));
    }

    return hash;

#undef ADD_HASH
}

static type_t** fetch_type(type_t** types, i32 capacity, type_t* type) {
    u32 hash = hash_type(type);
    u32 index = hash & (capacity - 1);

    for (;;) {
        type_t** entry = &types[index];
        if (*entry == NULL) {
            break;
        }

        if (orso_type_equal(*entry, type)) {
            return entry;
        }

        index = (index + 1) & (capacity - 1);
    }

    return &types[index];
}

static void adjust_capacity(type_set_t* set, i32 capacity) {
    type_t** entries = ALLOC_N(type_t*, capacity);

    for (i32 i = 0; i < capacity; i++) {
        entries[i] = NULL;
    }

    set->count = 0;
    for (i32 i = 0; i < set->capacity; i++) {
        type_t** entry = &set->entries[i];
        if (*entry == NULL) {
            continue;
        }

        type_t** destination = fetch_type(entries, capacity, *entry);
        *destination = *entry;
        set->count++;
    }

    set->entries = entries;
    set->capacity = capacity;
}

static bool add_type(type_set_t* set, type_t* type) {
    if (set->count + 1 > set->capacity * ORSO_TYPE_SET_MAX_LOAD) {
        i32 capacity = GROW_CAPACITY(set->capacity);
        adjust_capacity(set, capacity);
    }

    type_t** entry = fetch_type(set->entries, set->capacity, type);
    bool is_new_type = (*entry == NULL);

    if (!is_new_type) {
        return false;
    }

    set->count++;

    *entry = type;

    return true;
}

static i32 type_compare(const void* a, const void* b) {
    type_t* type_a = *((type_t**)a);
    type_t* type_b = *((type_t**)b);

    if (type_a == type_b) {
        return 0;
    }

    if (TYPE_IS_UNION(type_a) || TYPE_IS_UNION(type_b)) {
        if (!TYPE_IS_UNION(type_a)) {
            return -type_compare(&type_b, &type_a);
        }

        // non union types have higher priority than any other type
        if (!TYPE_IS_UNION(type_b)) {
            return -1;
        }

        if (type_a->data.union_.types.count != type_b->data.union_.types.count) {
            return type_b->data.union_.types.count - type_a->data.union_.types.count;
        }

        for (size_t i = 0; i < type_a->data.union_.types.count; ++i) {
            type_t *single_a = type_a->data.union_.types.items[i];
            type_t *single_b = type_b->data.union_.types.items[i];

            i32 result = type_compare(single_a, single_b);
            if (result == 0) {
                continue;
            }

            return result;
        }

        return 0;
    }

    if (TYPE_IS_POINTER(type_a) || TYPE_IS_POINTER(type_b)) {
        unless (TYPE_IS_POINTER(type_a)) {
            return -type_compare(type_b, type_a);
        }

        unless (TYPE_IS_POINTER(type_b)) {
            return -1;
        }
        
        return type_compare(type_a->data.pointer.type, type_b->data.pointer.type);
    }

    if (TYPE_IS_FUNCTION(type_a) || TYPE_IS_FUNCTION(type_b)) {
        if (!TYPE_IS_FUNCTION(type_a)) {
            return -type_compare(&type_b, &type_a);
        }

        // Primitive types have higher priority
        if (!TYPE_IS_FUNCTION(type_b)) {
            return -1;
        }

        if (type_a->data.function.return_type != type_b->data.function.return_type) {
            return type_compare(type_a->data.function.return_type, type_b->data.function.return_type);
        }

        if (type_a->data.function.argument_types.count != type_b->data.function.argument_types.count) {
            return type_b->data.function.argument_types.count - type_a->data.function.argument_types.count;
        }

        for (size_t i = 0; i < type_a->data.function.argument_types.count; ++i) {
            type_t *argument_type_a = type_a->data.function.argument_types.items[i];
            type_t *argument_type_b = type_b->data.function.argument_types.items[i];
            i32 result = type_compare(argument_type_a, argument_type_b);
            if (result == 0) {
                continue;
            }

            return result;
        }

        return 0;
    }

    if (TYPE_IS_STRUCT(type_a) || TYPE_IS_STRUCT(type_b)) {
        if (!TYPE_IS_STRUCT(type_a)) {
            return -type_compare(&type_b, &type_a);
        }

        if (!TYPE_IS_STRUCT(type_b)) {
            return -1;
        }

        // we want the names in a certain order so its easier to compare
        if (type_a->data.struct_.name == NULL && type_b->data.struct_.name != NULL) {
            return -type_compare(&type_b, &type_a);
        }

        // named structs go before anonymous ones
        if (type_a->data.struct_.name != NULL && type_b->data.struct_.name == NULL) {
            return -1;
        }

        // if both are named, then its alphabetical order
        if (type_a->data.struct_.name != NULL && type_b->data.struct_.name != NULL) {
            return strcmp(type_a->data.struct_.name, type_b->data.struct_.name);
        }

        // at this point we are comparing two different anonymous structs

        i32 a_constant_count = type_a->data.struct_.constant_count;
        i32 b_constant_count = type_b->data.struct_.constant_count;
        // structs with more constants before structs with less
        if (a_constant_count != b_constant_count) {
            return a_constant_count < b_constant_count ? 1 : -1;
        }

        // sort by constant type then
        for (i32 i = 0; i < a_constant_count; i++) {
            type_t* a_constant_type = type_a->data.struct_.constants[i].type;
            type_t* b_constant_type = type_b->data.struct_.constants[i].type;
            if (a_constant_type != b_constant_type) {
                return type_compare(a_constant_type, b_constant_type);
            }
        }

        // sort alphabetically by constant then
        for (i32 i = 0; i < a_constant_count; i++) {
            char* name_a = type_a->data.struct_.constants[i].name;
            char* name_b = type_b->data.struct_.constants[i].name;

            i32 result = strcmp(name_a, name_b);
            if (result != 0) {
                return result;
            }
        }

        // TODO: if names for constants match, I need to check their values and compare them
        // I'm think I do this once I write a universal hash function for any/all types...

        // smaller field counts before larger ones
        i32 a_field_count = type_a->data.struct_.field_count;
        i32 b_field_count = type_b->data.struct_.field_count;
        if (a_field_count != b_field_count) {
            return a_field_count < b_field_count ? -1 : 1;
        }


        // use type comparison if the field count is the same
        for (i32 i = 0; i < a_field_count; i++) {
            type_t* a_field_type = type_a->data.struct_.fields[i].type;
            type_t* b_field_type = type_b->data.struct_.fields[i].type;
            if (a_field_type != b_field_type) {
                return type_compare(a_field_type, b_field_type);
            }
        }

        // if the types are the same then the field names should be different
        for (i32 i = 0; i < a_field_count; i++) {
            char* name_a = type_a->data.struct_.fields[i].name;
            char* name_b = type_b->data.struct_.fields[i].name;

            i32 result = strcmp(name_a, name_b);
            if (result != 0) {
                return result;
            }
        }

        // it should be impossible for two unique anonymous structs types to be exactly the same,
        // since otherwise they are the same type
        // this is different for named structs, since only the name is considered when ordering those
        UNREACHABLE();
        
        return 0;
    }

    return ((i32)type_b->kind) - ((i32)type_a->kind);
}

type_t *type_set_fetch_union(type_set_t *set, types_t types) {
    type_t union_type = {
        .kind = TYPE_UNION,
        .data.union_.types = {.allocator=set->allocator},
    };

    for (size_t i = 0; i < types.count; i++) {
        array_push(&union_type.data.union_.types, types.items[i]);
    }

    qsort(union_type.data.union_.types.items, types.count, sizeof(type_t*), type_compare);

    if (set->capacity > 0) {
        type_t** entry = fetch_type(set->entries, set->capacity, &union_type);

        if (*entry != NULL) {
            return *entry;
        }
    }

    type_t* type = type_copy_new(set, &union_type);
    add_type(set, type);

    return type;
}

type_t* type_set_fetch_pointer(type_set_t* set, type_t* inner_type) {
    type_t pointer_type = {
        .kind = TYPE_POINTER,
        .data.pointer.type = inner_type,
    };

    if (set->capacity > 0) {
        type_t** entry = fetch_type(set->entries, set->capacity, &pointer_type);

        if (*entry != NULL) {
            return *entry;
        }
    }

    type_t* type = type_copy_new(set, &pointer_type);
    add_type(set, type);

    return type;
}

type_t *orso_type_set_fetch_function_(type_set_t *set, type_t *return_type, types_t arguments, bool is_native) {
    type_t function_type = {
        .kind = is_native ? TYPE_NATIVE_FUNCTION : TYPE_FUNCTION,
        .data.function.argument_types = arguments,
        .data.function.return_type = return_type,
    };

    if (set->capacity > 0) {
        type_t** entry = fetch_type(set->entries, set->capacity, &function_type);

        if (*entry != NULL) {
            return *entry;
        }
    }


    type_t* type = type_copy_new(set, (type_t*)&function_type);
    add_type(set, type);

    return type;
}

type_t *type_set_fetch_function(type_set_t *set, type_t *return_type, types_t arguments) {
    return orso_type_set_fetch_function_(set, return_type, arguments, false);
}

type_t *type_set_fetch_native_function(type_set_t *set, type_t *return_type, types_t arguments) {
    return orso_type_set_fetch_function_(set, return_type, arguments, true);
}

type_t* type_set_fetch_anonymous_struct(type_set_t *set, i32 field_count, struct_field_t *fields, i32 constant_count, struct_constant_t* constants) {
    type_t struct_type = {
        .kind = TYPE_STRUCT,
        .data.struct_.field_count = field_count,
        .data.struct_.fields = fields,

        .data.struct_.constant_count = constant_count,
        .data.struct_.constants = constants,

        .data.struct_.total_bytes = 0,
    };

    type_t* type;
    if (constant_count == 0) {
        if (set->capacity > 0) {
            type_t** entry = fetch_type(set->entries, set->capacity, &struct_type);

            if (*entry != NULL) {
                return *entry;
            }
        }

        type = type_copy_new(set, &struct_type);
        add_type(set, type);
    } else {
        // anonymous structs with constants must be unique
        type = type_copy_new(set, &struct_type);
    }

    // put the layout in a hash table separate from type
    // it should be impossible for sizing to be infinitely recursive because the types should have resolved
    // properly for this struct type to be created
    if (field_count > 0) {
        type->data.struct_.fields[0].offset = 0;
        for (i32 i = 1; i < field_count; i++) {
            i32 previous_offset = type->data.struct_.fields[i - 1].offset;
            type_t* previous_type = fields[i - 1].type;

            i32 bytes = orso_bytes_to_slots(orso_type_size_bytes(previous_type)) * sizeof(slot_t);
            type->data.struct_.fields[i].offset = previous_offset + bytes;
        }

        i32 size_of_final = orso_bytes_to_slots(orso_type_size_bytes(type->data.struct_.fields[field_count - 1].type)) * sizeof(slot_t);
        i32 total_size = type->data.struct_.fields[field_count - 1].offset + size_of_final;

        type->data.struct_.total_bytes = total_size;
    }

    return type;
}

type_t* type_create_struct(type_set_t* set, char* name, i32 name_length, type_t* anonymous_struct) {
    ASSERT(TYPE_IS_STRUCT(anonymous_struct) && anonymous_struct->data.struct_.name == NULL, "can only create struct from anonymous struct");

    if (anonymous_struct->data.struct_.constant_count == 0) {
        type_t* new_type = type_copy_new(set, anonymous_struct);
        new_type->data.struct_.name = ALLOC_N(char, name_length + 1);
        memcpy(new_type->data.struct_.name, name, name_length);
        new_type->data.struct_.name[name_length] = '\0';

        return new_type;
    } else {
        // here we know that the anonymous struct is unique because it has constants, and those are never shared
        type_t* new_type = type_copy_new(NULL, anonymous_struct);

        anonymous_struct->data.struct_ = new_type->data.struct_;

        anonymous_struct->data.struct_.name = ALLOC_N(char, name_length + 1);
        memcpy(anonymous_struct->data.struct_.name, name, name_length);
        anonymous_struct->data.struct_.name[name_length] = '\0';

        return anonymous_struct;
    }
}

type_t* type_unique_incomplete_struct_type(type_set_t* set) {
    type_t* new_type = struct_type_new(set, NULL, -1, NULL, -1, 0);
    return new_type;
}

void named_struct_copy_data_from_completed_struct_type(type_t* incomplete_named_struct, type_t* complete_anonymous_struct) {
    type_t* copied_type = type_copy_new(NULL, complete_anonymous_struct);

    incomplete_named_struct->data.struct_.field_count = copied_type->data.struct_.field_count;
    incomplete_named_struct->data.struct_.fields = copied_type->data.struct_.fields;

    incomplete_named_struct->data.struct_.constant_count = copied_type->data.struct_.constant_count;
    incomplete_named_struct->data.struct_.constants = copied_type->data.struct_.constants;

    incomplete_named_struct->data.struct_.total_bytes = copied_type->data.struct_.total_bytes;
}
