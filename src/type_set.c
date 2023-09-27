#include "type_set.h"

#include "sb.h"
#include <stdlib.h>
#include "slot.h"

#define ORSO_TYPE_SET_MAX_LOAD 0.75
#define GROW_CAPACITY(capacity) capacity == 0 ? 8 : capacity * 2

OrsoType OrsoTypeVoid = (OrsoType) { .kind = ORSO_TYPE_VOID };
OrsoType OrsoTypeBool = (OrsoType) { .kind = ORSO_TYPE_BOOL };
OrsoType OrsoTypeInteger32 = (OrsoType) { .kind = ORSO_TYPE_INT32 };
OrsoType OrsoTypeInteger64 = (OrsoType) { .kind = ORSO_TYPE_INT64 };
OrsoType OrsoTypeFloat32 = (OrsoType){ .kind = ORSO_TYPE_FLOAT32 };
OrsoType OrsoTypeFloat64 = (OrsoType){ .kind = ORSO_TYPE_FLOAT64 };
OrsoType OrsoTypeString = (OrsoType) { .kind = ORSO_TYPE_STRING };
OrsoType OrsoTypeSymbol = (OrsoType) { .kind = ORSO_TYPE_SYMBOL };
OrsoType OrsoTypeType = (OrsoType) { .kind = ORSO_TYPE_TYPE };
OrsoType OrsoTypeInvalid = (OrsoType){ .kind = ORSO_TYPE_INVALID  };
OrsoType OrsoTypeUnresolved = (OrsoType) { .kind = ORSO_TYPE_UNRESOLVED };
OrsoType OrsoTypeUndefined = (OrsoType) { .kind = ORSO_TYPE_UNDEFINED };

OrsoType OrsoTypeIncompleteStruct = (OrsoType) {
    .kind = ORSO_TYPE_STRUCT,
    .type.struct_.name = NULL,
    .type.struct_.field_count = 0,
    .type.struct_.field_names = NULL,
    .type.struct_.field_types = NULL,
};

OrsoType OrsoTypeEmptyFunction = (OrsoType) {
    .kind = ORSO_TYPE_FUNCTION,
    .type.function.argument_count = 0,
    .type.function.argument_types = NULL,
    .type.function.return_type = &OrsoTypeVoid,
};

OrsoType* union_type_new(OrsoTypeSet* set, OrsoType** types, i32 count) {
    //ASSERT(count <= ORSO_UNION_NUM_MAX, "cannot create union type with more than 4 types"); // TODO: Add this in later, need codegen and runtime errors first

    OrsoType* union_type = ORSO_ALLOCATE(OrsoType);
    union_type->kind = ORSO_TYPE_UNION;
    union_type->type.union_.count = count;
    union_type->type.union_.types = ORSO_ALLOCATE_N(OrsoType*, count);
    for (i32 i = 0; i < union_type->type.union_.count; i++) {
        union_type->type.union_.types[i] = types[i];
    }

    sb_push(set->heap, (OrsoType*)union_type);

    return union_type;
}

OrsoType* function_type_new(OrsoTypeSet* set, OrsoType** arguments, i32 argument_count, OrsoType* return_type, bool is_native) {
    OrsoType* function_type = ORSO_ALLOCATE(OrsoType);
    function_type->kind = is_native ? ORSO_TYPE_NATIVE_FUNCTION : ORSO_TYPE_FUNCTION;
    function_type->type.function.argument_count = argument_count;
    function_type->type.function.argument_types = ORSO_ALLOCATE_N(OrsoType*, argument_count);
    for (i32 i = 0; i < argument_count; i++) {
        function_type->type.function.argument_types[i] = arguments[i];
    }

    function_type->type.function.return_type = return_type;

    sb_push(set->heap, function_type);

    return function_type;
}

// only anonymous structs can be looked up in the type set
OrsoType* struct_type_new(OrsoTypeSet* set, char** field_names, OrsoType** field_types, i32 field_count, i32* field_offsets, i32 total_size) {
    ASSERT(field_names != NULL && field_types != NULL && field_count > 0, "cannot create a anonymous struct with no fields");

    OrsoType* struct_type = ORSO_ALLOCATE(OrsoType);
    struct_type->kind = ORSO_TYPE_STRUCT;

    struct_type->type.struct_.name = NULL;

    struct_type->type.struct_.field_count = field_count;
    struct_type->type.struct_.field_names = ORSO_ALLOCATE_N(char*, field_count);
    struct_type->type.struct_.field_types = ORSO_ALLOCATE_N(OrsoType*, field_count);

    struct_type->type.struct_.field_byte_offsets = NULL;
    struct_type->type.struct_.total_size = total_size;

    for (i32 i = 0; i < field_count; i++) {
        struct_type->type.struct_.field_types[i] = field_types[i];
    }

    for (i32 i = 0; i < field_count; i++) {
        i32 length = strlen(field_names[i]);
        char* name = ORSO_ALLOCATE_N(char, length + 1);
        memcpy(name, field_names[i], length);
        name[length] = '\0';

        struct_type->type.struct_.field_names[i] = name;
    }

    if (field_offsets) {
        struct_type->type.struct_.field_byte_offsets = ORSO_ALLOCATE_N(i32, field_count);
        for (i32 i = 0; i < field_count; i++) {
            struct_type->type.struct_.field_byte_offsets[i] = field_offsets[i];
        }
    }

    sb_push(set->heap, struct_type);

    return struct_type;
}

OrsoType* type_copy_new(OrsoTypeSet* set, OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return (OrsoType*)union_type_new(
            set,
            (OrsoType**)type->type.union_.types,
            type->type.union_.count
        );
    }

    if (ORSO_TYPE_IS_FUNCTION(type) || type->kind == ORSO_TYPE_NATIVE_FUNCTION) {
        return (OrsoType*)function_type_new(
            set,
            type->type.function.argument_types,
            type->type.function.argument_count,
            type->type.function.return_type,
            type->kind == ORSO_TYPE_NATIVE_FUNCTION
        );
    }

    if (ORSO_TYPE_IS_STRUCT(type)) {
        return struct_type_new(
            set,
            type->type.struct_.field_names,
            type->type.struct_.field_types,
            type->type.struct_.field_count,
            type->type.struct_.field_byte_offsets,
            type->type.struct_.total_size);
    }

    UNREACHABLE();
    return NULL;
}

void type_free(OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        free(type->type.union_.types);
    } else if (ORSO_TYPE_IS_FUNCTION(type)) {
        for (i32 i = 0; i < type->type.function.argument_count; i++) {
            type->type.function.argument_types[i] = NULL;
        }

        free((void**)type->type.function.argument_types);
    } else if (ORSO_TYPE_IS_STRUCT(type)) {
        for (i32 i = 0; i < type->type.struct_.field_count; i++) {
            free(type->type.struct_.field_names[i]);
        }

        free(type->type.struct_.field_names);
        type->type.struct_.field_names = NULL;

        free(type->type.struct_.field_types);
        type->type.struct_.field_types = NULL;

        free(type->type.struct_.name);
        type->type.struct_.name = NULL;

        free(type->type.struct_.field_byte_offsets);
        type->type.struct_.field_byte_offsets = NULL;

        type->type.struct_.total_size = 0;
    }
}

static bool add_type(OrsoTypeSet* set, OrsoType* type);

void orso_type_set_init(OrsoTypeSet* set) {
    set->capacity = 0;
    set->count = 0;
    set->entries = NULL;
    set->heap = NULL;

    add_type(set, (OrsoType*)&OrsoTypeEmptyFunction);
}

void orso_type_set_free(OrsoTypeSet* set) {
    for (i32 i = 0; i < sb_count(set->heap); i++) {
        OrsoType* type = set->heap[i];
        type_free(type);
        free(type);
        set->heap[i] = NULL;
    }

    sb_free(set->heap);

    free(set->entries);

    set->capacity = 0;
    set->count = 0;
    set->entries = NULL;
    set->heap = NULL;
}

static u32 hash_type(OrsoType* type) {
#define ADD_HASH(HASH, APPEND) HASH ^= APPEND; HASH *= 16777619

    u32 hash = 2166136261u;
    ADD_HASH(hash, type->kind);

    if (ORSO_TYPE_IS_UNION(type)) {
        for (i32 i = 0; i < type->type.union_.count; i++) {
            ADD_HASH(hash, hash_type(type->type.union_.types[i]));
        }
    } else if (ORSO_TYPE_IS_FUNCTION(type)) {
        ADD_HASH(hash, type->type.function.argument_count);

        for (i32 i = 0; i < type->type.function.argument_count; i++) {
            ADD_HASH(hash, hash_type(type->type.function.argument_types[i]));
        }

        ADD_HASH(hash, hash_type(type->type.function.return_type));
    } else if (ORSO_TYPE_IS_STRUCT(type)) {
        ADD_HASH(hash, type->type.struct_.field_count);

        for (i32 i = 0; i < type->type.struct_.field_count; i++) {
            char* name = type->type.struct_.field_names[i];
            i32 length = strlen(name);
            for (i32 i = 0; i < length; i++) {
                ADD_HASH(hash, name[i]);
            }
            
            ADD_HASH(hash, hash_type(type->type.struct_.field_types[i]));
        }
    }

    return hash;

#undef ADD_HASH
}

static OrsoType** fetch_type(OrsoType** types, i32 capacity, OrsoType* type) {
    u32 hash = hash_type(type);
    u32 index = hash & (capacity - 1);

    for (;;) {
        OrsoType** entry = &types[index];
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

static void adjust_capacity(OrsoTypeSet* set, i32 capacity) {
    OrsoType** entries = ORSO_ALLOCATE_N(OrsoType*, capacity);

    for (i32 i = 0; i < capacity; i++) {
        entries[i] = NULL;
    }

    set->count = 0;
    for (i32 i = 0; i < set->capacity; i++) {
        OrsoType** entry = &set->entries[i];
        if (*entry == NULL) {
            continue;
        }

        OrsoType** destination = fetch_type(entries, capacity, *entry);
        *destination = *entry;
        set->count++;
    }

    free(set->entries);
    set->entries = entries;
    set->capacity = capacity;
}

static bool add_type(OrsoTypeSet* set, OrsoType* type) {
    if (set->count + 1 > set->capacity * ORSO_TYPE_SET_MAX_LOAD) {
        i32 capacity = GROW_CAPACITY(set->capacity);
        adjust_capacity(set, capacity);
    }

    OrsoType** entry = fetch_type(set->entries, set->capacity, type);
    bool is_new_type = (*entry == NULL);

    if (!is_new_type) {
        return false;
    }

    set->count++;

    *entry = type;

    return true;
}

static i32 type_compare(const void* a, const void* b) {
    OrsoType* type_a = *((OrsoType**)a);
    OrsoType* type_b = *((OrsoType**)b);

    if (type_a == type_b) {
        return 0;
    }

    if (ORSO_TYPE_IS_UNION(type_a) || ORSO_TYPE_IS_UNION(type_b)) {
        if (!ORSO_TYPE_IS_UNION(type_a)) {
            return -type_compare(&type_b, &type_a);
        }

        // non union types have higher priority than any other type
        if (!ORSO_TYPE_IS_UNION(type_b)) {
            return -1;
        }

        if (type_a->type.union_.count != type_b->type.union_.count) {
            return type_b->type.union_.count - type_a->type.union_.count;
        }

        for (i32 i = 0; i < type_a->type.union_.count; i++) {
            OrsoType* single_a = type_a->type.union_.types[i];
            OrsoType* single_b = type_b->type.union_.types[i];

            i32 result = type_compare(single_a, single_b);
            if (result == 0) {
                continue;
            }

            return result;
        }

        return 0;
    }

    if (ORSO_TYPE_IS_FUNCTION(type_a) || ORSO_TYPE_IS_FUNCTION(type_b)) {
        if (!ORSO_TYPE_IS_FUNCTION(type_a)) {
            return -type_compare(&type_b, &type_a);
        }

        // Primitive types have higher priority
        if (!ORSO_TYPE_IS_FUNCTION(type_b)) {
            return -1;
        }

        if (type_a->type.function.return_type != type_b->type.function.return_type) {
            return type_compare(type_a->type.function.return_type, type_b->type.function.return_type);
        }

        if (type_a->type.function.argument_count != type_b->type.function.argument_count) {
            return type_b->type.function.argument_count - type_a->type.function.argument_count;
        }

        for (i32 i = 0; i < type_a->type.function.argument_count; i++) {
            OrsoType* argument_type_a = type_a->type.function.argument_types[i];
            OrsoType* argument_type_b = type_b->type.function.argument_types[i];
            i32 result = type_compare(argument_type_a, argument_type_b);
            if (result == 0) {
                continue;
            }

            return result;
        }

        return 0;
    }

    if (ORSO_TYPE_IS_STRUCT(type_a) || ORSO_TYPE_IS_STRUCT(type_b)) {
        if (!ORSO_TYPE_IS_STRUCT(type_a)) {
            return -type_compare(&type_b, &type_a);
        }

        if (!ORSO_TYPE_IS_STRUCT(type_b)) {
            return -1;
        }

        // we want the names in a certain order so its easier to compare
        if (type_a->type.struct_.name == NULL && type_b->type.struct_.name != NULL) {
            return -type_compare(&type_b, &type_a);
        }

        // named structs go before anonymous ones
        if (type_a->type.struct_.name != NULL && type_b->type.struct_.name == NULL) {
            return -1;
        }

        // if both are named, then its alphabetical order
        if (type_a->type.struct_.name != NULL && type_b->type.struct_.name != NULL) {
            return strcmp(type_a->type.struct_.name, type_b->type.struct_.name);
        }

        // at this point we are comparing two different anonymous structs

        // smaller field counts before larger ones
        i32 a_field_count = type_a->type.struct_.field_count;
        i32 b_field_count = type_b->type.struct_.field_count;
        if (a_field_count != b_field_count) {
            return a_field_count < b_field_count ? -1 : 1;
        }


        // use type comparison if the field count is the same
        for (i32 i = 0; i < a_field_count; i++) {
            OrsoType* a_field_type = type_a->type.struct_.field_types[i];
            OrsoType* b_field_type = type_b->type.struct_.field_types[i];
            if (a_field_type != b_field_type) {
                return type_compare(a_field_type, b_field_type);
            }
        }

        // if the types are the same then the field names should be different
        for (i32 i = 0; i < a_field_count; i++) {
            char* name_a = type_a->type.struct_.field_names[i];
            char* name_b = type_b->type.struct_.field_names[i];

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

OrsoType* orso_type_set_fetch_union(OrsoTypeSet* set, OrsoType** types, i32 count) {
    OrsoType union_type = {
        .kind = ORSO_TYPE_UNION,
        .type.union_.count = count,
        .type.union_.types = ORSO_ALLOCATE_N(OrsoType*, count)
    };

    for (i32 i = 0; i < count; i++) {
        if (i < count) {
            union_type.type.union_.types[i] = types[i];
        } else {
            union_type.type.union_.types[i] = NULL;
        }
    }

    qsort(union_type.type.union_.types, count, sizeof(OrsoType*), type_compare);

    if (set->capacity > 0) {
        OrsoType** entry = fetch_type(set->entries, set->capacity, (OrsoType*)&union_type);

        if (*entry != NULL) {
            return *entry;
        }
    }

    OrsoType* type = type_copy_new(set, (OrsoType*)&union_type);
    add_type(set, type);

    return type;
}

OrsoType* orso_type_set_fetch_function_(OrsoTypeSet* set, OrsoType* return_type, OrsoType** arguments, i32 argument_count, bool is_native) {
    OrsoType function_type = {
        .kind = is_native ? ORSO_TYPE_NATIVE_FUNCTION : ORSO_TYPE_FUNCTION,
        .type.function.argument_types = arguments,
        .type.function.argument_count = argument_count,
        .type.function.return_type = return_type,
    };

    if (set->capacity > 0) {
        OrsoType** entry = fetch_type(set->entries, set->capacity, &function_type);

        if (*entry != NULL) {
            return *entry;
        }
    }


    OrsoType* type = type_copy_new(set, (OrsoType*)&function_type);
    add_type(set, type);

    return type;
}

OrsoType* orso_type_set_fetch_function(OrsoTypeSet* set, OrsoType* return_type, OrsoType** arguments, i32 argument_count) {
    return orso_type_set_fetch_function_(set, return_type, arguments, argument_count, false);
}

OrsoType* orso_type_set_fetch_native_function(OrsoTypeSet* set, OrsoType* return_type, OrsoType** arguments, i32 argument_count) {
    return orso_type_set_fetch_function_(set, return_type, arguments, argument_count, true);
}

OrsoType* orso_type_set_fetch_anonymous_struct(OrsoTypeSet* set, i32 field_count, char** names, OrsoType** types) {
    OrsoType struct_type = {
        .kind = ORSO_TYPE_STRUCT,
        .type.struct_.field_count = field_count,
        .type.struct_.field_names = names,
        .type.struct_.field_types = types,

        .type.struct_.field_byte_offsets = NULL,
        .type.struct_.total_size = 0,
    };

    if (set->capacity > 0) {
        OrsoType** entry = fetch_type(set->entries, set->capacity, &struct_type);

        if (*entry != NULL) {
            return *entry;
        }
    }

    OrsoType* type = type_copy_new(set, &struct_type);
    add_type(set, type);

    // put the layout in a hash table separate from type
    // it should be impossible for sizing to be infinitely recursive because the types should have resolved
    // properly for this struct type to be created
    {
        i32* offsets = ORSO_ALLOCATE_N(i32, field_count);
        offsets[0] = 0;
        for (i32 i = 1; i < field_count; i++) {
            i32 previous_offset = offsets[i - 1];
            OrsoType* previous_type = types[i - 1];

            i32 bytes = orso_bytes_to_slots(orso_type_size_bytes(previous_type)) * ORSO_SLOT_SIZE_BYTES;
            offsets[i] = previous_offset + bytes;
        }

        i32 size_of_final = orso_bytes_to_slots(orso_type_size_bytes(types[field_count - 1])) * ORSO_SLOT_SIZE_BYTES;
        i32 total_size = offsets[field_count - 1] + size_of_final;

        type->type.struct_.field_byte_offsets = offsets;
        type->type.struct_.total_size = total_size;
    }

    return type;
}

OrsoType* orso_type_create_struct(OrsoTypeSet* set, char* name, i32 name_length, OrsoType* anonymous_struct) {
    ASSERT(ORSO_TYPE_IS_STRUCT(anonymous_struct) && anonymous_struct->type.struct_.name == NULL, "can only create struct from anonymous struct");

    OrsoType* new_type = type_copy_new(set, anonymous_struct);
    new_type->type.struct_.name = ORSO_ALLOCATE_N(char, name_length + 1);
    memcpy(new_type->type.struct_.name, name, name_length);
    new_type->type.struct_.name[name_length] = '\0';

    return new_type;
}

bool is_struct_incomplete(OrsoType* struct_) {
    return struct_->kind == ORSO_TYPE_STRUCT && struct_->type.struct_.field_count == 0;
}
