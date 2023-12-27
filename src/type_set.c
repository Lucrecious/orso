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

OrsoType OrsoTypeEmptyFunction = (OrsoType) {
    .kind = ORSO_TYPE_FUNCTION,
    .data.function.argument_count = 0,
    .data.function.argument_types = NULL,
    .data.function.return_type = &OrsoTypeVoid,
};

OrsoType* union_type_new(OrsoTypeSet* set, OrsoType** types, i32 count) {
    //ASSERT(count <= ORSO_UNION_NUM_MAX, "cannot create union type with more than 4 types"); // TODO: Add this in later, need codegen and runtime errors first

    OrsoType* union_type = ORSO_ALLOCATE(OrsoType);
    union_type->kind = ORSO_TYPE_UNION;
    union_type->data.union_.count = count;
    union_type->data.union_.types = ORSO_ALLOCATE_N(OrsoType*, count);
    for (i32 i = 0; i < union_type->data.union_.count; i++) {
        union_type->data.union_.types[i] = types[i];
    }

    sb_push(set->heap, (OrsoType*)union_type);

    return union_type;
}

OrsoType* function_type_new(OrsoTypeSet* set, OrsoType** arguments, i32 argument_count, OrsoType* return_type, bool is_native) {
    OrsoType* function_type = ORSO_ALLOCATE(OrsoType);
    function_type->kind = is_native ? ORSO_TYPE_NATIVE_FUNCTION : ORSO_TYPE_FUNCTION;
    function_type->data.function.argument_count = argument_count;
    function_type->data.function.argument_types = ORSO_ALLOCATE_N(OrsoType*, argument_count);
    for (i32 i = 0; i < argument_count; i++) {
        function_type->data.function.argument_types[i] = arguments[i];
    }

    function_type->data.function.return_type = return_type;

    sb_push(set->heap, function_type);

    return function_type;
}

// only anonymous structs can be looked up in the type set
OrsoType* struct_type_new(OrsoTypeSet* set, OrsoStructField* fields, i32 field_count, OrsoStructConstant* constants, i32 constant_count, i32 total_size) {
    OrsoType* struct_type = ORSO_ALLOCATE(OrsoType);
    struct_type->kind = ORSO_TYPE_STRUCT;

    struct_type->data.struct_.name = NULL;

    struct_type->data.struct_.field_count = field_count;
    struct_type->data.struct_.fields = NULL;
    struct_type->data.struct_.constant_count = constant_count;
    struct_type->data.struct_.constants = NULL;
    struct_type->data.struct_.total_bytes = total_size;

    if (field_count > 0) {
        struct_type->data.struct_.fields = ORSO_ALLOCATE_N(OrsoStructField, field_count);

        for (i32 i = 0; i < field_count; i++) {
            struct_type->data.struct_.fields[i] = fields[i];

            i32 length = strlen(fields[i].name);
            char* name = ORSO_ALLOCATE_N(char, length + 1);
            memcpy(name, fields[i].name, length);
            name[length] = '\0';

            struct_type->data.struct_.fields[i].name = name;
        }
    }

    if (constant_count > 0) {
        struct_type->data.struct_.constants = ORSO_ALLOCATE_N(OrsoStructConstant, constant_count);

        for (i32 i = 0; i < constant_count; i++) {
            struct_type->data.struct_.constants[i] = constants[i];

            i32 length = strlen(constants[i].name);
            char* name = ORSO_ALLOCATE_N(char, length + 1);
            memcpy(name, constants[i].name, length);
            name[length] = '\0';

            struct_type->data.struct_.constants[i].name = name;
        }
    }

    if (set) {
        sb_push(set->heap, struct_type);
    }

    return struct_type;
}

OrsoType* pointer_type_new(OrsoTypeSet* set, OrsoType* type) {
    OrsoType* pointer = ORSO_ALLOCATE(OrsoType);
    pointer->kind = ORSO_TYPE_POINTER;
    pointer->data.pointer.type = type;
    
    sb_push(set->heap, pointer);

    return pointer;
}

OrsoType* type_copy_new(OrsoTypeSet* set, OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return (OrsoType*)union_type_new(
            set,
            (OrsoType**)type->data.union_.types,
            type->data.union_.count
        );
    }

    if (ORSO_TYPE_IS_FUNCTION(type) || type->kind == ORSO_TYPE_NATIVE_FUNCTION) {
        return (OrsoType*)function_type_new(
            set,
            type->data.function.argument_types,
            type->data.function.argument_count,
            type->data.function.return_type,
            type->kind == ORSO_TYPE_NATIVE_FUNCTION
        );
    }

    if (ORSO_TYPE_IS_STRUCT(type)) {
        return struct_type_new(
            set,
            type->data.struct_.fields,
            type->data.struct_.field_count,
            type->data.struct_.constants,
            type->data.struct_.constant_count,
            type->data.struct_.total_bytes);
    }

    if (ORSO_TYPE_IS_POINTER(type)) {
        return pointer_type_new(
            set,
            type->data.pointer.type);
    }

    UNREACHABLE();
    return NULL;
}

void type_free(OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        free(type->data.union_.types);
    } else if (ORSO_TYPE_IS_FUNCTION(type)) {
        for (i32 i = 0; i < type->data.function.argument_count; i++) {
            type->data.function.argument_types[i] = NULL;
        }

        free((void**)type->data.function.argument_types);
    } else if (ORSO_TYPE_IS_STRUCT(type)) {
        for (i32 i = 0; i < type->data.struct_.field_count; i++) {
            free(type->data.struct_.fields[i].name);
        }

        free(type->data.struct_.fields);
        type->data.struct_.fields = NULL;

        for (i32 i = 0; i < type->data.struct_.constant_count; i++) {
            free(type->data.struct_.constants[i].name);
        }

        free(type->data.struct_.constants);
        type->data.struct_.constants = NULL;

        free(type->data.struct_.name);
        type->data.struct_.name = NULL;

        type->data.struct_.total_bytes = 0;
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
        for (i32 i = 0; i < type->data.union_.count; i++) {
            ADD_HASH(hash, (u64)(type->data.union_.types[i]));
        }
    } else if (ORSO_TYPE_IS_FUNCTION(type)) {
        ADD_HASH(hash, type->data.function.argument_count);

        for (i32 i = 0; i < type->data.function.argument_count; i++) {
            ADD_HASH(hash, (u64)(type->data.function.argument_types[i]));
        }

        ADD_HASH(hash, (u64)(type->data.function.return_type));
    } else if (ORSO_TYPE_IS_STRUCT(type)) {
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
    } else if (ORSO_TYPE_IS_POINTER(type)) {
        ADD_HASH(hash, (u64)(type->data.pointer.type));
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

        if (type_a->data.union_.count != type_b->data.union_.count) {
            return type_b->data.union_.count - type_a->data.union_.count;
        }

        for (i32 i = 0; i < type_a->data.union_.count; i++) {
            OrsoType* single_a = type_a->data.union_.types[i];
            OrsoType* single_b = type_b->data.union_.types[i];

            i32 result = type_compare(single_a, single_b);
            if (result == 0) {
                continue;
            }

            return result;
        }

        return 0;
    }

    if (ORSO_TYPE_IS_POINTER(type_a) || ORSO_TYPE_IS_POINTER(type_b)) {
        unless (ORSO_TYPE_IS_POINTER(type_a)) {
            return -type_compare(type_b, type_a);
        }

        unless (ORSO_TYPE_IS_POINTER(type_b)) {
            return -1;
        }
        
        return type_compare(type_a->data.pointer.type, type_b->data.pointer.type);
    }

    if (ORSO_TYPE_IS_FUNCTION(type_a) || ORSO_TYPE_IS_FUNCTION(type_b)) {
        if (!ORSO_TYPE_IS_FUNCTION(type_a)) {
            return -type_compare(&type_b, &type_a);
        }

        // Primitive types have higher priority
        if (!ORSO_TYPE_IS_FUNCTION(type_b)) {
            return -1;
        }

        if (type_a->data.function.return_type != type_b->data.function.return_type) {
            return type_compare(type_a->data.function.return_type, type_b->data.function.return_type);
        }

        if (type_a->data.function.argument_count != type_b->data.function.argument_count) {
            return type_b->data.function.argument_count - type_a->data.function.argument_count;
        }

        for (i32 i = 0; i < type_a->data.function.argument_count; i++) {
            OrsoType* argument_type_a = type_a->data.function.argument_types[i];
            OrsoType* argument_type_b = type_b->data.function.argument_types[i];
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
            OrsoType* a_constant_type = type_a->data.struct_.constants[i].type;
            OrsoType* b_constant_type = type_b->data.struct_.constants[i].type;
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
            OrsoType* a_field_type = type_a->data.struct_.fields[i].type;
            OrsoType* b_field_type = type_b->data.struct_.fields[i].type;
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

OrsoType* orso_type_set_fetch_union(OrsoTypeSet* set, OrsoType** types, i32 count) {
    OrsoType union_type = {
        .kind = ORSO_TYPE_UNION,
        .data.union_.count = count,
        .data.union_.types = ORSO_ALLOCATE_N(OrsoType*, count)
    };

    for (i32 i = 0; i < count; i++) {
        if (i < count) {
            union_type.data.union_.types[i] = types[i];
        } else {
            union_type.data.union_.types[i] = NULL;
        }
    }

    qsort(union_type.data.union_.types, count, sizeof(OrsoType*), type_compare);

    if (set->capacity > 0) {
        OrsoType** entry = fetch_type(set->entries, set->capacity, &union_type);

        if (*entry != NULL) {
            return *entry;
        }
    }

    OrsoType* type = type_copy_new(set, &union_type);
    add_type(set, type);

    return type;
}

OrsoType* orso_type_set_fetch_pointer(OrsoTypeSet* set, OrsoType* inner_type) {
    OrsoType pointer_type = {
        .kind = ORSO_TYPE_POINTER,
        .data.pointer.type = inner_type,
    };

    if (set->capacity > 0) {
        OrsoType** entry = fetch_type(set->entries, set->capacity, &pointer_type);

        if (*entry != NULL) {
            return *entry;
        }
    }

    OrsoType* type = type_copy_new(set, &pointer_type);
    add_type(set, type);

    return type;
}

OrsoType* orso_type_set_fetch_function_(OrsoTypeSet* set, OrsoType* return_type, OrsoType** arguments, i32 argument_count, bool is_native) {
    OrsoType function_type = {
        .kind = is_native ? ORSO_TYPE_NATIVE_FUNCTION : ORSO_TYPE_FUNCTION,
        .data.function.argument_types = arguments,
        .data.function.argument_count = argument_count,
        .data.function.return_type = return_type,
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

OrsoType* orso_type_set_fetch_anonymous_struct(OrsoTypeSet* set, i32 field_count, OrsoStructField* fields, i32 constant_count, OrsoStructConstant* constants) {
    OrsoType struct_type = {
        .kind = ORSO_TYPE_STRUCT,
        .data.struct_.field_count = field_count,
        .data.struct_.fields = fields,

        .data.struct_.constant_count = constant_count,
        .data.struct_.constants = constants,

        .data.struct_.total_bytes = 0,
    };

    OrsoType* type;
    if (constant_count == 0) {
        if (set->capacity > 0) {
            OrsoType** entry = fetch_type(set->entries, set->capacity, &struct_type);

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
            OrsoType* previous_type = fields[i - 1].type;

            i32 bytes = orso_bytes_to_slots(orso_type_size_bytes(previous_type)) * sizeof(OrsoSlot);
            type->data.struct_.fields[i].offset = previous_offset + bytes;
        }

        i32 size_of_final = orso_bytes_to_slots(orso_type_size_bytes(type->data.struct_.fields[field_count - 1].type)) * sizeof(OrsoSlot);
        i32 total_size = type->data.struct_.fields[field_count - 1].offset + size_of_final;

        type->data.struct_.total_bytes = total_size;
    }

    return type;
}

OrsoType* orso_type_create_struct(OrsoTypeSet* set, char* name, i32 name_length, OrsoType* anonymous_struct) {
    ASSERT(ORSO_TYPE_IS_STRUCT(anonymous_struct) && anonymous_struct->data.struct_.name == NULL, "can only create struct from anonymous struct");

    if (anonymous_struct->data.struct_.constant_count == 0) {
        OrsoType* new_type = type_copy_new(set, anonymous_struct);
        new_type->data.struct_.name = ORSO_ALLOCATE_N(char, name_length + 1);
        memcpy(new_type->data.struct_.name, name, name_length);
        new_type->data.struct_.name[name_length] = '\0';

        return new_type;
    } else {
        // here we know that the anonymous struct is unique because it has constants, and those are never shared
        OrsoType* new_type = type_copy_new(NULL, anonymous_struct);

        anonymous_struct->data.struct_ = new_type->data.struct_;

        anonymous_struct->data.struct_.name = ORSO_ALLOCATE_N(char, name_length + 1);
        memcpy(anonymous_struct->data.struct_.name, name, name_length);
        anonymous_struct->data.struct_.name[name_length] = '\0';

        free(new_type);

        return anonymous_struct;
    }
}

OrsoType* orso_type_unique_incomplete_struct_type(OrsoTypeSet* set) {
    OrsoType* new_type = struct_type_new(set, NULL, -1, NULL, -1, 0);
    return new_type;
}

void orso_named_struct_copy_data_from_completed_struct_type(OrsoType* incomplete_named_struct, OrsoType* complete_anonymous_struct) {
    OrsoType* copied_type = type_copy_new(NULL, complete_anonymous_struct);

    incomplete_named_struct->data.struct_.field_count = copied_type->data.struct_.field_count;
    incomplete_named_struct->data.struct_.fields = copied_type->data.struct_.fields;

    incomplete_named_struct->data.struct_.constant_count = copied_type->data.struct_.constant_count;
    incomplete_named_struct->data.struct_.constants = copied_type->data.struct_.constants;

    incomplete_named_struct->data.struct_.total_bytes = copied_type->data.struct_.total_bytes;

    free(copied_type);
}
