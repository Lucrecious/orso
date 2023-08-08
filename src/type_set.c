#include "type_set.h"

#include "sb.h"
#include <stdlib.h>

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

    sb_push(set->heap, (OrsoType*)function_type);

    return function_type;
}

OrsoType* type_copy_new(OrsoTypeSet* set, OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return (OrsoType*)union_type_new(set, (OrsoType**)type->type.union_.types, type->type.union_.count);
    }

    if (type->kind == ORSO_TYPE_FUNCTION || type->kind == ORSO_TYPE_NATIVE_FUNCTION) {
        return (OrsoType*)function_type_new(set, type->type.function.argument_types, type->type.function.argument_count, type->type.function.return_type, type->kind == ORSO_TYPE_NATIVE_FUNCTION);
    }

    UNREACHABLE();
    return NULL;
}

void type_free(OrsoType* type) {
    if (type->kind == ORSO_TYPE_UNION) {
        free(type->type.union_.types);
    } else if (type->kind == ORSO_TYPE_FUNCTION) {
        for (i32 i = 0; i < type->type.function.argument_count; i++) {
            type->type.function.argument_types[i] = NULL;
        }

        free((void**)type->type.function.argument_types);
    }
}

static bool add_type(OrsoTypeSet* set, OrsoType* type);

void orso_type_set_init(OrsoTypeSet* set) {
    set->capacity = 0;
    set->count = 0;
    set->entries = NULL;
    set->heap = NULL;

    add_type(set, &OrsoTypeVoid);
    add_type(set, &OrsoTypeBool);
    add_type(set, &OrsoTypeInteger32);
    add_type(set, &OrsoTypeInteger64);
    add_type(set, &OrsoTypeFloat32);
    add_type(set, &OrsoTypeFloat64);
    add_type(set, &OrsoTypeString);
    add_type(set, &OrsoTypeSymbol);
    add_type(set, &OrsoTypeType);
    add_type(set, &OrsoTypeInvalid);
    add_type(set, &OrsoTypeUnresolved);

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
    } else if (type->kind == ORSO_TYPE_FUNCTION) {
        for (i32 i = 0; i < type->type.function.argument_count; i++) {
            ADD_HASH(hash, hash_type(type->type.function.argument_types[i]));
        }

        ADD_HASH(hash, hash_type(type->type.function.return_type));
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

    if (type_a->kind == ORSO_TYPE_FUNCTION || type_b->kind == ORSO_TYPE_FUNCTION) {
        if (type_a->kind != ORSO_TYPE_FUNCTION) {
            return -type_compare(&type_b, &type_a);
        }

        // Primitive types have higher priority
        if (type_b->kind != ORSO_TYPE_FUNCTION) {
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
        OrsoType** entry = fetch_type(set->entries, set->capacity, (OrsoType*)&function_type);

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
