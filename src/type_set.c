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

OrsoFunctionType OrsoTypeEmptyFunction = (OrsoFunctionType) {
    .type.kind = ORSO_TYPE_FUNCTION,
    .argument_count = 0,
    .argument_types = NULL,
    .return_type = &OrsoTypeVoid,
};

OrsoUnionType* union_type_new(OrsoTypeSet* set, OrsoType** types, i32 count) {
    ASSERT(count <= ORSO_UNION_NUM_MAX, "cannot create union type with more than 4 types");

    OrsoUnionType* union_type = ORSO_ALLOCATE(OrsoUnionType);
    union_type->type.kind = ORSO_TYPE_UNION;
    union_type->count = count;
    for (i32 i = 0; i < union_type->count; i++) {
        union_type->types[i] = types[i];
    }

    sb_push(set->heap, (OrsoType*)union_type);

    return union_type;
}

OrsoFunctionType* function_type_new(OrsoTypeSet* set, OrsoType** arguments, i32 argument_count, OrsoType* return_type, bool is_native) {
    OrsoFunctionType* function_type = ORSO_ALLOCATE(OrsoFunctionType);
    function_type->type.kind = is_native ? ORSO_TYPE_NATIVE_FUNCTION : ORSO_TYPE_FUNCTION;
    function_type->argument_count = argument_count;
    function_type->argument_types = ORSO_ALLOCATE_N(OrsoType*, argument_count);
    for (i32 i = 0; i < argument_count; i++) {
        function_type->argument_types[i] = arguments[i];
    }

    function_type->return_type = return_type;

    sb_push(set->heap, (OrsoType*)function_type);

    return function_type;
}

OrsoType* type_copy_new(OrsoTypeSet* set, OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        OrsoUnionType const * union_type = (OrsoUnionType const *)type;
        return (OrsoType*)union_type_new(set, (OrsoType**)union_type->types, union_type->count);
    }

    if (type->kind == ORSO_TYPE_FUNCTION || type->kind == ORSO_TYPE_NATIVE_FUNCTION) {
        OrsoFunctionType const * function_type = (OrsoFunctionType const *)type;
        return (OrsoType*)function_type_new(set, function_type->argument_types, function_type->argument_count, function_type->return_type, type->kind == ORSO_TYPE_NATIVE_FUNCTION);
    }

    UNREACHABLE();
    return NULL;
}

void type_free(OrsoType* type) {
    if (type->kind == ORSO_TYPE_UNION) {
    } else if (type->kind == ORSO_TYPE_FUNCTION) {
        OrsoFunctionType* function_type = (OrsoFunctionType*)type;
        for (i32 i = 0; i < function_type->argument_count; i++) {
            function_type->argument_types[i] = NULL;
        }

        free((void**)function_type->argument_types);
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
        OrsoUnionType* union_type = (OrsoUnionType*)type;
        for (i32 i = 0; i < union_type->count; i++) {
            ADD_HASH(hash, hash_type(union_type->types[i]));
        }
    } else if (type->kind == ORSO_TYPE_FUNCTION) {
        OrsoFunctionType* function_type = (OrsoFunctionType*)type;
        for (i32 i = 0; i < function_type->argument_count; i++) {
            ADD_HASH(hash, hash_type(function_type->argument_types[i]));
        }

        ADD_HASH(hash, hash_type(function_type->return_type));
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

        OrsoUnionType* union_type_a = (OrsoUnionType*)type_a;
        OrsoUnionType* union_type_b = (OrsoUnionType*)type_b;

        if (union_type_a->count != union_type_b->count) {
            return union_type_b->count - union_type_a->count;
        }

        for (i32 i = 0; i < union_type_a->count; i++) {
            OrsoType* single_a = union_type_a->types[i];
            OrsoType* single_b = union_type_b->types[i];

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

        OrsoFunctionType* function_type_a = (OrsoFunctionType*)type_a;
        OrsoFunctionType* function_type_b = (OrsoFunctionType*)type_b;

        if (function_type_a->return_type != function_type_b->return_type) {
            return type_compare(function_type_a->return_type, function_type_b->return_type);
        }

        if (function_type_a->argument_count != function_type_b->argument_count) {
            return function_type_b->argument_count - function_type_a->argument_count;
        }

        for (i32 i = 0; i < function_type_a->argument_count; i++) {
            OrsoType* argument_type_a = function_type_a->argument_types[i];
            OrsoType* argument_type_b = function_type_b->argument_types[i];
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
    OrsoUnionType union_type = {
        .type.kind = ORSO_TYPE_UNION,
        .count = count,
    };

    for (i32 i = 0; i < ORSO_UNION_NUM_MAX; i++) {
        if (i < count) {
            union_type.types[i] = types[i];
        } else {
            union_type.types[i] = NULL;
        }
    }

    qsort(union_type.types, count, sizeof(OrsoType*), type_compare);

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
    OrsoFunctionType function_type = {
        .type.kind = is_native ? ORSO_TYPE_NATIVE_FUNCTION : ORSO_TYPE_FUNCTION,
        .argument_types = arguments,
        .argument_count = argument_count,
        .return_type = return_type,
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
