#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "intrinsics.h"
#include "stringt.h"

typedef struct types_t types_t;
struct types_t {
    type_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct typedatas_t typedatas_t;
typedef struct typedata_t typedata_t;

typedef enum type_kind_t {
    // analysis types
    TYPE_INVALID = 0,      // error type
    TYPE_UNRESOLVED,       // unresolved (not undefined, not error, not defined)
    TYPE_INFERRED_FUNCTION,// inferred function definition, copies created at compile-time when called
    TYPE_UNREACHABLE,      // for jmp expressions (return, break, continue)

    // simple types (types that do not require type info)
    TYPE_VOID,             // no symbol
    TYPE_BOOL,             // true false
    TYPE_STRING,           // "anything in here"
    TYPE_TYPE,             // s32, void, type, (int, int) -> int, etc
    
    // "complex" types
    TYPE_NUMBER,           // s8, u8, s16, u16, s32, u32, s64, u32, u64, f32, f64
    TYPE_FUNCTION,         // (type1, type2, ..., typen) -> return_type OR (foo := 0, bar := "") -> return_type
    TYPE_INTRINSIC_FUNCTION,  // (type1, type2, ..., typen) -> return_type
    TYPE_POINTER,          // &type
    TYPE_ARRAY,            // [n]type
    TYPE_STRUCT,           // used for both anonymous and named

    TYPE_COUNT,
} type_kind_t;

typedef struct struct_field_t {
    char* name;
    type_t type;

    // not relevant for hashing
    s32 offset;
} struct_field_t;

typedef struct struct_constant_t {
    char *name;
    type_t type;
} struct_constant_t;

struct typedatas_t {
    typedata_t **items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef enum num_type_t num_type_t;
enum num_type_t {
    NUM_TYPE_UNSIGNED = 0,
    NUM_TYPE_SIGNED,
    NUM_TYPE_FLOAT,
};

typedef enum num_size_t num_size_t;
enum num_size_t {
    NUM_SIZE_8 = sizeof(u8),
    NUM_SIZE_16 = sizeof(u16),
    NUM_SIZE_32 = sizeof(u32),
    NUM_SIZE_64 = sizeof(u64),
};

typedef enum type_caps_t type_caps_t;
enum type_caps_t {
    TYPE_CAP_NONE           = 0,
    TYPE_CAP_ARITHMETIC     = 1,
    TYPE_CAP_COMPARABLE     = 1 << 2,
    TYPE_CAP_LOGICAL        = 1 << 3,
};

struct typedata_t {
    string_t name;
    type_kind_t kind;
    type_caps_t capabilities;
    size_t size; // not serialized
    union {
        num_type_t num;

        struct {
            type_t return_type;
            types_t argument_types;
        } function;

        struct {
            // null if anonymous
            char *name;

            s32 field_count;
            struct_field_t *fields;

            s32 constant_count;
            struct_constant_t *constants;
        } struct_;

        struct {
            type_t type;
        } ptr;

        struct {
            size_t size;
            type_t type;
        } arr;
    } as;
};

#define TYPE_IS_VOID(TYPE) ((TYPE).i == TYPE_VOID)
#define TYPE_IS_TYPE(TYPE) ((TYPE).i == TYPE_TYPE)
#define TYPE_IS_INVALID(TYPE) ((TYPE).i == TYPE_INVALID)
#define TYPE_IS_LABEL(TYPE) ((TYPE).i == TYPE_LABEL)
#define TYPE_IS_UNREACHABLE(TYPE) ((TYPE).i == TYPE_UNREACHABLE)
#define TYPE_IS_UNRESOLVED(TYPE) ((TYPE).i == TYPE_UNRESOLVED)
#define TYPE_IS_RESOLVED(TYPE) ((TYPE).i != TYPE_UNRESOLVED)
#define TYPE_IS_INFERRED_FUNCTION(TYPE) ((TYPE).i == TYPE_INFERRED_FUNCTION)

struct type_table_t;

bool struct_type_is_incomplete(typedata_t *type);

bool type_equal(typedata_t *a, typedata_t *b);

struct_field_t *type_struct_find_field(typedata_t *struct_, const char *name, size_t name_length);

string_t type_to_string(typedatas_t types, type_t type, arena_t *allocator);

#endif
