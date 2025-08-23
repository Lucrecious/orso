#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "intrinsics.h"
#include "stringt.h"

typedef struct types_t types_t;
struct types_t {
    ortype_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct typedatas_t typedatas_t;
typedef struct typedata_t typedata_t;

size_t td_align(size_t b, size_t alignment);

typedef enum type_kind_t type_kind_t;
enum type_kind_t {
    // analysis types
    TYPE_INVALID = 0,      // error type
    TYPE_UNRESOLVED,       // unresolved (not undefined, not error, not defined)
    TYPE_INFERRED_FUNCTION,// inferred function definition, copies created at compile-time when called
    TYPE_PARAM_STRUCT,     // parameterized struct definition, copies created at compile-time when "called"
    TYPE_MODULE,           // modules
    TYPE_UNREACHABLE,      // for jmp expressions (return, break, continue)

    // simple types (types that do not require type info)
    TYPE_VOID,             // no symbol
    TYPE_BOOL,             // true false
    TYPE_TYPE,             // s32, void, type, (int, int) -> int, etc
    
    // "complex" types
    TYPE_STRING,           // "anything in here"
    TYPE_NUMBER,           // s8, u8, s16, u16, s32, u32, s64, u32, u64, f32, f64
    TYPE_FUNCTION,         // (type1, type2, ..., typen) -> return_type OR (foo := 0, bar := "") -> return_type
    TYPE_POINTER,          // &type
    TYPE_ARRAY,            // [n]type
    TYPE_STRUCT,           // used for both anonymous and named

    TYPE_COUNT,
};

typedef struct struct_field_t struct_field_t;
struct struct_field_t {
    oristring_t name;
    ortype_t type;
    orword_t default_value;

    // not relevant for hashing
    size_t offset;
};

typedef struct struct_fields_t struct_fields_t;
struct struct_fields_t {
    struct_field_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

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
    NUM_SIZE_8 = sizeof(oru8),
    NUM_SIZE_16 = sizeof(oru16),
    NUM_SIZE_32 = sizeof(oru32),
    NUM_SIZE_64 = sizeof(oru64),
};

typedef enum type_caps_t type_caps_t;
enum type_caps_t {
    TYPE_CAP_NONE           = 0,
    TYPE_CAP_ARITHMETIC     = 1,
    TYPE_CAP_LOGICAL        = 1 << 2,
    TYPE_CAP_ORDERABLE      = 1 << 3,
};

#define TYPE_CAP_NUMBER (TYPE_CAP_ARITHMETIC|TYPE_CAP_ORDERABLE)

typedef enum struct_status_t struct_status_t;
enum struct_status_t {
    STRUCT_STATUS_INVALID = 0,
    STRUCT_STATUS_INCOMPLETE,
    STRUCT_STATUS_COMPLETE
};

typedef struct struct_binding_t struct_binding_t;
struct struct_binding_t {
    orstring_t cname;
    struct_fields_t field_bindings;
    ortype_t type;
};

typedef enum intrinsic_struct_t intrinsic_struct_t;
enum intrinsic_struct_t {
    INTRINSIC_STRUCT_STRING,
    INTRINSIC_STRUCT_COUNT,
};

struct typedata_t {
    orstring_t name;
    type_kind_t kind;
    type_caps_t capabilities;
    size_t size; // not serialized
    size_t alignment; // not serialized
    orword_t default_value;

    union {
        num_type_t num;

        struct {
            ortype_t return_type;
            types_t argument_types;
        } function;

        // structs do not need to be serialized since they're always all unique
        struct {
            orcstr_t name_or_null;

            struct_fields_t params;
            struct_fields_t fields;
            struct_fields_t constants;

            struct_status_t status;

            struct_binding_t *binding_or_null;
        } struct_;

        struct {
            ortype_t type;
        } ptr;

        struct {
            size_t count;
            ortype_t type;
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

bool is_type_kind_aggregate(type_kind_t kind);

bool type_equal(typedata_t *a, typedata_t *b);

orstring_t type_to_string(typedatas_t types, ortype_t type, arena_t *allocator);

#endif
