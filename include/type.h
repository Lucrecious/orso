#ifndef TYPE_H_
#define TYPE_H_

#include <stdio.h>

#include "def.h"
#include "lexer.h"
#include "stringt.h"

typedef struct type_t type_t;
struct type_t {
    u64 i;
};

typedef struct types_t types_t;
struct types_t {
    type_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct type_infos_t type_infos_t;
typedef struct typedata_t typedata_t;

typedef enum type_kind_t {
    TYPE_INVALID = 0,      // error type
    TYPE_UNRESOLVED,       // unresolved (not undefined, not error, not defined)
    TYPE_UNDEFINED,

    // simple types (types that do not require type info)
    TYPE_UNREACHABLE,      // for jmp expressions (return, break, continue)
    TYPE_LABEL,     // for do labels

    TYPE_VOID,             // null
    TYPE_BOOL,             // true false
    TYPE_STRING,           // "anything in here"
    TYPE_TYPE,             // i32, void, type, () -> void, etc
    
    // "complex" types
    TYPE_NUMBER,           // i8, u8, i16, u16, i32, u32, i64, u32, u64, f32, f64
    TYPE_FUNCTION,         // (type1, type2, ..., typen) -> return_type OR (foo := 0, bar := "") -> return_type
    TYPE_NATIVE_FUNCTION,  // (type1, type2, ..., typen) -> return_type
    TYPE_POINTER,          // &type
    TYPE_STRUCT,           // used for both anonymous and named

    TYPE_COUNT,
} type_kind_t;

typedef struct struct_field_t {
    char* name;
    type_t type;

    // not relevant for hashing
    i32 offset;
} struct_field_t;

typedef struct struct_constant_t {
    char *name;
    type_t type;
} struct_constant_t;

struct type_infos_t {
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
    NUM_SIZE_BYTE = sizeof(byte),
    NUM_SIZE_SHORT = sizeof(u16),
    NUM_SIZE_SINGLE = sizeof(u32),
    NUM_SIZE_LONG = sizeof(u64),
};

struct typedata_t {
    type_kind_t kind;
    size_t size; // not serialized
    union {
        num_type_t num;

        struct {
            types_t types;
        } union_;

        struct {
            type_t return_type;
            types_t argument_types;
        } function;

        struct {
            // null if anonymous
            char *name;

            i32 field_count;
            struct_field_t *fields;

            i32 constant_count;
            struct_constant_t *constants;
        } struct_;

        struct {
            type_t type;
        } pointer;
    } data;
};

#define TYPE_IS_VOID(TYPE) ((TYPE).i == TYPE_VOID)
#define TYPE_IS_TYPE(TYPE) ((TYPE).i == TYPE_TYPE)
#define TYPE_IS_INVALID(TYPE) ((TYPE).i == TYPE_INVALID)
#define TYPE_IS_UNDEFINED(TYPE) ((TYPE).i == TYPE_UNDEFINED)
#define TYPE_IS_LABEL(TYPE) ((TYPE).i == TYPE_LABEL)
#define TYPE_IS_UNREACHABLE(TYPE) ((TYPE).i == TYPE_UNREACHABLE)
#define TYPE_IS_UNRESOLVED(TYPE) ((TYPE).i == TYPE_UNRESOLVED)

struct type_table_t;

bool struct_type_is_incomplete(typedata_t *type);

bool type_equal(typedata_t *a, typedata_t *b);

size_t bytes_to_words(i32 byte_count);

struct_field_t *type_struct_find_field(typedata_t *struct_, const char *name, size_t name_length);

string_t type_to_string(type_infos_t types, type_t type, arena_t *allocator);

#endif
