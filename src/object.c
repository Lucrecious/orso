#include "object.h"

#include <stdio.h>

#include "sb.h"
#include "symbol_table.h"
#include "type_set.h"

void* orso_object_reallocate(object_t* pointer, type_t* type, size_t old_size, size_t new_size) {
    (void)old_size;

    if (new_size == 0) {
        // Should only be here when called by GC
        free(pointer);
        return NULL;
    }

    object_t* result = realloc(pointer, new_size);
    if (result == NULL) {
        exit(1);
    }

    result->type = type;

    return result;
}

OrsoString *orso_new_string_from_cstrn(const char* start, i32 length) {
    OrsoString* string = ORSO_OBJECT_ALLOCATE_FLEX(OrsoString, &OrsoTypeString, length + 1);
    string->length = length;
    memcpy(string->text, start, length);
    string->text[length] = '\0';

    return string;
}

char* cstrn_new(const char* start, i32 length) {
    char* cstr = ORSO_ALLOCATE_N(char, length + 1);
    memcpy(cstr, start, length);
    cstr[length] = '\0';

    return cstr;
}

string_t slot_to_string(slot_t *slot, type_t *type, arena_t *allocator) {
    switch (type->kind) {
        case ORSO_TYPE_BOOL: {
            if (ORSO_SLOT_IS_FALSE((*slot))) {
                return str("false");
            } else {
                return str("true");
            }
        }

        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: {
            return string_format("%lld", allocator, slot->as.i);
        }

        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: {
            return string_format("%.14f", allocator, slot->as.f);
        }

        case ORSO_TYPE_VOID: return str("null");

        case ORSO_TYPE_STRING: return cstrn2string(((OrsoString*)slot->as.p)->text, ((OrsoString*)slot->as.p)->length, allocator);

        case ORSO_TYPE_SYMBOL: {
            symbol_t* symbol = (symbol_t*)slot->as.p;
            return string_format("'%s'", allocator, (char*)symbol->text);
        }

        case ORSO_TYPE_FUNCTION: {
            string_t string;

            arena_t tmp_allocator = {0}; {
                string_builder_t sb = {.allocator = &tmp_allocator};

                function_t* function = (function_t*)slot->as.p;
                sb_add_cstr(&sb, string_format("<%s :: (", &tmp_allocator,
                    function->binded_name ? function->binded_name->text : "<anonymous>").cstr);

                for (i32 i = 0; i < function->signature->data.function.argument_count; i++) {
                    string_t arg_type = type_to_string(function->signature->data.function.argument_types[i], &tmp_allocator);
                    sb_add_cstr(&sb, arg_type.cstr);

                    if (i < function->signature->data.function.argument_count - 1)  {
                        sb_add_cstr(&sb, ", ");
                    }
                }

                string_t return_type_string = type_to_string(function->signature->data.function.return_type, &tmp_allocator);
                sb_add_cstr(&sb, string_format(") -> %s>", &tmp_allocator, return_type_string.cstr).cstr);

                string = sb_render(&sb, allocator);
            } arena_free(&tmp_allocator);

            return string;
        }

        case ORSO_TYPE_NATIVE_FUNCTION: {
            return str("<native>");
        }

        case ORSO_TYPE_POINTER: {
            return str("<ptr>");
        }

        case ORSO_TYPE_STRUCT: {
            u32 size = type->data.struct_.total_bytes;
            return string_format("<struct %d bytes>", allocator, size);
        }

        case ORSO_TYPE_UNION: {
            type_t *type = (type_t*)slot->as.p;
            return slot_to_string(slot + 1, type, allocator);
        }

        case ORSO_TYPE_TYPE: {
            string_t result;

            arena_t tmp_allocator = {0}; {
                type_t *type = (type_t*)slot->as.p;
                string_t type_string = type_to_string(type, &tmp_allocator);
                result = string_format("<%s>", allocator, type_string.cstr);
            } arena_free(&tmp_allocator);

            return result;
        }
        case ORSO_TYPE_UNDEFINED: return str("<undefined>");
        case ORSO_TYPE_UNRESOLVED: return str("<unresolved>");
        case ORSO_TYPE_INVALID: return str("<invalid>");
    }
}

OrsoString *orso_slot_to_string(slot_t* slot, type_t* type) {
    OrsoString *string;

    arena_t tmp = {0}; {
        string_t value = slot_to_string(slot, type, &tmp);
        string = orso_new_string_from_cstrn(value.cstr, value.length);
    } arena_free(&tmp);

    return string;
}

OrsoString *orso_string_concat(OrsoString *a, OrsoString *b) {
    OrsoString *string = ORSO_OBJECT_ALLOCATE_FLEX(OrsoString, &OrsoTypeString, a->length + b->length + 1);
    string->length = a->length + b->length;
    memcpy(string->text, a->text, a->length);
    memcpy(string->text + a->length, b->text, b->length);
    string->text[a->length + b->length] = '\0';

    return string;
}

static object_t *object_new(size_t byte_size, type_t *type, arena_t *allocator) {
    void *object = arena_alloc(allocator, byte_size);
    memset(object, 0, byte_size);
    ((object_t*)object)->type = type;
    return (object_t*)object;
}

function_t *orso_new_function(arena_t *allocator) {
    function_t *function = (function_t*)object_new(sizeof(function_t), (type_t*)&OrsoTypeEmptyFunction, allocator);
    function->signature = &OrsoTypeEmptyFunction;
    chunk_init(&function->chunk, allocator);
    function->binded_name = NULL;

    return function;
}

bool is_function_compiled(function_t* function) {
    return function->chunk.code.items != NULL;
}

native_function_t* orso_new_native_function(native_function_interface_t function, type_t* type) {
    native_function_t* function_obj = ORSO_OBJECT_ALLOCATE(native_function_t, type);
    function_obj->function = function;
    function_obj->signature = type;

    return function_obj;
}

struct_t* orso_new_struct(void) {
    struct_t* struct_ = ORSO_ALLOCATE(struct_t);
    struct_->slots = NULL;
    return struct_;
}

void orso_free_struct(struct_t* struct_) {
    free(struct_->slots);
}

i64 cstrn_to_i64(const char* text, i32 length) {
    i64 integer = 0;

    for (i32 i = 0; i < length; i++) {
        char digit = text[i];
        if (digit == '_') {
            continue;
        }

        i32 ones = text[i] - '0';
        if (ones < 0 || ones > 9) {
            continue;
        }

        integer *= 10;
        integer += ones;
    }

    return integer;
}

f64 cstrn_to_f64(const char* text, i32 length) {
    f64 value = 0;
    f64 fact = 1;
    bool point_seen = false;

    for (char* c = (char*)text; c != (text + length); c++) {
        if (*c == '.') {
            point_seen = true;
            continue;
        }

        if (*c == '_') {
            continue;
        }

        i32 digit = *c - '0';
        if (digit < 0 || digit > 9) {
            continue;
        }

        if (point_seen) {
            fact /= 10.0f;
        }

        value = value * 10.0f + (float)digit;
    }

    return value * fact;
}

symbol_t* orso_unmanaged_symbol_from_cstrn(const char* start, i32 length, symbol_table_t* symbol_table) {
    u32 hash = orso_hash_cstrn(start, length);
    symbol_t* symbol = symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = ORSO_ALLOCATE_FLEX(symbol_t, length + 1);
    symbol->object.type = &OrsoTypeSymbol;
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';

    slot_t slot = ORSO_SLOT_I(0);
    symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}

symbol_t* orso_new_symbol_from_cstrn(const char* start, i32 length, symbol_table_t* symbol_table) {
    u32 hash = orso_hash_cstrn(start, length);
    symbol_t* symbol = symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = ORSO_OBJECT_ALLOCATE_FLEX(symbol_t, &OrsoTypeSymbol, length + 1);
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';

    slot_t slot = ORSO_SLOT_I(0);
    symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}
