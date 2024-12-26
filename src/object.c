#include "object.h"

#include <stdio.h>

#include "symbol_table.h"
#include "type_set.h"
#include "tmp.h"

static object_t *object_new(size_t byte_size, type_t type, arena_t *allocator) {
    void *object = arena_alloc(allocator, byte_size);
    memset(object, 0, byte_size);
    ((object_t*)object)->type = type;
    return (object_t*)object;
}


OrsoString *orso_new_string_from_cstrn(const char *start, i32 length, arena_t *allocator) {
    OrsoString *string = (OrsoString*)object_new(sizeof(OrsoString) + (length+1)*sizeof(char), typeid(TYPE_STRING), allocator);
    string->length = length;
    memcpy(string->text, start, length);
    string->text[length] = '\0';

    return string;
}

char *cstrn_new(const char *start, i32 length, arena_t *allocator) {
    char *cstr = arena_alloc(allocator, sizeof(char)*(length+1));
    memcpy(cstr, start, length);
    cstr[length] = '\0';

    return cstr;
}

string_t bytes_to_string(byte *data, type_infos_t *types, type_t type, arena_t *allocator) {
    type_info_t *type_info = get_type_info(types, type);

    switch (type_info->kind) {
        case TYPE_BOOL: {
            if (*data) {
                return str("false");
            } else {
                return str("true");
            }
        }

        case TYPE_INT32: return string_format("%d", allocator, cast(data, i32));
        case TYPE_INT64: return string_format("%lld", allocator, cast(data, i64));

        case TYPE_FLOAT32: return string_format("%.14f", allocator, cast(data, f32));
        case TYPE_FLOAT64: return string_format("%.14f", allocator, cast(data, f64));

        case TYPE_VOID: return str("null");

        case TYPE_STRING: return cstrn2string(cast(data, OrsoString*)->text, cast(data, OrsoString*)->length, allocator);

        case TYPE_SYMBOL: return string_format("'%s'", allocator, (char*)cast(data, symbol_t*)->text);

        case TYPE_FUNCTION: {
            string_t string;

            tmp_arena_t *tmp = allocator_borrow(); {
                string_builder_t sb = {.allocator = tmp->allocator};

                function_t_ *function = cast(data, function_t_*);
                sb_add_cstr(&sb, string_format("<%s :: (", tmp->allocator,
                    function->binded_name ? function->binded_name->text : "<anonymous>").cstr);
                
                type_info_t *signature = get_type_info(types, function->signature);

                for (size_t i = 0; i < signature->data.function.argument_types.count; ++i) {
                    string_t arg_type = type_to_string(*types, signature->data.function.argument_types.items[i], tmp->allocator);
                    sb_add_cstr(&sb, arg_type.cstr);

                    if (i < signature->data.function.argument_types.count - 1)  {
                        sb_add_cstr(&sb, ", ");
                    }
                }

                string_t return_type_string = type_to_string(*types, signature->data.function.return_type, tmp->allocator);
                sb_add_cstr(&sb, string_format(") -> %s>", tmp->allocator, return_type_string.cstr).cstr);

                string = sb_render(&sb, allocator);
            } allocator_return(tmp);

            return string;
        }

        case TYPE_NATIVE_FUNCTION: {
            return str("<native>");
        }

        case TYPE_POINTER: {
            return str("<ptr>");
        }

        case TYPE_STRUCT: {
            tmp_arena_t *tmp = allocator_borrow();
            string_builder_t sb = {.allocator=tmp->allocator};

            sb_add_cstr(&sb, "{");

            for (i32 i = 0; i < type_info->data.struct_.field_count; ++i) {
                struct_field_t *field = &type_info->data.struct_.fields[i];
                
                sb_add_cstr(&sb, field->name);

                sb_add_cstr(&sb, "=");

                string_t field_value = bytes_to_string(data+field->offset, types, field->type, tmp->allocator);

                sb_add_cstr(&sb, field_value.cstr);

                if (i < type_info->data.struct_.field_count - 1) {
                    sb_add_cstr(&sb, ", ");
                }
            }

            sb_add_cstr(&sb, "}");

            string_t result = sb_render(&sb, allocator);

            allocator_return(tmp);

            return  result;
        }

        case TYPE_UNION: {
            type_t type = cast(data, type_t);
            return bytes_to_string(data + sizeof(type), types, type, allocator);
        }

        case TYPE_TYPE: {
            string_t result;

            tmp_arena_t *tmp = allocator_borrow(); {
                type_t type = cast(data, type_t);
                string_t type_string = type_to_string(*types, type, tmp->allocator);
                result = string_format("<%s>", allocator, type_string.cstr);
            } allocator_return(tmp);

            return result;
        }
        case TYPE_COUNT: return str("<enum count>");
        case TYPE_UNDEFINED: return str("<undefined>");
        case TYPE_UNRESOLVED: return str("<unresolved>");
        case TYPE_INVALID: return str("<invalid>");
    }
}

OrsoString *orso_string_concat(OrsoString *a, OrsoString *b, arena_t *allocator) {
    OrsoString *string = (OrsoString*)object_new(sizeof(OrsoString) + (a->length + b->length + 1)*sizeof(char), typeid(TYPE_STRING), allocator);
    string->length = a->length + b->length;
    memcpy(string->text, a->text, a->length);
    memcpy(string->text + a->length, b->text, b->length);
    string->text[a->length + b->length] = '\0';

    return string;
}

function_t_ *orso_new_function(string_t file_defined_in, arena_t *allocator) {
    function_t_ *function = (function_t_*)object_new(sizeof(function_t_), typeid(TYPE_FUNCTION), allocator);
    function->signature = typeid(TYPE_FUNCTION);
    function->binded_name = NULL;
    function->file_defined_in = file_defined_in;

    return function;
}

bool is_function_compiled(function_t_* function) {
    UNUSED(function);
    return false;
}

native_function_t *orso_new_native_function(native_function_interface_t function, type_t type, arena_t *allocator) {
    native_function_t *function_obj = (native_function_t*)object_new(sizeof(native_function_t), type, allocator);
    function_obj->function = function;
    function_obj->signature = type;

    return function_obj;
}

struct_t *orso_new_struct(arena_t *allocator) {
    struct_t *struct_ = arena_alloc(allocator, sizeof(struct_t));
    struct_->slots = NULL;
    return struct_;
}

void orso_free_struct(struct_t *struct_) {
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

symbol_t *orso_unmanaged_symbol_from_cstrn(const char *start, i32 length, symbol_table_t *symbol_table, arena_t *allocator) {
    u32 hash = hash_cstrn(start, length);
    symbol_t *symbol = symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = (symbol_t*)object_new(sizeof(symbol_t) + (length + 1)*sizeof(char), typeid(TYPE_SYMBOL), allocator);
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';

    slot_t slot = SLOT_I(0);
    symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}

symbol_t *orso_new_symbol_from_cstrn(const char *start, i32 length, symbol_table_t *symbol_table, arena_t *allocator) {
    u32 hash = hash_cstrn(start, length);
    symbol_t* symbol = symbol_table_find_cstrn(symbol_table, start, length, hash);
    if (symbol != NULL) {
        return symbol;
    }

    symbol = (symbol_t*)object_new(sizeof(symbol_t) + (length+1)*sizeof(char), typeid(TYPE_SYMBOL), allocator);
    symbol->hash = hash;
    symbol->length = length;
    memcpy(symbol->text, start, length);
    symbol->text[length] = '\0';

    slot_t slot = SLOT_I(0);
    symbol_table_set(symbol_table, symbol, slot);

    return symbol;
}
