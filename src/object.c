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

string_t slot_to_string(slot_t *slot, type_infos_t *types, type_t type, arena_t *allocator) {
    type_info_t *type_info = get_type_info(types, type);

    switch (type_info->kind) {
        case TYPE_BOOL: {
            if (SLOT_IS_FALSE((*slot))) {
                return str("false");
            } else {
                return str("true");
            }
        }

        case TYPE_INT32:
        case TYPE_INT64: {
            return string_format("%lld", allocator, slot->as.i);
        }

        case TYPE_FLOAT32:
        case TYPE_FLOAT64: {
            return string_format("%.14f", allocator, slot->as.f);
        }

        case TYPE_VOID: return str("null");

        case TYPE_STRING: return cstrn2string(((OrsoString*)slot->as.p)->text, ((OrsoString*)slot->as.p)->length, allocator);

        case TYPE_SYMBOL: {
            symbol_t *symbol = (symbol_t*)slot->as.p;
            return string_format("'%s'", allocator, (char*)symbol->text);
        }

        case TYPE_FUNCTION: {
            string_t string;

            tmp_arena_t *tmp = allocator_borrow(); {
                string_builder_t sb = {.allocator = tmp->allocator};

                function_t *function = (function_t*)slot->as.p;
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

            slots_t slots = {.allocator=tmp->allocator};

            for (i32 i = 0; i < type_info->data.struct_.field_count; ++i) {
                struct_field_t *field = &type_info->data.struct_.fields[i];
                
                sb_add_cstr(&sb, field->name);

                sb_add_cstr(&sb, "=");

                byte *value = ((byte*)slot) + field->offset;

                type_info_t *field_type = get_type_info(types, field->type);

                size_t field_slot_size = type_slot_count(field_type);
                until (slots.count >= field_slot_size) {
                    array_push(&slots, (slot_t){0});
                }

                size_t field_size = type_size_bytes(field_type);
                copy_bytes_to_slots(slots.items, value, field_type->kind, field_size);

                string_t field_value = slot_to_string(slots.items, types, field->type, tmp->allocator);

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
            type_t type = (type_t){.i=slot->as.u};
            return slot_to_string(slot + 1, types, type, allocator);
        }

        case TYPE_TYPE: {
            string_t result;

            tmp_arena_t *tmp = allocator_borrow(); {
                type_t type = (type_t){.i=slot->as.u};
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

OrsoString *orso_slot_to_string(slot_t *slot, type_infos_t *types, type_t type, arena_t *allocator) {
    OrsoString *string;

    tmp_arena_t *tmp = allocator_borrow(); {
        string_t value = slot_to_string(slot, types, type, tmp->allocator);
        string = orso_new_string_from_cstrn(value.cstr, value.length, allocator);
    } allocator_return(tmp);

    return string;
}

void copy_bytes_to_slots(void *destination, void *source, type_kind_t type_kind, u64 size_bytes) {
    switch (type_kind) {
        case TYPE_COUNT:
        case TYPE_INVALID:
        case TYPE_UNDEFINED:
        case TYPE_UNRESOLVED: {
            UNREACHABLE();
            break;
        }
        case TYPE_VOID:
        case TYPE_BOOL: {
            byte field_value = *(byte*)source;
            ((slot_t*)destination)->as.u = field_value;
            break;
        }

        case TYPE_INT32: {
            i32 field_value = *((i32*)(source));
            ((slot_t*)destination)->as.i = field_value;
            break;
        }

        case TYPE_FLOAT32: {
            f32 field_value = *((f32*)(source));
            ((slot_t*)destination)->as.f = field_value;
            break;
        }
        
        case TYPE_TYPE:
        case TYPE_SYMBOL:
        case TYPE_STRING:
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
        case TYPE_INT64:
        case TYPE_FLOAT64: {
            *((slot_t*)destination) = *((slot_t*)(source));
            break;
        }

        case TYPE_UNION:
        case TYPE_STRUCT: {
            for (size_t i = 0; i < size_bytes; i += sizeof(slot_t)) {
                if (i + sizeof(slot_t) <= size_bytes) {
                    ((slot_t*)destination)[i/sizeof(slot_t)] = ((slot_t*)(source))[i/sizeof(slot_t)];
                } else {
                    ((slot_t*)destination)[i/sizeof(slot_t)].as.i = 0;
                    memcpy((byte*)destination + i, (byte*)source + i, size_bytes-i);
                }
            }
            break;
        }
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

function_t *orso_new_function(cstr_t file_defined_in, arena_t *allocator) {
    function_t *function = (function_t*)object_new(sizeof(function_t), typeid(TYPE_FUNCTION), allocator);
    function->signature = typeid(TYPE_FUNCTION);
    chunk_init(&function->chunk, allocator);
    function->binded_name = NULL;
    function->file_defined_in = file_defined_in;

    return function;
}

bool is_function_compiled(function_t* function) {
    return function->chunk.code.items != NULL;
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
