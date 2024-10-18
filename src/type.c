#include "type.h"

#include "symbol_table.h"
#include "type_set.h"

#include "sb.h"

bool orso_union_type_contains_type(type_t *union_, type_t *type) {
    if (!ORSO_TYPE_IS_UNION(type)) {
        return orso_union_type_has_type(union_, type);
    }

    for (size_t i = 0; i < type->data.union_.types.count; ++i){
        if (!orso_union_type_has_type(union_, type->data.union_.types.items[i])) {
            return false;
        }
    }

    return true;
}

bool orso_union_type_has_type(type_t *type, type_t *subtype) {
    ASSERT(type->kind == ORSO_TYPE_UNION, "must be a union type");

    for (size_t i = 0; i < type->data.union_.types.count; ++i) {
        if (type->data.union_.types.items[i] == subtype) {
            return true;
        }
    }

    return false;
}
bool orso_struct_type_is_incomplete(type_t *type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        for (size_t i = 0; i < type->data.union_.types.count; ++i) {
            unless (orso_struct_type_is_incomplete(type->data.union_.types.items[i])) {
                continue;
            }

            return true;
        }
    }
    return ORSO_TYPE_IS_STRUCT(type) && type->data.struct_.field_count < 0;
}

bool orso_type_equal(type_t *a, type_t *b) {
    if (a->kind != b->kind) {
        return false;
    }

    switch (a->kind) {
        case ORSO_TYPE_UNION: {
            if (a->data.union_.types.count != b->data.union_.types.count) {
                return false;
            }

            for (size_t i = 0; i < a->data.union_.types.count; ++i) {
                if (!orso_union_type_has_type(a, b->data.union_.types.items[i])) {
                    return false;
                }

                if (!orso_union_type_has_type(b, a->data.union_.types.items[i])) {
                    return false;
                }
            }

            return true;
        }

        case ORSO_TYPE_FUNCTION:
        case ORSO_TYPE_NATIVE_FUNCTION: {
            if (a->data.function.argument_types.count != b->data.function.argument_types.count) {
                return false;
            }

            if (a->data.function.return_type != b->data.function.return_type) {
                return false;
            }

            for (size_t i = 0; i < a->data.function.argument_types.count; ++i) {
                if (a->data.function.argument_types.items[i] != b->data.function.argument_types.items[i]) {
                    return false;
                }
            }

            return true;
        }
        case ORSO_TYPE_STRUCT: {
            i32 a_name_length = a->data.struct_.name ? strlen(a->data.struct_.name) : 0;
            i32 b_name_length = b->data.struct_.name ? strlen(b->data.struct_.name) : 0;
            if (a_name_length != b_name_length) {
                return false;
            }

            if (memcmp(a->data.struct_.name, b->data.struct_.name, a_name_length) != 0) {
                return false;
            }

            if (a->data.struct_.field_count != b->data.struct_.field_count) {
                return false;
            }

            for (i32 i = 0; i < a->data.struct_.field_count; i++) {
                if (a->data.struct_.fields[i].type != b->data.struct_.fields[i].type) {
                    return false;
                }

                i32 field_name_length_a = strlen(a->data.struct_.fields[i].name);
                i32 field_name_length_b = strlen(b->data.struct_.fields[i].name);
                if (field_name_length_a != field_name_length_b) {
                    return false;
                }

                if (memcmp(a->data.struct_.fields[i].name, b->data.struct_.fields[i].name, field_name_length_a) != 0) {
                    return false;
                }
            }

            return true;
        }

        case ORSO_TYPE_POINTER: {
            return a->data.pointer.type == b->data.pointer.type;
        }

        case ORSO_TYPE_BOOL:
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64:
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64:
        case ORSO_TYPE_STRING:
        case ORSO_TYPE_VOID:
        case ORSO_TYPE_TYPE:
        case ORSO_TYPE_SYMBOL: return true;

        case ORSO_TYPE_INVALID:
        case ORSO_TYPE_UNDEFINED:
        case ORSO_TYPE_UNRESOLVED: UNREACHABLE(); return false;

    }
}

bool type_in_list(types_t list, type_t* find) {
    for (size_t i = 0; i < list.count; i++) {
        if (list.items[i] == find) {
            return true;
        }
    }

    return false;
}

// TODO: make this faster... Preferably type_in_list should be O(1)
type_t *orso_type_merge(type_set_t *set, type_t *a, type_t *b) {
    if (a == b) {
        return a;
    }

    arena_t tmp_allocator={0};
    types_t types = {.allocator=&tmp_allocator};

    if (ORSO_TYPE_IS_UNION(a)) {
        for (size_t i = 0; i < a->data.union_.types.count; ++i) {
            array_push(&types, a->data.union_.types.items[i]);
        }
    } else {
        array_push(&types, a);
    }

    if (ORSO_TYPE_IS_UNION(b)) {
        for (size_t i = 0; i < b->data.union_.types.count; ++i) {
            if (type_in_list(types, b->data.union_.types.items[i])) {
                continue;
            }

            array_push(&types, b->data.union_.types.items[i]);
        }
    } else {
        unless (type_in_list(types, b)) {
            array_push(&types, b);
        }
    }

    type_t *merged = orso_type_set_fetch_union(set, types);

    arena_free(&tmp_allocator);

    return merged;
}

bool orso_type_is_float(type_t *type) {
    switch (type->kind) {
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: return true;

        default: return false;
    }
}

bool orso_type_is_integer(type_t *type, bool include_bool) {
    if (include_bool && type->kind == ORSO_TYPE_BOOL) {
        return true;
    }

    switch (type->kind) {
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: return true;

        default: return false;
    }
}

bool orso_type_is_number(type_t *type, bool include_bool) {
    return orso_type_is_float(type) || orso_type_is_integer(type, include_bool);
}


bool orso_union_has_float(type_t *type) {
    ASSERT(ORSO_TYPE_IS_UNION(type), "must be union type");

    for (size_t i = 0; i < type->data.union_.types.count; ++i) {
        if (!orso_type_is_float(type->data.union_.types.items[i])) {
            continue;
        }

        return true;
    }

    return false;
}

bool orso_type_is_or_has_float(type_t *type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return orso_union_has_float(type);
    } else {
        return orso_type_is_float(type);
    }
}

bool orso_union_has_integer(type_t* type, bool include_bool) {
    ASSERT(ORSO_TYPE_IS_UNION(type), "must be union type");

    for (size_t i = 0; i < type->data.union_.types.count; ++i) {
        if (!orso_type_is_integer(type->data.union_.types.items[i], include_bool)) {
            continue;
        }

        return true;
    }

    return false;
}

bool orso_type_is_or_has_integer(type_t* type, bool include_bool) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return orso_union_has_integer(type, include_bool);
    } else {
        return orso_type_is_integer(type, include_bool);
    }
}

i32 orso_bytes_to_slots(i32 byte_count) {
    if (byte_count == 0) {
        return 1;
    }

    return (byte_count / sizeof(slot_t)) + ((byte_count % sizeof(slot_t) != 0));
}

struct_field_t* orso_type_struct_find_field(type_t* struct_, const char* name, size_t name_length) {
    for (i32 i = 0; i < struct_->data.struct_.field_count; i++) {
        if (strlen(struct_->data.struct_.fields[i].name) != name_length) {
            continue;
        }

        if (strncmp(struct_->data.struct_.fields[i].name, name, name_length) != 0) {
            continue;
        }

        return struct_->data.struct_.fields + i;
    }

    return NULL;
}

u32 orso_type_size_bytes(type_t* type) {
    switch (type->kind) {
        case ORSO_TYPE_UNION: {
            // take the max amount of bytes that value can take up
            i32 total = 0;
            for (size_t i = 0; i < type->data.union_.types.count; ++i) {
                total = maxi32(orso_type_size_bytes(type->data.union_.types.items[i]), total);
            }

            return orso_bytes_to_slots(total + sizeof(type_t*)) * sizeof(slot_t);
        }

        case ORSO_TYPE_VOID:
            return 0;

        case ORSO_TYPE_BOOL:
            return 1;

        case ORSO_TYPE_INT64:
        case ORSO_TYPE_FLOAT64:
            return 8;

        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_INT32:
            return 4;

        case ORSO_TYPE_STRING:
        case ORSO_TYPE_SYMBOL:
        case ORSO_TYPE_TYPE:
        case ORSO_TYPE_FUNCTION:
        case ORSO_TYPE_NATIVE_FUNCTION:
        case ORSO_TYPE_POINTER:
            return 8;
        
        case ORSO_TYPE_STRUCT: {
            return type->data.struct_.total_bytes;
        }

        case ORSO_TYPE_INVALID:
        case ORSO_TYPE_UNDEFINED:
        case ORSO_TYPE_UNRESOLVED:
            UNREACHABLE();
    }

    return 0;
}

bool orso_integer_fit(type_t *storage_type, type_t *value_type, bool include_bool) {
    if (ORSO_TYPE_IS_UNION(storage_type)) {
        return false;
    }

    if (ORSO_TYPE_IS_UNION(value_type)) {
        return false;
    }

    if (!orso_type_is_integer(storage_type, include_bool)) {
        return false;
    }

    if (!orso_type_is_integer(value_type, include_bool)) {
        return false;
    }

    return orso_type_size_bytes(storage_type) >= orso_type_size_bytes(value_type);
}

i32 orso_type_slot_count(type_t* type) {
    return orso_bytes_to_slots(orso_type_size_bytes(type));
}

bool orso_type_fits(type_t* storage_type, type_t* value_type) {
    if (storage_type == value_type) {
        return true;
    }

    if (!ORSO_TYPE_IS_UNION(storage_type) && ORSO_TYPE_IS_UNION(value_type)) {
        return false;
    }

    if (orso_integer_fit(storage_type, value_type, true)) {
        return true;
    }

    if (ORSO_TYPE_IS_UNION(storage_type)) {
        if (ORSO_TYPE_IS_UNION(value_type)) {
            for (size_t i = 0; i < value_type->data.union_.types.count; ++i) {
                if (!orso_type_fits(storage_type, value_type->data.union_.types.items[i])) {
                    return false;
                }
            }

            return true;
        } else {
            for (size_t i = 0; i < storage_type->data.union_.types.count; ++i) {
                if (orso_type_fits(storage_type->data.union_.types.items[i], value_type)) {
                    return true;
                }
            }
        }

        return false;
    }

    // functions must match exactly to fit into a variable
    if (ORSO_TYPE_IS_FUNCTION(storage_type)) {
        return storage_type == value_type;
    }

    return false;
}

i32 copy_to_buffer(char* buffer, char* cstr) {
    i32 written = strlen(cstr);
    memcpy(buffer, cstr, written);
    return written;
}

string_t type_to_string_toplevel(type_t *type, arena_t *allocator, bool is_toplevel) {
    string_builder_t sb = {.allocator = allocator};

    // type1|type2|type3|type4
    if (ORSO_TYPE_IS_UNION(type)) {
        for (size_t i = 0; i < type->data.union_.types.count; ++i) {
            if (i != 0) {
                sb_add_char(&sb, '|');
            }

            if (ORSO_TYPE_IS_FUNCTION(type->data.union_.types.items[i]) || type->data.union_.types.items[i]->kind == ORSO_TYPE_NATIVE_FUNCTION) {
                sb_add_char(&sb, '(');
            }

            string_t inner_type = type_to_string_toplevel(type->data.union_.types.items[i], allocator, false);
            sb_add_cstr(&sb, inner_type.cstr);

            if (ORSO_TYPE_IS_FUNCTION(type->data.union_.types.items[i]) || type->data.union_.types.items[i]->kind == ORSO_TYPE_NATIVE_FUNCTION) {
                sb_add_char(&sb, ')');
            }
        }
    // (arg1_type, arg2_type, ..., argn_type) -> return_type
    } else if (ORSO_TYPE_IS_FUNCTION(type) || type->kind == ORSO_TYPE_NATIVE_FUNCTION) {
        sb_add_char(&sb, '(');

        for (size_t i = 0; i < type->data.function.argument_types.count; ++i) {
            if (i != 0) {
                sb_add_char(&sb, ',');
            }

            string_t arg_type = type_to_string_toplevel(type->data.function.argument_types.items[i], allocator, false);
            sb_add_cstr(&sb, arg_type.cstr);
        }

        sb_add_cstr(&sb, ") -> ");

        string_t return_type = type_to_string_toplevel(type->data.function.return_type, allocator, false);
        sb_add_cstr(&sb, return_type.cstr);
    } else if (ORSO_TYPE_IS_STRUCT(type)) {
        if (type->data.struct_.name) {
            sb_add_cstr(&sb, type->data.struct_.name);
        } else {
            sb_add_cstr(&sb, "struct");
        }
        
        if (is_toplevel) {
            sb_add_cstr(&sb, " { ");

            for (i32 i = 0; i < type->data.struct_.field_count; i++) {
                char *name = type->data.struct_.fields[i].name;

                sb_add_cstr(&sb, name);
                sb_add_cstr(&sb, ": ");

                type_t *field_type = type->data.struct_.fields[i].type;
                string_t type_string = type_to_string_toplevel(field_type, allocator, false);
                sb_add_cstr(&sb, type_string.cstr);

                sb_add_cstr(&sb, "; ");
            }

            sb_add_char(&sb, '}');
        }
    } else if (ORSO_TYPE_IS_POINTER(type)) {
        sb_add_char(&sb, '&');

        string_t type_string = type_to_string_toplevel(type->data.pointer.type, allocator, false);
        sb_add_cstr(&sb, type_string.cstr);
    } else {
        char *type_name;
        switch (type->kind) {
            case ORSO_TYPE_BOOL: type_name = "bool"; break;
            case ORSO_TYPE_FLOAT32: type_name = "f32"; break;
            case ORSO_TYPE_FLOAT64: type_name = "f64"; break;
            case ORSO_TYPE_INT32: type_name = "i32"; break;
            case ORSO_TYPE_INT64: type_name = "i64"; break;
            case ORSO_TYPE_STRING: type_name = "string"; break;
            case ORSO_TYPE_SYMBOL: type_name = "symbol"; break;
            case ORSO_TYPE_VOID: type_name = "void"; break;
            case ORSO_TYPE_TYPE: type_name = "type"; break;
            case ORSO_TYPE_INVALID: type_name = "<invalid>"; break;
            case ORSO_TYPE_UNRESOLVED: type_name = "<unresolved>"; break;
            case ORSO_TYPE_UNDEFINED: type_name = "<undefined>"; break;
            
            case ORSO_TYPE_STRUCT:
            case ORSO_TYPE_FUNCTION:
            case ORSO_TYPE_NATIVE_FUNCTION:
            case ORSO_TYPE_POINTER:
            case ORSO_TYPE_UNION:
                type_name = "<?>"; UNREACHABLE(); break;
        }

        sb_add_cstr(&sb, type_name);
    }

    string_t string = sb_render(&sb, allocator);

    return string;
}

string_t type_to_string(type_t *type, arena_t *allocator) {
    return type_to_string_toplevel(type, allocator, true);
}

bool orso_is_gc_type(type_t* type) {
    if (type->kind == ORSO_TYPE_STRING) {
        return true;
    }

    if (type->kind == ORSO_TYPE_SYMBOL) {
        return true;
    }

    if (ORSO_TYPE_IS_FUNCTION(type)) {
        return true;
    }

    if (ORSO_TYPE_IS_UNION(type)) {
        for (size_t i = 0; i < type->data.union_.types.count; ++i) {
            if (orso_is_gc_type(type->data.union_.types.items[i])) {
                return true;
            }
        }
    }

    return false;
}

bool can_cast_implicit(type_t *type_to_cast, type_t *type) {
    if (ORSO_TYPE_IS_INVALID(type_to_cast)) return false;
    if (ORSO_TYPE_IS_INVALID(type)) return false;

    if (type_to_cast == type) return true;

    if (ORSO_TYPE_IS_UNION(type)) {
        return orso_union_type_contains_type(type, type_to_cast);
    }

    if (ORSO_TYPE_IS_UNION(type_to_cast)) return false;

    u32 type_to_cast_size = orso_type_size_bytes(type_to_cast);
    u32 type_size = orso_type_size_bytes(type);
    if (orso_type_is_number(type_to_cast, true) && orso_type_is_number(type, true) && type_to_cast_size <= type_size) {
        if (orso_type_is_integer(type_to_cast, true) || (orso_type_is_float(type_to_cast) && orso_type_is_float(type))) {
            return true;
        }
    }

    return false;
}

type_t *orso_binary_arithmetic_cast(type_t *a, type_t *b, token_type_t operation) {
    if (ORSO_TYPE_IS_UNION(a) || ORSO_TYPE_IS_UNION(b)) {
        return &OrsoTypeInvalid;
    }

    if (ORSO_TYPE_IS_FUNCTION(a) || ORSO_TYPE_IS_FUNCTION(b)) {
        return &OrsoTypeInvalid;
    }

    if (operation == TOKEN_PLUS && a->kind == ORSO_TYPE_STRING && b->kind == ORSO_TYPE_STRING) {
        return a;
    }

    if (!orso_type_is_number(a, true)) {
        return &OrsoTypeInvalid;
    }

    if (!orso_type_is_number(b, true)) {
        return &OrsoTypeInvalid;
    }

    i32 a_count = orso_type_size_bytes(a);
    i32 b_count = orso_type_size_bytes(b);

    if (a_count == 0 || b_count == 0) {
        return &OrsoTypeInvalid;
    }

    if (a->kind == ORSO_TYPE_BOOL && b->kind == ORSO_TYPE_BOOL) {
        return &OrsoTypeInteger32;
    }

    bool include_bool = true;
    bool is_same_integer_types = (orso_type_is_integer(a, include_bool)) && (orso_type_is_integer(b, include_bool));
    bool is_same_float_types = orso_type_is_float(a) && orso_type_is_float(b);

    if (is_same_integer_types || is_same_float_types) {
        return a_count > b_count ? a : b;
    }

    return a_count > 4 || b_count > 4 ? &OrsoTypeFloat64 : &OrsoTypeFloat32;
}

void orso_binary_comparison_casts(type_t *a, type_t *b, type_t **a_cast, type_t **b_cast) {
    if (ORSO_TYPE_IS_UNION(a) || ORSO_TYPE_IS_UNION(b)) {
        *a_cast = &OrsoTypeInvalid;
        *b_cast = &OrsoTypeInvalid;
        return;
    }

    bool include_bool = false;
    if (orso_type_is_number(a, include_bool) && orso_type_is_number(b, include_bool)) {
        if (orso_type_is_integer(a, include_bool) && orso_type_is_integer(b, include_bool)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }
        
        if (orso_type_is_float(a) && orso_type_is_float(b)) {
            *a_cast = a;
            *b_cast = b;
            return;
        }

        i32 a_count = orso_type_size_bytes(a);
        i32 b_count = orso_type_size_bytes(b);

        if (a_count <= 32 && b_count <= 32) {
            *a_cast = &OrsoTypeFloat32;
            *b_cast = &OrsoTypeFloat32;
            return;
        }

        *a_cast = &OrsoTypeFloat64;
        *b_cast = &OrsoTypeFloat64;
        return;
    }

    *a_cast = &OrsoTypeInvalid;
    *b_cast = &OrsoTypeInvalid;
}

void orso_binary_equality_casts(type_t *a, type_t *b, type_t **a_cast, type_t **b_cast) {
    if (ORSO_TYPE_IS_UNION(a) || ORSO_TYPE_IS_UNION(b)) {
        *a_cast = &OrsoTypeInvalid;
        *b_cast = &OrsoTypeInvalid;
        return;
    }

    bool include_bool = false;
    if (orso_type_is_number(a, include_bool) && orso_type_is_number(b, include_bool)) {
        orso_binary_comparison_casts(a, b, a_cast, b_cast);
        return;
    }

    if ((a->kind == ORSO_TYPE_BOOL && b->kind == ORSO_TYPE_BOOL)
    || (a->kind == ORSO_TYPE_STRING && b->kind == ORSO_TYPE_STRING)
    || (a->kind == ORSO_TYPE_SYMBOL && b->kind == ORSO_TYPE_SYMBOL)
    || (a->kind == ORSO_TYPE_TYPE && b->kind == ORSO_TYPE_TYPE)) {
        *a_cast = a;
        *b_cast = b;
        return;
    }

    *a_cast = &OrsoTypeInvalid;
    *b_cast = &OrsoTypeInvalid;
}
