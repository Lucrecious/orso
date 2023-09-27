#include "type.h"

#include "symbol_table.h"
#include "type_set.h"

#include "sb.h"

bool orso_union_type_has_type(OrsoType* type, OrsoType* subtype) {
    ASSERT(type->kind == ORSO_TYPE_UNION, "must be a union type");

    for (i32 i = 0; i < type->type.union_.count; i++) {
        if (type->type.union_.types[i] == subtype) {
            return true;
        }
    }

    return false;
}

bool orso_type_equal(OrsoType* a, OrsoType* b) {
    if (a->kind != b->kind) {
        return false;
    }

    if (ORSO_TYPE_IS_UNION(a)) {
        if (a->type.union_.count != b->type.union_.count) {
            return false;
        }

        for (i32 i = 0; i < a->type.union_.count; i++) {
            if (!orso_union_type_has_type(a, b->type.union_.types[i])) {
                return false;
            }

            if (!orso_union_type_has_type(b, a->type.union_.types[i])) {
                return false;
            }
        }
    } else if (ORSO_TYPE_IS_FUNCTION(a)) {
        if (a->type.function.argument_count != b->type.function.argument_count) {
            return false;
        }

        if (!orso_type_equal(a->type.function.return_type, b->type.function.return_type)) {
            return false;
        }

        for (i32 i = 0; i < a->type.function.argument_count; i++) {
            if (!orso_type_equal(a->type.function.argument_types[i], b->type.function.argument_types[i])) {
                return false;
            }
        }
    } else if (ORSO_TYPE_IS_STRUCT(a)) {
        i32 a_name_length = strlen(a->type.struct_.name);
        i32 b_name_length = strlen(b->type.struct_.name);
        if (a_name_length != b_name_length) {
            return false;
        }

        if (memcmp(a->type.struct_.name, b->type.struct_.name, a_name_length) != 0) {
            return false;
        }

        if (a->type.struct_.field_count != b->type.struct_.field_count) {
            return false;
        }

        for (i32 i = 0; i < a->type.struct_.field_count; i++) {
            if (!orso_type_equal(a->type.struct_.field_types[i], b->type.struct_.field_types[i])) {
                return false;
            }

            i32 field_name_length_a = strlen(a->type.struct_.field_names[i]);
            i32 field_name_length_b = strlen(b->type.struct_.field_names[i]);
            if (field_name_length_a != field_name_length_b) {
                return false;
            }

            if (memcmp(a->type.struct_.field_names[i], b->type.struct_.field_names[i], field_name_length_a) != 0) {
                return false;
            }
        }
    } else {
        return false;
    }

    return true;
}

bool type_in_list(OrsoType** list, i32 count, OrsoType* find) {
    for (i32 i = 0; i < count; i++) {
        if (list[i] == find) {
            return true;
        }
    }

    return false;
}

// TODO: make this faster... Preferably type_in_list should be O(1)
OrsoType* orso_type_merge(OrsoTypeSet* set, OrsoType* a, OrsoType* b) {
    if (a == b) {
        return a;
    }

    OrsoType** types = NULL;

    if (ORSO_TYPE_IS_UNION(a)) {
        for (i32 i = 0; i < a->type.union_.count; i++) {
            sb_push(types, a->type.union_.types[i]);
        }
    } else {
        sb_push(types, a);
    }

    if (ORSO_TYPE_IS_UNION(b)) {
        for (i32 i = 0; i < b->type.union_.count; i++) {
            if (type_in_list(types, sb_count(types), b->type.union_.types[i])) {
                continue;
            }

            sb_push(types, b->type.union_.types[i]);
        }
    } else {
        if (!type_in_list(types, sb_count(types), b)) {
            sb_push(types, b);
        }
    }

    OrsoType* merged = orso_type_set_fetch_union(set, types, sb_count(types));

    sb_free(types);

    return merged;
}

bool orso_type_is_float(OrsoType* type) {
    switch (type->kind) {
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64: return true;

        default: return false;
    }
}

bool orso_type_is_integer(OrsoType* type, bool include_bool) {
    if (include_bool && type->kind == ORSO_TYPE_BOOL) {
        return true;
    }

    switch (type->kind) {
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64: return true;

        default: return false;
    }
}

bool orso_type_is_number(OrsoType* type, bool include_bool) {
    return orso_type_is_float(type) || orso_type_is_integer(type, include_bool);
}


bool orso_union_has_float(OrsoType* type) {
    ASSERT(ORSO_TYPE_IS_UNION(type), "must be union type");

    for (i32 i = 0; i < type->type.union_.count; i++) {
        if (!orso_type_is_float(type->type.union_.types[i])) {
            continue;
        }

        return true;
    }

    return false;
}

bool orso_type_is_or_has_float(OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return orso_union_has_float(type);
    } else {
        return orso_type_is_float(type);
    }
}

bool orso_union_has_integer(OrsoType* type, bool include_bool) {
    ASSERT(ORSO_TYPE_IS_UNION(type), "must be union type");

    for (i32 i = 0; i < type->type.union_.count; i++) {
        if (!orso_type_is_integer(type->type.union_.types[i], include_bool)) {
            continue;
        }

        return true;
    }

    return false;
}

bool orso_type_is_or_has_integer(OrsoType* type, bool include_bool) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return orso_union_has_integer(type, include_bool);
    } else {
        return orso_type_is_integer(type, include_bool);
    }
}

i32 orso_type_bits(OrsoType* type) {
    switch (type->kind) {
        case ORSO_TYPE_UNION: {
            // take the max amount of bytes that value can take up
            i32 total = 0;
            for (i32 i = 0; i < type->type.union_.count; i++) {
                total = i32max(orso_type_bits(type->type.union_.types[i]), total);
            }

            // Add 64 for the type
            return total + 64;
        }

        case ORSO_TYPE_VOID:
            return 0;

        case ORSO_TYPE_BOOL:
            return 1;

        case ORSO_TYPE_INT64:
        case ORSO_TYPE_FLOAT64:
            return 64;

        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_INT32:
            return 32;

        case ORSO_TYPE_STRING:
        case ORSO_TYPE_SYMBOL:
        case ORSO_TYPE_TYPE:
        case ORSO_TYPE_FUNCTION:
        case ORSO_TYPE_NATIVE_FUNCTION:
            return 64;
        
        case ORSO_TYPE_STRUCT: {
            ASSERT(type != &OrsoTypeIncompleteStruct, "not allowed incomplete structs in here");

            i32 bit_size = 0;
            for (i32 i = 0; i < type->type.struct_.field_count; i++) {
                i32 field_size = orso_type_bits(type->type.struct_.field_types[i]);
                bit_size += (field_size / 64 + (field_size % 64 != 0)) * 64;
            }

            return bit_size;
        }

        case ORSO_TYPE_INVALID:
        case ORSO_TYPE_UNDEFINED:
        case ORSO_TYPE_UNRESOLVED:
            UNREACHABLE();
    }

    return 0;
}

bool orso_integer_fit(OrsoType* storage_type, OrsoType* value_type, bool include_bool) {
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

    return orso_type_bits(storage_type) >= orso_type_bits(value_type);
}

i32 orso_type_slot_count(OrsoType* type) {
    i32 slot_size = 1;
    if (ORSO_TYPE_IS_UNION(type)) {
        slot_size = 2;
    }

    return slot_size;
}

bool orso_type_fits(OrsoType* storage_type, OrsoType* value_type) {
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
            for (i32 i = 0; i < value_type->type.union_.count; i++) {
                if (!orso_type_fits(storage_type, value_type->type.union_.types[i])) {
                    return false;
                }
            }

            return true;
        } else {
            for (i32 i = 0; i < storage_type->type.union_.count; i++) {
                if (orso_type_fits(storage_type->type.union_.types[i], value_type)) {
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

i32 orso_type_to_cstrn(OrsoType* type, char* buffer, i32 n) {
    // TODO: Make sure that n is taken into account
    i32 original_n = n;

    // type1|type2|type3|type4
    if (ORSO_TYPE_IS_UNION(type)) {
        for (i32 i = 0; i < type->type.union_.count; i++) {
            if (i != 0) {
                buffer[0] = '|';
                n -= 1;
                buffer += 1;
            }

            if (ORSO_TYPE_IS_FUNCTION(type->type.union_.types[i]) || type->type.union_.types[i]->kind == ORSO_TYPE_NATIVE_FUNCTION) {
                buffer[0] = '(';
                n -= 1;
                buffer += 1;
            }

            // -1 removes the \0
            i32 written = orso_type_to_cstrn(type->type.union_.types[i], buffer, n) - 1;
            n -= written;
            buffer += written;

            if (ORSO_TYPE_IS_FUNCTION(type->type.union_.types[i]) || type->type.union_.types[i]->kind == ORSO_TYPE_NATIVE_FUNCTION) {
                buffer[0] = ')';
                n -= 1;
                buffer += 1;
            }
        }
    // (arg1_type, arg2_type, ..., argn_type) -> return_type
    } else if (ORSO_TYPE_IS_FUNCTION(type) || type->kind == ORSO_TYPE_NATIVE_FUNCTION) {
        buffer[0] = '(';
        n -= 1;
        buffer += 1;

        for (i32 i = 0; i < type->type.function.argument_count; i++) {
            if (i != 0) {
                buffer[0] = ',';
                n -= 1;
                buffer += 1;
            }

            i32 written = orso_type_to_cstrn(type->type.function.argument_types[i], buffer, n);
            n -= (written - 1);
            buffer += (written - 1);
        }

        i32 written = copy_to_buffer(buffer, ") -> ");
        n -= written;
        buffer += written;

        written = orso_type_to_cstrn(type->type.function.return_type, buffer, n);
        n -= (written - 1);
        buffer += (written - 1);

    } else if (ORSO_TYPE_IS_STRUCT(type)) {
        i32 written = 0;
        if (type->type.struct_.name) {
            written = copy_to_buffer(buffer, type->type.struct_.name);
            n -= written;
            buffer += written;

            buffer[0] = ' ';
            n -= 1;
            buffer += 1;
        }

        char* struct_prefix = "{ ";
        written = copy_to_buffer(buffer, struct_prefix);
        n -= written;
        buffer += written;

        for (i32 i = 0; i < type->type.struct_.field_count; i++) {
            char* name = type->type.struct_.field_names[i];

            written = copy_to_buffer(buffer, name);
            n -= written;
            buffer += written;

            char* type_colin = ": ";
            written = copy_to_buffer(buffer, type_colin);
            n -= written;
            buffer += written;

            OrsoType* field_type = type->type.struct_.field_types[i];
            written = orso_type_to_cstrn(field_type, buffer, n);
            n -= (written - 1);
            buffer += (written - 1);

            char* ending_colin = "; ";
            written = copy_to_buffer(buffer, ending_colin);
            n -= written;
            buffer += written;
        }

        buffer[0] = '}';
        n -= 1;
        buffer += 1;
    } else {
        char* type_name;
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
            default: type_name = "<?>"; UNREACHABLE(); break;
        }

        i32 written = copy_to_buffer(buffer, type_name);
        n -= written;
        buffer += written;
    }

    buffer[0] = '\0';
    n -= 1;
    buffer += 1;
    return original_n - n;
}

bool orso_is_gc_type(OrsoType* type) {
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
        for (i32 i = 0; i < type->type.union_.count; i++) {
            if (orso_is_gc_type(type->type.union_.types[i])) {
                return true;
            }
        }
    }

    return false;
}

OrsoType* orso_binary_arithmetic_cast(OrsoType* a, OrsoType* b, TokenType operation) {
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

    i32 a_count = orso_type_bits(a);
    i32 b_count = orso_type_bits(b);

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

    return a_count > 32 || b_count > 32 ? &OrsoTypeFloat64 : &OrsoTypeFloat32;
}

void orso_binary_comparison_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast) {
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

        i32 a_count = orso_type_bits(a);
        i32 b_count = orso_type_bits(b);

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

void orso_binary_equality_casts(OrsoType* a, OrsoType* b, OrsoType** a_cast, OrsoType** b_cast) {
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
