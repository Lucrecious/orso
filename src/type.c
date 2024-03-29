#include "type.h"
#include "type_set.h"

bool orso_union_type_has_type(const OrsoUnionType* type, OrsoType* subtype) {
    for (i32 i = 0; i < type->count; i++) {
        if (type->types[i] == subtype) {
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
        OrsoUnionType* a_union = (OrsoUnionType*)a;
        OrsoUnionType* b_union = (OrsoUnionType*)b;
        if (a_union->count != b_union->count) {
            return false;
        }

        for (i32 i = 0; i < a_union->count; i++) {
            if (!orso_union_type_has_type(a_union, b_union->types[i])) {
                return false;
            }

            if (!orso_union_type_has_type(b_union, a_union->types[i])) {
                return false;
            }
        }
    } else if (a->kind == ORSO_TYPE_FUNCTION) {
        OrsoFunctionType* a_function = (OrsoFunctionType*)a;
        OrsoFunctionType* b_function = (OrsoFunctionType*)b;

        if (a_function->argument_count != b_function->argument_count) {
            return false;
        }

        if (!orso_type_equal(a_function->return_type, b_function->return_type)) {
            return false;
        }

        for (i32 i = 0; i < a_function->argument_count; i++) {
            if (!orso_type_equal(a_function->argument_types[i], b_function->argument_types[i])) {
                return false;
            }
        }
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

OrsoType* orso_type_merge(OrsoTypeSet* set, OrsoType* a, OrsoType* b) {
    if (a == b) {
        return a;
    }

    i32 count = 0;
    OrsoType* types[ORSO_UNION_NUM_MAX];

    if (ORSO_TYPE_IS_UNION(a)) {
        OrsoUnionType const * a_union = (OrsoUnionType const *)a;
        for (i32 i = 0; i < a_union->count; i++) {
            types[count++] = a_union->types[i];
        }
    } else {
        types[count++] = a;
    }

    if (ORSO_TYPE_IS_UNION(b)) {
        OrsoUnionType const * b_union = (OrsoUnionType const *)b;
        for (i32 i = 0; i < b_union->count; i++) {
            if (type_in_list(types, count, b_union->types[i])) {
                continue;
            }

            types[count++] = b_union->types[i];
        }
    } else {
        if (!type_in_list(types, count, b)) {
            types[count++] = b;
        }
    }

    OrsoType* merged = orso_type_set_fetch_union(set, types, count);
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


bool orso_union_has_float(OrsoUnionType* type) {
    for (i32 i = 0; i < type->count; i++) {
        if (!orso_type_is_float(type->types[i])) {
            continue;
        }

        return true;
    }

    return false;
}

bool orso_type_is_or_has_float(OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return orso_union_has_float((OrsoUnionType*)type);
    } else {
        return orso_type_is_float(type);
    }
}

bool orso_union_has_integer(OrsoUnionType* type, bool include_bool) {
    for (i32 i = 0; i < type->count; i++) {
        if (!orso_type_is_integer(type->types[i], include_bool)) {
            continue;
        }

        return true;
    }

    return false;
}

bool orso_type_is_or_has_integer(OrsoType* type, bool include_bool) {
    if (ORSO_TYPE_IS_UNION(type)) {
        return orso_union_has_integer((OrsoUnionType*)type, include_bool);
    } else {
        return orso_type_is_integer(type, include_bool);
    }
}

i32 orso_type_bits(OrsoType* type) {
    if (ORSO_TYPE_IS_UNION(type)) {
        OrsoUnionType* union_type = (OrsoUnionType*)type;
        i32 total = 0;
        for (i32 i = 0; i < union_type->count; i++) {
            total += orso_type_bits(union_type->types[i]);
        }

        return total;
    }

    switch (type->kind) {
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
            return 64;

        case ORSO_TYPE_INVALID:
        case ORSO_TYPE_UNRESOLVED:
        default: UNREACHABLE();
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
        OrsoUnionType const * storage_union = (OrsoUnionType*)storage_type;
        if (ORSO_TYPE_IS_UNION(value_type)) {
            OrsoUnionType const * value_union = (OrsoUnionType*)value_type;
            for (i32 i = 0; i < value_union->count; i++) {
                if (!orso_type_fits((OrsoType*)storage_union, value_union->types[i])) {
                    return false;
                }
            }

            return true;
        } else {
            for (i32 i = 0; i < storage_union->count; i++) {
                if (orso_type_fits(storage_union->types[i], value_type)) {
                    return true;
                }
            }
        }

        return false;
    }

    // functions must match exactly to fit into a variable
    if (storage_type->kind == ORSO_TYPE_FUNCTION) {
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
        OrsoUnionType* union_type = (OrsoUnionType*)type;

        for (i32 i = 0; i < union_type->count; i++) {
            if (i != 0) {
                buffer[0] = '|';
                n -= 1;
                buffer += 1;
            }

            // -1 removes the \0
            i32 written = orso_type_to_cstrn(union_type->types[i], buffer, n) - 1;
            n -= written;
            buffer += written;
        }
    // (arg1_type, arg2_type, ..., argn_type) -> return_type
    } else if (type->kind == ORSO_TYPE_FUNCTION) {
        buffer[0] = '(';
        n -= 1;
        buffer += 1;

        OrsoFunctionType* function_type = (OrsoFunctionType*)type;
        for (i32 i = 0; i < function_type->argument_count; i++) {
            if (i != 0) {
                buffer[0] = ',';
                n -= 1;
                buffer += 1;
            }

            i32 written = orso_type_to_cstrn(function_type->argument_types[i], buffer, n);
            n -= (written - 1);
            buffer += (written - 1);
        }

        i32 written = copy_to_buffer(buffer, ") -> ");
        n -= written;
        buffer += written;

        written = orso_type_to_cstrn(function_type->return_type, buffer, n);
        n -= (written - 1);
        buffer += (written - 1);

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
            case ORSO_TYPE_INVALID: type_name = "<invalid>"; break;
            case ORSO_TYPE_UNRESOLVED: type_name = "<unresolved>"; break;
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

    if (type->kind == ORSO_TYPE_FUNCTION) {
        return true;
    }

    if (ORSO_TYPE_IS_UNION(type)) {
        OrsoUnionType* union_type = (OrsoUnionType*)type;
        for (i32 i = 0; i < union_type->count; i++) {
            if (orso_is_gc_type(union_type->types[i])) {
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

    if (a->kind == ORSO_TYPE_FUNCTION || b->kind == ORSO_TYPE_FUNCTION) {
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
    || (a->kind == ORSO_TYPE_SYMBOL && b->kind == ORSO_TYPE_SYMBOL)) {
        *a_cast = a;
        *b_cast = b;
        return;
    }

    *a_cast = &OrsoTypeInvalid;
    *b_cast = &OrsoTypeInvalid;
}
