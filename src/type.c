#include "type.h"

#include "symbol_table.h"
#include "type_set.h"
#include "tmp.h"

bool union_type_contains_type(types_t types, type_id_t union_id, type_id_t type_id) {
    type_t *union_ = types.items[union_id.i];
    type_t *type = types.items[type_id.i];
    if (!type_is_union(types, type_id)) {
        return union_type_has_type(union_, type_id);
    }

    for (size_t i = 0; i < type->data.union_.types.count; ++i){
        if (!union_type_has_type(union_, type->data.union_.types.items[i])) {
            return false;
        }
    }

    return true;
}

bool union_type_has_type(type_t *type, type_id_t subtype_id) {
    ASSERT(type->kind == TYPE_UNION, "must be a union type");

    for (size_t i = 0; i < type->data.union_.types.count; ++i) {
        if (typeid_eq(type->data.union_.types.items[i], subtype_id)) {
            return true;
        }
    }

    return false;
}
bool struct_type_is_incomplete(type_t *type) {
    return type->kind == TYPE_STRUCT && type->data.struct_.field_count < 0;
}

bool type_equal(type_t *a, type_t *b) {
    if (a->kind != b->kind) {
        return false;
    }

    switch (a->kind) {
        case TYPE_UNION: {
            if (a->data.union_.types.count != b->data.union_.types.count) {
                return false;
            }

            for (size_t i = 0; i < a->data.union_.types.count; ++i) {
                if (!union_type_has_type(a, b->data.union_.types.items[i])) {
                    return false;
                }

                if (!union_type_has_type(b, a->data.union_.types.items[i])) {
                    return false;
                }
            }

            return true;
        }

        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION: {
            if (a->data.function.argument_types.count != b->data.function.argument_types.count) {
                return false;
            }

            unless (typeid_eq(a->data.function.return_type, b->data.function.return_type)) {
                return false;
            }

            for (size_t i = 0; i < a->data.function.argument_types.count; ++i) {
                unless (typeid_eq(a->data.function.argument_types.items[i], b->data.function.argument_types.items[i])) {
                    return false;
                }
            }

            return true;
        }
        case TYPE_STRUCT: {
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
                unless (typeid_eq(a->data.struct_.fields[i].type, b->data.struct_.fields[i].type)) {
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

        case TYPE_POINTER: {
            return typeid_eq(a->data.pointer.type, b->data.pointer.type);
        }

        case TYPE_BOOL:
        case TYPE_FLOAT32:
        case TYPE_FLOAT64:
        case TYPE_INT32:
        case TYPE_INT64:
        case TYPE_STRING:
        case TYPE_VOID:
        case TYPE_TYPE:
        case TYPE_SYMBOL: return true;

        case TYPE_COUNT:
        case TYPE_INVALID:
        case TYPE_UNDEFINED:
        case TYPE_UNRESOLVED: UNREACHABLE(); return false;

    }
}

bool type_id_in_list(type_ids_t list, type_id_t find_id) {
    for (size_t i = 0; i < list.count; i++) {
        if (typeid_eq(list.items[i], find_id)) {
            return true;
        }
    }

    return false;
}

// TODO: make this faster... Preferably type_in_list should be O(1)
type_id_t type_merge(type_table_t *set, type_id_t a_id, type_id_t b_id) {
    if (typeid_eq(a_id, b_id)) {
        return a_id;
    }

    type_t *a = set->types.items[a_id.i];
    type_t *b = set->types.items[b_id.i];

    tmp_arena_t *tmp = allocator_borrow();
    type_ids_t type_ids = {.allocator=tmp->allocator};

    if (type_is_union(set->types, a_id)) {
        for (size_t i = 0; i < a->data.union_.types.count; ++i) {
            array_push(&type_ids, a->data.union_.types.items[i]);
        }
    } else {
        array_push(&type_ids, a_id);
    }

    if (type_is_union(set->types, b_id)) {
        for (size_t i = 0; i < b->data.union_.types.count; ++i) {
            if (type_id_in_list(type_ids, b->data.union_.types.items[i])) {
                continue;
            }

            array_push(&type_ids, b->data.union_.types.items[i]);
        }
    } else {
        unless (type_id_in_list(type_ids, b_id)) {
            array_push(&type_ids, b_id);
        }
    }

    type_id_t merged_id = type_set_fetch_union(set, type_ids);

    allocator_return(tmp);

    return merged_id;
}

bool type_is_float(type_t *type) {
    switch (type->kind) {
        case TYPE_FLOAT32:
        case TYPE_FLOAT64: return true;

        default: return false;
    }
}

bool type_is_integer(type_t *type, bool include_bool) {
    if (include_bool && type->kind == TYPE_BOOL) {
        return true;
    }

    switch (type->kind) {
        case TYPE_INT32:
        case TYPE_INT64: return true;

        default: return false;
    }
}

bool type_is_number(type_t *type, bool include_bool) {
    return type_is_float(type) || type_is_integer(type, include_bool);
}

size_t bytes_to_slots(i32 byte_count) {
    if (byte_count == 0) {
        return 1;
    }

    return (byte_count / sizeof(slot_t)) + ((byte_count % sizeof(slot_t) != 0));
}

struct_field_t *type_struct_find_field(type_t *struct_, const char *name, size_t name_length) {
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

u32 type_size_bytes(type_t *type) {
    switch (type->kind) {
        case TYPE_UNION: {
            return 1;
        }

        case TYPE_VOID:
            return 0;

        case TYPE_BOOL:
            return 1;

        case TYPE_INT64:
        case TYPE_FLOAT64:
            return 8;

        case TYPE_FLOAT32:
        case TYPE_INT32:
            return 4;

        case TYPE_STRING:
        case TYPE_SYMBOL:
        case TYPE_TYPE:
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
            return 8;
        
        case TYPE_STRUCT: {
            return type->data.struct_.total_bytes;
        }

        case TYPE_COUNT:
        case TYPE_INVALID:
        case TYPE_UNDEFINED:
        case TYPE_UNRESOLVED:
            UNREACHABLE();
    }

    return 0;
}

size_t type_slot_count(type_t *type) {
    return bytes_to_slots(type_size_bytes(type));
}

bool type_fits(type_t* storage_type, type_t* value_type) {
    UNUSED(storage_type);
    UNUSED(value_type);
    return false;
}

string_t type_to_string_toplevel(types_t types, type_id_t type_id, arena_t *allocator, bool is_toplevel) {
    string_builder_t sb = {.allocator = allocator};

    type_t *type = types.items[type_id.i];

    // type1|type2|type3|type4
    if (type_is_union(types, type_id)) {
        for (size_t i = 0; i < type->data.union_.types.count; ++i) {
            if (i != 0) {
                sb_add_char(&sb, '|');
            }

            if (type_is_function(types, type->data.union_.types.items[i]) || type_is_native_function(types, type->data.union_.types.items[i])) {
                sb_add_char(&sb, '(');
            }

            string_t inner_type = type_to_string_toplevel(types, type->data.union_.types.items[i], allocator, false);
            sb_add_cstr(&sb, inner_type.cstr);

            if (type_is_function(types, type->data.union_.types.items[i]) || type_is_native_function(types, type->data.union_.types.items[i])) {
                sb_add_char(&sb, ')');
            }
        }
    // (arg1_type, arg2_type, ..., argn_type) -> return_type
    } else if (type_is_function(types, type_id) || type_is_native_function(types, type_id)) {
        sb_add_char(&sb, '(');

        for (size_t i = 0; i < type->data.function.argument_types.count; ++i) {
            if (i != 0) {
                sb_add_char(&sb, ',');
            }

            string_t arg_type = type_to_string_toplevel(types, type->data.function.argument_types.items[i], allocator, false);
            sb_add_cstr(&sb, arg_type.cstr);
        }

        sb_add_cstr(&sb, ") -> ");

        string_t return_type = type_to_string_toplevel(types, type->data.function.return_type, allocator, false);
        sb_add_cstr(&sb, return_type.cstr);
    } else if (type_is_struct(types, type_id)) {
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

                type_id_t field_type_id = type->data.struct_.fields[i].type;
                string_t type_string = type_to_string_toplevel(types, field_type_id, allocator, false);
                sb_add_cstr(&sb, type_string.cstr);

                sb_add_cstr(&sb, "; ");
            }

            sb_add_char(&sb, '}');
        }
    } else if (type_is_pointer(types, type_id)) {
        sb_add_char(&sb, '&');

        string_t type_string = type_to_string_toplevel(types, type->data.pointer.type, allocator, false);
        sb_add_cstr(&sb, type_string.cstr);
    } else {
        char *type_name;
        switch (type->kind) {
            case TYPE_BOOL: type_name = "bool"; break;
            case TYPE_FLOAT32: type_name = "f32"; break;
            case TYPE_FLOAT64: type_name = "f64"; break;
            case TYPE_INT32: type_name = "i32"; break;
            case TYPE_INT64: type_name = "i64"; break;
            case TYPE_STRING: type_name = "string"; break;
            case TYPE_SYMBOL: type_name = "symbol"; break;
            case TYPE_VOID: type_name = "void"; break;
            case TYPE_TYPE: type_name = "type"; break;
            case TYPE_INVALID: type_name = "<invalid>"; break;
            case TYPE_UNRESOLVED: type_name = "<unresolved>"; break;
            case TYPE_UNDEFINED: type_name = "<undefined>"; break;
            
            case TYPE_COUNT:
            case TYPE_STRUCT:
            case TYPE_FUNCTION:
            case TYPE_NATIVE_FUNCTION:
            case TYPE_POINTER:
            case TYPE_UNION:
                type_name = "<?>"; UNREACHABLE(); break;
        }

        sb_add_cstr(&sb, type_name);
    }

    string_t string = sb_render(&sb, allocator);

    return string;
}

string_t type_to_string(types_t types, type_id_t type_id, arena_t *allocator) {
    return type_to_string_toplevel(types, type_id, allocator, true);
}

bool orso_is_gc_type(type_t *type) {
    UNUSED(type);
    return false;
}

bool can_cast_implicit(types_t types, type_id_t type_id_to_cast, type_id_t type_id) {

    if (TYPE_IS_INVALID(type_id_to_cast)) return false;
    if (TYPE_IS_INVALID(type_id)) return false;

    if (typeid_eq(type_id_to_cast, type_id)) return true;

    type_t *type_to_cast = types.items[type_id_to_cast.i];
    type_t *type = types.items[type_id.i];

    if (type_is_union(types, type_id)) {
        return union_type_contains_type(types, type_id, type_id_to_cast);
    }

    if (type_is_union(types, type_id_to_cast)) return false;

    u32 type_to_cast_size = type_size_bytes(type_to_cast);
    u32 type_size = type_size_bytes(type);
    if (type_is_number(type_to_cast, true) && type_is_number(type, true) && type_to_cast_size <= type_size) {
        if (type_is_integer(type_to_cast, true) || (type_is_float(type_to_cast) && type_is_float(type))) {
            return true;
        }
    }

    return false;
}
