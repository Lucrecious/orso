#include "type.h"

#include "type_set.h"
#include "tmp.h"

bool is_type_kind_aggregate(type_kind_t kind) {
    return ((kind) == TYPE_ARRAY || (kind) == TYPE_STRUCT);
}

size_t td_align(size_t b, size_t alignment) {
    return ((b + alignment-1) / alignment) * alignment;
}

bool type_equal(typedata_t *a, typedata_t *b) {
    if (a->kind != b->kind) return false;

    if (a->size != b->size) return false;

    switch (a->kind) {
        case TYPE_PARAM_STRUCT: return false;
        case TYPE_INFERRED_FUNCTION: return false;
        case TYPE_MODULE: return false;

        case TYPE_FUNCTION: {
            if (a->as.function.argument_types.count != b->as.function.argument_types.count) {
                return false;
            }

            unless (ortypeid_eq(a->as.function.return_type, b->as.function.return_type)) {
                return false;
            }

            for (size_t i = 0; i < a->as.function.argument_types.count; ++i) {
                unless (ortypeid_eq(a->as.function.argument_types.items[i], b->as.function.argument_types.items[i])) {
                    return false;
                }
            }

            return true;
        }

        case TYPE_STRUCT: {
            MUST(a->as.struct_.name_or_null == NULL);
            size_t a_name_length = a->as.struct_.name_or_null ? strlen(a->as.struct_.name_or_null) : 0;
            size_t b_name_length = b->as.struct_.name_or_null ? strlen(b->as.struct_.name_or_null) : 0;
            if (a_name_length != b_name_length) {
                return false;
            }

            if (memcmp(a->as.struct_.name_or_null, b->as.struct_.name_or_null, a_name_length) != 0) {
                return false;
            }

            if (a->as.struct_.fields.count != b->as.struct_.fields.count) {
                return false;
            }

            for (size_t i = 0; i < a->as.struct_.fields.count; i++) {
                unless (ortypeid_eq(a->as.struct_.fields.items[i].type, b->as.struct_.fields.items[i].type)) {
                    return false;
                }

                if (string_eq(a->as.struct_.fields.items[i].name, b->as.struct_.fields.items[i].name)) {
                    return false;
                }
            }

            return true;
        }

        case TYPE_POINTER: {
            return ortypeid_eq(a->as.ptr.type, b->as.ptr.type);
        }

        case TYPE_ARRAY: {
            if (!ortypeid_eq(a->as.arr.type, b->as.arr.type)) return false;
            if (a->as.arr.count != b->as.arr.count) return false;
            return true;
        }

        case TYPE_BOOL: return true;

        case TYPE_NUMBER: return a->as.num == b->as.num;

        case TYPE_STRING:
        case TYPE_VOID:
        case TYPE_UNREACHABLE:
        case TYPE_TYPE: return true;

        case TYPE_COUNT:
        case TYPE_INVALID:
        case TYPE_UNRESOLVED: UNREACHABLE(); return false;

    }
}

bool type_in_list(types_t list, ortype_t find) {
    for (size_t i = 0; i < list.count; i++) {
        if (ortypeid_eq(list.items[i], find)) {
            return true;
        }
    }

    return false;
}

orstring_t type_to_string_toplevel(typedatas_t types, ortype_t type, arena_t *allocator, bool is_toplevel) {
    tmp_arena_t *tmp_arena = allocator_borrow();

    string_builder_t sb = {.allocator = tmp_arena->allocator};

    typedata_t *type_info = type2typedata(&types, type);

    if (type_is_function(types, type)) {
        sb_add_char(&sb, '(');

        for (size_t i = 0; i < type_info->as.function.argument_types.count; ++i) {
            if (i != 0) {
                sb_add_char(&sb, ',');
            }

            orstring_t arg_type = type_to_string_toplevel(types, type_info->as.function.argument_types.items[i], allocator, false);
            sb_add_cstr(&sb, arg_type.cstr);
        }

        sb_add_cstr(&sb, ") -> ");

        orstring_t return_type = type_to_string_toplevel(types, type_info->as.function.return_type, allocator, false);
        sb_add_cstr(&sb, return_type.cstr);
    } else if (type_is_struct(types, type)) {
        if (type_info->as.struct_.name_or_null) {
            sb_add_cstr(&sb, type_info->as.struct_.name_or_null);
        } else {
            sb_add_cstr(&sb, "struct");
        }
        
        if (is_toplevel) {
            sb_add_cstr(&sb, " { ");

            for (size_t i = 0; i < type_info->as.struct_.fields.count; i++) {
                orstring_t name = type_info->as.struct_.fields.items[i].name;

                sb_add_cstr(&sb, name.cstr);
                sb_add_cstr(&sb, ": ");

                ortype_t field_type = type_info->as.struct_.fields.items[i].type;
                orstring_t type_string = type_to_string_toplevel(types, field_type, allocator, false);
                sb_add_cstr(&sb, type_string.cstr);

                sb_add_cstr(&sb, "; ");
            }

            sb_add_char(&sb, '}');
        }
    } else if (type_is_pointer(types, type)) {
        sb_add_char(&sb, '&');

        orstring_t type_string = type_to_string_toplevel(types, type_info->as.ptr.type, allocator, true);
        sb_add_cstr(&sb, type_string.cstr);
    } else {
        orcstr_t type_name;
        switch (type_info->kind) {
            case TYPE_NUMBER:
            case TYPE_BOOL:
            case TYPE_STRING:
            case TYPE_VOID:
            case TYPE_TYPE: {
                type_name = type_info->name.cstr;
                break;
            }

            case TYPE_ARRAY: type_name = "<array>"; break;

            case TYPE_INVALID: type_name = "<invalid>"; break;
            case TYPE_UNRESOLVED: type_name = "<unresolved>"; break;
            case TYPE_UNREACHABLE: type_name = "<unreachable>"; break;
            case TYPE_INFERRED_FUNCTION: type_name = "<inferred funcdef>"; break;
            case TYPE_MODULE: type_name = "<module>"; break;
            case TYPE_PARAM_STRUCT: type_name = "param struct"; break;

            case TYPE_COUNT:
            case TYPE_STRUCT:
            case TYPE_FUNCTION:
            case TYPE_POINTER:
                type_name = "<?>"; UNREACHABLE(); break;
        }

        sb_add_cstr(&sb, type_name);
    }

    orstring_t string = sb_render(&sb, allocator);

    allocator_return(tmp_arena);

    return string;
}

orstring_t type_to_string(typedatas_t types, ortype_t type, arena_t *allocator) {
    return type_to_string_toplevel(types, type, allocator, true);
}
