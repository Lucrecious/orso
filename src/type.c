#include "type.h"

#include "type_set.h"
#include "tmp.h"


bool struct_type_is_incomplete(typedata_t *type) {
    return type->kind == TYPE_STRUCT && type->as.struct_.field_count < 0;
}

bool type_equal(typedata_t *a, typedata_t *b) {
    if (a->kind != b->kind) return false;

    if (a->size != b->size) return false;

    switch (a->kind) {
        case TYPE_INFERRED: return false;
        case TYPE_INFERRED_FUNCTION: return false;

        case TYPE_FUNCTION:
        case TYPE_INTRINSIC_FUNCTION: {
            if (a->as.function.argument_types.count != b->as.function.argument_types.count) {
                return false;
            }

            unless (typeid_eq(a->as.function.return_type, b->as.function.return_type)) {
                return false;
            }

            for (size_t i = 0; i < a->as.function.argument_types.count; ++i) {
                unless (typeid_eq(a->as.function.argument_types.items[i], b->as.function.argument_types.items[i])) {
                    return false;
                }
            }

            return true;
        }
        case TYPE_STRUCT: {
            s32 a_name_length = a->as.struct_.name ? strlen(a->as.struct_.name) : 0;
            s32 b_name_length = b->as.struct_.name ? strlen(b->as.struct_.name) : 0;
            if (a_name_length != b_name_length) {
                return false;
            }

            if (memcmp(a->as.struct_.name, b->as.struct_.name, a_name_length) != 0) {
                return false;
            }

            if (a->as.struct_.field_count != b->as.struct_.field_count) {
                return false;
            }

            for (s32 i = 0; i < a->as.struct_.field_count; i++) {
                unless (typeid_eq(a->as.struct_.fields[i].type, b->as.struct_.fields[i].type)) {
                    return false;
                }

                s32 field_name_length_a = strlen(a->as.struct_.fields[i].name);
                s32 field_name_length_b = strlen(b->as.struct_.fields[i].name);
                if (field_name_length_a != field_name_length_b) {
                    return false;
                }

                if (memcmp(a->as.struct_.fields[i].name, b->as.struct_.fields[i].name, field_name_length_a) != 0) {
                    return false;
                }
            }

            return true;
        }

        case TYPE_POINTER: {
            return typeid_eq(a->as.ptr.type, b->as.ptr.type);
        }

        case TYPE_ARRAY: {
            return typeid_eq(a->as.arr.type, b->as.arr.type) && a->as.arr.size == b->as.arr.size;
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

bool type_in_list(types_t list, type_t find) {
    for (size_t i = 0; i < list.count; i++) {
        if (typeid_eq(list.items[i], find)) {
            return true;
        }
    }

    return false;
}

struct_field_t *type_struct_find_field(typedata_t *struct_, const char *name, size_t name_length) {
    for (s32 i = 0; i < struct_->as.struct_.field_count; i++) {
        if (strlen(struct_->as.struct_.fields[i].name) != name_length) {
            continue;
        }

        if (strncmp(struct_->as.struct_.fields[i].name, name, name_length) != 0) {
            continue;
        }

        return struct_->as.struct_.fields + i;
    }

    return NULL;
}

string_t type_to_string_toplevel(typedatas_t types, type_t type, arena_t *allocator, bool is_toplevel) {
    tmp_arena_t *tmp_arena = allocator_borrow();

    string_builder_t sb = {.allocator = tmp_arena->allocator};

    typedata_t *type_info = type2typedata(&types, type);

    if (type_is_function(types, type) || type_is_intrinsic_function(types, type)) {
        sb_add_char(&sb, '(');

        for (size_t i = 0; i < type_info->as.function.argument_types.count; ++i) {
            if (i != 0) {
                sb_add_char(&sb, ',');
            }

            string_t arg_type = type_to_string_toplevel(types, type_info->as.function.argument_types.items[i], allocator, false);
            sb_add_cstr(&sb, arg_type.cstr);
        }

        sb_add_cstr(&sb, ") -> ");

        string_t return_type = type_to_string_toplevel(types, type_info->as.function.return_type, allocator, false);
        sb_add_cstr(&sb, return_type.cstr);
    } else if (type_is_struct(types, type)) {
        if (type_info->as.struct_.name) {
            sb_add_cstr(&sb, type_info->as.struct_.name);
        } else {
            sb_add_cstr(&sb, "struct");
        }
        
        if (is_toplevel) {
            sb_add_cstr(&sb, " { ");

            for (s32 i = 0; i < type_info->as.struct_.field_count; i++) {
                char *name = type_info->as.struct_.fields[i].name;

                sb_add_cstr(&sb, name);
                sb_add_cstr(&sb, ": ");

                type_t field_type = type_info->as.struct_.fields[i].type;
                string_t type_string = type_to_string_toplevel(types, field_type, allocator, false);
                sb_add_cstr(&sb, type_string.cstr);

                sb_add_cstr(&sb, "; ");
            }

            sb_add_char(&sb, '}');
        }
    } else if (type_is_pointer(types, type)) {
        sb_add_char(&sb, '&');

        string_t type_string = type_to_string_toplevel(types, type_info->as.ptr.type, allocator, false);
        sb_add_cstr(&sb, type_string.cstr);
    } else {
        cstr_t type_name;
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
            case TYPE_INFERRED: type_name = "<inferred>"; break;
            case TYPE_INFERRED_FUNCTION: type_name = "<inferred funcdev>"; break;

            case TYPE_COUNT:
            case TYPE_STRUCT:
            case TYPE_FUNCTION:
            case TYPE_INTRINSIC_FUNCTION:
            case TYPE_POINTER:
                type_name = "<?>"; UNREACHABLE(); break;
        }

        sb_add_cstr(&sb, type_name);
    }

    string_t string = sb_render(&sb, allocator);

    allocator_return(tmp_arena);

    return string;
}

string_t type_to_string(typedatas_t types, type_t type, arena_t *allocator) {
    return type_to_string_toplevel(types, type, allocator, true);
}
