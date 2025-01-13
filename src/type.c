#include "type.h"

#include "slot.h"
#include "type_set.h"
#include "tmp.h"


bool struct_type_is_incomplete(typedata_t *type) {
    return type->kind == TYPE_STRUCT && type->data.struct_.field_count < 0;
}

bool type_equal(typedata_t *a, typedata_t *b) {
    if (a->kind != b->kind) return false;

    if (a->size != b->size) return false;

    switch (a->kind) {
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

        case TYPE_BOOL: return true;

        case TYPE_NUMBER: return a->data.num == b->data.num;

        case TYPE_STRING:
        case TYPE_VOID:
        case TYPE_UNREACHABLE:
        case TYPE_LABEL:
        case TYPE_TYPE: return true;

        case TYPE_COUNT:
        case TYPE_INVALID:
        case TYPE_UNDEFINED:
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

size_t bytes_to_words(i32 byte_count) {
    return (byte_count + (WORD_SIZE-1)) / WORD_SIZE;
}

struct_field_t *type_struct_find_field(typedata_t *struct_, const char *name, size_t name_length) {
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

string_t type_to_string_toplevel(type_infos_t types, type_t type, arena_t *allocator, bool is_toplevel) {
    tmp_arena_t *tmp_arena = allocator_borrow();

    string_builder_t sb = {.allocator = tmp_arena->allocator};

    typedata_t *type_info = type2typedata(&types, type);

    if (type_is_function(types, type) || type_is_native_function(types, type)) {
        sb_add_char(&sb, '(');

        for (size_t i = 0; i < type_info->data.function.argument_types.count; ++i) {
            if (i != 0) {
                sb_add_char(&sb, ',');
            }

            string_t arg_type = type_to_string_toplevel(types, type_info->data.function.argument_types.items[i], allocator, false);
            sb_add_cstr(&sb, arg_type.cstr);
        }

        sb_add_cstr(&sb, ") -> ");

        string_t return_type = type_to_string_toplevel(types, type_info->data.function.return_type, allocator, false);
        sb_add_cstr(&sb, return_type.cstr);
    } else if (type_is_struct(types, type)) {
        if (type_info->data.struct_.name) {
            sb_add_cstr(&sb, type_info->data.struct_.name);
        } else {
            sb_add_cstr(&sb, "struct");
        }
        
        if (is_toplevel) {
            sb_add_cstr(&sb, " { ");

            for (i32 i = 0; i < type_info->data.struct_.field_count; i++) {
                char *name = type_info->data.struct_.fields[i].name;

                sb_add_cstr(&sb, name);
                sb_add_cstr(&sb, ": ");

                type_t field_type = type_info->data.struct_.fields[i].type;
                string_t type_string = type_to_string_toplevel(types, field_type, allocator, false);
                sb_add_cstr(&sb, type_string.cstr);

                sb_add_cstr(&sb, "; ");
            }

            sb_add_char(&sb, '}');
        }
    } else if (type_is_pointer(types, type)) {
        sb_add_char(&sb, '&');

        string_t type_string = type_to_string_toplevel(types, type_info->data.pointer.type, allocator, false);
        sb_add_cstr(&sb, type_string.cstr);
    } else {
        cstr_t type_name;
        switch (type_info->kind) {
            case TYPE_BOOL: type_name = "bool"; break;
            case TYPE_NUMBER: {
                switch (type_info->data.num) {
                    case NUM_TYPE_FLOAT: sb_add_char(&sb, 'f'); break;
                    case NUM_TYPE_SIGNED: sb_add_char(&sb, 'i'); break;
                    case NUM_TYPE_UNSIGNED: sb_add_char(&sb, 'u'); break;
                }

                string_t s = string_format("%llu", tmp_arena->allocator, (type_info->size*8));
                type_name = s.cstr;
                break;
            }

            case TYPE_LABEL: type_name = "label"; break;
            case TYPE_STRING: type_name = "string"; break;
            case TYPE_VOID: type_name = "void"; break;
            case TYPE_TYPE: type_name = "type"; break;

            case TYPE_INVALID: type_name = "<invalid>"; break;
            case TYPE_UNRESOLVED: type_name = "<unresolved>"; break;
            case TYPE_UNREACHABLE: type_name = "<unreachable>"; break;
            case TYPE_UNDEFINED: type_name = "<undefined>"; break;
            
            case TYPE_COUNT:
            case TYPE_STRUCT:
            case TYPE_FUNCTION:
            case TYPE_NATIVE_FUNCTION:
            case TYPE_POINTER:
                type_name = "<?>"; UNREACHABLE(); break;
        }

        sb_add_cstr(&sb, type_name);
    }

    string_t string = sb_render(&sb, allocator);

    allocator_return(tmp_arena);

    return string;
}

string_t type_to_string(type_infos_t types, type_t type, arena_t *allocator) {
    return type_to_string_toplevel(types, type, allocator, true);
}
