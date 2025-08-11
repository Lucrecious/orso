#ifndef CODEGENC_H_
#define CODEGENC_H_

#include "stringt.h"
#include "parser.h"
#include "tmp.h"
#include <inttypes.h>

bool compile_ast_to_c(ast_t *ast, orstring_t build_directory, strings_t *sources, strings_t *libs, arena_t *arena);

#endif

#ifdef CODEGENC_IMPLEMENTATION
#include "memarr.h"

#define CORE_H_FILENAME "core.h"
#define ORFFI_H_FILENAME "orffi.h"


typedef struct funcdata_t funcdata_t;
struct funcdata_t {
    orstring_t name;
    ortype_t type;
};

define_table(p2n, void*, funcdata_t, hashptr_, hasheq_);

typedef struct cgen_state_t cgen_state_t;
struct cgen_state_t {
    size_t *tmp_count;
    table_t(p2n) *functions;
};

typedef struct cgen_t cgen_t;
struct cgen_t {
    ast_t *ast;
    size_t indent;

    arena_t *tmp_arena;

    string_builder_t sb;

    cgen_state_t state;
};


typedef struct cgen_var_t cgen_var_t;
struct cgen_var_t {
    orstring_t name;
    bool is_new;
    size_t id;
    ortype_t type;
};

static cgen_var_t nil_cvar = {.is_new = false, .id = 0 , .type = ortypeid(TYPE_INVALID) };


#define no_var(tv) ((tv).id == 0 && (tv).name.length == 0)
#define has_var(tv) ((tv).id > 0 || (tv).name.length > 0)

static orstring_t cgen_function_name(cgen_state_t state, string_view_t name, arena_t *arena) {
    orstring_t r = string_format("__or%.*s_%zu_", arena, (int)name.length, name.data, ++(*state.tmp_count));
    return r;
}

static orstring_t cgen_global_new_name(string_view_t name, cgen_state_t state, arena_t *arena) {
    size_t tmpid = ++(*state.tmp_count);
    orstring_t new_name = string_format("__or%.*s_%zu_", arena, name.length, name.data, tmpid);
    return new_name;
}

static orstring_t cgen_local_new_name(string_view_t name, cgen_state_t state, arena_t *arena) {
    size_t tmpid = ++(*state.tmp_count);
    orstring_t new_name = string_format("%.*s_%zu_", arena, name.length, name.data, tmpid);
    return new_name;
}

static cgen_var_t cgen_user_var(cgen_t *cgen, string_view_t name, ortype_t type, bool is_global) {
    UNUSED(is_global);

    cgen_var_t var = {0};
    var.type = type;
    var.is_new = true;

    var.name = sv2string(name, cgen->tmp_arena);

    return var;
}

static cgen_var_t cgen_var_used(cgen_var_t var) {
    cgen_var_t r = var;
    r.is_new = false;
    return r;
}

static cgen_var_t cgen_next_tmpid(cgen_t *cgen, ortype_t type) {
    return (cgen_var_t){ .is_new = true, .id = ++(*cgen->state.tmp_count), .type = type };
}

static void cgen_indent(cgen_t *cgen) {
    ++cgen->indent;
}

static void cgen_unindent(cgen_t *cgen) {
    --cgen->indent;
}

static void cgen_add_indent(cgen_t *cgen) {
    for (size_t i = 0; i < cgen->indent; ++i) {
        sb_add_cstr(&cgen->sb, "  ");
    }
}

static void cgen_add_include(cgen_t *cgen, orcstr_t include_path) {
    sb_add_format(&cgen->sb, "#include \"%s\"\n", include_path);
}

static orcstr_t cgen_typedata_name(typedata_t *typedata) {
    ASSERT(typedata->name.length > 0, "all types must have a name before calling this");
    return typedata->name.cstr;
}

static orcstr_t cgen_type_name(cgen_t *cgen, ortype_t type) {
    typedata_t *typedata = type2typedata(&cgen->ast->type_set.types, type);
    orcstr_t value = cgen_typedata_name(typedata);
    return value;
}

static orcstr_t cgen_var_name(cgen_t *cgen, cgen_var_t var) {
    if (var.name.length > 0) {
        return var.name.cstr;
    } else {
        orstring_t result = string_format("tmp%llu", cgen->tmp_arena, var.id);
        return result.cstr;
    }
}

static orcstr_t cgen_var(cgen_t *cgen, cgen_var_t var) {
    if (var.is_new) {
        orstring_t result = string_format("%s %s", cgen->tmp_arena, cgen_type_name(cgen, var.type), cgen_var_name(cgen, var));
        return result.cstr;
    } else {
        return cgen_var_name(cgen, var);
    }
}

static orcstr_t cgen_lvar(cgen_t *cgen, cgen_var_t var) {
    if (var.is_new) {
        orstring_t result = string_format("%s *%s", cgen->tmp_arena, cgen_type_name(cgen, var.type), cgen_var_name(cgen, var));
        return result.cstr;
    } else {
        return cgen_var_name(cgen, var);
    }
}

static orstring_t cgen_next_label(cgen_t *cgen, orcstr_t label_name) {
    ++(*cgen->state.tmp_count);
    orstring_t label = string_format("%s%zu_", cgen->tmp_arena, label_name, *cgen->state.tmp_count);
    return label;
}

static void cgen_jmp_label(cgen_t *cgen, orstring_t label) {
    sb_add_format(&cgen->sb, "%s:", label.cstr);
}

static void cgen_expression(cgen_t *cgen, ast_node_t *expression, cgen_var_t tmp_var);

static bool cgen_binary_is_macro(token_type_t type, typedata_t *optd, orcstr_t *operator_or_func_name) {
    #define set_op(lit, is_func) { if (operator_or_func_name) *operator_or_func_name = (lit); return (is_func); } break

    if (is_type_kind_aggregate(optd->kind)) {
        return false;
    }

    #define case_block(numtype) do { switch (type) {\
        case TOKEN_PLUS: set_op("oradd"#numtype, true); break; \
        case TOKEN_MINUS: set_op("orsub"#numtype, true); \
        case TOKEN_SLASH: set_op("ordiv"#numtype, true); \
        case TOKEN_STAR: set_op("ormul"#numtype, true); \
        case TOKEN_PERCENT: set_op("ormod"#numtype, true); \
        case TOKEN_PERCENT_PERCENT: set_op("orrem"#numtype, true); \
        default: set_op("", false); \
    }} while (false)

    if (operator_is_arithmetic(type)) {
        switch (optd->kind) {
        case TYPE_NUMBER: {
            switch ((num_size_t)optd->size) {
            case NUM_SIZE_8: {
                switch (optd->as.num) {
                case NUM_TYPE_SIGNED: case_block(s8); break;
                case NUM_TYPE_UNSIGNED: case_block(u8); break;
                default: UNREACHABLE(); break;
                }
                break;
            }

            case NUM_SIZE_16: {
                switch (optd->as.num) {
                case NUM_TYPE_SIGNED: case_block(s16); break;
                case NUM_TYPE_UNSIGNED: case_block(u16); break;
                default: UNREACHABLE(); break;
                }
                break;
            }

            case NUM_SIZE_32: {
                switch (optd->as.num) {
                case NUM_TYPE_SIGNED: case_block(s32); break;
                case NUM_TYPE_UNSIGNED: case_block(u32); break;
                case NUM_TYPE_FLOAT: case_block(f); break;
                }
                break;
            }

            case NUM_SIZE_64: {
                switch (optd->as.num) {
                case NUM_TYPE_SIGNED: case_block(s64); break;
                case NUM_TYPE_UNSIGNED: case_block(u64); break;
                case NUM_TYPE_FLOAT: case_block(d); break;
                }
                break;
            
            default: UNREACHABLE(); break;
            }

            }
            break;
        }

        // only numbers have arithmetic right now
        default: UNREACHABLE(); break;
        }
    } else {
        switch (type) {
        case TOKEN_GREATER: set_op(">", false);
        case TOKEN_GREATER_EQUAL: set_op(">=", false);
        case TOKEN_LESS: set_op("<", false);
        case TOKEN_LESS_EQUAL: set_op("<=", false);
        case TOKEN_EQUAL_EQUAL: {
            switch (optd->kind) {
            case TYPE_POINTER:
            case TYPE_BOOL:
            case TYPE_NUMBER: {
                set_op("==", false);
                break;
            }

            case TYPE_TYPE: {
                set_op("ortypeid_eq", true);
                break;
            }

            default: UNREACHABLE(); break; // todo
            }
            break;
        }
        case TOKEN_BANG_EQUAL: {
            switch (optd->kind) {
            case TYPE_POINTER:
            case TYPE_BOOL:
            case TYPE_NUMBER: {
                set_op("!=", false);
                break;
            }

            case TYPE_TYPE: {
                set_op("ortypeid_nq", true);
                break;
            }

            default: UNREACHABLE(); break; // todo
            }
            break;
        }
        case TOKEN_AND: set_op("&&", false);
        case TOKEN_OR: set_op("||", false);

        default: set_op("", false); UNREACHABLE();
        }
    }

    #undef case_block
}

static void cgen_cache_requires_tmp(typedatas_t *types, ast_node_t *expression) {
    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            expression->requires_tmp_for_cgen = (expression->operator.type != TOKEN_EQUAL);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: {
            if (string_eq(*expression->identifier, lit2str("fficall"))) {
                expression->requires_tmp_for_cgen = true;
            } else {
                expression->requires_tmp_for_cgen = false;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
            if (expression->subscript_call_or_null) {
                cgen_cache_requires_tmp(types, expression->subscript_call_or_null);
                expression->requires_tmp_for_cgen = expression->subscript_call_or_null->requires_tmp_for_cgen;
            } else {
                expression->requires_tmp_for_cgen = false;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case AST_NODE_TYPE_EXPRESSION_JMP:
        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            expression->requires_tmp_for_cgen = true;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            typedata_t *operandtd = type2typedata(types, an_lhs(expression)->value_type);
            if (is_type_kind_aggregate(operandtd->kind)) {
                expression->requires_tmp_for_cgen = true;
            } else {
                if (cgen_binary_is_macro(expression->operator.type, operandtd, NULL)) {
                    // ast_node_t *lhs = an_lhs(expression);
                    // ast_node_t *rhs = an_rhs(expression);
                    // if ((lhs->node_type == AST_NODE_TYPE_EXPRESSION_PRIMARY || lhs->node_type == AST_NODE_TYPE_DECL)
                    // && (rhs->node_type == AT)) {
                        expression->requires_tmp_for_cgen = false;
                    // } else {
                    //     expression->requires_tmp_for_cgen = true;
                    // }
                } else {
                    expression->requires_tmp_for_cgen = false;
                }
            }
            break;
        }

        default:  {
            expression->requires_tmp_for_cgen = false;
            break;
        }
    }

    for (size_t i = 0; i < expression->children.count; ++i) {
        ast_node_t *child = expression->children.items[i];
        if (TYPE_IS_INFERRED_FUNCTION(child->value_type) && child->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) {
            for (size_t i = 0; i < child->realized_copies.count; ++i) {
                inferred_copy_t funcdef_copy = child->realized_copies.items[i];
                cgen_cache_requires_tmp(types, funcdef_copy.copy);
            }
        } else {
            cgen_cache_requires_tmp(types, child);
            if (expression->node_type != AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION && child->requires_tmp_for_cgen) {
                expression->requires_tmp_for_cgen = true;
            }
        }
    }

    if (expression->expr_val.is_concrete) {
        expression->requires_tmp_for_cgen = false;
    }
}

static void cgen_array_start(cgen_t *cgen, ortype_t type) {
    sb_add_format(&cgen->sb, "((%s){ .arr={", ast_type2td(cgen->ast, type)->name);
}

static void cgen_array_end(cgen_t *cgen) {
    sb_add_cstr(&cgen->sb, " }})");
}

static void cgen_struct_start(cgen_t *cgen, ortype_t type) {
    sb_add_format(&cgen->sb, "((%s){ ", ast_type2td(cgen->ast, type)->name);
}

static void cgen_struct_end(cgen_t *cgen) {
    sb_add_cstr(&cgen->sb, " })");
}

static orstring_t cgen_get_function_name(cgen_t *cgen, function_t *function) {
    funcdata_t result = {.name=lit2str("")};
    bool success = table_get(p2n, cgen->state.functions, function, &result);
    UNUSED(success);
    ASSERT(success, "the function should be in there already");
    return result.name;
}

static orstring_t cgen_get_instrinsic_fn_name(ast_t *ast, orintrinsic_fn_t fn, arena_t *arena) {
    UNUSED(ast);
    orstring_t name = string_format("or%s", arena, fn.name->cstr);
    return name;
}

static void cgen_constant(cgen_t *cgen, orword_t word, ortype_t type) {
    typedata_t *typedata = ast_type2td(cgen->ast, type);

    switch (typedata->kind) {
        case TYPE_NUMBER: {
            orcstr_t numtype = cgen_typedata_name(typedata);
            switch ((num_size_t)typedata->size) {
                case NUM_SIZE_8: {
                    switch (typedata->as.num) {
                    case NUM_TYPE_FLOAT: UNREACHABLE(); break;
                    case NUM_TYPE_SIGNED: sb_add_format(&cgen->sb, "_%s(%"PRIi32")", numtype, (ors32)word.as.s); break;
                    case NUM_TYPE_UNSIGNED: sb_add_format(&cgen->sb, "_%s(%"PRIu32")", numtype, (oru32)word.as.u); break;
                    }
                    break;
                }

                case NUM_SIZE_16: {
                    switch (typedata->as.num) {
                    case NUM_TYPE_FLOAT: UNREACHABLE(); break;
                    case NUM_TYPE_SIGNED: sb_add_format(&cgen->sb, "_%s(%"PRIi32")", numtype, (ors32)word.as.s); break;
                    case NUM_TYPE_UNSIGNED: sb_add_format(&cgen->sb, "_%s(%"PRIu32")", numtype, (oru32)word.as.u); break;
                    }
                    break;
                }

                case NUM_SIZE_32: {
                    switch (typedata->as.num) {
                    case NUM_TYPE_FLOAT: sb_add_format(&cgen->sb, "_%s(%g)", numtype, (orf32)word.as.d); break;
                    case NUM_TYPE_SIGNED: sb_add_format(&cgen->sb, "_%s(%"PRIi32")", numtype, (ors32)word.as.s); break;
                    case NUM_TYPE_UNSIGNED: sb_add_format(&cgen->sb, "_%s(%"PRIu32")", numtype, (oru32)word.as.u); break;
                    }
                    break;
                }

                case NUM_SIZE_64: {
                    switch (typedata->as.num) {
                        case NUM_TYPE_FLOAT: sb_add_format(&cgen->sb, "%lg", (orf64)word.as.d); break;
                        case NUM_TYPE_SIGNED: {
                            if (word.as.s == INT64_MIN) {
                                // the smallest 64bit integer cannot be expressed as a single number literal
                                // due to how C99 parses numbers
                                sb_add_format(&cgen->sb, "INT64_MIN", (ors64)word.as.s); break;
                            } else {
                                sb_add_format(&cgen->sb, "%"PRIi64"ll", (ors64)word.as.s); break;
                            }
                        }
                        case NUM_TYPE_UNSIGNED: sb_add_format(&cgen->sb, "%"PRIu64""PRIu64, (oru64)word.as.u); break;
                    }
                    break;
                }

                default: UNREACHABLE(); break;
            }
            break;
        }

        case TYPE_BOOL: {
            sb_add_format(&cgen->sb, "%s", (word.as.u != 0) ? "true" : "false");
            break;
        }

        case TYPE_TYPE: {
            sb_add_format(&cgen->sb, "ortypeid(%"PRIu64")", word.as.t.i);
            break;
        }

        case TYPE_FUNCTION: {
            function_t *function = word.as.p;
            orstring_t funcname = cgen_get_function_name(cgen, function);

            sb_add_cstr(&cgen->sb, "(");
            sb_add_format(&cgen->sb, "%s", funcname.cstr);
            sb_add_cstr(&cgen->sb, ")");
            break;
        }

        case TYPE_ARRAY: {
            cgen_array_start(cgen, type);

            void *data_addr;
            if (typedata->size > ORWORD_SIZE) {
                data_addr = word.as.p;
            } else {
                data_addr = &word;
            }

            ortype_t inner_type = typedata->as.arr.type;
            typedata_t *innertd = ast_type2td(cgen->ast, inner_type);
            size_t inner_item_size = td_align(innertd->size, innertd->alignment);
            for (size_t i = 0; i < typedata->as.arr.count; ++i) {
                if (i != 0) {
                    sb_add_cstr(&cgen->sb, ", ");
                }

                void *item_data_addr = data_addr + i*inner_item_size;
                if (innertd->size > ORWORD_SIZE) {
                    cgen_constant(cgen, ORWORDP(item_data_addr), inner_type);
                } else {
                    orword_t val = ast_mem2word(cgen->ast, item_data_addr, inner_type);
                    cgen_constant(cgen, val, inner_type);
                }
            }
                
            cgen_array_end(cgen);
            break;
        }

        case TYPE_STRING:
        case TYPE_STRUCT: {
            cgen_struct_start(cgen, type);

            void *data_addr;
            if (typedata->size > ORWORD_SIZE) {
                data_addr = word.as.p;
            } else {
                data_addr = &word;
            }

            for (size_t i = 0; i < typedata->as.struct_.fields.count; ++i) {
                if (i != 0) {
                    sb_add_cstr(&cgen->sb, ", ");
                }

                struct_field_t field = typedata->as.struct_.fields.items[i];
                typedata_t *fieldtd = ast_type2td(cgen->ast, field.type);
                void *item_data_addr = data_addr + field.offset;

                sb_add_format(&cgen->sb, ".%s = ", field.name->cstr);

                if (typedata->kind == TYPE_STRING && string_eq(*field.name, lit2str("cstr"))) {
                    // todo: needs better conversion, use hexadecimal instead
                    orcstr_t cstr = *((char**)item_data_addr);
                    sb_add_format(&cgen->sb, "\"%s\"", cstr);
                } else {
                    if (fieldtd->size > ORWORD_SIZE) {
                        cgen_constant(cgen, ORWORDP(item_data_addr), field.type);
                    } else {
                        orword_t val = ast_mem2word(cgen->ast, item_data_addr, field.type);
                        cgen_constant(cgen, val, field.type);
                    }
                }
            }

            cgen_struct_end(cgen);
            break;
        }
        case TYPE_POINTER: {
            sb_add_cstr(&cgen->sb, "0");
            break;
        }

        case TYPE_VOID:
        case TYPE_MODULE:
        case TYPE_PARAM_STRUCT:
        case TYPE_INFERRED_FUNCTION:
        case TYPE_UNREACHABLE:
        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_COUNT: UNREACHABLE(); break;
    }

    #undef value_at
}

static void cgen_semicolon_nl(cgen_t *cgen) {
    sb_add_cstr(&cgen->sb, ";\n");
}

static void cgen_statement(cgen_t *cgen, ast_node_t *expression, cgen_var_t var, bool add_indent) {
    if (add_indent) cgen_add_indent(cgen);
    cgen_expression(cgen, expression, var);

    cgen_semicolon_nl(cgen);
}

static void cgen_declaration(cgen_t *cgen, ast_node_t *declaration) {
    switch (declaration->node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            // skip over the constants since they are inlined
            if (!declaration->is_mutable) break;

            if (!declaration->is_global) {
                declaration->ccode_var_name = cgen_local_new_name(string2sv(*declaration->identifier), cgen->state, cgen->ast->arena);
            }

            string_view_t identifier = string2sv(declaration->ccode_var_name);
            cgen_var_t var = cgen_user_var(cgen, identifier, declaration->value_type, declaration->is_global);
            cgen_statement(cgen, an_decl_expr(declaration), var, true);
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            cgen_statement(cgen, an_expression(declaration), nil_cvar, true);
            break;
        }

        default: UNREACHABLE(); break;
    }
}

static void cgen_scaler_aggregate_binary(cgen_t *cgen, ortype_t type, token_type_t op, string_view_t lhs, string_view_t rhs) {
    typedata_t *td = ast_type2td(cgen->ast, type);
    orcstr_t name_or_op;
    bool is_macro = cgen_binary_is_macro(op, td, &name_or_op);
    if (is_macro) {
        // pointer arithmetic is always a macro
        if (td->kind == TYPE_POINTER) {
            sb_add_format(&cgen->sb, "%s((%s)%.*s, %.*s)", name_or_op, td->name.cstr, lhs.length, lhs.data, rhs.length, rhs.data);
        } else {
            sb_add_format(&cgen->sb, "%s(%.*s, %.*s)", name_or_op, lhs.length, lhs.data, rhs.length, rhs.data);
        }
    } else {
        sb_add_format(&cgen->sb, "%.*s %s %.*s", lhs.length, lhs.data, name_or_op, rhs.length, rhs.data);
    }
}

static void cgen_aggregate_arith_binary(cgen_t *cgen, token_type_t op, ortype_t type, string_builder_t *lhs_lvalue, string_builder_t *rhs_lvalue) {
    typedata_t *td = ast_type2td(cgen->ast, type);

    switch (td->kind) {
    case TYPE_ARRAY: {
        cgen_array_start(cgen, type);

        size_t reset_length_lhs = lhs_lvalue->count;
        size_t reset_length_rhs = rhs_lvalue->count;

        for (size_t i = 0; i < td->as.arr.count; ++i) {
            if (i > 0) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            lhs_lvalue->count = reset_length_lhs;
            rhs_lvalue->count = reset_length_rhs;
            sb_add_format(lhs_lvalue, ".arr[%zu]", i);
            sb_add_format(rhs_lvalue, ".arr[%zu]", i);
            cgen_aggregate_arith_binary(cgen, op, td->as.arr.type, lhs_lvalue, rhs_lvalue);
        }

        cgen_array_end(cgen);
        break;
    }

    case TYPE_STRUCT: {
        cgen_struct_start(cgen, type);

        size_t reset_length_lhs = lhs_lvalue->count;
        size_t reset_length_rhs = rhs_lvalue->count;

        for (size_t i = 0; i < td->as.struct_.fields.count; ++i) {
            if (i > 0) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            struct_field_t field = td->as.struct_.fields.items[i];

            lhs_lvalue->count = reset_length_lhs;
            rhs_lvalue->count = reset_length_rhs;
            sb_add_format(lhs_lvalue, ".%s", field.name->cstr);
            sb_add_format(rhs_lvalue, ".%s", field.name->cstr);
            cgen_aggregate_arith_binary(cgen, op, field.type, lhs_lvalue, rhs_lvalue);
        }

        cgen_struct_end(cgen);
        break;
    }

    case TYPE_NUMBER: {
        string_view_t lhs = {.length=lhs_lvalue->count, .data=lhs_lvalue->items};
        string_view_t rhs = {.length=rhs_lvalue->count, .data=rhs_lvalue->items};
        cgen_scaler_aggregate_binary(cgen, type, op, lhs, rhs);
        break;
    }

    case TYPE_MODULE:
    case TYPE_VOID:
    case TYPE_BOOL:
    case TYPE_STRING:
    case TYPE_TYPE:
    case TYPE_FUNCTION:
    case TYPE_INVALID:
    case TYPE_UNRESOLVED:
    case TYPE_PARAM_STRUCT:
    case TYPE_INFERRED_FUNCTION:
    case TYPE_UNREACHABLE:
    case TYPE_POINTER:
    case TYPE_COUNT: UNREACHABLE(); break;
    }
}

static void cgen_binary(cgen_t *cgen, ast_node_t *binary, cgen_var_t var) {
    orcstr_t operator_or_function_name = NULL;

    ast_node_t *lhs = an_lhs(binary);
    ast_node_t *rhs = an_rhs(binary);

    typedata_t *lhstd = type2typedata(&cgen->ast->type_set.types, lhs->value_type);
    bool is_macro = cgen_binary_is_macro(binary->operator.type, lhstd, &operator_or_function_name);

    unless (binary->requires_tmp_for_cgen) {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        if (is_macro) {
            sb_add_format(&cgen->sb, "%s", operator_or_function_name);
        }
        sb_add_cstr(&cgen->sb, "(");
        
        if (lhstd->kind == TYPE_POINTER) {
            sb_add_format(&cgen->sb, "(%s)", lhstd->name.cstr);
        }

        cgen_expression(cgen, lhs, nil_cvar);

        if (is_macro) {
            sb_add_cstr(&cgen->sb, ", ");
        } else {
            sb_add_cstr(&cgen->sb, " ");
            sb_add_format(&cgen->sb, "%s", operator_or_function_name);
            sb_add_cstr(&cgen->sb, " ");
        }

        cgen_expression(cgen, rhs, nil_cvar);

        sb_add_cstr(&cgen->sb, ")");

    } else {
        cgen_var_t lhs_var = cgen_next_tmpid(cgen, lhs->value_type);
        cgen_var_t rhs_var = cgen_next_tmpid(cgen, rhs->value_type);

        // only for logical ops
        switch (binary->operator.type) {
            case TOKEN_AND:
            case TOKEN_OR: {
                sb_add_format(&cgen->sb, "%s = false", cgen_var(cgen, lhs_var));
                cgen_semicolon_nl(cgen);

                cgen_add_indent(cgen);
                sb_add_format(&cgen->sb, "%s = false;\n", cgen_var(cgen, rhs_var));
                cgen_semicolon_nl(cgen);

                cgen_add_indent(cgen);

                lhs_var = cgen_var_used(lhs_var);
                rhs_var = cgen_var_used(rhs_var);
                break;
            }

            default: break;
        }

        cgen_statement(cgen, lhs, lhs_var, false);

        cgen_add_indent(cgen);
        sb_add_format(&cgen->sb, "%s;\n", cgen_var(cgen, rhs_var));
        rhs_var.is_new = false;

        bool close_if = false;

        switch (binary->operator.type) {
            case TOKEN_AND: {
                close_if = true;

                cgen_add_indent(cgen);

                sb_add_format(&cgen->sb, "if (%s) {\n", cgen_var_name(cgen, lhs_var));
                cgen_indent(cgen);
                break;
            }

            case TOKEN_OR: {
                close_if = true;
                cgen_add_indent(cgen);

                sb_add_format(&cgen->sb, "unless (%s) {\n", cgen_var_name(cgen, lhs_var));
                cgen_indent(cgen);
                break;
            }

            default: break;
        }

        cgen_statement(cgen, rhs, rhs_var, true);

        if (close_if) {
            cgen_unindent(cgen);
            cgen_add_indent(cgen);
            sb_add_cstr(&cgen->sb, "}\n");
        }
        
        cgen_add_indent(cgen);
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        if (is_type_kind_aggregate(lhstd->kind)) {
            MUST(operator_is_arithmetic(binary->operator.type) || operator_is_comparing(binary->operator.type));

            if (operator_is_comparing(binary->operator.type)) {
                orcstr_t memcmp = "__ormemcmp";
                string_view_t lhs = cstr2sv(cgen_var_name(cgen, lhs_var));
                string_view_t rhs = cstr2sv(cgen_var_name(cgen, rhs_var));

                switch (binary->operator.type) {
                case TOKEN_EQUAL_EQUAL: break;
                case TOKEN_BANG_EQUAL: {
                    sb_add_cstr(&cgen->sb, "!");
                    break;
                }

                default: UNREACHABLE(); break;
                }
                sb_add_format(&cgen->sb, "%s(&%.*s, &%.*s, sizeof(%.*s))", memcmp, lhs.length, lhs.data, rhs.length, rhs.data, lhs.length, lhs.data);
            } else {
                tmp_arena_t *tmp = allocator_borrow();
                string_builder_t lhs_lvalue = {.allocator=tmp->allocator};
                string_builder_t rhs_lvalue = {.allocator=tmp->allocator};

                sb_add_format(&lhs_lvalue, "%s", cgen_var_name(cgen, lhs_var));
                sb_add_format(&rhs_lvalue, "%s", cgen_var_name(cgen, rhs_var));
                cgen_aggregate_arith_binary(cgen, binary->operator.type, lhs->value_type, &lhs_lvalue, &rhs_lvalue);

                allocator_return(tmp);
            }
        } else {
            string_view_t lhsv = cstr2sv(cgen_var_name(cgen, lhs_var));
            string_view_t rhsv = cstr2sv(cgen_var_name(cgen, rhs_var));
            cgen_scaler_aggregate_binary(cgen, lhs->value_type, binary->operator.type, lhsv, rhsv);
        }
    }
}

static orcstr_t cgen_token2unary(token_type_t type) {
    switch (type) {
        case TOKEN_MINUS: return "-";
        case TOKEN_NOT: return "!";
        case TOKEN_AMPERSAND: return "&";
        case TOKEN_STAR: return "*";

        default: UNREACHABLE(); return "";
    }
}

static void cgen_lvalue(cgen_t *cgen, ast_node_t *lvalue, cgen_var_t var);

static void cgen_unary(cgen_t *cgen, ast_node_t *unary, cgen_var_t var) {
    orcstr_t op = cgen_token2unary(unary->operator.type);
    ast_node_t *oprnd = an_operand(unary);
    switch (unary->operator.type) {
    case TOKEN_AMPERSAND: {
        unless (unary->requires_tmp_for_cgen) {
            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
            }

            sb_add_format(&cgen->sb, "&(");
            cgen_expression(cgen, oprnd, nil_cvar);
            sb_add_cstr(&cgen->sb, ")");
        } else {
            ortype_t inner = ast_type2td(cgen->ast, unary->value_type)->as.ptr.type;
            cgen_var_t tmp = cgen_next_tmpid(cgen, inner);
            cgen_lvalue(cgen, oprnd, tmp);
            cgen_semicolon_nl(cgen);

            cgen_add_indent(cgen);

            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
            }

            sb_add_format(&cgen->sb, "%s", cgen_var_name(cgen, tmp));
        }
        break;
    }

    case TOKEN_MINUS:
    case TOKEN_NOT:
    case TOKEN_STAR: {
        unless (unary->requires_tmp_for_cgen) {
            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
            }

            sb_add_format(&cgen->sb, "%s(", op);
            cgen_expression(cgen, an_operand(unary), nil_cvar);
            sb_add_cstr(&cgen->sb, ")");
        } else {
            cgen_var_t op_var = cgen_next_tmpid(cgen, an_operand(unary)->value_type);
            cgen_statement(cgen, an_operand(unary), op_var, false);

            cgen_add_indent(cgen);

            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
            }

            sb_add_format(&cgen->sb, "%s%s", op, cgen_var_name(cgen, op_var));
        } 
        break;
    }

    default: UNREACHABLE(); break;
    }
}
    
static void cgen_subscript_call(cgen_t *cgen, ast_node_t *subscript_call, cgen_var_t var) {
    if (!subscript_call->requires_tmp_for_cgen) {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        sb_add_cstr(&cgen->sb, "*(");
        cgen_expression(cgen, subscript_call, nil_cvar);
        sb_add_cstr(&cgen->sb, ")");
    } else {
        cgen_var_t tmp_var = cgen_next_tmpid(cgen, subscript_call->value_type);
        cgen_statement(cgen, subscript_call, tmp_var, false);

        cgen_add_indent(cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        sb_add_format(&cgen->sb, "*(%s)", cgen_var_name(cgen, tmp_var));
    }
}

static void cgen_lvalue(cgen_t *cgen, ast_node_t *lvalue, cgen_var_t var) {
    switch (lvalue->node_type) {
    case AST_NODE_TYPE_EXPRESSION_UNARY: {
        MUST(lvalue->operator.type == TOKEN_STAR);
        ast_node_t *operand = an_operand(lvalue);

        unless (lvalue->requires_tmp_for_cgen) {
            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_lvar(cgen, var));
            }

            cgen_expression(cgen, operand, nil_cvar);
        } else {
            cgen_var_t lvalue_var = cgen_next_tmpid(cgen, operand->value_type);
            cgen_statement(cgen, operand, lvalue_var, false);

            cgen_add_indent(cgen);

            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_lvar(cgen, var));
            }

            sb_add_format(&cgen->sb, "%s", cgen_var_name(cgen, lvalue_var));
        }
        break;
    }

    case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
        if (lvalue->subscript_call_or_null) {
            cgen_subscript_call(cgen, lvalue->subscript_call_or_null, var);
        } else {
            ast_node_t *accessee = an_item_accessee(lvalue);
            typedata_t *accessee_td = ast_type2td(cgen->ast, accessee->value_type);

            ast_node_t *accessor = an_item_accessor(lvalue);

            unless (lvalue->requires_tmp_for_cgen) {
                if (has_var(var)) {
                    sb_add_format(&cgen->sb, "%s = ", cgen_lvar(cgen, var));
                }

                switch (accessee_td->kind) {
                case TYPE_ARRAY: {
                    break;
                }

                case TYPE_POINTER: {
                    sb_add_cstr(&cgen->sb, "(");
                    break;
                }

                default: UNREACHABLE(); break;
                }

                cgen_lvalue(cgen, accessee->lvalue_node, nil_cvar);

                switch (accessee_td->kind) {
                case TYPE_ARRAY: {
                    sb_add_cstr(&cgen->sb, "->arr");
                    break;
                }

                case TYPE_POINTER: {
                    sb_add_cstr(&cgen->sb, ")->arr");
                    break;
                }

                default: UNREACHABLE(); break;
                }

                sb_add_cstr(&cgen->sb, " + (");

                cgen_expression(cgen, accessor, nil_cvar);

                sb_add_cstr(&cgen->sb, ")");
            } else {

                bool is_pointer = false;
                switch (accessee_td->kind) {
                case TYPE_ARRAY: {
                    is_pointer = false;
                    break;
                }

                case TYPE_POINTER: {
                    is_pointer = true;
                    break;
                }

                default: UNREACHABLE(); break;
                }

                cgen_var_t lvalue_var;
                if (is_pointer) {
                    lvalue_var = cgen_next_tmpid(cgen, accessee->value_type);
                    cgen_expression(cgen, accessee, lvalue_var);
                    cgen_semicolon_nl(cgen);

                    cgen_var_t ptr_var = lvalue_var;
                    lvalue_var = cgen_next_tmpid(cgen, lvalue->value_type);

                    cgen_add_indent(cgen);
                    sb_add_format(&cgen->sb, "%s = %s->arr", cgen_lvar(cgen, lvalue_var), cgen_var_name(cgen, ptr_var));

                    cgen_semicolon_nl(cgen);
                } else {
                    lvalue_var = cgen_next_tmpid(cgen, lvalue->value_type);
                    cgen_lvalue(cgen, accessee->lvalue_node, lvalue_var);
                    sb_add_cstr(&cgen->sb, "->arr");

                    cgen_semicolon_nl(cgen);
                }

                cgen_var_t key_var = cgen_next_tmpid(cgen, accessor->value_type);
                cgen_statement(cgen, accessor, key_var, true);

                cgen_add_indent(cgen);

                if (has_var(var)) {
                    sb_add_format(&cgen->sb, "%s = ", cgen_lvar(cgen, var));
                }

                sb_add_format(&cgen->sb, "%s + %s", cgen_var_name(cgen, lvalue_var), cgen_var_name(cgen, key_var));
            }
        }
        break;
    }

    case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
        ASSERT(!lvalue->requires_tmp_for_cgen, "must");

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_lvar(cgen, var));
        }

        string_view_t identifier = string2sv(lvalue->ref_decl->ccode_var_name);
        cgen_var_t lvalue_var = cgen_user_var(cgen, identifier, lvalue->value_type, lvalue->ref_decl->is_global);
        sb_add_format(&cgen->sb, "(&%s)", cgen_var_name(cgen, lvalue_var));
        break;
    }

    case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS: {
        ast_node_t *lhs = an_dot_lhs(lvalue);
        typedata_t *lhstd = ast_type2td(cgen->ast, lhs->value_type);

        struct_field_t field;
        {
            ortype_t struct_type = lhs->value_type;
            if (lhstd->kind == TYPE_POINTER) {
                struct_type = lhstd->as.ptr.type;
            }

            typedata_t *structtd = ast_type2td(cgen->ast, struct_type);
            field = structtd->as.struct_.fields.items[lvalue->arg_index];
        }

        unless (lvalue->requires_tmp_for_cgen) {
            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_lvar(cgen, var));
            }

            sb_add_cstr(&cgen->sb, "&(");

            cgen_expression(cgen, lhs, nil_cvar);

            orcstr_t op = lhstd->kind == TYPE_POINTER ? "->" : ".";
            sb_add_cstr(&cgen->sb, op);

            sb_add_format(&cgen->sb, "%s)", field.name->cstr);

        } else {
            cgen_var_t lhsvar = cgen_next_tmpid(cgen, lhs->value_type);
            cgen_lvalue(cgen, lhs->lvalue_node, lhsvar);
            cgen_semicolon_nl(cgen);

            orcstr_t deref = lhstd->kind == TYPE_POINTER ? "*" : "";

            cgen_add_indent(cgen);

            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_lvar(cgen, var));
            }
            
            sb_add_format(&cgen->sb, "(&(%s%s)->%s)", deref, cgen_var_name(cgen, lhsvar), field.name->cstr);
        }
        break;
    }

    case AST_NODE_TYPE_MODULE:
    case AST_NODE_TYPE_DECLARATION_DEFINITION:
    case AST_NODE_TYPE_DECLARATION_STATEMENT:
    case AST_NODE_TYPE_EXPRESSION_JMP:
    case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL:
    case AST_NODE_TYPE_EXPRESSION_CAST:
    case AST_NODE_TYPE_EXPRESSION_BINARY:
    case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE:
    case AST_NODE_TYPE_EXPRESSION_GROUPING:
    case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL:
    case AST_NODE_TYPE_EXPRESSION_CALL:
    case AST_NODE_TYPE_EXPRESSION_PRIMARY:
    case AST_NODE_TYPE_EXPRESSION_NIL:
    case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
    case AST_NODE_TYPE_EXPRESSION_BLOCK:
    case AST_NODE_TYPE_EXPRESSION_BRANCHING:
    case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
    case AST_NODE_TYPE_EXPRESSION_ENUM:
    case AST_NODE_TYPE_EXPRESSION_STRUCT:
    case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST:
    case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
    case AST_NODE_TYPE_EXPRESSION_DIRECTIVE:
    case AST_NODE_TYPE_NONE: break;
    }
}

static void cgen_assignment(cgen_t *cgen, ast_node_t *assignment, cgen_var_t var) {
    unless (assignment->requires_tmp_for_cgen) {
        ASSERT(assignment->operator.type == TOKEN_EQUAL, "other operators require a tmp");

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
        }

        sb_add_cstr(&cgen->sb, "(");
        cgen_expression(cgen, an_lhs(assignment), nil_cvar);

        sb_add_cstr(&cgen->sb, " = ");
        
        cgen_expression(cgen, an_rhs(assignment), nil_cvar);

        sb_add_cstr(&cgen->sb, ")");

        if (has_var(var)) {
            sb_add_cstr(&cgen->sb, ")");
        }
    } else {
        ast_node_t *lhs = an_lhs(assignment);

        cgen_var_t lvalue_var = cgen_next_tmpid(cgen, lhs->value_type);
        cgen_lvalue(cgen, lhs->lvalue_node, lvalue_var);
        cgen_semicolon_nl(cgen);

        token_type_t equals_tok = parser_opeq2op(assignment->operator.type);

        ast_node_t *rhs = an_rhs(assignment);
        cgen_var_t rhs_var = cgen_next_tmpid(cgen, rhs->value_type);

        cgen_statement(cgen, rhs, rhs_var, true);

        if (equals_tok != TOKEN_EQUAL) {
            typedata_t *td = ast_type2td(cgen->ast, assignment->value_type);
            if (is_type_kind_aggregate(td->kind)) {
                token_type_t op = parser_opeq2op(assignment->operator.type);

                cgen_add_indent(cgen);
                cgen_var_t lhs_var = cgen_next_tmpid(cgen, assignment->value_type);
                sb_add_format(&cgen->sb, "%s = *(%s)", cgen_var(cgen, lhs_var), cgen_var_name(cgen, lvalue_var));
                cgen_semicolon_nl(cgen);

                cgen_add_indent(cgen);
                sb_add_format(&cgen->sb, "%s = ", cgen_var_name(cgen, rhs_var));

                tmp_arena_t *tmp = allocator_borrow();
                string_builder_t lhs_lvalue = {.allocator=tmp->allocator};
                string_builder_t rhs_lvalue = {.allocator=tmp->allocator};

                sb_add_format(&lhs_lvalue, "%s", cgen_var_name(cgen, lhs_var));
                sb_add_format(&rhs_lvalue, "%s", cgen_var_name(cgen, rhs_var));
                cgen_aggregate_arith_binary(cgen, op, lhs->value_type, &lhs_lvalue, &rhs_lvalue);

                cgen_semicolon_nl(cgen);

                allocator_return(tmp);
            } else {
                cgen_add_indent(cgen);

                orcstr_t func_or_op;
                bool is_macro = cgen_binary_is_macro(equals_tok, td, &func_or_op);
                if (is_macro) {
                    sb_add_format(&cgen->sb, "(%s) = %s(*(%s), %s)",
                        cgen_var_name(cgen, rhs_var),
                        func_or_op,
                        cgen_var_name(cgen, lvalue_var),
                        cgen_var_name(cgen, rhs_var));
                } else {
                    sb_add_format(&cgen->sb, "(%s) = (*(%s) %s %s)",
                        cgen_var_name(cgen, rhs_var),
                        cgen_var_name(cgen, lvalue_var),
                        func_or_op,
                        cgen_var_name(cgen, rhs_var));
                }
                cgen_semicolon_nl(cgen);

            }
        }

        cgen_add_indent(cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
        }

        sb_add_format(&cgen->sb, "*(%s) = %s", cgen_var_name(cgen, lvalue_var), cgen_var_name(cgen, rhs_var));

        if (has_var(var)) {
            sb_add_format(&cgen->sb, ")");
        }
    }
}

static void cgen_constant_or_nil(cgen_t *cgen, ast_node_t *constant_or_nil, cgen_var_t var) {
    ASSERT(!constant_or_nil->requires_tmp_for_cgen, "primaries should never require tmps");

    if (TYPE_IS_VOID(constant_or_nil->value_type)) {
        // ASSERT(!has_var(var), "shouldnt have a var since its void");
        // if (has_var(var)) sb_add_cstr(&cgen->sb, "");
        // sb_add_cstr(&cgen->sb, "NOP()\n");
    } else {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        cgen_constant(cgen, constant_or_nil->expr_val.word, constant_or_nil->value_type);
    }
}

static void cgen_grouping(cgen_t *cgen, ast_node_t *grouping, cgen_var_t var) {
    bool can_inline = !grouping->requires_tmp_for_cgen;

    if (can_inline) {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ",  cgen_var(cgen, var));
        }

        sb_add_cstr(&cgen->sb, "(");
        ast_node_t *operand = an_operand(grouping);
        cgen_expression(cgen, operand, nil_cvar);
        sb_add_cstr(&cgen->sb, ")");
    } else {
        ast_node_t *operand = an_expression(grouping);
        cgen_expression(cgen, operand, var);
    }
}

static void cgen_block(cgen_t *cgen, ast_node_t *block, cgen_var_t var) {
    ASSERT(block->requires_tmp_for_cgen, "must require a tmp");

    if (no_var(var) || !var.is_new) {
        sb_add_cstr(&cgen->sb, "{\n");
    } else {
        sb_add_format(&cgen->sb, "%s; {\n", cgen_var(cgen, var));
    }

    cgen_indent(cgen);

    for (size_t i = 0; i < block->children.count; ++i) {
        ast_node_t *declaration = block->children.items[i];
        if (declaration == block->last_statement) {
            cgen_statement(cgen, an_expression(block->last_statement), cgen_var_used(var), true);
            if (i != block->children.count-1) {
                sb_add_char(&cgen->sb, '\n');
            }
        } else {
            cgen_declaration(cgen, declaration);
            sb_add_char(&cgen->sb, '\n');
        }
    }

    cgen_unindent(cgen);
    cgen_add_indent(cgen);
    sb_add_format(&cgen->sb, "}");
}

static void cgen_begin_branch(cgen_t *cgen, ast_node_t *branch, cgen_var_t var, bool skip_else) {
    if (no_var(var)) {
        sb_add_cstr(&cgen->sb, "{\n");
    } else {
        if (!skip_else) {
            if (var.is_new) {
                sb_add_format(&cgen->sb, "%s; {\n", cgen_var(cgen, var));
            } else {
                sb_add_cstr(&cgen->sb, "{\n");
            }
        } else {
            cgen_statement(cgen, an_else(branch), var, false);
            // sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
            ASSERT(!an_else(branch)->requires_tmp_for_cgen, "this should be nil and thus doesn't need a tmp");
            // cgen_expression(cgen, an_else(branch), nil_tmp_var);
            sb_add_cstr(&cgen->sb, "; {\n");
        }
    }
}

static void cgen_end_branch(cgen_t *cgen) {
    cgen_unindent(cgen);
    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "}");
}

static void cgen_condition_and_open_block(cgen_t *cgen, ast_node_t *branch, orcstr_t branch_cstr) {
    if (an_condition(branch)->requires_tmp_for_cgen) {
        if (branch->branch_type == BRANCH_TYPE_WHILE) {
            cgen_add_indent(cgen);
            sb_add_format(&cgen->sb, "while (true) {\n");

            cgen_indent(cgen);

            cgen_var_t conditionid = cgen_next_tmpid(cgen, an_condition(branch)->value_type);
            cgen_statement(cgen, an_condition(branch), conditionid, true);

            branch_cstr = branch->condition_negated ? "if" : "unless";
            cgen_add_indent(cgen);
            sb_add_format(&cgen->sb, "%s (%s) break;\n", branch_cstr, cgen_var_name(cgen, conditionid));
        } else {
            cgen_var_t conditionid = cgen_next_tmpid(cgen, an_condition(branch)->value_type);
            cgen_statement(cgen, an_condition(branch), conditionid, true);

            cgen_add_indent(cgen);
            sb_add_format(&cgen->sb, "%s (%s) {\n", branch_cstr, cgen_var_name(cgen, conditionid));

            cgen_indent(cgen);
        }
        
    } else {
        cgen_add_indent(cgen);
        sb_add_format(&cgen->sb, "%s (", branch_cstr);
        cgen_expression(cgen, an_condition(branch), nil_cvar);
        sb_add_cstr(&cgen->sb, ") {\n");
        cgen_indent(cgen);
    }

}

static void cgen_then(cgen_t *cgen, ast_node_t *branch, cgen_var_t var) {
    if (an_then(branch)->requires_tmp_for_cgen) {
        cgen_statement(cgen, an_then(branch), cgen_var_used(var), true);
    } else {
        cgen_add_indent(cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var_name(cgen, var));
        }

        cgen_expression(cgen, an_then(branch), nil_cvar);

        cgen_semicolon_nl(cgen);
    }
}

static void cgen_if(cgen_t *cgen, ast_node_t *branch, cgen_var_t var) {
    ASSERT(branch->requires_tmp_for_cgen, "branches always require a tmp");
    bool skip_else = an_else(branch)->node_type == AST_NODE_TYPE_EXPRESSION_NIL;
    cgen_begin_branch(cgen, branch, var, skip_else);

    orcstr_t if_or_unless = branch->condition_negated ? "unless" : "if";
    cgen_indent(cgen);
    cgen_condition_and_open_block(cgen, branch, if_or_unless);

    cgen_then(cgen, branch, var);

    cgen_unindent(cgen);

    if (!skip_else) {
        cgen_add_indent(cgen);
        sb_add_cstr(&cgen->sb, "} else {\n");

        cgen_indent(cgen);

        if (an_else(branch)->requires_tmp_for_cgen) {
            cgen_statement(cgen, an_else(branch), cgen_var_used(var), true);
        } else {
            cgen_add_indent(cgen);

            if (has_var(var)) {
                sb_add_format(&cgen->sb, "%s = ", cgen_var_name(cgen, var));
            }

            cgen_expression(cgen, an_else(branch), nil_cvar);
            cgen_semicolon_nl(cgen);
        }

        cgen_unindent(cgen);
    }

    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "}\n");

    cgen_end_branch(cgen);
}

static void cgen_while_or_for(cgen_t *cgen, ast_node_t *branch, cgen_var_t var)  {
    ASSERT(branch->requires_tmp_for_cgen, "branches always require a tmp");
    cgen_begin_branch(cgen, branch, var, false);
    cgen_indent(cgen);

    if (branch->branch_type == BRANCH_TYPE_FOR) {
        cgen_declaration(cgen, an_for_decl(branch));
    }

    orcstr_t while_or_until = branch->condition_negated ? "until" : "while";
    cgen_condition_and_open_block(cgen, branch, while_or_until);

    cgen_then(cgen, branch, nil_cvar);

    cgen_add_indent(cgen);
    cgen_jmp_label(cgen, branch->ccode_continue_label);
    cgen_semicolon_nl(cgen);

    if (branch->branch_type == BRANCH_TYPE_FOR) {
        ast_node_t *incr = an_for_incr(branch);
        cgen_add_indent(cgen);
        cgen_expression(cgen, incr, nil_cvar);
        cgen_semicolon_nl(cgen);
    }

    cgen_unindent(cgen);
    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "}\n");

    ast_node_t *elze = an_else(branch);
    if (elze->node_type != AST_NODE_TYPE_EXPRESSION_NIL || has_var(var))
        cgen_statement(cgen, elze, var, true);

    cgen_end_branch(cgen);

    sb_add_cstr(&cgen->sb, " ");
    cgen_jmp_label(cgen, branch->ccode_break_label);

}

static void cgen_do(cgen_t *cgen, ast_node_t *branch, cgen_var_t var) {
    ASSERT(branch->requires_tmp_for_cgen, "branches always require a tmp");
    cgen_begin_branch(cgen, branch, var, false);

    cgen_indent(cgen);

    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "do {\n");

    cgen_indent(cgen);

    if (an_expression(branch)->requires_tmp_for_cgen) {
        cgen_statement(cgen, an_expression(branch), cgen_var_used(var), true);
    } else {
        cgen_add_indent(cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var_name(cgen, var));
        }

        cgen_expression(cgen, an_expression(branch), nil_cvar);

        cgen_semicolon_nl(cgen);
    }

    cgen_unindent(cgen);

    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "} while(false); \n");

    cgen_end_branch(cgen);

    sb_add_cstr(&cgen->sb, " ");
    cgen_jmp_label(cgen, branch->ccode_break_label);
}

static void cgen_branching(cgen_t *cgen, ast_node_t *branch, cgen_var_t var) {
    switch (branch->branch_type) {
        case BRANCH_TYPE_WHILE:
        case BRANCH_TYPE_FOR:
        case BRANCH_TYPE_DO: {
            if (has_var(var)) {
                branch->ccode_var_name = cstr2string(cgen_var(cgen, cgen_var_used(var)), cgen->ast->arena);
            }
            branch->ccode_break_label = string_copy(cgen_next_label(cgen, "break"), cgen->ast->arena);
            branch->ccode_continue_label = string_copy(cgen_next_label(cgen, "continue"), cgen->ast->arena);
            if (branch->branch_type == BRANCH_TYPE_WHILE || branch->branch_type == BRANCH_TYPE_FOR) {
                cgen_while_or_for(cgen, branch, var);
            } else {
                cgen_do(cgen, branch, var);
            }
            break;
        }

        case BRANCH_TYPE_IF: {
            cgen_if(cgen, branch, var);
            break;
        }
    }
}

static void cgen_def_value(cgen_t *cgen, ast_node_t *def_value, cgen_var_t var) {
    ASSERT(!def_value->requires_tmp_for_cgen, "this should always be placable, like primaries");

    if (has_var(var)) {
        sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
    }

    string_view_t identifier = string2sv(def_value->ref_decl->ccode_var_name);

    cgen_var_t defname = cgen_user_var(cgen, identifier, def_value->value_type, def_value->ref_decl->is_global);
    sb_add_format(&cgen->sb, "%s", cgen_var_name(cgen, defname));
}

static void cgen_break_or_continue(cgen_t *cgen, ast_node_t *jmp, token_type_t type) {
    ASSERT(jmp->requires_tmp_for_cgen, "requires a tmp because its leaving the scope");

    cgen_var_t jmp_var = nil_cvar;
    if (type == TOKEN_BREAK && jmp->jmp_out_scope_node->ccode_var_name.length > 0) {
        jmp_var.name = jmp->jmp_out_scope_node->ccode_var_name;
        jmp_var.is_new = false;
    }

    cgen_expression(cgen, an_expression(jmp), jmp_var);
    cgen_semicolon_nl(cgen);
    
    cgen_add_indent(cgen);

    switch (type) {
        case TOKEN_CONTINUE: {
            orstring_t continue_label = jmp->jmp_out_scope_node->ccode_continue_label;
            sb_add_format(&cgen->sb, "goto %s", continue_label.cstr);
            break;
        }

        case TOKEN_BREAK: {
            orstring_t break_label = jmp->jmp_out_scope_node->ccode_break_label;
            sb_add_format(&cgen->sb, "goto %s", break_label.cstr);
            break;
        }
        default: UNREACHABLE();
    }
}

static bool cgen_is_void_nil(ast_node_t *expr) {
    return ortypeid_eq(ortypeid(TYPE_VOID), expr->value_type) && expr->node_type == AST_NODE_TYPE_EXPRESSION_NIL;
}

static void cgen_return(cgen_t *cgen, ast_node_t *ret) {
    bool is_void_nil = cgen_is_void_nil(an_expression(ret));

    cgen_var_t var;
    unless (is_void_nil) {
        var = cgen_next_tmpid(cgen, an_expression(ret)->value_type);
        cgen_expression(cgen, an_expression(ret), var);
        cgen_semicolon_nl(cgen);
        cgen_add_indent(cgen);
    }

    sb_add_cstr(&cgen->sb, "return");

    unless (is_void_nil) {
        sb_add_format(&cgen->sb, " %s", cgen_var_name(cgen, var));
    }
}

static void cgen_call(cgen_t *cgen, ast_node_t *call, cgen_var_t var, cstr_t builtin_func_name_or_null) {
    ast_node_t *callee = an_callee(call);

    bool is_builtin_call = builtin_func_name_or_null != NULL;
    
    // cgen_call should only be called with a builtin function if the builtin function has arguments
    MUST(!is_builtin_call || (is_builtin_call && (an_bcall_arg_end(call) - an_bcall_arg_start(call)) > 0));

    /*
    * the expressions (callee and arguments) need to be evaluated from left to right.
    * this means that only trailing expressions that can be inlined, can be inlined.
    * i.e. an expression can only be inlined for the call if it can be inlined itself or
    * there exists no following expression that cannot be inlined (requires tmp)
    * otherwise, if we do the tmps first and inline everything else, the order of evaluation
    * will change resulting in unexpected behaviour.
    * i could write an algorithm to check whether or not evaluation order matters so i can
    * inline as much as can, but that would only be for aesthetic reasons, so fuck that.
    */
    if (call->requires_tmp_for_cgen) {
        cgen_var_t callee_tmp;
        if (!is_builtin_call) {
            /*
            * no matter what, the callee requires a tmp because either it itself requires a tmp
            * or because an argument following it needs it. which is why we are in this if branch
            */
            callee_tmp = cgen_next_tmpid(cgen, callee->value_type);
            cgen_statement(cgen, callee, callee_tmp, false);
        }

        size_t arg_start = is_builtin_call ? an_bcall_arg_start(call) : an_call_arg_start(call);
        size_t arg_end = is_builtin_call ? an_bcall_arg_end(call) : an_call_arg_end(call);

        size_t start_inline_index = arg_end;
        for (size_t i = arg_end; i > arg_start; --i) {
            size_t i_ = i-1;
            ast_node_t *child = call->children.items[i_];
            if (child->requires_tmp_for_cgen) break;
            start_inline_index = i_;
        }

        size_t tmp_arg_var_count = (start_inline_index > arg_start) ? (start_inline_index - arg_start) : 0;

        // so this isn't a size 0 array
        cgen_var_t tmp_arg_vars[tmp_arg_var_count+1];

        // put all arguments before arguments that can be inlined in tmp variables
        for (size_t i = arg_start; i < start_inline_index; ++i) {
            ast_node_t *arg = call->children.items[i];
            cgen_var_t arg_var = cgen_next_tmpid(cgen, arg->value_type);

            size_t tmp_index = i - arg_start;
            tmp_arg_vars[tmp_index] = arg_var;

            cgen_statement(cgen, arg, arg_var, true);
        }

        cgen_add_indent(cgen);
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        if (is_builtin_call) {
            MUST(builtin_func_name_or_null);

            sb_add_format(&cgen->sb, "%s(", builtin_func_name_or_null);
        } else {
            sb_add_format(&cgen->sb, "%s(", cgen_var_name(cgen, callee_tmp));
        }

        // tmps
        for (size_t i = 0; i < tmp_arg_var_count; ++i) {
            if (i != 0) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            cgen_var_t arg_var = tmp_arg_vars[i];
            sb_add_format(&cgen->sb, "%s", cgen_var_name(cgen, arg_var));
        }

        // inlined 
        for (size_t i = start_inline_index; i < arg_end; ++i) {
            if (i != arg_start) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            ast_node_t *arg = call->children.items[i];
            cgen_expression(cgen, arg, nil_cvar);
        }

        sb_add_cstr(&cgen->sb, ")");
    } else {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        sb_add_cstr(&cgen->sb, "(");

        if (is_builtin_call) {
            sb_add_format(&cgen->sb, "%s", builtin_func_name_or_null);
        } else {
            cgen_expression(cgen, callee, nil_cvar);
        }
        sb_add_cstr(&cgen->sb, "(");

        size_t arg_start = is_builtin_call ? an_bcall_arg_start(call) : an_call_arg_start(call);
        size_t arg_end = is_builtin_call ? an_bcall_arg_end(call) : an_call_arg_end(call);
        for (size_t i = arg_start; i < arg_end; ++i) {
            if (i != arg_start) {
                sb_add_cstr(&cgen->sb, ", ");
            }
            ast_node_t *child = call->children.items[i];
            cgen_expression(cgen, child, nil_cvar);
        }

        sb_add_cstr(&cgen->sb, "))");
    }
}

static void cgen_builtin_call(cgen_t *cgen, ast_node_t *bcall, cgen_var_t var) {
    if (string_eq(*bcall->identifier, lit2str("sizeof"))) {
        MUST(!bcall->requires_tmp_for_cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        typedata_t *td = ast_type2td(cgen->ast, bcall->value_type);

        ast_node_t *op = bcall->children.items[an_bcall_arg_start(bcall)];
        typedata_t *optd = ast_type2td(cgen->ast, op->value_type);
        if (optd->kind == TYPE_TYPE) {
            MUST(op->expr_val.is_concrete);
            optd = ast_type2td(cgen->ast, op->expr_val.word.as.t);
        }
        sb_add_format(&cgen->sb, "(%s)sizeof(%s)", td->name.cstr, optd->name.cstr);

    } else if (bcall->operator.type == TOKEN_OFFSETPTR) {
        cgen_call(cgen, bcall, var, "oroffsetptr");
    } else if (bcall->operator.type == TOKEN_PTRDIFF) {
        cgen_call(cgen, bcall, var, "orptrdiff");
    } else {
        UNREACHABLE();
    }
}

static void cgen_cast(cgen_t *cgen, ast_node_t *cast, cgen_var_t var) {
    ast_node_t *castee = an_cast_expr(cast);
    if (cast->requires_tmp_for_cgen) {
        cgen_var_t tmp_var = cgen_next_tmpid(cgen, castee->value_type);
        cgen_statement(cgen, castee, tmp_var, false);

        cgen_add_indent(cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        sb_add_format(&cgen->sb, "orcast(%s, %s)", cgen_type_name(cgen, cast->value_type), cgen_var_name(cgen, tmp_var));
    } else {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        sb_add_format(&cgen->sb, "orcast(%s, ", cgen_type_name(cgen, cast->value_type));

        cgen_expression(cgen, castee, nil_cvar);

        sb_add_cstr(&cgen->sb, ")");
    }
}

static void cgen_item_access(cgen_t *cgen, ast_node_t *item_access, cgen_var_t var) {
    ast_node_t *accessee = an_item_accessee(item_access);
    typedata_t *accessee_td = ast_type2td(cgen->ast, accessee->value_type);

    unless (item_access->requires_tmp_for_cgen) {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
        }

        cgen_expression(cgen, accessee, nil_cvar);

        if (accessee_td->kind == TYPE_POINTER) {
            sb_add_cstr(&cgen->sb, "->");
        } else {
            sb_add_cstr(&cgen->sb, ".");
        }

        sb_add_cstr(&cgen->sb, "arr[");

        ast_node_t *accessor = an_item_accessor(item_access);
        cgen_expression(cgen, accessor, nil_cvar);

        sb_add_cstr(&cgen->sb, "]");

        if (has_var(var)) {
            sb_add_cstr(&cgen->sb, ")");
        }
    } else {
        cgen_var_t accessee_var = cgen_next_tmpid(cgen, accessee->value_type);
        cgen_statement(cgen, accessee, accessee_var, false);

        ast_node_t *accessor = an_item_accessor(item_access);
        cgen_var_t accessor_var = cgen_next_tmpid(cgen, accessor->value_type);
        cgen_statement(cgen, accessor, accessor_var, true);

        cgen_add_indent(cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
        }

        sb_add_format(&cgen->sb, "%s", cgen_var_name(cgen, accessee_var));

        typedata_t *accessee_td = ast_type2td(cgen->ast, accessee->value_type);
        if (accessee_td->kind == TYPE_POINTER) {
            sb_add_cstr(&cgen->sb, "->");
        } else {
            sb_add_cstr(&cgen->sb, ".");
        }

        sb_add_format(&cgen->sb, "arr[%s]", cgen_var_name(cgen, accessor_var));

        if (has_var(var)) {
            sb_add_cstr(&cgen->sb, ")");
        }
    }
}

static void cgen_initializer_list(cgen_t *cgen, ast_node_t *list, cgen_var_t var) {
    tmp_arena_t *tmp = allocator_borrow();
    ortype_t list_type = list->value_type;
    typedata_t *list_td = ast_type2td(cgen->ast, list_type);

    switch (list_td->kind) {
    // no need to differentiate between requires cache or not explicitly
    // because implicitly by outputting the statements that need a cache
    // it's handling the case where the list itself needs the cache
    case TYPE_ARRAY: {
        size_t list_start = an_list_start(list);
        size_t list_end = an_list_end(list);
        size_t last_requires_tmp_arg = an_list_start(list);
        for (size_t i = list_end; i > list_start; --i) {
            size_t i_ = i - 1;
            if (list->children.items[i_]->requires_tmp_for_cgen) {
                last_requires_tmp_arg = i;
                break;
            }
        }

        size_t length = list_end - list_start;
        cgen_var_t *tmp_vars = arena_alloc(tmp->allocator, sizeof(cgen_var_t)*length);

        for (size_t i = list_start; i < last_requires_tmp_arg; ++i) {
            ast_node_t *arg = list->children.items[i];

            cgen_var_t tmpvar = cgen_next_tmpid(cgen, arg->value_type);
            tmp_vars[i-list_start] = tmpvar;

            cgen_statement(cgen, arg, tmpvar, i != list_start);

            if (i == last_requires_tmp_arg-1) {
                cgen_add_indent(cgen);
            }
        }

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
        }

        cgen_array_start(cgen, list->value_type);

        for (size_t i = list_start; i < list_end; ++i) {
            if (i != list_start) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            if (i < last_requires_tmp_arg) {
                sb_add_format(&cgen->sb, "%s", cgen_var_name(cgen, tmp_vars[i-list_start]));
            } else {
                ast_node_t *arg = list->children.items[i];
                cgen_expression(cgen, arg, nil_cvar);
            }
        }

        cgen_array_end(cgen);

        if (has_var(var)) {
            sb_add_cstr(&cgen->sb, ")");
        }
        break;
    }

    case TYPE_STRING:
    case TYPE_STRUCT: {
        size_t list_start = an_list_start(list);
        size_t list_end = an_list_end(list);
        size_t list_count = list_end - list_start;

        size_t last_field_requires_tmp = 0;
        for (size_t i = list_end; i > list_start; --i) {
            ast_node_t *arg = list->children.items[i-1];
            if (arg->requires_tmp_for_cgen) {
                last_field_requires_tmp = arg->arg_index+1;
            }
        }

        size_t arg_pos = 0;

        cgen_var_t *tmp_vars = arena_alloc(tmp->allocator, sizeof(cgen_var_t)*last_field_requires_tmp);

        for (size_t i = 0; i < last_field_requires_tmp; ++i) {
            struct_field_t field = list_td->as.struct_.fields.items[i];
            ast_node_t *arg = NULL;
            cgen_var_t field_var = cgen_next_tmpid(cgen, field.type);
            tmp_vars[i] = field_var;
            if (arg_pos < list_count && (arg = list->children.items[an_list_start(list) + arg_pos])->arg_index == i) {
                ++arg_pos;
                cgen_statement(cgen, arg, field_var, i != 0);
            } else {
                cgen_constant(cgen, field.default_value, field.type);
            }
        }
        if (last_field_requires_tmp > 0) {
            cgen_add_indent(cgen);
        }

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
        }

        cgen_struct_start(cgen, list->value_type);

        for (size_t i = 0; i < last_field_requires_tmp; ++i) {
            if (i != 0) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            struct_field_t field = list_td->as.struct_.fields.items[i];
            cgen_var_t var = tmp_vars[i];
            sb_add_format(&cgen->sb, ".%s = %s", field.name->cstr, cgen_var_name(cgen, var));
        }

        if (last_field_requires_tmp > 0) {
            sb_add_cstr(&cgen->sb, ", ");
        }

        for (size_t i = last_field_requires_tmp; i < list_td->as.struct_.fields.count; ++i) {
            if (i != last_field_requires_tmp) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            struct_field_t field = list_td->as.struct_.fields.items[i];

            sb_add_format(&cgen->sb, ".%s = ", field.name->cstr);

            ast_node_t *arg = NULL;
            if (arg_pos < list_count && (arg = list->children.items[an_list_start(list) + arg_pos])->arg_index == i) {
                ++arg_pos;
                cgen_expression(cgen, arg, nil_cvar);
            } else {
                cgen_constant(cgen, field.default_value, field.type);
            }
        }

        cgen_struct_end(cgen);

        if (has_var(var)) {
            sb_add_cstr(&cgen->sb, ")");
        }
        break;
    }

    default: UNREACHABLE(); break;
    }

    allocator_return(tmp);
}

static void cgen_dot_access(cgen_t *cgen, ast_node_t *dot, cgen_var_t var) {
    ast_node_t *lhs = an_dot_lhs(dot);
    typedata_t *lhstd = ast_type2td(cgen->ast, lhs->value_type);
    orcstr_t operator;
    if (lhstd->kind == TYPE_POINTER) {
        operator = "->";
    }  else {
        operator = ".";
    }

    unless (dot->requires_tmp_for_cgen) {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        cgen_expression(cgen, lhs, nil_cvar);

        sb_add_format(&cgen->sb, "%s%s", operator, dot->identifier->cstr);
    } else {
        cgen_var_t lhs_var = cgen_next_tmpid(cgen, lhs->value_type);
        cgen_statement(cgen, lhs, lhs_var, false);

        cgen_add_indent(cgen);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        sb_add_format(&cgen->sb, "(%s%s)%s%s", "", cgen_var_name(cgen, lhs_var), operator, dot->identifier->cstr);
    }
}

static orstring_t ffi_cfuncname(ffi_t *ffi, arena_t *arena) {
    orstring_t libpath = ffi->libpath;
    string_view_t prefix = sv_filename(string2sv(libpath));
    prefix = sv_no_ext(prefix);

    orstring_t result = string_format("__orffi%.*s_%s", arena, prefix.length, prefix.data, ffi->funcname);
    return result;
}

static void cgen_fficall(cgen_t *cgen, ast_node_t *fficall, cgen_var_t var) {
    MUST(fficall->requires_tmp_for_cgen);

    size_t arg_count = an_fficall_arg_end(fficall) - an_fficall_arg_start(fficall);
    cgen_var_t vars[arg_count];

    for (size_t i = an_fficall_arg_start(fficall); i < an_fficall_arg_end(fficall); ++i) {
        ast_node_t *arg = fficall->children.items[i];

        cgen_var_t tmp = cgen_next_tmpid(cgen, arg->value_type);
        vars[i-an_fficall_arg_start(arg)] = tmp;
        cgen_statement(cgen, arg, tmp, i != an_fficall_arg_start(arg));
    }

    if (arg_count > 0) {
        cgen_add_indent(cgen);
    }

    if (has_var(var)) {
        sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
    }

    {
        MUST(fficall->ffi_or_null);
        tmp_arena_t *tmp = allocator_borrow();
        sb_add_format(&cgen->sb, "%s(", ffi_cfuncname(fficall->ffi_or_null, tmp->allocator).cstr);

        allocator_return(tmp);
    }
    for (size_t i = 0; i < arg_count; ++i) {
        if (i != 0) {
            sb_add_cstr(&cgen->sb, ", ");
        }

        cgen_var_t tmp = vars[i];
        sb_add_format(&cgen->sb, "%s", cgen_var_name(cgen, tmp));
    }
    sb_add_cstr(&cgen->sb, ")");
}

void cgen_icall(cgen_t *cgen, ast_node_t *icall, cgen_var_t var) {
    if (!icall->requires_tmp_for_cgen) {
        tmp_arena_t *tmp = allocator_borrow();
        orstring_t name = cgen_get_instrinsic_fn_name(cgen->ast, icall->intrinsic_fn, tmp->allocator);

        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
        }

        sb_add_format(&cgen->sb, "%s(", name.cstr);

        size_t start = 1;
        for (size_t i = start; i < icall->children.count; ++i) {
            ast_node_t *arg = icall->children.items[i];

            if (i != start) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            cgen_expression(cgen, arg, nil_cvar);
        }

        sb_add_format(&cgen->sb, ")");

        if (has_var(var)) {
            sb_add_cstr(&cgen->sb, ")");
        }

        allocator_return(tmp);
    } else {
        UNREACHABLE();
    }
}

static void cgen_expression(cgen_t *cgen, ast_node_t *expression, cgen_var_t var) {
    if (expression->expr_val.is_concrete) {
        cgen_constant_or_nil(cgen, expression, var);
        return;
    }

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: UNREACHABLE(); break;

        case AST_NODE_TYPE_EXPRESSION_NIL:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            cgen_constant_or_nil(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            cgen_binary(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            cgen_unary(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            cgen_assignment(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            cgen_grouping(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            cgen_block(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            cgen_branching(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            cgen_def_value(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            switch (expression->start.type) {
                case TOKEN_CONTINUE:
                case TOKEN_BREAK: {
                    cgen_break_or_continue(cgen, expression, expression->start.type);
                    break;
                }

                case TOKEN_RETURN: {
                    cgen_return(cgen, expression);
                    break;
                }

                default: UNREACHABLE();
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: {
            cgen_builtin_call(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            cgen_call(cgen, expression, var, NULL); 
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            cgen_cast(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
            if (expression->subscript_call_or_null) {
                cgen_subscript_call(cgen, expression->subscript_call_or_null, var);
            } else {
                cgen_item_access(cgen, expression, var);
            }
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
            cgen_initializer_list(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS: {
            cgen_dot_access(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT: break;

        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: {
            if (string_eq(*expression->identifier, lit2str("fficall"))) {
                cgen_fficall(cgen, expression, var);
            } else if (string_eq(*expression->identifier, lit2str("icall"))) {
                cgen_icall(cgen, expression, var);
            } else {
                UNREACHABLE();
            }
            break;
        }

        default: UNREACHABLE(); break;
    }
}

static void cgen_generate_function_names(ast_t *ast, cgen_state_t state, ast_node_t *module) {
    for (size_t i = 0; i < module->owned_funcdefs.count; ++i) {
        ast_node_t *funcdef = module->owned_funcdefs.items[i];
        function_t *function = funcdef->expr_val.word.as.p;
        MUST(funcdef->expr_val.is_concrete);

        funcdata_t funcdata;
        unless (table_get(p2n, state.functions, function, &funcdata)) {
            funcdata.type = funcdef->value_type;

            if (function->name_or_null == NULL || function->name_or_null->length == 0) {
                funcdata.name = string_format("__oranonfn_%zu", ast->arena, ++(*state.tmp_count));
            } else {
                funcdata.name = cgen_function_name(state, string2sv(*function->name_or_null), ast->arena);
            }

            table_put(p2n, state.functions, function, funcdata);
        }
    }
}

void cgen_forward_declare_functions(cgen_t *cgen, ast_node_t *module) {
    for (size_t i = 0; i < module->owned_funcdefs.count; ++i) {
        ast_node_t *funcdef = module->owned_funcdefs.items[i];

        if (ortypeid_eq(funcdef->value_type, ortypeid(TYPE_INFERRED_FUNCTION))) {
            continue;
        }

        MUST(funcdef->expr_val.is_concrete);

        function_t *function = (function_t*)funcdef->expr_val.word.as.p;
        funcdata_t funcdata;
        bool success = table_get(p2n, cgen->state.functions, function, &funcdata);
        UNUSED(success); MUST(success);

        typedata_t *td = type2typedata(&cgen->ast->type_set.types, funcdata.type);
        ortype_t rettype = td->as.function.return_type;
        
        sb_add_format(&cgen->sb, "%s %s(", cgen_type_name(cgen, rettype), funcdata.name);
        
        for (size_t a = 0; a < td->as.function.argument_types.count; ++a) {
            ortype_t argtype = td->as.function.argument_types.items[a];
            if (a != 0) {
                sb_add_cstr(&cgen->sb, ", ");
            }
            sb_add_format(&cgen->sb, "%s", cgen_type_name(cgen, argtype));
        }

        sb_add_cstr(&cgen->sb, ");\n");
    }
}

static void cgen_function_definitions(cgen_t *cgen, ast_node_t *module) {
    for (size_t i = 0; i < module->owned_funcdefs.count; ++i) {
        // <ret_type> <func_name>(<arg1_type> <arg1_name>, <arg2_type> <arg2_name>, ..., <argn_type> <argn_name>) {
        ast_node_t *funcdef = module->owned_funcdefs.items[i];

        if (ortypeid_eq(funcdef->value_type, ortypeid(TYPE_INFERRED_FUNCTION))) {
            continue;
        }

        typedata_t *td = type2typedata(&cgen->ast->type_set.types, funcdef->value_type);

        typedata_t *rettd = type2typedata(&cgen->ast->type_set.types, td->as.function.return_type);

        sb_add_format(&cgen->sb, "%s ", rettd->name.cstr);

        function_t *function = (function_t*)funcdef->expr_val.word.as.p;

        funcdata_t funcdata;
        bool success = table_get(p2n, cgen->state.functions, function, &funcdata);
        UNUSED(success);
        ASSERT(success, "the funcdata should already be there");

        sb_add_format(&cgen->sb, "%s(", funcdata.name.cstr);

        for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
            if (i != an_func_def_arg_start(funcdef)) {
                sb_add_cstr(&cgen->sb, ", ");
            }

            ast_node_t *arg = funcdef->children.items[i];
            ortype_t arg_type = arg->value_type;

            arg->ccode_var_name = cgen_local_new_name(string2sv(*arg->identifier), cgen->state, cgen->ast->arena);
            cgen_var_t var = cgen_user_var(cgen, string2sv(arg->ccode_var_name), arg_type, false);
            sb_add_format(&cgen->sb, "%s", cgen_var(cgen, var));
        }

        sb_add_cstr(&cgen->sb, ") {\n");
        cgen_indent(cgen);

        cgen_add_indent(cgen);

        cgen_expression(cgen, an_func_def_block(funcdef), nil_cvar);

        cgen_semicolon_nl(cgen);

        cgen_unindent(cgen);
        sb_add_cstr(&cgen->sb, "}\n\n");
    }
}

static void cgen_struct(cgen_t *cgen, ortype_t type, bools_t *bools) {
    if (bools->items[type.i]) return;
    bools->items[type.i] = true;

    typedata_t *td = ast_type2td(cgen->ast, type);
    switch (td->kind) {
    case TYPE_STRING:
    case TYPE_STRUCT: {
        if (td->as.struct_.binding_or_null != NULL) return;

        for (size_t i = 0; i < td->as.struct_.fields.count; ++i) {
            struct_field_t field = td->as.struct_.fields.items[i];
            cgen_struct(cgen, field.type, bools);
        }

        sb_add_format(&cgen->sb, "struct %s {\n", td->name.cstr);
        cgen_indent(cgen);

        for (size_t i = 0; i < td->as.struct_.fields.count; ++i) {
            cgen_add_indent(cgen);

            struct_field_t field = td->as.struct_.fields.items[i];
            typedata_t *fieldtd = ast_type2td(cgen->ast, field.type);

            sb_add_format(&cgen->sb, "%s %s;\n", fieldtd->name.cstr, field.name->cstr);
        }

        cgen_unindent(cgen);
        sb_add_format(&cgen->sb, "};\n");
        break;
    }

    case TYPE_ARRAY: {
        cgen_struct(cgen, td->as.arr.type, bools);

        typedata_t *innertd = ast_type2td(cgen->ast, td->as.arr.type);
        sb_add_format(&cgen->sb, "struct %s { %s arr[%zu]; };\n", td->name.cstr, innertd->name.cstr, td->as.arr.count);
        break;
    }

    default: break;
    }
}

 static void cgen_structs(cgen_t *cgen) {
    tmp_arena_t *tmp = allocator_borrow();
    bools_t bools = {.allocator=tmp->allocator};

    for (size_t i = 0; i < cgen->ast->type_set.types.count; ++i) {
        array_push(&bools, false);
    }

    for (size_t i = 0; i < cgen->ast->type_set.types.count; ++i) {
        cgen_struct(cgen, ortypeid(i), &bools);
    }

    allocator_return(tmp);
}

void cgen_define_functions(cgen_t *cgen, ast_node_t *module) {
    cgen_function_definitions(cgen, module);
}

static void cgen_generate_cnames_for_types(ast_t *ast) {
    typedatas_t *tds = &ast->type_set.types;
    arena_t *arena = ast->arena;

    for (size_t i = 0; i < tds->count; ++i) {
        typedata_t *td = tds->items[i];
        switch(td->kind) {
        case TYPE_BOOL: td->name = lit2str("orbool"); break;
        case TYPE_STRING:
        case TYPE_NUMBER: ASSERT(td->name.length != 0, "should be already set"); break;
        case TYPE_TYPE: td->name = lit2str("ortype_t"); break;
        case TYPE_VOID: td->name = lit2str("void"); break;
        case TYPE_POINTER: {
            typedata_t *innertd = type2typedata(tds, td->as.ptr.type);
            ASSERT(innertd->name.length != 0, "all dependendant types should be before this one by construction");

            orstring_t name = string_format("p_%s", arena, innertd->name.cstr);
            td->name = name;
            break;
        }

        case TYPE_FUNCTION: {
            tmp_arena_t *tmp = allocator_borrow();
            string_builder_t sb = {.allocator=tmp->allocator};
            // fn_<arg1_type>_<arg2_type>_..._<argn_type>_<ret_type>

            sb_add_cstr(&sb, "fn_");

            for (size_t i = 0; i < td->as.function.argument_types.count; ++i) {
                ortype_t arg_type = td->as.function.argument_types.items[i];
                typedata_t *argtd = type2typedata(tds, arg_type);
                sb_add_format(&sb, "%s_", argtd->name.cstr);
            }

            ortype_t ret_type = td->as.function.return_type;
            typedata_t *rettd = type2typedata(tds, ret_type);

            sb_add_format(&sb, "%s", rettd->name.cstr);

            td->name = sb_render(&sb, arena);

            allocator_return(tmp);
            break;
        }

        case TYPE_ARRAY: {
            //arr_<size>_<type>
            tmp_arena_t *tmp = allocator_borrow();
            string_builder_t sb = {.allocator=tmp->allocator};

            
            sb_add_format(&sb, "arr_%zu_%s", td->as.arr.count, type2typedata(tds, td->as.arr.type)->name.cstr);

            td->name = sb_render(&sb, arena);
            allocator_return(tmp);
            break;
        }

        case TYPE_STRUCT: {
            if (td->name.length > 0) break;

            // struct_typeid<ortypeid>

            tmp_arena_t *tmp = allocator_borrow();
            string_builder_t sb = {.allocator=tmp->allocator};

            
            sb_add_format(&sb, "struct%zu", i);

            td->name = sb_render(&sb, arena);
            allocator_return(tmp);
            break;
        }

        case TYPE_MODULE:
        case TYPE_PARAM_STRUCT:
        case TYPE_INFERRED_FUNCTION:
        case TYPE_UNREACHABLE:
        case TYPE_UNRESOLVED:
        case TYPE_INVALID: break;

        case TYPE_COUNT: UNREACHABLE(); break;
        }
    }
}

static void cgen_typedefs(cgen_t *cgen, typedatas_t *tds) {
    for (size_t i = 0; i < tds->count; ++i) {
        typedata_t *td = tds->items[i];

        switch (td->kind) {
        case TYPE_ARRAY:  {
            // typedef <type> <typename>[<count>]
            sb_add_format(&cgen->sb, "typedef struct %s %s;\n", td->name.cstr, td->name.cstr);
            break;
        }

        case TYPE_POINTER: {
            // typedef <inner_typename>* <typename>

            typedata_t *innertd = type2typedata(tds, td->as.ptr.type);
            sb_add_format(&cgen->sb, "typedef %s* %s;\n", innertd->name.cstr, td->name.cstr);
            break;
        }

        case TYPE_STRING:
        case TYPE_STRUCT: {
            if (td->as.struct_.binding_or_null != NULL) continue;

            // typedef struct <struct_name> <struct_name>
            sb_add_format(&cgen->sb, "typedef struct %s %s;\n", td->name.cstr, td->name.cstr);
            break;
        }

        case TYPE_FUNCTION: {
            // typedef <ret_type>(*<type_name>)(<arg1_type>,<arg2_type>, ...,<argn_type>);

            sb_add_cstr(&cgen->sb, "typedef ");
            typedata_t *rettd = type2typedata(tds, td->as.function.return_type);
            sb_add_format(&cgen->sb, "%s(*%s)(", rettd->name.cstr, td->name.cstr);

            for (size_t i = 0; i < td->as.function.argument_types.count; ++i) {
                ortype_t arg_type = td->as.function.argument_types.items[i];
                typedata_t *argtd = type2typedata(tds, arg_type);

                if (i != 0) {
                    sb_add_cstr(&cgen->sb, ",");
                }

                sb_add_format(&cgen->sb, "%s", argtd->name.cstr);
            }

            sb_add_format(&cgen->sb, ");\n");

            break;
        }

        default: break;;
        }
    }
}

cgen_state_t make_cgen_state(arena_t *arena) {
    cgen_state_t state = {0};
    state.tmp_count = (size_t*)arena_alloc(arena, sizeof(size_t));
    *state.tmp_count = 0;
    state.functions = table_new(p2n, arena);
    return state;
}

cgen_t make_cgen(ast_t *ast, arena_t *arena, cgen_state_t state) {
    cgen_t cgen = {.ast = ast, .state = state, .tmp_arena=arena };
    cgen.sb.allocator = arena;
    cgen.sb.allocator = arena;

    return cgen;
}

void cgen_declare_global_decls(cgen_t *cgen, ast_nodes_t *decls) {
    for (size_t i = 0; i < decls->count; ++i) {
        ast_node_t *decl = decls->items[i];
        if (an_is_constant(decl)) continue;

        cgen_var_t var = cgen_user_var(cgen, string2sv(decl->ccode_var_name), decl->value_type, true);
        sb_add_format(&cgen->sb, "%s;\n", cgen_var(cgen, var));
    }

    sb_add_cstr(&cgen->sb, "\n");
}

static void cgen_begin_h(cgen_t *cgenh, orstring_t header_define) {
    sb_add_format(&cgenh->sb, "#ifndef %s\n", header_define.cstr);
    sb_add_format(&cgenh->sb, "#define %s\n\n", header_define.cstr);
}

static void cgen_end_h(cgen_t *cgenh) {
    sb_add_format(&cgenh->sb, "\n#endif\n");
}

void cgen_init_function_file(cgen_t *cgen, cgen_t *cgenh, ast_t *ast) {
    // size_t id = ++(*cgen->state.tmp_count);
    orstring_t init_func_name = lit2str("__orminit_");//string_format("__orminit_", cgen->tmp_arena);

    // .h
    {
        cgen_begin_h(cgenh, lit2str("__orinit_func_h"));

        sb_add_format(&cgenh->sb, "void %s(void);\n", init_func_name.cstr);

        cgen_end_h(cgenh);
    }

    // .c
    {
        cgen_add_include(cgen, "__orinit_func.h");

        {
            ast_node_t *module;
            kh_foreach_value(ast->moduleid2node, module, {
                cgen_add_include(cgen, module->ccode_associated_h.cstr);
            });
        }

        sb_add_format(&cgen->sb, "void %s(void) {\n", init_func_name.cstr);
        cgen_indent(cgen);

        for (size_t i = 0; i < ast->global_decls_in_resolution_order.count; ++i) {
            ast_node_t *decl = ast->global_decls_in_resolution_order.items[i];

            cgen_var_t var = cgen_user_var(cgen, string2sv(decl->ccode_var_name), decl->value_type, true);
            var.is_new = false;

            ast_node_t *init_expr = an_decl_expr(decl);
            cgen_statement(cgen, init_expr, var, true);

            sb_add_cstr(&cgen->sb, "\n");
        }

        cgen_unindent(cgen);
        sb_add_cstr(&cgen->sb, "}\n\n");
    }
}


void cgen_global_decls(cgen_t *cgenh, ast_node_t *module) {
    cgen_declare_global_decls(cgenh, &module->children);
}

static void cgen_module(cgen_t *cgen, cgen_t *cgenh, bool is_core, ast_node_t *module, orstring_t moduleid) {
    // .h
    {
        cgen_begin_h(cgenh, moduleid);
        if (is_core) {
            cgen_add_include(cgenh, "intrinsics.h");
            cgen_add_include(cgenh, "orso.h");
            cgen_typedefs(cgenh, &cgen->ast->type_set.types);
            cgen_structs(cgenh);
        } else {
            cgen_add_include(cgenh, CORE_H_FILENAME);
        }

        cgen_global_decls(cgenh, module);

        cgen_forward_declare_functions(cgenh, module);

        cgen_end_h(cgenh);
    }

    // .c
    {
        if (is_core) {
            cgen_add_include(cgen, CORE_H_FILENAME);

            // implementation is not needed because its included in liborso
            cgen_add_include(cgen, "intrinsics.h");
        } else {
            cgen_add_include(cgen, module->ccode_associated_h.cstr);
            cgen_add_include(cgen, CORE_H_FILENAME);
            cgen_add_include(cgen, ORFFI_H_FILENAME);

            for (size_t i = 0; i < module->module_deps.count; ++i) {
                ast_node_t *m = module->module_deps.items[i];
                cgen_add_include(cgen, m->ccode_associated_h.cstr);
            }
        }

        cgen_define_functions(cgen, module);
    }
}

static orstring_t cgen_generate_filename_hext(orstring_t moduleid, orstring_t filepath, arena_t *arena) {
    string_view_t filename = sv_filename(string2sv(filepath));

    orstring_t result = string_format("%s_%.*s.h", arena, moduleid.cstr, filename.length, filename.data);
    return result;
}

static orstring_t cgen_generate_associated_c_from_h_filename(orstring_t hfilename, arena_t *arena) {
    string_view_t noext = sv_no_ext(string2sv(hfilename));
    orstring_t cname = string_format("%.*s.c", arena, noext.length, noext.data);
    return cname;
}

static void cgen_generate_associated_h_filenames(ast_t *ast) {
    ast->core_module_or_null->ccode_associated_h = lit2str(CORE_H_FILENAME);

    orstring_t moduleid;
    ast_node_t *module;
    kh_foreach(ast->moduleid2node, moduleid, module, {
        orstring_t filename = cgen_generate_filename_hext(moduleid, module->filepath, ast->arena);
        module->ccode_associated_h = filename;
    });
}

static void cgen_generate_global_names(ast_t *ast, cgen_state_t state) {
    for (size_t i = 0; i < ast->global_decls_in_resolution_order.count; ++i) {
        ast_node_t *decl = ast->global_decls_in_resolution_order.items[i];
        orstring_t name = cgen_global_new_name(string2sv(*decl->identifier), state, ast->arena);
        decl->ccode_var_name = name;
    }
}

typedef struct ffis_t ffis_t;
struct ffis_t {
    ffi_t **items;
    size_t capacity;
    size_t count;
    arena_t *allocator;
};

typedef struct ffilib_t ffilib_t;
struct ffilib_t {
    orstring_t libpath;
    ffis_t ffis;
};

typedef struct ffilibs_t ffilibs_t;
struct ffilibs_t {
    struct ffilib_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};


static bool find_ffilib(ffilibs_t *libs, orstring_t libpath, ffilib_t **ffilib) {
    for (size_t i = 0; i < libs->count; ++i) {
        orstring_t path = libs->items[i].libpath;
        if (string_eq(path, libpath)) {
            *ffilib = &libs->items[i];
            return true;
        }
    }

    return false;
}

static bool cgen_generate_ffi(ast_t *ast, cgen_t *cgen, cgen_t *cgenh, cgen_state_t cgen_state, orstring_t build_dir, strings_t *sources, strings_t *libpaths) {
    tmp_arena_t *tmp = allocator_borrow();

    ffilibs_t libs = {.allocator=tmp->allocator};
    {
        orstring_t key;
        ffi_t *ffi;
        kh_foreach(ast->ffis, key, ffi, {
            ffilib_t *fl = NULL;
            if (!find_ffilib(&libs, ffi->libpath, &fl)) {
                ffilib_t flib = {0};
                flib.libpath = ffi->libpath;
                flib.ffis = (ffis_t){.allocator=tmp->allocator};
                array_push(&libs, flib);
                fl = &libs.items[libs.count-1];

                array_push(libpaths, flib.libpath);
            }

            array_push(&fl->ffis, ffi);
        });
    }

    bool success = true;

    strings_t ffihs = {.allocator=tmp->allocator};

    for (size_t i = 0; i < libs.count; ++i) {
        *cgen = make_cgen(ast, tmp->allocator, cgen_state);
        *cgenh = make_cgen(ast, tmp->allocator, cgen_state);

        orstring_t libpath = libs.items[i].libpath;
        ffilib_t lib = libs.items[i];

        string_view_t filename = sv_no_ext(sv_filename(string2sv(libpath)));
        orstring_t header_define = string_format("orffi%.*s_H_", tmp->allocator, filename.length, filename.data);

        // h
        {
            cgen_begin_h(cgenh, header_define);

            cgen_add_include(cgenh, CORE_H_FILENAME);

            for (size_t j = 0; j < lib.ffis.count; ++j) {
                ffi_t *ffi = libs.items[i].ffis.items[j];

                sb_add_format(&cgenh->sb, "%s %s(", cgen_type_name(cgenh, ffi->return_type), ffi_cfuncname(ffi, tmp->allocator).cstr);
                for (size_t i = 0; i < ffi->arg_types.count; ++i) {
                    if (i != 0) {
                        sb_add_cstr(&cgenh->sb, ", ");
                    }

                    sb_add_format(&cgenh->sb, "%s", cgen_type_name(cgenh, ffi->arg_types.items[i]));
                }

                sb_add_cstr(&cgenh->sb, ")");
                cgen_semicolon_nl(cgenh);
            }

            cgen_end_h(cgenh);
        }

        // c
        {
            orstring_t orffih = string_format("orffi%.*s.h", tmp->allocator, filename.length, filename.data);
            array_push(&ffihs, orffih);

            cgen_add_include(cgen, orffih.cstr);

            for (size_t j = 0; j < lib.ffis.count; ++j) {
                ffi_t *ffi = libs.items[i].ffis.items[j];

                sb_add_format(&cgen->sb, "%s %s(", cgen_type_name(cgen, ffi->return_type), ffi->funcname.cstr);

                for (size_t i = 0; i < ffi->arg_types.count; ++i) {
                    if (i != 0) {
                        sb_add_cstr(&cgen->sb, ", ");
                    }

                    sb_add_format(&cgen->sb, "%s", cgen_type_name(cgen, ffi->arg_types.items[i]));
                }
                sb_add_cstr(&cgen->sb, ")");
                cgen_semicolon_nl(cgen);

                sb_add_format(&cgen->sb, "%s %s(", cgen_type_name(cgen, ffi->return_type), ffi_cfuncname(ffi, tmp->allocator).cstr);
                for (size_t i = 0; i < ffi->arg_types.count; ++i) {
                    if (i != 0) {
                        sb_add_cstr(&cgen->sb, ", ");
                    }

                    sb_add_format(&cgen->sb, "%s arg%zu", cgen_type_name(cgen, ffi->arg_types.items[i]), i);
                }

                sb_add_cstr(&cgen->sb, ") {\n");
                
                cgen_indent(cgen);
                cgen_add_indent(cgen);
                if (!TYPE_IS_VOID(ffi->return_type)) {
                    sb_add_cstr(&cgen->sb, "return ");
                }

                sb_add_format(&cgen->sb, "%s(", ffi->funcname.cstr);
                for (size_t i = 0; i < ffi->arg_types.count; ++i) {
                    if (i != 0) {
                        sb_add_cstr(&cgen->sb, ", ");
                    }

                    sb_add_format(&cgen->sb, "arg%zu", i);
                }

                sb_add_cstr(&cgen->sb, ")");
                cgen_semicolon_nl(cgen);

                cgen_unindent(cgen);
                cgen_add_indent(cgen);

                sb_add_cstr(&cgen->sb, "}\n\n");
            }
        }

        orstring_t orffic = string_format("%sorffi%.*s.c", tmp->allocator, build_dir.cstr, filename.length, filename.data);
        orstring_t orffih = string_format("%sorffi%.*s.h", tmp->allocator, build_dir.cstr, filename.length, filename.data);
        array_push(sources, orffic);

        success &= write_entire_file(orffic.cstr, cgen->sb.items, cgen->sb.count);
        success &= write_entire_file(orffih.cstr, cgenh->sb.items, cgenh->sb.count);
    }

    // orffi.h
    {
        *cgenh = make_cgen(ast, tmp->allocator, cgen_state);
        cgen_begin_h(cgenh, lit2str("ORFFI_H_"));

        for (size_t i = 0; i < ffihs.count; ++i) {
            orstring_t h = ffihs.items[i];
            cgen_add_include(cgenh, h.cstr);
        }

        cgen_end_h(cgenh);

        orstring_t h = string_format("%s"ORFFI_H_FILENAME, tmp->allocator, build_dir.cstr);
        success &= write_entire_file(h.cstr, cgenh->sb.items, cgenh->sb.count);
    }

    allocator_return(tmp);

    return success;
}

bool compile_ast_to_c(ast_t *ast, orstring_t build_directory, strings_t *sources, strings_t *libs, arena_t *arena) {
    cgen_generate_cnames_for_types(ast);

    cgen_generate_associated_h_filenames(ast);

    tmp_arena_t *tmp = allocator_borrow();
    cgen_state_t cgen_state = make_cgen_state(tmp->allocator);
    cgen_generate_global_names(ast, cgen_state);
    cgen_cache_requires_tmp(&ast->type_set.types, ast->core_module_or_null);
    cgen_generate_function_names(ast, cgen_state, ast->core_module_or_null);

    {
        ast_node_t *module;
        kh_foreach_value(ast->moduleid2node, module, cgen_cache_requires_tmp(&ast->type_set.types, module));
        kh_foreach_value(ast->moduleid2node, module, cgen_generate_function_names(ast, cgen_state, module));
    }

    *sources = (strings_t){.allocator=arena};
    *libs = (strings_t){.allocator=arena};


    cgen_t cgen = {0};
    cgen_t cgenh = {0};
    bool success = true;

    // core
    {
        cgen = make_cgen(ast, tmp->allocator, cgen_state);
        cgenh = make_cgen(ast, tmp->allocator, cgen_state);

        orstring_t corec = cgen_generate_associated_c_from_h_filename(ast->core_module_or_null->ccode_associated_h, tmp->allocator);

        cgen_module(&cgen, &cgenh, true, ast->core_module_or_null, lit2str(ORCORE_MODULE_NAME));

        corec = string_format("%s%s", tmp->allocator, build_directory.cstr, corec.cstr);
        array_push(sources, corec);

        orstring_t coreh = string_format("%s%s", tmp->allocator, build_directory.cstr, ast->core_module_or_null->ccode_associated_h.cstr);

        success &= write_entire_file(corec.cstr, cgen.sb.items, cgen.sb.count);
        success &= write_entire_file(coreh.cstr, cgenh.sb.items, cgenh.sb.count);
    }

    // ffi
    {
        success &= cgen_generate_ffi(ast, &cgen, &cgenh, cgen_state, build_directory, sources, libs);
    }

    // init func
    {
        cgen = make_cgen(ast, tmp->allocator, cgen_state);
        cgenh = make_cgen(ast, tmp->allocator, cgen_state);

        cgen_init_function_file(&cgen, &cgenh, ast);

        orstring_t init_funcc = string_format("%s%s", tmp->allocator, build_directory.cstr, "__orinit_func.c");
        array_push(sources, init_funcc);

        orstring_t init_funch = string_format("%s%s", tmp->allocator, build_directory.cstr, "__orinit_func.h");

        success &= write_entire_file(init_funcc.cstr, cgen.sb.items, cgen.sb.count);
        success &= write_entire_file(init_funch.cstr, cgenh.sb.items, cgenh.sb.count);
    }

    // code
    {
        orstring_t moduleid;
        ast_node_t *module;

        kh_foreach(ast->moduleid2node, moduleid, module, {
            cgen = make_cgen(ast, tmp->allocator, cgen_state);
            cgenh = make_cgen(ast, tmp->allocator, cgen_state);

            cgen_module(&cgen, &cgenh, false, module, moduleid);

            function_t *main_or_null = find_main_or_null(module);

            if (main_or_null) {
                cgen_add_include(&cgen, "__orinit_func.h");
                orstring_t funcname = cgen_get_function_name(&cgen, main_or_null);
                sb_add_format(&cgen.sb, "int main() { __orminit_(); %s(); }\n\n", funcname.cstr);
            }

            orstring_t pathc = cgen_generate_associated_c_from_h_filename(module->ccode_associated_h, tmp->allocator);
            pathc = string_format("%s%s", tmp->allocator, build_directory.cstr, pathc.cstr);

            orstring_t pathh = string_format("%s%s", tmp->allocator, build_directory.cstr, module->ccode_associated_h.cstr);;

            success &= write_entire_file(pathc.cstr, cgen.sb.items, cgen.sb.count);
            success &= write_entire_file(pathh.cstr, cgenh.sb.items, cgenh.sb.count);

            array_push(sources, pathc);
        });
    }

    allocator_return(tmp);

    return success;
}

#undef CODEGENC_IMPLEMENTATION
#endif
