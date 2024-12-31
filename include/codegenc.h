#ifndef CODEGENC_H_
#define CODEGENC_H_

#include "stringt.h"
#include "parser.h"
#include "tmp.h"

string_t compile_expr_to_c(ast_t *ast, arena_t *allocator);

#endif

#ifdef CODEGENC_IMPLEMENTATION
#include "memarr.h"

typedef struct cgen_t cgen_t;
struct cgen_t {
    ast_t *ast;
    size_t tmp_count;
    size_t indent;

    arena_t tmp_arena;
};

typedef struct cgen_var_t cgen_var_t;
struct cgen_var_t {
    string_t name;
    bool is_new;
    size_t id;
    type_t type;
};

static cgen_var_t nil_tmp_var = {.is_new = false, .id = 0 , .type = typeid(TYPE_INVALID) };


#define no_var(tv) ((tv).id == 0 && (tv).name.length == 0)
#define has_var(tv) ((tv).id > 0 || (tv).name.length > 0)

static string_t cgen_definition_name(cgen_t *cgen, string_view_t name) {
    string_t r = string_format("%.*s_", &cgen->tmp_arena, (int)name.length, name.data);
    return r;
}

static cgen_var_t cgen_user_var(cgen_t *cgen, string_view_t name, type_t type) {
    cgen_var_t var = {0};
    var.name = cgen_definition_name(cgen, name);
    var.type = type;
    var.is_new = true;

    return var;
}

static cgen_var_t cgen_var_not_new(cgen_var_t var) {
    cgen_var_t r = var;
    r.is_new = false;
    return r;
}

static cgen_var_t cgen_next_tmpid(cgen_t *cgen, type_t type) {
    return (cgen_var_t){ .is_new = true, .id = ++cgen->tmp_count, .type = type };
}

static void cgen_indent(cgen_t *cgen) {
    ++cgen->indent;
}

static void cgen_unindent(cgen_t *cgen) {
    --cgen->indent;
}

static void cgen_add_indent(string_builder_t *sb, size_t indent) {
    for (size_t i = 0; i < indent; ++i) {
        sb_add_cstr(sb, "  ");
    }
}

static cstr_t cgen_type_name(cgen_t *cgen, type_t type) {
    type_info_t *type_info = get_type_info(&cgen->ast->type_set.types, type);

    switch (type_info->kind) {
        case TYPE_NUMBER: {
            switch (type_info->size) {
                case 1: UNREACHABLE(); return "byte";
                case 2: UNREACHABLE(); return "u16";
                case 4: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: return "f32";
                        case NUM_TYPE_SIGNED: return "i32";
                        case NUM_TYPE_UNSIGNED: return "u32";
                    }
                    break;
                }

                case 8: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: return "f64";
                        case NUM_TYPE_SIGNED: return "i64";
                        case NUM_TYPE_UNSIGNED: return "u64";
                    }
                    break;
                }

                default: UNREACHABLE(); return "void";
            }
        }

        case TYPE_BOOL: return "bool";

        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_TYPE:
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
        case TYPE_STRUCT: UNREACHABLE(); return "void";

        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_UNDEFINED:
        case TYPE_COUNT: UNREACHABLE(); return "void";
    }
}

static cstr_t cgen_var_name(cgen_t *cgen, cgen_var_t var) {
    if (var.name.length > 0) {
        return var.name.cstr;
    } else {
        string_t result = string_format("tmp%llu", &cgen->tmp_arena, var.id);
        return result.cstr;
    }
}

static cstr_t cgen_var(cgen_t *cgen, cgen_var_t var) {
    if (var.is_new) {
        string_t result = string_format("%s %s", &cgen->tmp_arena, cgen_type_name(cgen, var.type), cgen_var_name(cgen, var));
        return result.cstr;
    } else {
        return cgen_var_name(cgen, var);
    }
}

static void *memarr_value_at(memarr_t *memarr, value_index_t value_index) {
    if (value_index.exists && value_index.index < memarr->count) {
        return memarr_get_ptr(memarr, value_index);
    }

    UNREACHABLE();
    return NULL;
}

static void cgen_expression(cgen_t *cgen, string_builder_t *sb, ast_node_t *expression, cgen_var_t tmp_var);

static void cgen_cache_requires_tmp(ast_node_t *expression) {
    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            expression->requires_tmp_for_cgen = true;
            break;
        }

        default:  {
            expression->requires_tmp_for_cgen = false;
            break;
        }
    }

    for (size_t i = 0; i < expression->children.count; ++i) {
        ast_node_t *child = expression->children.items[i];
        cgen_cache_requires_tmp(child);
        if (child->requires_tmp_for_cgen) {
            expression->requires_tmp_for_cgen = true;
        }
    }
}

static void cgen_primary(cgen_t *cgen, string_builder_t *sb, value_index_t value_index, type_info_t *type_info) {
    #define value_at(ty) (*((ty*)(memarr_value_at(&cgen->ast->constants, value_index))))

    switch (type_info->kind) {
        case TYPE_NUMBER: {
            switch (type_info->size) {
                case 1: UNREACHABLE(); break;
                case 2: UNREACHABLE(); break;
                case 4: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: sb_add_format(sb, "%g", value_at(f32)); break;
                        case NUM_TYPE_SIGNED: sb_add_format(sb, "%lu", value_at(u32)); break;
                        case NUM_TYPE_UNSIGNED: sb_add_format(sb, "%d", value_at(i32)); break;
                    }
                    break;
                }
                case 8: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: sb_add_format(sb, "%lg", value_at(f64)); break;
                        case NUM_TYPE_SIGNED: sb_add_format(sb, "%llu", value_at(u64)); break;
                        case NUM_TYPE_UNSIGNED: sb_add_format(sb, "%lld", value_at(i64)); break;
                    }
                    break;
                }

                default: UNREACHABLE(); break;
            }
            break;
        }

        case TYPE_VOID:
        case TYPE_BOOL:
        case TYPE_STRING:
        case TYPE_TYPE:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
        case TYPE_FUNCTION:
        case TYPE_STRUCT: UNREACHABLE(); break;

        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_UNDEFINED:
        case TYPE_COUNT: UNREACHABLE(); break;
    }

    #undef value_at
}

static cstr_t token2opcstr(token_t op) {
    switch (op.type) {
        case TOKEN_PLUS: return "+";
        case TOKEN_MINUS: return "-";
        case TOKEN_STAR: return "*";
        case TOKEN_SLASH: return "/";

        case TOKEN_GREATER: return ">";
        case TOKEN_GREATER_EQUAL: return ">=";
        case TOKEN_LESS: return "<";
        case TOKEN_LESS_EQUAL: return "<=";
        case TOKEN_EQUAL_EQUAL: return "==";
        case TOKEN_BANG_EQUAL: return "!=";

        default: UNREACHABLE(); break;
    }

    return "+";
}

static void cgen_declaration(cgen_t *cgen, string_builder_t *sb, ast_node_t *declaration) {
    switch (declaration->node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            cgen_var_t var = cgen_user_var(cgen, declaration->identifier.view, declaration->value_type);
            cgen_expression(cgen, sb, an_decl_expr(declaration), var);
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            cgen_add_indent(sb, cgen->indent);
            cgen_expression(cgen, sb, an_expression(declaration), nil_tmp_var);
            sb_add_cstr(sb, ";\n");
            break;
        }

        default: UNREACHABLE(); break;
    }
}

static void cgen_expression(cgen_t *cgen, string_builder_t *sb, ast_node_t *expression, cgen_var_t var) {
    bool requires_tmp = expression->requires_tmp_for_cgen;

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            if (requires_tmp) {
                cgen_var_t lhs_var = cgen_next_tmpid(cgen, an_lhs(expression)->value_type);
                cgen_expression(cgen, sb, an_lhs(expression), lhs_var);

                cgen_var_t rhs_var = cgen_next_tmpid(cgen, an_rhs(expression)->value_type);
                cgen_expression(cgen, sb, an_rhs(expression), rhs_var);

                if (has_var(var)) {
                    cgen_add_indent(sb, cgen->indent);
                    sb_add_format(sb, "%s = ", cgen_var(cgen, var));
                }

                cstr_t opcstr = token2opcstr(expression->operator);
                sb_add_format(sb, "%s %s %s", cgen_var_name(cgen, lhs_var), opcstr, cgen_var_name(cgen, rhs_var));

                if (has_var(var)) {
                    sb_add_cstr(sb, ";\n");
                }

            } else {
                if (has_var(var)) {
                    cgen_add_indent(sb, cgen->indent);
                    sb_add_format(sb, "%s = ", cgen_var(cgen, var));
                }

                sb_add_cstr(sb, "(");

                cgen_expression(cgen, sb, an_lhs(expression), nil_tmp_var);

                sb_add_cstr(sb, " ");
                sb_add_cstr(sb, token2opcstr(expression->operator));
                sb_add_cstr(sb, " ");

                cgen_expression(cgen, sb, an_rhs(expression), nil_tmp_var);

                sb_add_cstr(sb, ")");

                if (has_var(var)) {
                    sb_add_cstr(sb, ";\n");
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            ASSERT(!requires_tmp, "primaries should never require tmps");

            type_info_t *type_info = get_type_info(&cgen->ast->type_set.types, expression->value_type);

            if (has_var(var)) {
                cgen_add_indent(sb, cgen->indent);
                sb_add_format(sb, "%s = ", cgen_var(cgen, var));
            }

            cgen_primary(cgen, sb, expression->value_index, type_info);

            if (has_var(var)) {
                sb_add_cstr(sb, ";\n");
            }

            switch (type_info->kind) {
                case TYPE_NUMBER: {
                    break;
                }
                
                default: UNREACHABLE(); break;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            if (requires_tmp) {
                ast_node_t *operand = an_expression(expression);
                cgen_add_indent(sb, cgen->indent);
                cgen_expression(cgen, sb, operand, var);
            } else {
                if (has_var(var)) {
                    cgen_add_indent(sb, cgen->indent);
                    sb_add_format(sb, "%s = ",  cgen_var(cgen, var));
                }

                sb_add_cstr(sb, "(");
                ast_node_t *operand = an_operand(expression);
                cgen_expression(cgen, sb, operand, nil_tmp_var);
                sb_add_cstr(sb, ")");

                if (has_var(var)) {
                    sb_add_cstr(sb, ";\n");
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            ASSERT(requires_tmp, "must require a tmp");

            if (has_var(var)) {
                cgen_add_indent(sb, cgen->indent);
            }

            if (no_var(var) || !var.is_new) {
                sb_add_cstr(sb, "{\n");
            } else {
                sb_add_format(sb, "%s; {\n", cgen_var(cgen, var));
            }

            cgen_indent(cgen);

            if (expression->children.count > 0) {
                for (size_t i = 0; i < expression->children.count-1; ++i) {
                    ast_node_t *declaration = expression->children.items[i];
                    cgen_declaration(cgen, sb, declaration);
                    sb_add_char(sb, '\n');
                }
            }

            ast_node_t *last_declaration = NULL;
            if (expression->children.count > 0) {
                last_declaration = expression->children.items[expression->children.count-1];
            }

            unless (last_declaration) {
                UNREACHABLE();
            } else {
                switch (last_declaration->node_type) {
                    case AST_NODE_TYPE_DECLARATION_DEFINITION: {
                        UNREACHABLE();
                        break;
                    }

                    case AST_NODE_TYPE_DECLARATION_STATEMENT: {
                        cgen_expression(cgen, sb, an_expression(last_declaration), cgen_var_not_new(var));
                        break;
                    }

                    default: UNREACHABLE(); break;
                }
            }

            cgen_unindent(cgen);
            cgen_add_indent(sb, cgen->indent);
            sb_add_format(sb, "}\n");
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            ASSERT(requires_tmp, "branches always require a tmp");

            if (expression->looping) {
                // todo
                UNREACHABLE();
            }

            if (has_var(var)) {
                cgen_add_indent(sb, cgen->indent);
            }

            if (no_var(var) || !var.is_new) {
                sb_add_cstr(sb, "{\n");
            } else {
                sb_add_format(sb, "%s; {\n", cgen_var(cgen, var));
            }

            cgen_indent(cgen);

            if (an_condition(expression)->requires_tmp_for_cgen) {
                cgen_var_t condition_id = cgen_next_tmpid(cgen, an_condition(expression)->value_type);
                cgen_expression(cgen, sb, an_condition(expression), condition_id);

                cgen_add_indent(sb, cgen->indent);
                sb_add_format(sb, "%s (%s) {\n", expression->condition_negated ? "unless" : "if", cgen_var_name(cgen, condition_id));
                
            } else {
                cgen_add_indent(sb, cgen->indent);
                sb_add_format(sb, "%s ", expression->condition_negated ? "unless" : "if");
                cgen_expression(cgen, sb, an_condition(expression), nil_tmp_var);
                sb_add_cstr(sb, " {\n");
            }

            cgen_indent(cgen);

            if (an_then(expression)->requires_tmp_for_cgen) {
                cgen_expression(cgen, sb, an_then(expression), cgen_var_not_new(var));
            } else {
                cgen_add_indent(sb, cgen->indent);
                if (has_var(var)) {
                    sb_add_format(sb, "%s = ", cgen_var_name(cgen, var));
                }

                cgen_expression(cgen, sb, an_then(expression), nil_tmp_var);
                sb_add_cstr(sb, ";\n");
            }

            cgen_unindent(cgen);

            if (an_is_notnone(an_else(expression))) {
                cgen_add_indent(sb, cgen->indent);
                sb_add_cstr(sb, "} else {\n");

                cgen_indent(cgen);

                if (an_else(expression)->requires_tmp_for_cgen) {
                    cgen_expression(cgen, sb, an_else(expression), cgen_var_not_new(var));
                } else {
                    cgen_add_indent(sb, cgen->indent);

                    if (has_var(var)) {
                        sb_add_format(sb, "%s = ", cgen_var_name(cgen, var));
                    }

                    cgen_expression(cgen, sb, an_else(expression), nil_tmp_var);
                    sb_add_cstr(sb, ";\n");
                }

                cgen_unindent(cgen);

                cgen_add_indent(sb, cgen->indent);
                sb_add_cstr(sb, "}\n");
            } else {
                sb_add_cstr(sb, "}\n");
            }

            cgen_unindent(cgen);
            cgen_add_indent(sb, cgen->indent);
            sb_add_cstr(sb, "}\n");
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            ASSERT(!requires_tmp, "this should always be placable, like primaries");

            if (has_var(var)) {
                cgen_add_indent(sb, cgen->indent);
                sb_add_format(sb, "%s = ", cgen_var(cgen, var));
            }

            sb_add_format(sb, "%s", cgen_definition_name(cgen, expression->identifier.view).cstr);

            if (has_var(var)) {
                sb_add_cstr(sb, ";\n");
            }
            break;
        }

        default: UNREACHABLE(); break;
    }
}

string_t compile_expr_to_c(ast_t *ast, arena_t *arena) {
    ast_node_t *expr_node = ast->root;
    
    tmp_arena_t *tmp_arena = allocator_borrow();
    string_builder_t sb = {.allocator=tmp_arena->allocator};

    cgen_t cgen = {.ast = ast, .tmp_count = 0};

    cgen_cache_requires_tmp(expr_node);

    cgen_var_t var = cgen_user_var(&cgen, lit2sv("result"), expr_node->value_type);

    cgen_expression(&cgen, &sb, expr_node, var);

    allocator_return(tmp_arena);

    string_t code = sb_render(&sb, arena);

    return code;
}

#undef CODEGENC_IMPLEMENTATION
#endif
