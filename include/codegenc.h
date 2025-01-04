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

    string_builder_t sb;
};

typedef struct cgen_var_t cgen_var_t;
struct cgen_var_t {
    string_t name;
    bool is_new;
    size_t id;
    type_t type;
};

static cgen_var_t nil_cvar = {.is_new = false, .id = 0 , .type = typeid(TYPE_INVALID) };


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

static cgen_var_t cgen_var_used(cgen_var_t var) {
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

static void cgen_add_indent(cgen_t *cgen) {
    for (size_t i = 0; i < cgen->indent; ++i) {
        sb_add_cstr(&cgen->sb, "  ");
    }
}

static void cgen_add_include(cgen_t *cgen, cstr_t include_path) {
    sb_add_format(&cgen->sb, "#include \"%s\"\n", include_path);
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

        case TYPE_BOOL: return "bool_";

        case TYPE_VOID: return "void";

        case TYPE_STRING:
        case TYPE_TYPE:
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
        case TYPE_LABEL:
        case TYPE_STRUCT: UNREACHABLE(); return "void";

        case TYPE_INVALID:
        case TYPE_UNRESOLVED:
        case TYPE_UNDEFINED:
        case TYPE_UNREACHABLE:
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

static string_t cgen_next_label(cgen_t *cgen, cstr_t label_name) {
    ++cgen->tmp_count;
    string_t label = string_format("%s%zu_", &cgen->tmp_arena, label_name, cgen->tmp_count);
    return label;
}

static void *memarr_value_at(memarr_t *memarr, value_index_t value_index) {
    if (value_index.exists && value_index.index < memarr->count) {
        return memarr_get_ptr(memarr, value_index);
    }

    UNREACHABLE();
    return NULL;
}

static void cgen_expression(cgen_t *cgen, ast_node_t *expression, cgen_var_t tmp_var);

static void cgen_cache_requires_tmp(ast_node_t *expression) {
    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case AST_NODE_TYPE_EXPRESSION_JMP:
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

static void cgen_primary(cgen_t *cgen, value_index_t value_index, type_info_t *type_info) {
    #define value_at(ty) (*((ty*)(memarr_value_at(&cgen->ast->constants, value_index))))

    switch (type_info->kind) {
        case TYPE_NUMBER: {
            switch (type_info->size) {
                case 1: UNREACHABLE(); break;
                case 2: UNREACHABLE(); break;
                case 4: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: sb_add_format(&cgen->sb, "%g", value_at(f32)); break;
                        case NUM_TYPE_SIGNED: sb_add_format(&cgen->sb, "%lu", value_at(u32)); break;
                        case NUM_TYPE_UNSIGNED: sb_add_format(&cgen->sb, "%d", value_at(i32)); break;
                    }
                    break;
                }
                case 8: {
                    switch (type_info->data.num) {
                        case NUM_TYPE_FLOAT: sb_add_format(&cgen->sb, "%lg", value_at(f64)); break;
                        case NUM_TYPE_SIGNED: sb_add_format(&cgen->sb, "%llu", value_at(u64)); break;
                        case NUM_TYPE_UNSIGNED: sb_add_format(&cgen->sb, "%lld", value_at(i64)); break;
                    }
                    break;
                }

                default: UNREACHABLE(); break;
            }
            break;
        }

        case TYPE_BOOL: {
            sb_add_format(&cgen->sb, "%s", (value_at(byte) == 1) ? "true" : "false");
            break;
        }

        case TYPE_LABEL:
        case TYPE_VOID:
        case TYPE_STRING:
        case TYPE_TYPE:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
        case TYPE_FUNCTION:
        case TYPE_STRUCT: UNREACHABLE(); break;

        case TYPE_UNREACHABLE:
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

static void cgen_semicolon_nl(cgen_t *cgen) {
    sb_add_cstr(&cgen->sb, ";\n");
}

static void cgen_statement(cgen_t *cgen, ast_node_t *expression, cgen_var_t var, bool add_indent) {
    if (add_indent) cgen_add_indent(cgen);
    cgen_expression(cgen, expression, var);

    cgen_semicolon_nl(cgen);
    // if (TYPE_IS_VOID(expression->value_type)) {
    //     cgen_semicolon_nl(cgen);
    // } else {
    //     cgen_semicolon_nl(cgen);
    // }
}

static void cgen_declaration(cgen_t *cgen, ast_node_t *declaration) {
    switch (declaration->node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            cgen_var_t var = cgen_user_var(cgen, declaration->identifier.view, declaration->value_type);
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

static void cgen_binary(cgen_t *cgen, ast_node_t *binary, cgen_var_t var) {
    unless (binary->requires_tmp_for_cgen) {
        if (has_var(var))
        {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        sb_add_cstr(&cgen->sb, "(");

        cgen_expression(cgen, an_lhs(binary), nil_cvar);

        sb_add_cstr(&cgen->sb, " ");
        sb_add_cstr(&cgen->sb, token2opcstr(binary->operator));
        sb_add_cstr(&cgen->sb, " ");

        cgen_expression(cgen, an_rhs(binary), nil_cvar);

        sb_add_cstr(&cgen->sb, ")");

    } else {
        cgen_var_t lhs_var = cgen_next_tmpid(cgen, an_lhs(binary)->value_type);
        cgen_statement(cgen, an_lhs(binary), lhs_var, false);

        cgen_var_t rhs_var = cgen_next_tmpid(cgen, an_rhs(binary)->value_type);
        cgen_statement(cgen, an_rhs(binary), rhs_var, true);

        cgen_add_indent(cgen);
        if (has_var(var))
        {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        cstr_t opcstr = token2opcstr(binary->operator);
        sb_add_format(&cgen->sb, "%s %s %s", cgen_var_name(cgen, lhs_var), opcstr, cgen_var_name(cgen, rhs_var));
    }
}

static void cgen_assignment(cgen_t *cgen, ast_node_t *assignment, cgen_var_t var) {
    ast_node_t *lhs = an_lhs(assignment);
    switch (lhs->lvalue_node->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            ASSERT(!lhs->requires_tmp_for_cgen, "def values do not require tmp");
            cgen_var_t lhs_var = cgen_user_var(cgen, lhs->lvalue_node->identifier.view, lhs->value_type);
            lhs_var.is_new = false;

            unless (assignment->requires_tmp_for_cgen) {
                if (has_var(var)) {
                    sb_add_format(&cgen->sb, "%s = (", cgen_var(cgen, var));
                }

                sb_add_format(&cgen->sb, "(%s = ", cgen_var_name(cgen, lhs_var));

                cgen_expression(cgen, an_rhs(assignment), nil_cvar);

                sb_add_cstr(&cgen->sb, ")");

                if (has_var(var)) {
                    sb_add_cstr(&cgen->sb, ")");
                }
            } else {
                cgen_var_t rhs_var = cgen_next_tmpid(cgen, an_rhs(assignment)->value_type);
                cgen_statement(cgen, an_rhs(assignment), rhs_var, false);
                
                cgen_add_indent(cgen);
                if (has_var(var)) {
                    sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
                }

                sb_add_format(&cgen->sb, "(%s = %s)", cgen_var_name(cgen, lhs_var), cgen_var_name(cgen, rhs_var));
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: UNREACHABLE(); break;

        default: UNREACHABLE(); break;
    }
}

static void cgen_primary_or_nil(cgen_t *cgen, ast_node_t *primary_or_nil, cgen_var_t var) {
    ASSERT(!primary_or_nil->requires_tmp_for_cgen, "primaries should never require tmps");

    type_info_t *type_info = get_type_info(&cgen->ast->type_set.types, primary_or_nil->value_type);

    if (TYPE_IS_VOID(primary_or_nil->value_type)) {
        // ASSERT(!has_var(var), "shouldnt have a var since its void");
        // if (has_var(var)) sb_add_cstr(&cgen->sb, "");
        // sb_add_cstr(&cgen->sb, "NOP()\n");
    } else {
        if (has_var(var)) {
            sb_add_format(&cgen->sb, "%s = ", cgen_var(cgen, var));
        }

        cgen_primary(cgen, primary_or_nil->value_index, type_info);
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

static void cgen_condition_and_open_block(cgen_t *cgen, ast_node_t *branch, cstr_t branch_cstr) {
    if (an_condition(branch)->requires_tmp_for_cgen) {
        cgen_var_t conditionid = cgen_next_tmpid(cgen, an_condition(branch)->value_type);
        cgen_statement(cgen, an_condition(branch), conditionid, true);

        cgen_add_indent(cgen);
        sb_add_format(&cgen->sb, "%s (%s) {\n", branch_cstr, cgen_var_name(cgen, conditionid));
        
    } else {
        cgen_add_indent(cgen);
        sb_add_format(&cgen->sb, "%s (", branch_cstr);
        cgen_expression(cgen, an_condition(branch), nil_cvar);
        sb_add_cstr(&cgen->sb, ") {\n");
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
    bool skip_else = an_else(branch)->node_type == AST_NODE_TYPE_EXPRESSION_NIL && branch->branch_type != BRANCH_TYPE_LOOPING;
    cgen_begin_branch(cgen, branch, var, skip_else);

    cstr_t if_or_unless = branch->condition_negated ? "unless" : "if";
    cgen_indent(cgen);
    cgen_condition_and_open_block(cgen, branch, if_or_unless);
    cgen_indent(cgen);

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

static void cgen_jmp_label(cgen_t *cgen, string_t label) {
    sb_add_format(&cgen->sb, "%s:", label.cstr);
}

static void cgen_while(cgen_t *cgen, ast_node_t *branch, cgen_var_t var)  {
    ASSERT(branch->requires_tmp_for_cgen, "branches always require a tmp");
    cgen_begin_branch(cgen, branch, var, false);

    cstr_t while_or_until = branch->condition_negated ? "until" : "while";
    cgen_indent(cgen);
    cgen_condition_and_open_block(cgen, branch, while_or_until);
    cgen_indent(cgen);

    cgen_then(cgen, branch, var);

    cgen_unindent(cgen);

    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "}\n");

    cgen_statement(cgen, an_else(branch), cgen_var_used(var), true);

    cgen_end_branch(cgen);

    sb_add_cstr(&cgen->sb, " ");
    cgen_jmp_label(cgen, branch->code_jmp_label);

}

static void cgen_do(cgen_t *cgen, ast_node_t *branch, cgen_var_t var) {
    ASSERT(branch->requires_tmp_for_cgen, "branches always require a tmp");
    cgen_begin_branch(cgen, branch, var, false);

    cgen_indent(cgen);

    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "do {\n");

    cgen_indent(cgen);

    cgen_then(cgen, branch, var);

    cgen_unindent(cgen);

    cgen_add_indent(cgen);
    sb_add_cstr(&cgen->sb, "} while(false); \n");

    cgen_statement(cgen, an_else(branch), cgen_var_used(var), true);

    cgen_end_branch(cgen);

    sb_add_cstr(&cgen->sb, " ");
    cgen_jmp_label(cgen, branch->code_jmp_label);
}

static void cgen_branching(cgen_t *cgen, ast_node_t *branch, cgen_var_t var) {
    switch (branch->branch_type) {
        case BRANCH_TYPE_LOOPING:
        case BRANCH_TYPE_DO: {
            if (has_var(var)) {
                branch->code_cvar_name = cstr2string(cgen_var(cgen, cgen_var_used(var)), &cgen->ast->allocator);
            }
            branch->code_jmp_label = string_copy(cgen_next_label(cgen, "blockend"), &cgen->ast->allocator);
            if (branch->branch_type == BRANCH_TYPE_LOOPING) {
                cgen_while(cgen, branch, var);
            } else {
                cgen_do(cgen, branch, var);
            }
            break;
        }

        case BRANCH_TYPE_IFTHEN: {
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

    sb_add_format(&cgen->sb, "%s", cgen_definition_name(cgen, def_value->identifier.view).cstr);

    if (has_var(var)) {
        sb_add_cstr(&cgen->sb, ";\n");
    }
}

static void cgen_goto(cgen_t *cgen, ast_node_t *jmp) {
    string_t jmp_label = jmp->jmp_out_scope_node->code_jmp_label;
    sb_add_format(&cgen->sb, "goto %s", jmp_label.cstr);
}

static void cgen_break_jmp(cgen_t *cgen, ast_node_t *jmp, cgen_var_t var) {
    ASSERT(jmp->requires_tmp_for_cgen, "requires a tmp because its leaving the scope");

    cgen_var_t jmp_var = nil_cvar;
    if (jmp->jmp_out_scope_node->code_cvar_name.length > 0) {
        jmp_var.name = jmp->jmp_out_scope_node->code_cvar_name;
        jmp_var.is_new = false;
    }

    cgen_expression(cgen, an_expression(jmp), jmp_var);
    cgen_semicolon_nl(cgen);
    
    cgen_add_indent(cgen);
    cgen_goto(cgen, jmp);
}

static void cgen_continue_jmp(cgen_t *cgen, ast_node_t *jmp) {
    switch (jmp->jmp_out_scope_node->branch_type) {
        case BRANCH_TYPE_LOOPING: {
            sb_add_cstr(&cgen->sb, "continue");
            break;
        }
        case BRANCH_TYPE_DO: {
            sb_add_cstr(&cgen->sb, "break");
            break;
        }
        case BRANCH_TYPE_IFTHEN: UNREACHABLE();
    }
}


static void cgen_expression(cgen_t *cgen, ast_node_t *expression, cgen_var_t var) {
    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            cgen_binary(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            cgen_assignment(cgen, expression, var);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_NIL:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            cgen_primary_or_nil(cgen, expression, var);
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
                case TOKEN_BREAK: {
                    cgen_break_jmp(cgen, expression, var);
                    break;
                }

                case TOKEN_CONTINUE: {
                    cgen_continue_jmp(cgen, expression);
                    break;
                }

                
                case TOKEN_RETURN: UNREACHABLE();

                default: UNREACHABLE();
            }
            break;
        }

        default: UNREACHABLE(); break;
    }
}

string_t compile_expr_to_c(ast_t *ast, arena_t *arena) {
    ast_node_t *expr_node = ast->root;
    cgen_cache_requires_tmp(expr_node);
    
    tmp_arena_t *tmp_arena = allocator_borrow();
    cgen_t cgen = {.ast = ast, .tmp_count = 0 };
    cgen.sb.allocator = &cgen.tmp_arena;

    cgen_add_include(&cgen, "intrinsics.h");

    sb_add_format(&cgen.sb, "%s expr(void) {\n", cgen_type_name(&cgen, expr_node->value_type));
    cgen_indent(&cgen);

    cgen_var_t var = cgen_user_var(&cgen, lit2sv("result"), expr_node->value_type);
    cgen_expression(&cgen, expr_node, var);
    cgen_semicolon_nl(&cgen);

    sb_add_cstr(&cgen.sb, "\n");
    cgen_add_indent(&cgen);
    sb_add_format(&cgen.sb, "return %s;\n", cgen_var_name(&cgen, var));

    cgen_unindent(&cgen);

    sb_add_cstr(&cgen.sb, "}\n");

    allocator_return(tmp_arena);

    string_t code = sb_render(&cgen.sb, arena);

    return code;
}

#undef CODEGENC_IMPLEMENTATION
#endif
