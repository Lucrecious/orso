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

};

static size_t cgen_next_tmpid(cgen_t *cgen) {
    return ++cgen->tmp_count;
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

static void *memarr_value_at(memarr_t *memarr, value_index_t value_index) {
    if (value_index.exists && value_index.index < memarr->count) {
        return memarr_get_ptr(memarr, value_index);
    }

    UNREACHABLE();
    return NULL;
}

static void cgen_expression(cgen_t *cgen, string_builder_t *sb, ast_node_t *expression, size_t tmpid);

static bool expression_requires_tmp(ast_node_t *expression) {
    // UNUSED(expression);
    // return true;

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BLOCK: return true;
        default: break;
    }

    for (size_t i = 0; i < expression->children.count; ++i) {
        ast_node_t *child = expression->children.items[i];
        if (expression_requires_tmp(child)) return true;
    }

    return false;
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
        case TYPE_SYMBOL:
        case TYPE_TYPE:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
        case TYPE_FUNCTION:
        case TYPE_STRUCT: UNREACHABLE(); break;

        case TYPE_UNION:
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

        default: UNREACHABLE(); break;
    }

    return "+";
}

static void cgen_declaration(cgen_t *cgen, string_builder_t *sb, ast_node_t *declaration) {
    switch (declaration->node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            UNREACHABLE();
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            cgen_expression(cgen, sb, an_expression(declaration), 0);
            break;
        }

        default: UNREACHABLE(); break;
    }

    sb_add_cstr(sb, ";\n");
}

static void cgen_expression(cgen_t *cgen, string_builder_t *sb, ast_node_t *expression, size_t tmpid) {

    bool requires_tmp = expression_requires_tmp(expression);

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            if (requires_tmp) {
                size_t lhs_tmpid = cgen_next_tmpid(cgen);
                cgen_expression(cgen, sb, an_lhs(expression), lhs_tmpid);

                size_t rhs_tmpid = cgen_next_tmpid(cgen);
                cgen_expression(cgen, sb, an_rhs(expression), rhs_tmpid);

                if (tmpid > 0) {
                    cgen_add_indent(sb, cgen->indent);
                    sb_add_format(sb, "tmp%llu = ", tmpid);
                }

                cstr_t opcstr = token2opcstr(expression->operator);
                sb_add_format(sb, "tmp%llu %s tmp%llu", lhs_tmpid, opcstr, rhs_tmpid);

                if (tmpid > 0) {
                    sb_add_cstr(sb, ";\n");
                }

            } else {
                if (tmpid > 0) {
                    cgen_add_indent(sb, cgen->indent);
                    sb_add_format(sb, "tmp%llu = ", tmpid);
                }

                cgen_expression(cgen, sb, an_lhs(expression), 0);

                sb_add_cstr(sb, token2opcstr(expression->operator));

                cgen_expression(cgen, sb, an_rhs(expression), 0);

                if (tmpid > 0) {
                    sb_add_cstr(sb, ";\n");
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            // ASSERT(!requires_tmp, "primaries should never require tmps");

            type_info_t *type_info = get_type_info(&cgen->ast->type_set.types, expression->value_type);

            if (tmpid > 0) {
                cgen_add_indent(sb, cgen->indent);
                sb_add_format(sb, "tmp%llu = ", tmpid);
            }

            cgen_primary(cgen, sb, expression->value_index, type_info);

            if (tmpid > 0) {
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
                ast_node_t *operand = an_operand(expression);
                cgen_add_indent(sb, cgen->indent);
                cgen_expression(cgen, sb, operand, tmpid);
            } else {
                if (tmpid > 0) {
                    cgen_add_indent(sb, cgen->indent);
                    sb_add_format(sb, "tmp%llu = ", tmpid);
                }

                sb_add_cstr(sb, "(");
                ast_node_t *operand = an_operand(expression);
                cgen_expression(cgen, sb, operand, 0);
                sb_add_cstr(sb, ")");

                if (tmpid > 0) {
                    sb_add_cstr(sb, ";\n");
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            ASSERT(requires_tmp, "must require a tmp");
            size_t block_tmpid = cgen_next_tmpid(cgen);

            cgen_add_indent(sb, cgen->indent);
            sb_add_format(sb, "tmp%llu; {\n", block_tmpid);
            cgen_indent(cgen);

            if (expression->children.count > 0) {
                for (size_t i = 0; i < expression->children.count-1; ++i) {
                    cgen_add_indent(sb, cgen->indent);
                    ast_node_t *declaration = expression->children.items[i];
                    cgen_declaration(cgen, sb, declaration);
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
                        cgen_expression(cgen, sb, an_expression(last_declaration), block_tmpid);
                        break;
                    }

                    default: UNREACHABLE(); break;
                }
            }

            cgen_unindent(cgen);
            cgen_add_indent(sb, cgen->indent);
            sb_add_format(sb, "}\n");

            if (tmpid > 0) {
                cgen_add_indent(sb, cgen->indent);
                sb_add_format(sb, "tmp%llu = tmp%llu;\n", tmpid, block_tmpid);
            } else {
                sb_add_format(sb, "tmp%llu", block_tmpid);
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

    cgen_t cgen = {.ast = ast};

    cgen_expression(&cgen, &sb, expr_node, 0);
    sb_add_cstr(&sb, ";");

    allocator_return(tmp_arena);

    string_t code = sb_render(&sb, arena);

    return code;
}

#undef CODEGENC_IMPLEMENTATION
#endif
