#ifndef CODEGENC_H_
#define CODEGENC_H_

#include "stringt.h"
#include "parser.h"
#include "tmp.h"

string_t compile_expr_to_c(ast_t *ast, arena_t *allocator);

#endif

#ifdef CODEGENC_IMPLEMENTATION
#include "memarr.h"

static void *memarr_value_at(memarr_t *memarr, value_index_t value_index) {
    if (value_index.exists && value_index.index < memarr->count) {
        return memarr_get_ptr(memarr, value_index);
    }

    UNREACHABLE();
    return NULL;
}

static void cgen_primary(ast_t *ast, string_builder_t *sb, value_index_t value_index, type_info_t *type_info) {
    #define value_at(ty) (*((ty*)(memarr_value_at(&ast->constants, value_index))))

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

static void cgen_expression(ast_t *ast, string_builder_t *sb, ast_node_t *expression) {
    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            cgen_expression(ast, sb, an_lhs(expression));
            switch (expression->operator.type) {
                case TOKEN_PLUS: sb_add_cstr(sb, " + "); break;
                case TOKEN_MINUS: sb_add_cstr(sb, " - "); break;
                case TOKEN_STAR: sb_add_cstr(sb, " * "); break;
                case TOKEN_SLASH: sb_add_cstr(sb, " / "); break;

                default: UNREACHABLE(); break;
            }

            cgen_expression(ast, sb, an_rhs(expression));
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            type_info_t *type_info = get_type_info(&ast->type_set.types, expression->value_type);
            cgen_primary(ast, sb, expression->value_index, type_info);

            switch (type_info->kind) {
                case TYPE_NUMBER: {
                    break;
                }
                
                default: UNREACHABLE(); break;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            sb_add_cstr(sb, "(");
            ast_node_t *operand = an_operand(expression);
            cgen_expression(ast, sb, operand);
            sb_add_cstr(sb, ")");
            break;
        }

        default: UNREACHABLE(); break;
    }
}

string_t compile_expr_to_c(ast_t *ast, arena_t *arena) {
    ast_node_t *expr_node = ast->root;
    
    tmp_arena_t *tmp_arena = allocator_borrow();
    string_builder_t sb = {.allocator=tmp_arena->allocator};

    cgen_expression(ast, &sb, expr_node);
    sb_add_cstr(&sb, ";");

    allocator_return(tmp_arena);

    string_t code = sb_render(&sb, arena);

    return code;
}

#undef CODEGENC_IMPLEMENTATION
#endif
