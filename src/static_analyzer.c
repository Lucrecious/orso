#include "static_analyzer.h"

#include <stdio.h>

#include "type_set.h"
#include "error.h"
#include "vm.h"
#include "codegen.h"
#include "tmp.h"

#include "intrinsics.h"

#include <time.h>

#define EXPRESSION_RESOLVED(expression) (!TYPE_IS_UNRESOLVED((expression)->value_type))

#define INVALIDATE(NODE) do {\
    ast_node_t* node = NODE;\
    node->value_type = typeid(TYPE_INVALID);\
    node->expr_val = ast_node_val_nil();\
} while(false)

typedef bool (*IsCircularDependencyFunc)(analyzer_t*, ast_node_t*);

typedef struct analysis_state_t {
    scope_t *scope;
} analysis_state_t;

static void scope_init(scope_t *scope, arena_t *allocator, scope_type_t type, scope_t *outer, ast_node_t *creator_expression) {
    scope->outer = outer;
    scope->creator = creator_expression;
    scope->type = type;
    scope->definitions = table_new(s2w, allocator);
}

static void add_definition(scope_t *scope, arena_t *allocator, string_view_t identifier, ast_node_t *decl) {
    table_put(s2w, scope->definitions, sv2string(identifier, allocator), WORDP(decl));
}

static ast_node_t *add_builtin_definition(ast_t *ast, string_view_t identifier, type_t type, word_t word) {
    ast_node_t *decl = ast_node_new(ast, AST_NODE_TYPE_DECLARATION_DEFINITION, nil_token);
    decl->value_type = type;
    decl->expr_val = ast_node_val_word(word);
    decl->is_mutable = false;

    table_put(s2w, ast->builtins, sv2string(identifier, &ast->allocator), WORDP(decl));

    return decl;
}

#define type2td(ast, type) type2typedata(&((ast)->type_set.types), (type))

static void stan_error(analyzer_t *analyzer, error_t error) {
    analyzer->had_error = true;
    if (analyzer->error_fn) analyzer->error_fn(analyzer->ast, error);
}

static ast_node_t *get_def_by_identifier_or_error(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *def,
        scope_t **found_scope);

static bool is_builtin_type(type_table_t *t, string_view_t identifier, type_t *type) {
#define RETURN_IF_TYPE(SYMBOL, TYPE_STRING, TYPE) \
if (sv_eq(identifier, lit2sv(#TYPE_STRING))) {\
    *type = (TYPE); \
    return true; \
}

    RETURN_IF_TYPE(identifier, void, t->void_)

    RETURN_IF_TYPE(identifier, f32, t->f32_)
    RETURN_IF_TYPE(identifier, f64, t->f64_)

    RETURN_IF_TYPE(identifier, u8, t->u8_)
    RETURN_IF_TYPE(identifier, s8, t->s8_)

    RETURN_IF_TYPE(identifier, u16, t->u16_)
    RETURN_IF_TYPE(identifier, s16, t->s16_)

    RETURN_IF_TYPE(identifier, s32, t->s32_)
    RETURN_IF_TYPE(identifier, u32, t->u32_)

    RETURN_IF_TYPE(identifier, u64, t->u64_)
    RETURN_IF_TYPE(identifier, s64, t->s64_)

    RETURN_IF_TYPE(identifier, int, t->int_)
    RETURN_IF_TYPE(identifier, uint, t->uint_)
    RETURN_IF_TYPE(identifier, size_t, t->size_t_)

#undef RETURN_IF_TYPE

#define RETURN_IF_TYPE(SYMBOL, TYPE_STRING, TYPE) \
if (sv_eq(identifier, lit2sv(#TYPE_STRING))){\
    *type = typeid(TYPE); \
    return true; \
}

    RETURN_IF_TYPE(identifier, void, TYPE_INVALID)
    RETURN_IF_TYPE(identifier, bool, TYPE_BOOL)
    RETURN_IF_TYPE(identifier, type, TYPE_TYPE)
#undef RETURN_IF_TYPE

    return false;

#undef RETURN_IF_TYPE
}

static bool check_call_on_func(analyzer_t *analyzer, ast_t *ast, ast_node_t *call) {
    ast_node_t *callee = an_callee(call);
    typedata_t *func_td = type2td(ast, callee->value_type);

    ASSERT(func_td->kind == TYPE_FUNCTION, "must be used only on func");

    size_t arg_start = an_call_arg_start(call);
    size_t arg_end = an_call_arg_end(call);

    bool errored = false;
    if (func_td->data.function.argument_types.count != (arg_end - arg_start)) {
        errored = true;
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_NUMBER_ARGS_CALL_FUNC_MISTMATCH, call));
    }

    for (size_t i = 0; i < func_td->data.function.argument_types.count; ++i) {
        type_t parameter_type = func_td->data.function.argument_types.items[i];

        ast_node_t *arg = call->children.items[arg_start + i];
        type_t argument_type = arg->value_type;
        unless (typeid_eq(parameter_type, argument_type)) {
            errored = true;
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_ARG_VS_PARAM_FUNC_CALL_MISMATCH, arg));
        }
    }

    return !errored;
}

static bool is_declaration_resolved(ast_node_t *definition) {
    if (TYPE_IS_INVALID(definition->value_type)) {
        return true;
    }

    return (definition->expr_val.is_concrete || (an_decl_expr(definition)->node_type != AST_NODE_TYPE_NONE && TYPE_IS_RESOLVED(an_decl_expr(definition)->value_type))) && TYPE_IS_RESOLVED(definition->value_type);
}

static bool fold_funcsig_or_error(analyzer_t *analyzer, ast_t *ast, ast_node_t *expression) {
    ASSERT(expression->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");

    unless (an_func_def_return(expression)->expr_val.is_concrete) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, an_func_def_return(expression)));
        return false;
    }

    bool hit_error = false;
    types_t parameter_types = {.allocator=&ast->allocator};

    for (size_t i = an_func_def_arg_start(expression); i < an_func_def_arg_end(expression); ++i) {
        ast_node_t *parameter = expression->children.items[i];
        unless (parameter->expr_val.is_concrete) {
            hit_error = true;
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, parameter));
            break;
        }

        unless (TYPE_IS_TYPE(parameter->value_type)) {
            hit_error = true;
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_TYPE, parameter));
            break;
        }

        type_t type = typeid(parameter->expr_val.word.as.u);
        array_push(&parameter_types, type);
    }

    if (hit_error) {
        return false;
    }

    ast_node_t *return_type_expression = an_func_def_return(expression);
    if (return_type_expression && !TYPE_IS_TYPE(return_type_expression->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_RETURN_TYPE, return_type_expression));
        return false;
    }

    type_t return_type = typeid(return_type_expression->expr_val.word.as.u);

    type_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);

    word_t funcsig_word = WORDU(function_type.i);

    expression->foldable = true;
    expression->expr_val = ast_node_val_word(funcsig_word);

    return true;
}

static bool get_nearest_scope_in_func_or_error(
        analyzer_t *analyzer, ast_node_t *jmp_node, scope_t *scope,
        scope_type_t search_type, string_view_t label, scope_t **found_scope) {
    
    bool check_label = label.length > 0;
    while (scope->outer) {
        if (scope->type == search_type) {
            if (check_label) {
                if (sv_eq(label, scope->creator->identifier.view)) {
                    *found_scope = scope;
                    return true;
                }
            } else {
                *found_scope = scope;
                return true;
            }
        }

        if (search_type != SCOPE_TYPE_FUNCDEF && scope->type == SCOPE_TYPE_FUNC_DEF_BODY) {
            break;
        }

        if (scope->type == SCOPE_TYPE_CONDITION) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_JMP_IN_CONDITION, jmp_node));
            return false;
        }

        scope = scope->outer;
    }

    if (search_type == SCOPE_TYPE_FUNCDEF) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_RETURN_OUTSIDE_FUNC_DEF, jmp_node));
    } else {
        if (label.length != 0) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_FIND_JMP_LABEL, jmp_node));
        } else {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_NO_VALID_JMP_BLOCK, jmp_node));
        }
    }
    return false;
}
type_t resolve_block_return_types(ast_node_t *block) {
    switch (block->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            ast_node_t *then = an_then(block);
            ast_node_t *else_ = an_else(block);

            tmp_arena_t *tmp = allocator_borrow();
            types_t types = {.allocator=tmp->allocator};

            unless (TYPE_IS_UNREACHABLE(then->value_type)) {
                array_push(&types, then->value_type);
            }

            unless (TYPE_IS_UNREACHABLE(else_->value_type)) {
                array_push(&types, else_->value_type);
            }

            for (size_t i = 0; i < block->jmp_nodes.count; ++i) {
                ast_node_t *jmp_node_expr = an_expression(block->jmp_nodes.items[i]);
                unless (TYPE_IS_UNREACHABLE(jmp_node_expr->value_type)) {
                    array_push(&types, jmp_node_expr->value_type);
                }
            }

            types_t sans_void = {.allocator=tmp->allocator};
            for (size_t i = 0; i < types.count; ++i) {
                type_t t = types.items[i];
                unless (TYPE_IS_VOID(t)) array_push(&sans_void, t);
            }

            type_t type = (sans_void.count != types.count) ? typeid(TYPE_VOID) : typeid(TYPE_UNREACHABLE);
            if (sans_void.count > 0) {
                type = sans_void.items[0];
            }

            for (size_t i = 1; i < sans_void.count; ++i) {
                unless (typeid_eq(sans_void.items[i], type)) {
                    type = typeid(TYPE_INVALID);
                    break;
                }
            }

            allocator_return(tmp);

            return type;
        }

        default: UNREACHABLE();
    }

    return typeid(TYPE_INVALID);
}

static void resolve_declarations(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_nodes_t declarations,
        s32 count);

static void resolve_declaration(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *declaration_node);

static void resolve_declaration_statement(
    analyzer_t *analyzer,
    ast_t *ast,
    analysis_state_t state,
    ast_node_t *statement);

static void resolve_funcdef(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *function_definition_expression);

static void resolve_struct_definition(
    analyzer_t *analyzer,
    ast_t *ast,
    analysis_state_t state,
    ast_node_t *struct_definition);

static void declare_definition(analyzer_t *analyzer, scope_t *scope, ast_node_t *definition);

static void forward_scan_declaration_names(analyzer_t *analyzer, scope_t *scope, ast_nodes_t declarations, s32 count) {
    for (s32 i = 0; i < count; i++) {
        ast_node_t* declaration = declarations.items[i];
        if (declaration->node_type != AST_NODE_TYPE_DECLARATION_DEFINITION) {
            continue;
        }

        declare_definition(analyzer, scope, declaration);
    }
}

word_t constant_fold_bin_arithmetic(ast_t *ast, token_type_t operator, type_t type, word_t l, word_t r) {
    typedata_t *numtype = type2td(ast, type);

    #define case_block(type, l, r) do { switch (operator) { \
        case TOKEN_PLUS: result = add##type##_(l, r); break; \
        case TOKEN_MINUS: result = sub##type##_(l, r); break; \
        case TOKEN_SLASH: result = div##type##_(l, r); break; \
        case TOKEN_STAR: result = mul##type##_(l, r); break; \
        case TOKEN_PERCENT: result = mod##type##_(l, r); break; \
        case TOKEN_PERCENT_PERCENT: result = rem##type##_(l, r); break; \
        default: UNREACHABLE(); \
        }} while (false)

    switch (numtype->kind) {
    case TYPE_NUMBER: {
        switch (numtype->data.num) {
        case NUM_TYPE_SIGNED: {
            s64 lhsi = l.as.s;
            s64 rhsi = r.as.s;
            s64 result = 0;

            switch((num_size_t)numtype->size) {
            case NUM_SIZE_8: case_block(s8, lhsi, rhsi); break;
            case NUM_SIZE_16: case_block(s16, lhsi, rhsi); break;
            case NUM_SIZE_32: case_block(s32, lhsi, rhsi); break;
            case NUM_SIZE_64: case_block(s64, lhsi, rhsi); break;
            }

            return WORDI(result);
        }
        
        case NUM_TYPE_UNSIGNED: {
            u64 lhsu = l.as.u;
            u64 rhsu = l.as.u;
            u64 result = 0;

            switch((num_size_t)numtype->size) {
            case NUM_SIZE_8: case_block(u8, lhsu, rhsu); break;
            case NUM_SIZE_16: case_block(u16, lhsu, rhsu); break;
            case NUM_SIZE_32: case_block(u32, lhsu, rhsu); break;
            case NUM_SIZE_64: case_block(u64, lhsu, rhsu); break;
            }

            return WORDU(result);
        }

        case NUM_TYPE_FLOAT: {
            f64 lhsf = l.as.d;
            f64 rhsf = r.as.d;
            f64 result = 0;
            switch((num_size_t)numtype->size) {
            case NUM_SIZE_32: case_block(f, lhsf, rhsf); break;
            case NUM_SIZE_64: case_block(d, lhsf, rhsf); break;
            default: UNREACHABLE(); break;
            }

            return WORDD(result);
        }
        }
        break;
    }

    case TYPE_STRUCT: UNREACHABLE(); /*todo*/ break;

    default: UNREACHABLE(); break;
    }

    #undef case_block
}

word_t constant_fold_bin_comparison(ast_t *ast, token_type_t operator, type_t type, word_t a, word_t b) {
    typedata_t *typedata = type2td(ast, type);

    switch (typedata->kind) {
    case TYPE_NUMBER: {
        switch (typedata->data.num) {
        case NUM_TYPE_SIGNED: {
            s64 lhsi = a.as.s;
            s64 rhsi = b.as.s;
            bool result = 0;

            switch (operator) {
            case TOKEN_GREATER: result = (lhsi > rhsi); break;
            case TOKEN_GREATER_EQUAL: result = (lhsi >= rhsi); break;
            case TOKEN_LESS: result = (lhsi < rhsi); break;
            case TOKEN_LESS_EQUAL: result = (lhsi <= rhsi); break;
            default: UNREACHABLE();
            }

            return WORDU((u64)result);
        }
        
        case NUM_TYPE_UNSIGNED: {
            u64 lhsu = a.as.u;
            u64 rhsu = b.as.u;
            bool result = 0;

            switch (operator) {
            case TOKEN_EQUAL_EQUAL: result = (lhsu == rhsu); break;
            case TOKEN_BANG_EQUAL: result = (lhsu != rhsu); break;
            case TOKEN_GREATER: result = (lhsu > rhsu); break;
            case TOKEN_GREATER_EQUAL: result = (lhsu >= rhsu); break;
            case TOKEN_LESS: result = (lhsu < rhsu); break;
            case TOKEN_LESS_EQUAL: result = (lhsu <= rhsu); break;
            default: UNREACHABLE();
            }

            return WORDU((u64)result);
        }

        case NUM_TYPE_FLOAT: {
            f64 lhsf = a.as.d;
            f64 rhsf = b.as.d;
            bool result = 0;

            switch (operator) {
            case TOKEN_EQUAL_EQUAL: result = (lhsf == rhsf); break;
            case TOKEN_BANG_EQUAL: result = (lhsf != rhsf); break;
            case TOKEN_GREATER: result = (lhsf > rhsf); break;
            case TOKEN_GREATER_EQUAL: result = (lhsf >= rhsf); break;
            case TOKEN_LESS: result = (lhsf < rhsf); break;
            case TOKEN_LESS_EQUAL: result = (lhsf <= rhsf); break;
            default: UNREACHABLE();
            }

            return WORDU((u64)result);
        }
        }
    }

    case TYPE_STRUCT: UNREACHABLE(); /*todo*/ return (word_t){0};

    default: UNREACHABLE(); return (word_t){0};
    }
}

static word_t constant_fold_cast(ast_t *ast, word_t in, type_t dst, type_t src) {
    typedata_t *desttd = type2td(ast, dst);
    typedata_t *sourcetd = type2td(ast, src);
    ASSERT(desttd->kind == TYPE_NUMBER && desttd->kind == sourcetd->kind, "must both be number types for now");

    #define CASTING(src_type, dst_type) (sourcetd->data.num == NUM_TYPE_##src_type && desttd->data.num == NUM_TYPE_##dst_type)
    #define DSIZE(size_) (desttd->size == (size_/8))
    #define VALU (in.as.u)
    #define VALI (in.as.s)
    #define VALF (in.as.d)

    word_t result = {0};
    if (CASTING(SIGNED, SIGNED)) {
        if (DSIZE(8)) result.as.s = cast(s8, VALI);
        else if (DSIZE(16)) result.as.s = cast(s16, VALI);
        else if (DSIZE(32)) result.as.s = cast(s32, VALI);
        else result.as.s = VALI;
    } else if (CASTING(SIGNED, UNSIGNED)) {
        if (DSIZE(8)) result.as.u = cast(u8, cast(u64, VALI));
        else if (DSIZE(16)) result.as.u = cast(u16, cast(u64, VALI));
        else if (DSIZE(32)) result.as.u = cast(u32, cast(u64, VALI));
        else if (DSIZE(64)) result.as.u = cast(u64, VALI);
        else UNREACHABLE();
    } else if (CASTING(SIGNED, FLOAT)) {
        if (DSIZE(32)) result.as.d = cast(f32, VALI);
        else if (DSIZE(64)) result.as.d = cast(f64, VALI);
        else UNREACHABLE();
    } else if (CASTING(UNSIGNED, UNSIGNED)) {
        if (DSIZE(8)) result.as.u = cast(u8, VALU);
        else if (DSIZE(16)) result.as.u = cast(u16, VALU);
        else if (DSIZE(32)) result.as.u = cast(u32, VALU);
        else result.as.u = VALU;
    } else if (CASTING(UNSIGNED, SIGNED)) {
        if (DSIZE(8)) result.as.s = cast(s8, cast(s64, VALU));
        else if (DSIZE(16)) result.as.s = cast(s16, cast(s64, VALU));
        else if (DSIZE(32)) result.as.s = cast(s32, cast(s64, VALU));
        else if (DSIZE(64)) result.as.s = cast(s64, VALU);
        else UNREACHABLE();
    } else if (CASTING(UNSIGNED, FLOAT)) {
        if (DSIZE(32)) result.as.d = cast(f32, VALU);
        else if (DSIZE(64)) result.as.d = cast(f64, VALU);
        else UNREACHABLE();
    } else if (CASTING(FLOAT, FLOAT)) {
        if (DSIZE(32)) result.as.d = cast(f32, VALF);
        else result.as.d = VALF;
    } else if (CASTING(FLOAT, SIGNED)) {
        if (DSIZE(8)) result.as.u = cast(u8, cast(u64, VALF));
        else if (DSIZE(16)) result.as.u = cast(u16, cast(u64, VALF));
        else if (DSIZE(32)) result.as.u = cast(u32, cast(u64, VALF));
        else if (DSIZE(64)) result.as.u = cast(u64, VALF);
        else UNREACHABLE();
    } else if (CASTING(FLOAT, SIGNED)) {
        if (DSIZE(8)) result.as.s = cast(s8, cast(s64, VALF));
        else if (DSIZE(16)) result.as.s = cast(s16, cast(s64, VALF));
        else if (DSIZE(32)) result.as.s = cast(s32, cast(s64, VALF));
        else if (DSIZE(64)) result.as.s = cast(s64, VALF);
        else UNREACHABLE();
    } else UNREACHABLE();

    #undef VALF
    #undef VALI
    #undef VALU
    #undef DSIZE
    #undef CASTING

    return result;
}

static ast_node_t *cast_implicitly_if_necessary(ast_t *ast, type_t destination_type, ast_node_t *expr) {
    ASSERT(TYPE_IS_RESOLVED(expr->value_type), "expression must be resolved already");

    if (typeid_eq(destination_type, expr->value_type)) return expr;

    typedata_t *destinationtd = type2td(ast, destination_type);
    typedata_t *exprtd = type2td(ast, expr->value_type);
    
    // for now only numbers can be cast/promoted implicitly
    if (destinationtd->kind != TYPE_NUMBER) return expr;
    if (exprtd->kind != TYPE_NUMBER) return expr;

    num_type_t dst_num = destinationtd->data.num;
    num_type_t expr_num = exprtd->data.num;

    // if we now what the value of the number we just implicitly cast it...
    if (expr->expr_val.is_concrete && expr->is_free_number) {
        word_t w = expr->expr_val.word;
        switch (dst_num) {
        case NUM_TYPE_SIGNED:
            switch ((num_size_t)(destinationtd->size)) {
            case NUM_SIZE_8:
                switch(expr_num) {
                case NUM_TYPE_SIGNED: if (w.as.s < INT8_MIN || w.as.s > INT8_MAX) return expr; break;
                case NUM_TYPE_UNSIGNED: if (w.as.u > INT8_MAX) return expr; break;
                case NUM_TYPE_FLOAT: if (w.as.d < INT8_MIN || w.as.d > INT8_MAX || w.as.d != ((s8)w.as.d)) return expr; break;
                }
                break;
            
            case NUM_SIZE_16:
                switch(expr_num) {
                case NUM_TYPE_SIGNED: if (w.as.s < INT16_MIN || w.as.s > INT16_MAX) return expr; break;
                case NUM_TYPE_UNSIGNED: if (w.as.u > INT16_MAX) return expr; break;
                case NUM_TYPE_FLOAT: if (w.as.d < INT16_MIN || w.as.d > INT16_MAX || w.as.d != ((s16)w.as.d)) return expr; break;
                }
                break;
            
            case NUM_SIZE_32:
                switch(expr_num) {
                case NUM_TYPE_SIGNED: if (w.as.s < INT32_MIN || w.as.s > INT32_MAX) return expr; break;
                case NUM_TYPE_UNSIGNED: if (w.as.u > INT32_MAX)  return expr; break;
                case NUM_TYPE_FLOAT: if (w.as.d < INT32_MIN || w.as.d > INT32_MAX || w.as.d != ((s32)w.as.d)) return expr; break;
                }
                break;
            
            case NUM_SIZE_64: break;
                switch(expr_num) {
                case NUM_TYPE_SIGNED: break;
                case NUM_TYPE_UNSIGNED: if (w.as.u > INT64_MAX)  return expr; break;
                case NUM_TYPE_FLOAT: if (w.as.d < INT64_MIN || w.as.d > INT64_MAX || w.as.d != ((s64)w.as.d)) return expr; break;
                }
                break;
            }
            break;

        case NUM_TYPE_UNSIGNED:
            switch ((num_size_t)(destinationtd->size)) {
            case NUM_SIZE_8:
                switch(expr_num) {
                case NUM_TYPE_SIGNED: if (w.as.s < 0 || w.as.s > UINT8_MAX) return expr; break;
                case NUM_TYPE_UNSIGNED: if (w.as.u > UINT8_MAX) return expr; break;
                case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT8_MAX || w.as.d != ((u8)w.as.d)) return expr; break;
                }
                break;
            
            case NUM_SIZE_16:
                switch(expr_num) {
                case NUM_TYPE_SIGNED: if (w.as.s < 0 || w.as.s > UINT16_MAX) return expr; break;
                case NUM_TYPE_UNSIGNED: if (w.as.u > UINT16_MAX) return expr; break;
                case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT16_MAX || w.as.d != ((u16)w.as.d)) return expr; break;
                }
                break;
            
            case NUM_SIZE_32:
                switch(expr_num) {
                case NUM_TYPE_SIGNED: if (w.as.s < 0 || w.as.s > UINT32_MAX) return expr; break;
                case NUM_TYPE_UNSIGNED: if (w.as.u > UINT32_MAX)  return expr; break;
                case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT32_MAX || w.as.d != ((u32)w.as.d)) return expr; break;
                }
                break;
            
            case NUM_SIZE_64: break;
                switch(expr_num) {
                case NUM_TYPE_SIGNED: if (w.as.s < 0) return expr; break;
                case NUM_TYPE_UNSIGNED: break;
                case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT64_MAX || w.as.d != ((u64)w.as.d)) return expr; break;
                }
                break;
            }
            break;

        case NUM_TYPE_FLOAT: break;
        }
    } else {
        // if not concrete then they need to be the same data type
        if (exprtd->data.num != destinationtd->data.num) return expr;

        // cannot store a type in storage with a smaller size
        if (exprtd->size > destinationtd->size) return expr;
    }

    ast_node_t *cast = ast_cast(ast, destination_type, expr, token_implicit_at_start(expr->start));

    if (expr->expr_val.is_concrete) {
        word_t result = constant_fold_cast(ast, expr->expr_val.word, destination_type, expr->value_type);
        cast->expr_val = ast_node_val_word(result);
        cast->is_free_number = expr->is_free_number;
    }

    return cast;
}

static bool stan_run(analyzer_t *analyzer, ast_node_t *expr, word_t *out_result) {
    tmp_arena_t *tmp = allocator_borrow();

    function_t *function = new_function(analyzer->env_or_null->memory, tmp->allocator);
    compile_expr_to_function(function, analyzer->ast, expr, analyzer->error_fn, analyzer->env_or_null->arena);

    vm_fresh_run(analyzer->env_or_null->vm, function);

    word_t result = analyzer->env_or_null->vm->registers[REG_RESULT];
    *out_result = result;

    allocator_return(tmp);
    return true;
}

void resolve_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *expr) {
    
    ASSERT(ast_node_type_is_expression(expr->node_type), "should be only expressions");

    if (TYPE_IS_RESOLVED(expr->value_type)) {
        return;
    }

    switch (expr->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: {
            ASSERT(sv_eq(expr->identifier.view, lit2sv("@run")), "only run is implemented right now");

            scope_t scope = {0};
            scope_init(&scope, &analyzer->allocator, SCOPE_TYPE_FOLD_DIRECTIVE, state.scope, expr);
            analysis_state_t new_state = state;
            new_state.scope = &scope;

            array_push(&analyzer->pending_dependencies, expr);

            ast_node_t *child = expr->children.items[0];

            resolve_expression(analyzer, ast, new_state, child);

            --analyzer->pending_dependencies.count;

            if (!analyzer->had_error && analyzer->env_or_null) {
                unless (TYPE_IS_INVALID(child->value_type)) {
                    typedata_t *td = type2td(ast, child->value_type);
                    size_t size = bytes_to_words(td->size);
                    ASSERT(size == 1, "for now types can only be as large as a word");

                    word_t result[size];
                    bool success = stan_run(analyzer, child, result);
                    if (success) {
                        expr->expr_val = ast_node_val_word(result[0]);
                        expr->value_type = child->value_type;
                    } else {
                        INVALIDATE(expr);
                    }
                } else {
                    INVALIDATE(expr);
                }
                break;
            } else {
                // make sure we cannot compile
                analyzer->had_error = true;

                // todo: output a warning that is not buildable or something
                expr->value_type = child->value_type;
                expr->expr_val = child->expr_val;
            }
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            resolve_expression(analyzer, ast, state, an_operand(expr));
            expr->lvalue_node = an_operand(expr);
            expr->value_type = an_operand(expr)->value_type;
            expr->expr_val = an_operand(expr)->expr_val;
            expr->is_free_number = an_operand(expr)->is_free_number;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_NIL: {
            if (TYPE_IS_UNRESOLVED(expr->value_type)) {
                // todo
                UNREACHABLE();
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            resolve_expression(analyzer, ast, state, expr->as.initiailizer.type);

            unless (TYPE_IS_TYPE(expr->as.initiailizer.type->value_type)) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_TYPE, expr->as.initiailizer.type));
                INVALIDATE(expr);
                break;
            }

            unless (expr->as.initiailizer.type->expr_val.is_concrete) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, expr->as.initiailizer.type));
                INVALIDATE(expr);
                break;
            }

            type_t type = typeid(expr->as.initiailizer.type->expr_val.word.as.u);
            typedata_t *typedata = type2td(ast, type);

            if (typedata->kind == TYPE_STRUCT) {
                int arg_count = expr->as.initiailizer.arguments.count;
                if (typedata->data.struct_.field_count < arg_count) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_TOO_MANY_STRUCT_ARGUMENTS, expr));
                    INVALIDATE(expr);
                    break;
                }

                bool is_invalidated = false;
                for (int i = 0; i < arg_count; ++i) {
                    ast_node_t *arg = expr->as.initiailizer.arguments.items[i];
                    unless (arg) continue;
                    resolve_expression(analyzer, ast, state, arg);
                    if (TYPE_IS_INVALID(arg->value_type)) {
                        is_invalidated = true;
                        continue;
                    }

                    if (i < typedata->data.struct_.field_count) {
                        type_t field_type = typedata->data.struct_.fields[i].type;
                        type_t arg_type = arg->value_type;
                        unless (typeid_eq(field_type, arg_type)) {
                            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_TYPE_MISMATCH,  arg));
                            is_invalidated = true;
                            continue;
                        }
                    }
                }

                if (is_invalidated) {
                    INVALIDATE(expr);
                    break;
                }

                type_t type = typeid(expr->as.initiailizer.type->expr_val.word.as.u);
                expr->value_type = type;
                break;
            } else {
                printf("need to look up field arg from struct as well for first arg");
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_TYPE_FOR_TYPE_INITIALIZER_LIST, expr));
                INVALIDATE(expr);
                break;
            }
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            INVALIDATE(expr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            ast_node_t *left = an_lhs(expr);
            resolve_expression(analyzer, ast, state, left);

            ast_node_t *right = an_rhs(expr);
            resolve_expression(analyzer, ast, state, right);

            an_lhs(expr) = (left = cast_implicitly_if_necessary(ast, right->value_type, left));
            an_rhs(expr) = (right = cast_implicitly_if_necessary(ast, left->value_type, right));


            if (TYPE_IS_INVALID(left->value_type) || TYPE_IS_INVALID(right->value_type)) {
                INVALIDATE(expr);
                break;
            }

            typedata_t *left_td = type2td(ast, left->value_type);
            typedata_t *right_td = type2td(ast, right->value_type);

            if (operator_is_arithmetic(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_ARITHMETIC) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_ARITHMETIC_OPERAND_TYPES, left));
                    INVALIDATE(expr);
                }

                unless (right_td->capabilities&TYPE_CAP_ARITHMETIC) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_ARITHMETIC_OPERAND_TYPES, right));
                    INVALIDATE(expr);
                    break;
                }

                unless (typeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_ARITHMETIC_OPERANDS_REQUIRES_EXPLICIT_CAST, expr));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = left->value_type;
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    word_t word = constant_fold_bin_arithmetic(ast, expr->operator.type, left->value_type, left->expr_val.word, right->expr_val.word);
                    expr->expr_val = ast_node_val_word(word);
                    expr->is_free_number = left->is_free_number && right->is_free_number;
                }
            } else if (operator_is_equating(expr->operator.type)) {
                // everything is equatable but they need to be the same type
                unless (typeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_ARITHMETIC_OPERANDS_REQUIRES_EXPLICIT_CAST, expr));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = typeid(TYPE_BOOL);
                
                // constant fold
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    word_t wordl = left->expr_val.word;
                    word_t wordr = right->expr_val.word;

                    typedata_t *td = type2td(ast, left->value_type);
                    if (td->size > WORD_SIZE) {
                        // todo
                        UNREACHABLE();
                    } else {
                        u8 result = (u8)(memcmp(&wordl, &wordr, WORD_SIZE) == 0);
                        expr->expr_val = ast_node_val_word(WORDU(result));
                    }

                    expr->is_free_number = left->is_free_number && right->is_free_number;
                }
            } else if (operator_is_comparing(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_COMPARABLE) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_COMPARISON_OPERAND_TYPES, left));
                    INVALIDATE(expr);
                }

                unless (right_td->capabilities&TYPE_CAP_COMPARABLE) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_COMPARISON_OPERAND_TYPES, right));
                    INVALIDATE(expr);
                    break;
                }

                unless (typeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_ARITHMETIC_OPERANDS_REQUIRES_EXPLICIT_CAST, expr));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = typeid(TYPE_BOOL);
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    word_t word = constant_fold_bin_comparison(ast, expr->operator.type, left->value_type, left->expr_val.word, right->expr_val.word);
                    expr->expr_val = ast_node_val_word(word);
                    expr->is_free_number = left->is_free_number && right->is_free_number;
                }
            } else if (operator_is_logical(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_LOGICAL) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_LOGICAL_OPERAND_TYPES, right));
                    INVALIDATE(expr);
                    break;
                }

                unless (right_td->capabilities&TYPE_CAP_LOGICAL) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_LOGICAL_OPERAND_TYPES, right));
                    INVALIDATE(expr);
                    break;
                }

                if (left_td->kind != TYPE_BOOL) {
                    // todo
                    UNREACHABLE();
                }
                
                expr->value_type = typeid(TYPE_BOOL);

                // constant fold
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    bool lhsb = left->expr_val.word.as.u;
                    bool rhsb = right->expr_val.word.as.u;
                    bool result = false;

                    switch (expr->operator.type) {
                    case TOKEN_AND: result = (lhsb && rhsb); break;
                    case TOKEN_OR: result = (lhsb || rhsb); break;
                    default: UNREACHABLE();
                    }

                    expr->expr_val = ast_node_val_word(WORDU((u64)result));
                    expr->is_free_number = left->is_free_number && right->is_free_number;
                }
            } else {
                UNREACHABLE();
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            resolve_expression(analyzer, ast, state, an_operand(expr));

            if (TYPE_IS_INVALID(an_operand(expr)->value_type)) {
                INVALIDATE(expr);
                break;
            }

            bool is_inside_type_context = (state.scope->type == SCOPE_TYPE_TYPE_CONTEXT);
            if (is_inside_type_context && expr->operator.type == TOKEN_AMPERSAND) {
                unless (TYPE_IS_TYPE(an_operand(expr)->value_type)) {
                    INVALIDATE(expr);
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_UNARY_OPERAND, expr));
                    break;
                }

                unless (an_operand(expr)->expr_val.is_concrete) {
                    INVALIDATE(expr);
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, an_operand(expr)));
                    break;
                }

                // todo
                INVALIDATE(expr);
            } else {
                typedata_t *operand_td = type2td(ast, an_operand(expr)->value_type);

                switch (expr->operator.type) {
                case TOKEN_NOT: {
                    if (operand_td->kind != TYPE_BOOL) {
                        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_NOT_OPERAND, expr));
                        INVALIDATE(expr);
                        break;
                    }

                    expr->value_type = an_operand(expr)->value_type;

                    if (an_operand(expr)->expr_val.is_concrete) {
                        bool value = an_operand(expr)->expr_val.word.as.u;
                        value = !value;
                        expr->expr_val = ast_node_val_word(WORDU((u64)value));
                    }
                    break;
                }

                case TOKEN_MINUS: {
                    if (operand_td->kind != TYPE_NUMBER) {
                        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_MINUS_OPERAND, expr));
                        INVALIDATE(expr);
                        break;
                    }

                    if (operand_td->data.num == NUM_TYPE_UNSIGNED) {
                        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_NEGATE_UNSIGNED_NUMBER, expr));
                        INVALIDATE(expr);
                        break;
                    }

                    ast_node_t *operand = an_operand(expr);
                    expr->value_type = operand->value_type;

                    // constand fold
                    if (an_operand(expr)->expr_val.is_concrete) {
                        switch (operand_td->data.num) {
                        case NUM_TYPE_SIGNED: {
                            s64 val = operand->expr_val.word.as.s;
                            val = -val;
                            expr->expr_val = ast_node_val_word(WORDI(val));
                            break;
                        }

                        case NUM_TYPE_FLOAT: {
                            f64 val = operand->expr_val.word.as.d;
                            val = -val;
                            expr->expr_val = ast_node_val_word(WORDD(val));
                            break;
                        }

                        default: UNREACHABLE();
                        }

                        expr->is_free_number = an_operand(expr)->is_free_number;
                    }
                    break;
                }

                default: UNREACHABLE();
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            expr->lvalue_node = expr;

            scope_t *def_scope;

            ast_node_t *decl = get_def_by_identifier_or_error(analyzer, ast, state, expr, &def_scope);

            if (decl == NULL) {
                INVALIDATE(expr);
                break;
            }

            expr->ref_decl = decl;

            expr->value_type = decl->value_type;

            if (!decl->is_mutable) {
                expr->foldable = true;
                expr->expr_val = decl->expr_val;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            // todo
            UNREACHABLE();

            ast_node_t *left = an_lhs(expr);

            resolve_expression(analyzer, ast, state, left);

            if (TYPE_IS_INVALID(left->value_type)) {
                INVALIDATE(expr);
                break;
            }

            ast_node_and_scope_t node_and_scope;
            bool skip_mutable = false;
            if (type_is_struct(ast->type_set.types, left->value_type)) {
                table_get(type2ns, ast->type_to_creation_node, left->value_type, &node_and_scope);
            } else if (TYPE_IS_TYPE(left->value_type) && type_is_struct(ast->type_set.types, typeid(left->expr_val.word.as.u))) {
                type_t struct_type = typeid(left->expr_val.word.as.u);
                table_get(type2ns, ast->type_to_creation_node, struct_type, &node_and_scope);
                skip_mutable = true;
            } else {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_MEMBER_ACCESS, expr));
                INVALIDATE(expr);
                break;
            }

            ast_node_t *referencing_declaration = NULL;
            for (size_t i = 0; i < node_and_scope.node->as.struct_.declarations.count; ++i) {
                ast_node_t *declaration = node_and_scope.node->as.struct_.declarations.items[i];
                if (skip_mutable && an_decl_expr(declaration)->is_mutable) {
                    continue;
                }

                token_t declaration_name = declaration->identifier;
                if (sv_eq(declaration_name.view, expr->identifier.view)) {
                    break;
                }
            }

            if (referencing_declaration == NULL) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_MEMBER_DOES_NOT_EXIST, expr));
                INVALIDATE(expr);
                break;
            }

            // keep until constant folding so no need to redo the definition look up
            expr->ref_decl = referencing_declaration;

            unless (is_declaration_resolved(referencing_declaration)) {
                analysis_state_t search_state = state;
                search_state.scope = node_and_scope.scope;

                scope_t* found_scope;
                ast_node_t *decl = get_def_by_identifier_or_error(analyzer, ast, search_state, expr, &found_scope);

                unless (decl) {
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = decl->value_type;
                break;
            }

            expr->value_type = referencing_declaration->value_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            ast_node_t *rhs = an_rhs(expr);
            resolve_expression(analyzer, ast, state, rhs);

            if (TYPE_IS_INVALID(rhs->value_type)) {
                INVALIDATE(expr);
                break;
            }

            ast_node_t *lhs = an_lhs(expr);
            resolve_expression(analyzer, ast, state, lhs);
            ast_node_t *lvalue_node = lhs->lvalue_node;
            if (an_is_none(lvalue_node)) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_LVALUE, lhs));
                INVALIDATE(expr);
                break;
            } 

            if (an_is_notnone(lvalue_node->ref_decl)) {
                scope_t *def_scope;
                ast_node_t *def;
                def = get_def_by_identifier_or_error(analyzer, ast, state, lvalue_node, &def_scope);

                
                if (def == NULL) {
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = def->value_type;

                type_t rhs_type = rhs->value_type;
                unless (typeid_eq(def->value_type, rhs_type)) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_TYPE_MISMATCH, expr));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = rhs_type;

            } else if (lvalue_node->node_type == AST_NODE_TYPE_EXPRESSION_DOT) {
                // todo
                UNREACHABLE();

                ASSERT(EXPRESSION_RESOLVED(lvalue_node), "must be resolved");
                // resolve_expression(analyzer, ast, state, lvalue_node);

                if (TYPE_IS_INVALID(lvalue_node->value_type)) {
                    INVALIDATE(expr);
                    break;
                }

                unless (typeid_eq(lvalue_node->value_type, rhs->value_type)) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_TYPE_MISMATCH, expr));
                    break;
                }

                expr->value_type = lvalue_node->value_type;

            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            s32 declarations_count = expr->children.count;

            scope_t block_scope;
            scope_init(&block_scope, &analyzer->allocator, SCOPE_TYPE_BLOCK, state.scope, expr);

            forward_scan_declaration_names(analyzer, &block_scope, expr->children, declarations_count);

            analysis_state_t block_state = state;
            block_state.scope = &block_scope;
            resolve_declarations(analyzer, ast, block_state, expr->children, declarations_count);

            size_t last_unreachable_index = expr->children.count;
            for (size_t i = 0; i < expr->children.count; ++i) {
                ast_node_t *decl = expr->children.items[i];
                if (TYPE_IS_UNREACHABLE(decl->value_type)) {
                    last_unreachable_index = i;
                    break;
                }
            }

            // discard everything past an unreachable type
            if (last_unreachable_index < expr->children.count) {
                expr->children.count = last_unreachable_index+1;

                // todo: add a warning for this
            }

            size_t last_decl_def = expr->children.count;
            size_t last_non_void_decl_stmt = expr->children.count;
            size_t last_decl_stmt = expr->children.count;

            for (size_t i = 0; i < expr->children.count; ++i) {
                ast_node_t *decl = expr->children.items[i];
                switch (decl->node_type) {
                    case AST_NODE_TYPE_DECLARATION_DEFINITION: {
                        last_decl_def = i;
                        break;
                    }

                    case AST_NODE_TYPE_DECLARATION_STATEMENT: {
                        last_decl_stmt = i;
                        unless (TYPE_IS_VOID(an_expression(decl)->value_type)) {
                            last_non_void_decl_stmt = i;
                        }
                        break;
                    }

                    default: UNREACHABLE();
                }
            }

            if (last_non_void_decl_stmt < expr->children.count && (last_decl_def == expr->children.count || last_decl_def < last_non_void_decl_stmt)) {
                expr->last_statement = expr->children.items[last_non_void_decl_stmt];
            } else if (last_decl_stmt < expr->children.count && (last_decl_def == expr->children.count || last_decl_def < last_decl_stmt)) {
                expr->last_statement = expr->children.items[last_decl_stmt];
            } else {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_BLOCKS_MUST_BE_EMPTY_OR_END_IN_STATEMENT, expr));
                INVALIDATE(expr);
                break;
            }

            expr->value_type = expr->last_statement->value_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            {
                scope_t condition_scope = {0};
                scope_init(&condition_scope, &ast->allocator, SCOPE_TYPE_CONDITION, state.scope, an_condition(expr));

                analysis_state_t cond_state = state;
                cond_state.scope = &condition_scope;

                resolve_expression(analyzer, ast, cond_state, an_condition(expr));
            }

            scope_t return_scope = {0};
            analysis_state_t return_state = state;

            switch (expr->branch_type) {
                case BRANCH_TYPE_DO:
                case BRANCH_TYPE_LOOPING: {
                    scope_init(&return_scope, &analyzer->allocator, SCOPE_TYPE_JMPABLE, state.scope, expr);

                    return_state = state;
                    return_state.scope = &return_scope;
                    break;
                }

                case BRANCH_TYPE_IFTHEN: break;
            }

            resolve_expression(analyzer, ast, return_state, an_then(expr));

            resolve_expression(analyzer, ast, state, an_else(expr));

            type_t branch_type = resolve_block_return_types(expr);
            expr->value_type = branch_type;

            unless (TYPE_IS_UNREACHABLE(branch_type)) {
                if (TYPE_IS_VOID(an_then(expr)->value_type) && an_then(expr)->node_type == AST_NODE_TYPE_EXPRESSION_NIL) {
                    an_then(expr) = ast_nil(ast, branch_type, token_implicit_at_end(an_then(expr)->start));
                }

                if (TYPE_IS_VOID(an_else(expr)->value_type) && an_else(expr)->node_type == AST_NODE_TYPE_EXPRESSION_NIL) {
                    an_else(expr) = ast_nil(ast, branch_type, token_implicit_at_end(an_else(expr)->start));
                }
            }

            if (TYPE_IS_INVALID(branch_type)) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_BLOCKS_TYPE_MISMATCH, expr));
                INVALIDATE(expr);
            }

            if (TYPE_IS_INVALID(an_then(expr)->value_type) || TYPE_IS_INVALID(an_else(expr)->value_type)) {
                INVALIDATE(expr);
            }

            if (!TYPE_IS_INVALID(an_condition(expr)->value_type) && !typeid_eq(an_condition(expr)->value_type, typeid(TYPE_BOOL))) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CONDITION_MUST_BE_BOOL, an_condition(expr)));
                INVALIDATE(expr);
            }

            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            resolve_expression(analyzer, ast, state, an_expression(expr));

            switch (expr->start.type) {
                case TOKEN_RETURN: {
                    scope_t *func_def_scope = NULL;
                    bool success = get_nearest_scope_in_func_or_error(analyzer, expr, state.scope, SCOPE_TYPE_FUNCDEF, lit2sv(""), &func_def_scope);
                    unless (success) {
                        INVALIDATE(expr);
                    } else {
                        expr->jmp_out_scope_node = func_def_scope->creator;
                        array_push(&func_def_scope->creator->jmp_nodes, expr);
                    }
                    break;
                }

                case TOKEN_CONTINUE:
                case TOKEN_BREAK: {
                    scope_t *found_scope = NULL;
                    bool success = get_nearest_scope_in_func_or_error(analyzer, expr, state.scope, SCOPE_TYPE_JMPABLE, expr->identifier.view, &found_scope);
                    unless (success) {
                        INVALIDATE(expr);
                    } else {
                        expr->jmp_out_scope_node = found_scope->creator;
                        array_push(&found_scope->creator->jmp_nodes, expr);
                        ASSERT(found_scope->creator->node_type == AST_NODE_TYPE_EXPRESSION_BRANCHING, "only supports branches rn");
                    }
                    break;
                }

                default: UNREACHABLE();
            }


            expr->value_type = typeid(TYPE_UNREACHABLE);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            bool argument_invalid = false;
            for (size_t i = an_call_arg_start(expr); i < an_call_arg_end(expr); ++i) {
                ast_node_t *argument = expr->children.items[i];
                resolve_expression(analyzer, ast, state, argument);
                if (TYPE_IS_INVALID(argument->value_type)) {
                    argument_invalid = true;
                }
            }

            ast_node_t *callee = an_callee(expr);
            resolve_expression(analyzer, ast, state, callee);

            if (argument_invalid || TYPE_IS_INVALID(callee->value_type)) {
                INVALIDATE(expr);
                break;
            }

            type_t callee_type = callee->value_type;

            if ((!type_is_function(ast->type_set.types, callee_type) && !type_is_native_function(ast->type_set.types, callee_type))) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CALLABLE, an_callee(expr)));
                break;
            }

            bool success = check_call_on_func(analyzer, ast, expr);
            if (!success) {
                INVALIDATE(expr);
                break;
            }

            typedata_t *callee_td = ast->type_set.types.items[callee_type.i];
            expr->value_type = callee_td->data.function.return_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            resolve_funcdef(analyzer, ast, state, expr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            resolve_struct_definition(analyzer, ast, state, expr);
            break;
        }
        
         case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            bool invalid_parameter = false;
            for (size_t i = an_func_def_arg_start(expr); i < an_func_def_arg_end(expr); ++i) {
                ast_node_t *parameter = expr->children.items[i];
                resolve_expression(analyzer, ast, state, parameter);

                if (TYPE_IS_INVALID(parameter->value_type)) {
                    invalid_parameter = true;
                }
            }

            resolve_expression(analyzer, ast, state, an_func_def_return(expr));

            if (invalid_parameter || TYPE_IS_INVALID(an_func_def_return(expr)->value_type)) {
                INVALIDATE(expr);
                break;
            }

            bool success = fold_funcsig_or_error(analyzer, ast, expr);
            unless (success) {
                INVALIDATE(expr);
                break;
            }

            expr->value_type = typeid(TYPE_TYPE);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: {
            size_t arg_start = an_bcall_arg_start(expr);
            size_t arg_end = an_bcall_arg_end(expr);
            size_t count = arg_end - arg_start;
            switch (expr->identifier.type) {
            case TOKEN_TYPEOF: {
                unless (count == 1) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_TYPEOF_REQUIRES_ONE_ARG, expr));
                    INVALIDATE(expr);
                    break;
                }

                ast_node_t *expr_arg = expr->children.items[arg_start];
                resolve_expression(analyzer, ast, state, expr_arg);

                if (TYPE_IS_INVALID(expr_arg->value_type)) {
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = typeid(TYPE_TYPE);
                expr->expr_val = ast_node_val_word(WORDT(expr_arg->value_type));
                break;
            }
            default: UNREACHABLE(); break;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            ast_node_t *cast_expr = an_expression(expr);
            resolve_expression(analyzer, ast, state, cast_expr);
            if (TYPE_IS_INVALID(cast_expr->value_type)) {
                INVALIDATE(expr);
                break;
            }

            typedata_t *desttd = type2td(ast, cast_expr->value_type);
            typedata_t *sourcetd = type2td(ast, cast_expr->value_type);

            if (desttd->kind != TYPE_NUMBER || sourcetd->kind != TYPE_NUMBER) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_OPERAND_CANNOT_CASTED, expr));
                INVALIDATE(expr);
                break;
            }

            if (cast_expr->expr_val.is_concrete) {
                word_t result = constant_fold_cast(ast, cast_expr->expr_val.word, expr->value_type, cast_expr->value_type);
                expr->expr_val = ast_node_val_word(result);
                expr->is_free_number = cast_expr->is_free_number;
            }
            break;
        }

        case AST_NODE_TYPE_NONE:
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_MODULE: {
            UNREACHABLE();
            break;
        }
    }
}

static void declare_definition(analyzer_t *analyzer, scope_t *scope, ast_node_t *definition) {
    word_t def_word;
    
    tmp_arena_t *tmp = allocator_borrow();
    string_t identifier = sv2string(definition->identifier.view, tmp->allocator);

    if (table_get(s2w, scope->definitions, identifier, &def_word)) {
        const char message[126];
        snprintf((char*)message, 126, "Duplicate definition definition of '%.*s'.", (int)definition->start.view.length, definition->start.view.data);
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_OVERLOAD_DEFINITION, definition));
        return;
    }

    add_definition(scope, &analyzer->allocator, string2sv(identifier), definition);

    allocator_return(tmp);
}

static void resolve_declaration_definition(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *declaration) {
    type_t declaration_type = typeid(TYPE_UNRESOLVED);
    if (TYPE_IS_UNRESOLVED(declaration->value_type) && !an_is_none(an_decl_type(declaration))) {
        analysis_state_t new_state = state;
        resolve_expression(analyzer, ast, new_state, an_decl_type(declaration));

        ast_node_t *decl_type = an_decl_type(declaration);
        declaration_type = decl_type->expr_val.word.as.t;
    }

    unless (an_is_none(an_decl_expr(declaration))) {
        if (TYPE_IS_UNRESOLVED(an_decl_expr(declaration)->value_type)) {
            analysis_state_t new_state = state;
            switch (state.scope->type) {
            case SCOPE_TYPE_FUNC_DEF_BODY:
            case SCOPE_TYPE_BLOCK:
            case SCOPE_TYPE_CONDITION:
            case SCOPE_TYPE_JMPABLE:
            case SCOPE_TYPE_MODULE:
            case SCOPE_TYPE_STRUCT:
            case SCOPE_TYPE_FOLD_DIRECTIVE:
            case SCOPE_TYPE_FUNCDEF: {
                break;
            }

            case SCOPE_TYPE_TYPE_CONTEXT:
            case SCOPE_TYPE_NONE: UNREACHABLE(); break;
            }

            resolve_expression(analyzer, ast, new_state, an_decl_expr(declaration));
        }
    } else {
        ASSERT(TYPE_IS_RESOLVED(declaration_type), "must be resolved from explicit type");
        an_decl_expr(declaration) = ast_nil(ast, declaration_type, token_implicit_at_end(declaration->end));
    }

    // we are resolved
    if (is_declaration_resolved(declaration)) {
        unless (TYPE_IS_TYPE(declaration->value_type)) {
            return;
        }

        // This must be available at compile time
        type_t struct_type = declaration->expr_val.word.as.t;
        typedata_t *struct_type_info = ast->type_set.types.items[struct_type.i];
        unless (struct_type_is_incomplete(struct_type_info) && struct_type_info->data.struct_.name) {
            return;
        }

        ast_node_t *cast_node = an_decl_expr(declaration);
        ASSERT(cast_node->node_type == AST_NODE_TYPE_EXPRESSION_CAST, "must be implicit casting node");
        
        type_t completed_struct_type = an_operand(cast_node)->value_type;
        
        // this means that we found out later that the struct this was supposed to be was actually invalid, so we need to fix the ast
        if (TYPE_IS_INVALID(completed_struct_type)) {
            INVALIDATE(cast_node);
            INVALIDATE(declaration);
            return;
        }

        ASSERT(type_is_struct(ast->type_set.types, completed_struct_type), "casted expression must be a struct type");

        typedata_t *completed_struct_type_info = ast->type_set.types.items[completed_struct_type.i];
        if (struct_type_is_incomplete(completed_struct_type_info)) {
            return;
        }

        named_struct_copy_data_from_completed_struct_type(&ast->type_set, struct_type, completed_struct_type);
        return;
    }


    if (TYPE_IS_INVALID(an_decl_expr(declaration)->value_type)) {
        INVALIDATE(declaration);
    }

    if (TYPE_IS_VOID(an_decl_expr(declaration)->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_STORE_VOID_EXPRESSIONS, declaration));
        INVALIDATE(declaration);
    }

    unless (declaration->is_mutable) {
        if (type_is_struct(ast->type_set.types, an_decl_expr(declaration)->value_type)
        && (TYPE_IS_UNRESOLVED(declaration_type) || TYPE_IS_TYPE(declaration_type))) {
            ast_node_t *to_struct_type = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_CAST, an_decl_expr(declaration)->start);
            to_struct_type->value_type = typeid(TYPE_TYPE);
            an_operand(to_struct_type) = an_decl_expr(declaration);

            to_struct_type->foldable = true;
            typedata_t *initial_expression_type_info = type2typedata(&ast->type_set.types, an_decl_expr(declaration)->value_type);
            type_t named_struct_id = type_create_struct(&ast->type_set, declaration->start.view.data, declaration->start.view.length, initial_expression_type_info);
            to_struct_type->expr_val = ast_node_val_word(WORDT(named_struct_id));

            type_t initial_expression_type = an_decl_expr(declaration)->value_type;

            ast_node_and_scope_t node_and_scope;
            bool found = table_get(type2ns, ast->type_to_creation_node, initial_expression_type, &node_and_scope);
            ASSERT(found, "this should always find something");

            an_decl_expr(declaration) = to_struct_type;

            table_put(type2ns, ast->type_to_creation_node, named_struct_id, node_and_scope);
            
            {
                // todo: add zero value for structs
                UNREACHABLE();
            }
        }
    }

    // Could be resolved could be unresolved at this point.
    declaration->value_type = declaration_type;

    if (TYPE_IS_UNRESOLVED(declaration->value_type)) {
        type_t expression_type = an_decl_expr(declaration)->value_type;
        declaration->value_type = expression_type;
    }

    {
        ast_node_t *decl_expr = an_decl_expr(declaration);
        type_t declared_type = declaration->value_type;

        ast_node_t *casted_expr = cast_implicitly_if_necessary(ast, declared_type, decl_expr);
        unless (typeid_eq(declared_type, casted_expr->value_type)) {
            unless (TYPE_IS_INVALID(decl_expr->value_type)) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_TYPE_MISMATCH, declaration));
            }
        }

        an_decl_expr(declaration) = casted_expr;
    }

    if (an_decl_expr(declaration)->expr_val.is_concrete) {
        declaration->expr_val = an_decl_expr(declaration)->expr_val;
    }
}

static ast_node_t *get_builtin_decl(ast_t *ast, string_view_t identifier) {
    tmp_arena_t *tmp = allocator_borrow();
    string_t identifier_ = sv2string(identifier, tmp->allocator);

    ast_node_t *decl;
    word_t def_slot;
    unless (table_get(s2w, ast->builtins, identifier_, &def_slot)) {
        type_t type;
        // native_function_t *function;
        bool has_value = false;
        type_t value_type;
        word_t value_slot;
        if (is_builtin_type(&ast->type_set, identifier, &type)) {
            has_value = true;
            value_slot = WORDT(type);
            value_type = typeid(TYPE_TYPE);
        }

        if (has_value) {
            decl = add_builtin_definition(ast, identifier, value_type, value_slot);
        } else {
            decl = NULL;
        }
    } else {
        decl = (ast_node_t*)def_slot.as.p;
    }

    allocator_return(tmp);
    return decl;
}

static ast_node_t *get_def_by_identifier_or_error(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *def,
        scope_t **search_scope) { // TODO: consider removing search scope from params, check if it's actually used

    bool passed_local_mutable_access_barrier = false;
    bool passed_local_fold_scope = false;

    // early return if looking at a built in type
    {
        ast_node_t *decl = get_builtin_decl(ast, def->identifier.view);
        if (decl) {
            search_scope = NULL;
            return decl;
        }
    }

    word_t def_slot;
    *search_scope = state.scope;

    ast_node_t *decl = NULL;

    while (*search_scope) {
        bool is_function_scope = (*search_scope)->type == SCOPE_TYPE_FUNCDEF;
        bool is_fold_scope = (*search_scope)->type == SCOPE_TYPE_FOLD_DIRECTIVE;

    #define NEXT_SCOPE() \
        if (is_function_scope) { passed_local_mutable_access_barrier = true; }\
        if (is_fold_scope) { passed_local_fold_scope = true; } \
        *search_scope = (*search_scope)->outer

        {
            tmp_arena_t *tmp = allocator_borrow();
            string_t identifier_ = sv2string(def->identifier.view, tmp->allocator);

            unless (table_get(s2w, (*search_scope)->definitions, identifier_, &def_slot)) {
                NEXT_SCOPE();
                continue;
            }

            allocator_return(tmp);
        }


        decl = (ast_node_t*)def_slot.as.p;

        if (passed_local_fold_scope && decl->is_mutable) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_DEFINITION_DOES_NOT_EXIST_IN_THE_SAME_RUN_SCOPE, def));
            return NULL;
        }

        if (passed_local_mutable_access_barrier && (*search_scope)->creator != NULL && decl->is_mutable) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CAN_ONLY_ACCESS_CONSTANTS_AND_GLOBALS, def));
            return NULL;
        }

        // this means that the declaration is *after* the definition we are trying to resolve
        if (decl->is_mutable && !is_declaration_resolved(decl)) {
            NEXT_SCOPE();
            continue;
        }
        
        break;
    }

    unless (decl) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_DEFINITION_DOES_NOT_EXIST, def));
        return NULL;
    }
    
    // ensure that the value is resolved
    if (an_is_notnone(an_decl_expr(decl))) {
        analysis_state_t new_state = state;
        new_state.scope = *search_scope;
        resolve_declaration_definition(analyzer, ast, new_state, decl);
    }

    #undef NEXT_SCOPE

    typedata_t *td = type2td(ast, decl->value_type);
    if (td->kind == TYPE_FUNCTION) {
        ASSERT(decl->expr_val.is_concrete, "should be constant and thus should be concrete");
        function_t *function = (function_t*)decl->expr_val.word.as.p;
        bool passed_through_fold = false;
        for (size_t i = analyzer->pending_dependencies.count; i > 0; --i) {
            ast_node_t *dep = analyzer->pending_dependencies.items[i-1];

            /**
             * proof by contradiction: assume the function is not compiled, but
             * it didn't pass through a fold barrier or the function was not found in the
             * dependencies list
             * 
             * the functon is not found in the dependency list and the function is not compiled
             * is impossible because before this is called, we resolve the declaration, and we find
             * that it's a function type declaration. this wouldn't be possible if we didn't try to 
             * resolve the function definition. so the function definition was resolved without calling itself
             * otherwise, we would have found in the dependencies list because the function is currently 
             * being resolved.
             * 
             * if the function doesn't pass through a fold barrier and the function is not compiled,
             * then the function must be in the dependency list. if it's before the fold barrier, this
             * is fine bceause it means the function will resolve *before* the fold needs to occur.
             * 
             * if the function is after it means, the fold needs to resolve before the function definition, but
             * the function definition needs the fold to resolve, causing a loop
             * 
             * not really proof by contradiction actually will rewrite
             * 
             */

            switch (dep->node_type) {
            case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: passed_through_fold = true; break;
            case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
                function_t *pending_function = (function_t*)dep->expr_val.word.as.p;
                if (passed_through_fold && function == pending_function) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_FUNCTION_PART_OF_CYCLICAL_DEPENDENCY, def));
                    return decl;
                }
                break;
            }

            default: UNREACHABLE(); break;
            }
        }
    }

    return decl;
}

static void resolve_funcdef(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *funcdef) {
    ASSERT(funcdef->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, "must be function declaration at this point");

    scope_t funcdef_scope;
    scope_init(&funcdef_scope, &analyzer->allocator, SCOPE_TYPE_FUNCDEF, state.scope, funcdef);
    types_t parameter_types = {.allocator=&ast->allocator};

    {
        size_t parameter_count = funcdef->children.count-2;
        tmp_arena_t *tmp = allocator_borrow();

        ast_nodes_t parameters = {.allocator=tmp->allocator};
        for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
            array_push(&parameters, funcdef->children.items[i]);
        }

        forward_scan_declaration_names(analyzer, &funcdef_scope, parameters, parameter_count);

        allocator_return(tmp);
    }

    bool parameter_invalid = false;

    for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
        analysis_state_t new_state = state;
        new_state.scope = &funcdef_scope;
        resolve_declaration_definition(analyzer, ast, new_state, funcdef->children.items[i]);
        array_push(&parameter_types, funcdef->children.items[i]->value_type);
        
        if (TYPE_IS_INVALID(funcdef->children.items[i]->value_type)) {
            parameter_invalid = true;
        }
    }

    type_t return_type = typeid(TYPE_VOID);
    {
        resolve_expression(analyzer, ast, state, an_func_def_return(funcdef));
    }

    ast_node_t *ret_expr = an_func_def_return(funcdef);
    unless (TYPE_IS_TYPE(ret_expr->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_RETURN_TYPE, an_func_def_return(funcdef)));
    } else {
        return_type = an_func_def_return(funcdef)->expr_val.word.as.t;
    }

    if (parameter_invalid || TYPE_IS_INVALID(return_type)) {
        INVALIDATE(funcdef);
        return;
    }

    type_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);
    funcdef->value_type = function_type;

    // create empty placeholder function immeidately in case definition is recursive
    function_t *function = NULL;
    unless (TYPE_IS_INVALID(an_func_def_block(funcdef)->value_type)) {
        if (analyzer->env_or_null) {
            function = new_function(analyzer->env_or_null->memory, analyzer->env_or_null->arena);
        } else {
            function = &analyzer->placeholder;
        }

        funcdef->expr_val = ast_node_val_word(WORDP(function));
    }

    array_push(&analyzer->pending_dependencies, funcdef);

    {
        analysis_state_t new_state = state;

        scope_t function_scope;
        scope_init(&function_scope, &analyzer->allocator, SCOPE_TYPE_FUNC_DEF_BODY, &funcdef_scope, funcdef);
        new_state.scope = &function_scope;
        resolve_expression(analyzer, ast, new_state, an_func_def_block(funcdef));
    }

    unless (TYPE_IS_UNREACHABLE(an_func_def_block(funcdef)->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES, funcdef));
    }

    for (size_t i = 0; i < funcdef->jmp_nodes.count; ++i) {
        ast_node_t *jmp = funcdef->jmp_nodes.items[i];
        
        ast_node_t *ret_expr = an_expression(jmp);

        type_t ret_expr_type = ret_expr->value_type;
        unless (typeid_eq(ret_expr_type, return_type)) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_TYPE_MISMATCH, funcdef));
        }
    }

    --analyzer->pending_dependencies.count;

    if (analyzer->had_error) {
        return;
    }

    unless (analyzer->env_or_null) {
        return;
    }

    ASSERT(function, "it should be set because we had no errors");

    gen_function_def(ast, analyzer->env_or_null, funcdef, analyzer->error_fn);
}

// footnote(struct-resolution)
static void resolve_struct_definition(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *struct_definition) {
    scope_t struct_scope;
    scope_init(&struct_scope, &analyzer->allocator, SCOPE_TYPE_STRUCT, state.scope, struct_definition);

    state.scope = &struct_scope;

    forward_scan_declaration_names(analyzer, state.scope, struct_definition->as.struct_.declarations, struct_definition->as.struct_.declarations.count);

    s32 declarations_count = struct_definition->as.struct_.declarations.count;

    type_t incomplete_struct_id = type_unique_incomplete_struct_type(&ast->type_set);
    struct_definition->value_type = incomplete_struct_id;

    // todo: correct value
    UNREACHABLE();
    // struct_definition->value_index = value_index_(0);

    ast_node_and_scope_t node_and_scope = {
        .node = struct_definition,
        .scope = &struct_scope
    };
    table_put(type2ns, ast->type_to_creation_node, struct_definition->value_type, node_and_scope);

    s32 field_count = 0;

    bool invalid_struct = false;
    for (s32 i = 0; i < declarations_count; i++) {
        ast_node_t* declaration = struct_definition->as.struct_.declarations.items[i];
        field_count += (declaration->is_mutable);

        resolve_declaration_definition(analyzer, ast, state, declaration);

        if (TYPE_IS_INVALID(declaration->value_type) || TYPE_IS_UNRESOLVED(declaration->value_type)) {
            invalid_struct = true;
            break;
        }
    }

    if (invalid_struct) {
        INVALIDATE(struct_definition);
        ast_node_and_scope_t node_and_scope;
        table_get(type2ns, ast->type_to_creation_node, struct_definition->value_type, &node_and_scope);
        node_and_scope.scope = NULL;
        table_put(type2ns, ast->type_to_creation_node, struct_definition->value_type, node_and_scope);
        return;
    }

    s32 constant_count = declarations_count - field_count;

    struct_field_t fields[field_count];
    ast_node_t* ast_fields[field_count];
    struct_constant_t constants[constant_count];

    {
        s32 field_counter = 0;
        s32 constant_counter = 0;

        for (s32 i = 0; i < declarations_count; ++i) {
            token_t identifier = struct_definition->as.struct_.declarations.items[i]->identifier;
            char* name = arena_alloc(&analyzer->allocator, sizeof(char)*(identifier.view.length + 1));

            memcpy(name, identifier.view.data, identifier.view.length);
            
            name[identifier.view.length] = '\0';

            if (struct_definition->as.struct_.declarations.items[i]->is_mutable) {
                fields[field_counter].type = struct_definition->as.struct_.declarations.items[i]->value_type;
                fields[field_counter].name = name;

                ast_fields[field_counter] = struct_definition->as.struct_.declarations.items[i];

                ++field_counter;
            } else {
                constants[constant_counter].type = struct_definition->as.struct_.declarations.items[i]->value_type;
                constants[constant_counter].name = name;
                ++constant_counter;
            }
        }
    }

    typedata_t *complete_struct_type_info;

    type_t complete_struct_type = type_set_fetch_anonymous_struct(&ast->type_set, field_count, fields, constant_count, constants);
    complete_struct_type_info = ast->type_set.types.items[complete_struct_type.i];

    s32 incomplete_index = -1;
    for (s32 i = 0; i < complete_struct_type_info->data.struct_.field_count; i++) {
        type_t field_type = complete_struct_type_info->data.struct_.fields[i].type;
        typedata_t *field_type_info = ast->type_set.types.items[field_type.i];
        if (struct_type_is_incomplete(field_type_info)) {
            incomplete_index = i;
            break;
        }
    }

    if (incomplete_index < 0) {
        struct_definition->value_type = complete_struct_type;

        ASSERT(complete_struct_type_info->data.struct_.field_count == field_count, "completed struct must have the same number as fields as ast field declaration nodes");

        u64 size_in_slots = bytes_to_words(complete_struct_type_info->size);
        byte struct_data[size_in_slots * sizeof(word_t)];
        for (u64 i = 0; i < size_in_slots * sizeof(word_t); i++) {
            struct_data[i] = 0;
        }

        // for (s32 i = 0; i < field_count; i++) {
            // type_t field_type = complete_struct_type_info->data.struct_.fields[i].type;
            // u32 offset = complete_struct_type_info->data.struct_.fields[i].offset;
            // ast_node_t *declaration = ast_fields[i];

            // typedata_t *field_type_info = type2typedata(&ast->type_set.types, field_type);

            // u32 bytes_to_copy = field_type_info->size;

            UNREACHABLE();
            // todo: struct copy
            // void *value_src = ast->constants.data + declaration->value_index.index;
            // memcpy(struct_data + offset, value_src, bytes_to_copy);
        // }

        UNREACHABLE();
        // todo: store struct value somewhere
        // struct_definition->value_index = ast_push_constant(ast, struct_data, complete_struct_type);

        node_and_scope.scope = NULL;
        table_put(type2ns, ast->type_to_creation_node, complete_struct_type, node_and_scope);
    } else {
        INVALIDATE(struct_definition);

        ast_node_t *incomplete_field = struct_definition->as.struct_.declarations.items[incomplete_index];
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_RESOLVED, incomplete_field));
    }
}

static void resolve_declaration_statement(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *statement) {
    resolve_expression(analyzer, ast, state, an_expression(statement));
    statement->value_type = an_expression(statement)->value_type;
}

static void resolve_declaration(
        analyzer_t* analyzer,
        ast_t* ast,
        analysis_state_t state,
        ast_node_t* declaration_node) {

    switch (declaration_node->node_type) {
        case AST_NODE_TYPE_DECLARATION_DEFINITION: {
            resolve_declaration_definition(analyzer, ast, state, declaration_node);
            break;
        }

        case AST_NODE_TYPE_DECLARATION_STATEMENT: {
            resolve_declaration_statement(analyzer, ast, state, declaration_node);
            break;
        }

        default: UNREACHABLE();
    }
}

void resolve_declarations(analyzer_t* analyzer, ast_t* ast, analysis_state_t state, ast_nodes_t declarations, s32 count) {
    for (s32 i = 0; i < count; i++) {
        ast_node_t* declaration = declarations.items[i];
        if (declaration->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION && !declaration->is_mutable) {
            continue;
        }

        resolve_declaration(analyzer, ast, state, declarations.items[i]);
    }

    for (s32 i = 0; i < count; i++) {
        ast_node_t *declaration = declarations.items[i];
        if (declaration->node_type != AST_NODE_TYPE_DECLARATION_DEFINITION) {
            continue;
        }

        if (declaration->is_mutable) {
            continue;
        }

        if (is_declaration_resolved(declaration)) {
            continue;
        }

        resolve_declaration(analyzer, ast, state, declaration);
    }
}

bool resolve_ast(analyzer_t *analyzer, ast_t *ast) {
    if (ast->root->node_type == AST_NODE_TYPE_MODULE) {
        UNREACHABLE();
        if (ast->root->children.items == NULL) {
            ast->resolved = false;
            // error(analyzer, 0, "No code to run. Akin to having no main function.");
            return false;
        }

        scope_t global_scope = {0};
        scope_init(&global_scope, &analyzer->allocator, SCOPE_TYPE_MODULE, NULL, NULL);

        analysis_state_t analysis_state = (analysis_state_t) {
            .scope = &global_scope,
        };

        s32 declaration_count = ast->root->children.count;
        forward_scan_declaration_names(analyzer, &global_scope, ast->root->children, declaration_count);
        resolve_declarations(analyzer, ast, analysis_state, ast->root->children, declaration_count);

        ast->resolved = !analyzer->had_error;

        return ast->resolved;
    } else {
        scope_t global_scope = {0};
        scope_init(&global_scope, &analyzer->allocator, SCOPE_TYPE_MODULE, NULL, NULL);

        analysis_state_t state = (analysis_state_t) {
            .scope = &global_scope,
        };

        resolve_expression(analyzer, ast, state, ast->root);

        ast->resolved = !analyzer->had_error;

        return ast->resolved;
    }
}

void analyzer_init(analyzer_t *analyzer, env_t *env, write_function_t write_fn, error_function_t error_fn) {
    *analyzer = zer0(analyzer_t);

    analyzer->error_fn = error_fn;
    analyzer->had_error = false;
    analyzer->env_or_null = env;

    analyzer->allocator = zer0(arena_t);
    analyzer->placeholder = zer0(function_t);
    analyzer->pending_dependencies.allocator = &analyzer->allocator;

    // TODO: fix
    (void)write_fn;
}

void analyzer_free(analyzer_t *analyzer) {
    analyzer->ast = NULL;

    analyzer->error_fn = NULL;
    analyzer->had_error = false;
}
