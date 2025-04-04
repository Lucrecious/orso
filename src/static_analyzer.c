#include "static_analyzer.h"

#include <stdio.h>

#include "type_set.h"
#include "error.h"
#include "vm.h"
#include "codegen.h"
#include "tmp.h"
#include "core.h"

#include "intrinsics.h"

#define X(export_fn_name, c_fn_name, return_type, code, ...) { .name=(#export_fn_name), .cname=(#c_fn_name), .fn=(c_fn_name##_i_) },
#define XARG(name, type)
#define XRET(type, c_fn_name, ...)
struct { cstr_t name; cstr_t cname; intrinsic_fn_t fn; } intrinsic_fns[] = {
#undef XRET
#undef XARG
#include "intrinsic_fns.x"
};
#undef X
#define INTRINSIC_FUNCTION_COUNT (sizeof(intrinsic_fns) / sizeof(intrinsic_fns[0]))

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

    table_put(s2w, ast->builtins, sv2string(identifier, ast->arena), WORDP(decl));

    for (size_t i = 0; i < INTRINSIC_FUNCTION_COUNT; ++i) {
        string_t name = cstr2string(intrinsic_fns[i].name, ast->arena);
        string_t cname = cstr2string(intrinsic_fns[i].cname, ast->arena);
        intrinsic_fn_t fn = intrinsic_fns[i].fn;

        table_put(s2w, ast->intrinsic_fns, name, WORDP(fn));
        table_put(p2s, ast->intrinsicfn2cname, fn, cname);
    }

    return decl;
}

static void stan_error(analyzer_t *analyzer, error_t error) {
    analyzer->had_error = true;
    array_push(&analyzer->ast->errors, error);
}

static ast_node_t *get_defval_or_null_by_identifier_and_error(
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
    RETURN_IF_TYPE(identifier, ptrdiff_t, t->ptrdiff_t_)

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
    typedata_t *func_td = ast_type2td(ast, callee->value_type);

    ASSERT(func_td->kind == TYPE_FUNCTION || func_td->kind == TYPE_INTRINSIC_FUNCTION, "must be used only on func");

    size_t arg_start = an_call_arg_start(call);
    size_t arg_end = an_call_arg_end(call);
    size_t arg_count = arg_end - arg_start;

    bool errored = false;
    size_t func_type_arg_count = func_td->as.function.argument_types.count;
    if (func_type_arg_count != arg_count) {
        errored = true;
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_NUMBER_ARGS_CALL_FUNC_MISTMATCH,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("call to '$0.$' requires $1.$ arguments but got $2.$"),
            .args = ORERR_ARGS(error_arg_node(callee), error_arg_sz(func_type_arg_count), error_arg_sz(arg_count)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    size_t min_args = func_type_arg_count < arg_count ? func_type_arg_count : arg_count;

    for (size_t i = 0; i < min_args; ++i) {
        type_t parameter_type = func_td->as.function.argument_types.items[i];

        ast_node_t *arg = call->children.items[arg_start + i];
        type_t argument_type = arg->value_type;
        unless (typeid_eq(parameter_type, argument_type) && !TYPE_IS_INVALID(argument_type)) {
            errored = true;
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("argument $0.$ is type '$1.$' but '$2.$' requires type '$3.$'"),
                .args = ORERR_ARGS(
                    error_arg_sz(i+1),
                    error_arg_type(arg->value_type),
                    error_arg_node(callee),
                    error_arg_type(parameter_type),
                    error_arg_node(arg)),
                .show_code_lines = ORERR_LINES(4),
            ));
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
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_EXPECTED_CONSTANT,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("return type for function signature must be a constant"),
            .args = ORERR_ARGS(error_arg_node(an_func_def_return(expression))),
            .show_code_lines = ORERR_LINES(0),
        ));
        return false;
    }

    bool hit_error = false;

    tmp_arena_t *tmp = allocator_borrow();
    types_t parameter_types = {.allocator=tmp->allocator};

    bool type_is_inferred = false;

    for (size_t i = an_func_def_arg_start(expression); i < an_func_def_arg_end(expression); ++i) {
        ast_node_t *parameter = expression->children.items[i];
        unless (parameter->expr_val.is_concrete) {
            hit_error = true;
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_EXPECTED_CONSTANT,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("parameter type $1.$ for function signature must be a constant"),
                .args = ORERR_ARGS(error_arg_node(parameter), error_arg_sz(i-an_func_def_arg_start(expression) + 1)),
                .show_code_lines = ORERR_LINES(0),
            ));
            break;
        }

        if (TYPE_IS_UNRESOLVED(parameter->value_type)) {
            type_is_inferred = true;
            continue;
        }

        unless (TYPE_IS_TYPE(parameter->value_type)) {
            hit_error = true;
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("parameter expression must be type 'type'"),
                .args = ORERR_ARGS(error_arg_node(parameter), error_arg_sz(i-an_func_def_arg_start(expression) + 1)),
                .show_code_lines = ORERR_LINES(0),
            ));
            break;
        }

        type_t type = typeid(parameter->expr_val.word.as.u);
        array_push(&parameter_types, type);
    }

    bool result = false;

    if (type_is_inferred) {
        expression->expr_val = ast_node_val_word(WORDT(typeid(TYPE_UNRESOLVED)));

        result = true;
        goto defer;
    }

    if (hit_error) {
        result = false;
        goto defer;
    }

    ast_node_t *return_type_expression = an_func_def_return(expression);
    if (!TYPE_IS_TYPE(return_type_expression->value_type)) {
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("return expression must be type 'type'"),
            .args = ORERR_ARGS(error_arg_node(return_type_expression)),
            .show_code_lines = ORERR_LINES(0),
        ));
        result = false;
        goto defer;
    }

    type_t return_type = typeid(return_type_expression->expr_val.word.as.u);

    type_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);

    word_t funcsig_word = WORDU(function_type.i);

    expression->expr_val = ast_node_val_word(funcsig_word);

    result = true;

defer:
    allocator_return(tmp);
    return result;
}

static bool get_nearest_jmp_scope_in_func_or_error(
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
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_CANNOT_JMP_IN_CONDITION,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("jmp expressions cannot leave their condition's scopes"),
                .args = ORERR_ARGS(error_arg_node(jmp_node), error_arg_node(scope->creator)),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return false;
        }

        scope = scope->outer;
    }

    if (search_type == SCOPE_TYPE_FUNCDEF) {
        // i think this is only for parsing expression files
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_CANNOT_FIND_FUNCTION_TO_RETURN_FROM,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("cannot find function to return from"),
            .args = ORERR_ARGS(error_arg_node(jmp_node)),
            .show_code_lines = ORERR_LINES(0),
        ));
    } else {
        if (label.length != 0) {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_CANNOT_FIND_JMP_LABEL,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot find jmp label '$0.$'"),
                .args = ORERR_ARGS(error_arg_token(jmp_node->identifier)),
                .show_code_lines = ORERR_LINES(0),
            ));
        } else {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_NO_VALID_JMP_BLOCK,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("no valid scope to jmp out of"),
                .args = ORERR_ARGS(error_arg_node(jmp_node)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }
    return false;
}
type_t resolve_block_return_types_or_error(analyzer_t *analyzer, ast_node_t *block) {
    switch (block->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            ast_node_t *then = an_then(block);
            ast_node_t *else_ = an_else(block);

            tmp_arena_t *tmp = allocator_borrow();
            types_t types = {.allocator=tmp->allocator};
            ast_nodes_t nodes = {.allocator=tmp->allocator};

            unless (TYPE_IS_UNREACHABLE(then->value_type)) {
                array_push(&nodes, then);
                array_push(&types, then->value_type);
            }

            unless (TYPE_IS_UNREACHABLE(else_->value_type)) {
                array_push(&nodes, else_);
                array_push(&types, else_->value_type);
            }

            for (size_t i = 0; i < block->jmp_nodes.count; ++i) {
                ast_node_t *jmp_node = block->jmp_nodes.items[i];
                ast_node_t *jmp_node_expr = an_expression(jmp_node);
                unless (TYPE_IS_UNREACHABLE(jmp_node_expr->value_type)) {
                    array_push(&nodes, jmp_node_expr);
                    array_push(&types, jmp_node_expr->value_type);
                }
            }

            MUST(types.count > 0);
            type_t type = types.items[0];
            ast_node_t *node = nodes.items[0];

            for (size_t i = 1; i < types.count; ++i) {
                type_t other_type = types.items[i];
                ast_node_t *other_node = nodes.items[i];

                unless (typeid_eq(other_type, type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_BLOCKS_TYPE_MISMATCH,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("all '$0.kind$' branches and jmps must match types when consumed but got '$3.$' and '$4.$'"),
                        .args = ORERR_ARGS(error_arg_token(block->start), error_arg_node(node), error_arg_node(other_node), error_arg_type(type), error_arg_type(other_type)),
                        .show_code_lines = ORERR_LINES(1, 2),
                    ));

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

static void resolve_declaration_definition(
    analyzer_t *analyzer,
    ast_t *ast,
    analysis_state_t state,
    ast_node_t *decl);

static void resolve_declarations(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_nodes_t declarations);

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

static void declare_definition(analyzer_t *analyzer, scope_t *scope, ast_node_t *definition);

static void forward_scan_constant_names(analyzer_t *analyzer, scope_t *scope, ast_nodes_t declarations) {
    for (size_t i = 0; i < declarations.count; i++) {
        ast_node_t *decl = declarations.items[i];
        if (decl->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION && an_is_constant(decl)) {
            declare_definition(analyzer, scope, decl);
        }
    }
}

word_t constant_fold_bin_arithmetic(ast_t *ast, token_type_t operator, type_t type, word_t l, word_t r) {
    typedata_t *numtype = ast_type2td(ast, type);

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
        switch (numtype->as.num) {
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
    typedata_t *typedata = ast_type2td(ast, type);

    switch (typedata->kind) {
    case TYPE_NUMBER: {
        switch (typedata->as.num) {
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

static bool stan_can_cast(typedatas_t *types, type_t dst, type_t src) {
    typedata_t *srctd = type2typedata(types, src);
    typedata_t *dsttd = type2typedata(types, dst);

    if (srctd->kind == TYPE_NUMBER && dsttd->kind == TYPE_NUMBER) return true;
    if (srctd->kind == TYPE_POINTER && dsttd->kind == TYPE_POINTER) {
        if (TYPE_IS_VOID(dsttd->as.ptr.type)) return true;
        if (TYPE_IS_VOID(srctd->as.ptr.type)) return true;

        typedata_t *src_inner_td = type2typedata(types, srctd->as.ptr.type);
        typedata_t *dst_inner_td = type2typedata(types, dsttd->as.ptr.type);

        if (src_inner_td->kind == dst_inner_td->kind && src_inner_td->kind == TYPE_ARRAY) {
            if (typeid_eq(src_inner_td->as.arr.type, dst_inner_td->as.arr.type)) {
                if (dst_inner_td->as.arr.count > src_inner_td->as.arr.count) return false;
                return true;
            } 
        }

        return false;
    }

    if (srctd->kind == TYPE_FUNCTION && dsttd->kind == TYPE_POINTER && TYPE_IS_VOID(dsttd->as.ptr.type)) return true;
    if (srctd->kind == TYPE_POINTER && TYPE_IS_VOID(srctd->as.ptr.type) && dsttd->kind == TYPE_FUNCTION) return true;

    if (srctd->kind == TYPE_POINTER && dsttd->kind == TYPE_NUMBER &&
            (dsttd->as.num == NUM_TYPE_SIGNED || dsttd->as.num == NUM_TYPE_UNSIGNED)) return true;

    if (srctd->kind == TYPE_NUMBER && (srctd->as.num == NUM_TYPE_SIGNED || srctd->as.num == NUM_TYPE_UNSIGNED) &&
            dsttd->kind == TYPE_POINTER) return true;


    return false;
}

static word_t constant_fold_cast(ast_t *ast, word_t in, type_t dst, type_t src) {
    typedata_t *desttd = ast_type2td(ast, dst);
    typedata_t *sourcetd = ast_type2td(ast, src);
    ASSERT(stan_can_cast(&ast->type_set.types, dst, src), "must be castable");

    switch (desttd->kind) {
    case TYPE_NUMBER: {
        #define CASTING(src_type, dst_type) (sourcetd->as.num == NUM_TYPE_##src_type && desttd->as.num == NUM_TYPE_##dst_type)
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

    case TYPE_FUNCTION:
    case TYPE_POINTER: return in;

    default: UNREACHABLE(); return in;
    }
}

static ast_node_t *ast_implicit_cast(ast_t *ast, ast_node_t *expr, type_t dst_type) {
    ast_node_t *inferred_type = ast_inferred_type_decl(ast, token_implicit_at_start(expr->start), token_implicit_at_start(expr->start));
    inferred_type->value_type = typeid(TYPE_TYPE);
    inferred_type->expr_val = ast_node_val_word(WORDT(dst_type));

    ast_node_t *cast = ast_cast(ast, inferred_type, expr);
    cast->value_type = dst_type;

    if (expr->expr_val.is_concrete) {
        word_t result = constant_fold_cast(ast, expr->expr_val.word, dst_type, expr->value_type);
        cast->expr_val = ast_node_val_word(result);
        cast->is_free_number = expr->is_free_number;
    }

    return cast;
}
bool type_size_is_platform_specific(type_table_t *type_table, type_t type) {
    #define IS(t) typeid_eq(type, type_table->t##_)
    if (IS(s8) || IS(s16) || IS(s32) || IS(s64) ||
        IS(u8) || IS(u16) || IS(u32) || IS(u64) ||
        IS(f64) || IS(f32)) return false;
    #undef IS

    return true;
}

static ast_node_t *cast_implicitly_if_necessary(ast_t *ast, type_t dst_type, ast_node_t *expr) {
    ASSERT(TYPE_IS_RESOLVED(expr->value_type), "expression must be resolved already");

    if (typeid_eq(dst_type, expr->value_type)) return expr;

    unless (stan_can_cast(&ast->type_set.types, dst_type, expr->value_type)) return expr;

    typedata_t *dsttd = ast_type2td(ast, dst_type);
    typedata_t *exprtd = ast_type2td(ast, expr->value_type);

    // for now only numbers can be cast/promoted implicitly
    if (dsttd->kind == TYPE_NUMBER && exprtd->kind == TYPE_NUMBER) {
        num_type_t dst_num = dsttd->as.num;
        num_type_t expr_num = exprtd->as.num;

        // if we now what the value of the number we just implicitly cast it...
        if (expr->expr_val.is_concrete && expr->is_free_number) {
            word_t w = expr->expr_val.word;
            switch (dst_num) {
            case NUM_TYPE_SIGNED:
                switch ((num_size_t)(dsttd->size)) {
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
                switch ((num_size_t)(dsttd->size)) {
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
            if (exprtd->as.num != dsttd->as.num) return expr;

            // shouldn't convert types unless they are platform specific
            if (type_size_is_platform_specific(&ast->type_set, expr->value_type) || type_size_is_platform_specific(&ast->type_set, dst_type)) {
                return expr;
            }

            // cannot store a type in storage with a smaller size
            if (exprtd->size > dsttd->size) return expr;
        }
    } else if (exprtd->kind == TYPE_POINTER && dsttd->kind == TYPE_POINTER && TYPE_IS_VOID(dsttd->as.ptr.type)) {
        // good to go
    } else {
        return expr;
    }

    ast_node_t *cast = ast_implicit_cast(ast, expr, dst_type);

    return cast;
}

static bool stan_run(analyzer_t *analyzer, vm_t *vm, ast_node_t *expr, word_t *out_result) {
    tmp_arena_t *tmp = allocator_borrow();

    function_t *function = new_function(vm->program_mem, tmp->allocator);
    compile_expr_to_function(function, analyzer->ast, expr, vm->program_mem, vm->arena);

    vm_fresh_run(analyzer->run_vm, function);

    word_t result = analyzer->run_vm->registers[REG_RESULT];
    *out_result = result;

    allocator_return(tmp);
    return true;
}

static matched_value_t stan_pattern_match_or_error(analyzer_t *analyzer, ast_node_t *decl, type_path_t *expected, type_t actual, ast_node_t *arg) {
    typedata_t *td = ast_type2td(analyzer->ast, actual);
    switch (expected->kind) {
    case MATCH_TYPE_POINTER: {
        if (td->kind != TYPE_POINTER) {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_COULD_NOT_PATTERN_MATCH_TYPE,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match inferred declaration with argument type"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=typeid(TYPE_INVALID)};
        }

        return stan_pattern_match_or_error(analyzer, decl, expected->next, td->as.ptr.type, arg);
    }

    case MATCH_TYPE_ARRAY_TYPE: {
        if (td->kind != TYPE_ARRAY) {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_COULD_NOT_PATTERN_MATCH_TYPE,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match inferred declaration with argument type"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=typeid(TYPE_INVALID)};
        }

        return stan_pattern_match_or_error(analyzer, decl, expected->next, td->as.arr.type, arg);
    }

    case MATCH_TYPE_ARRAY_SIZE: {
        if (td->kind != TYPE_ARRAY) {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_COULD_NOT_PATTERN_MATCH_TYPE,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match inferred declaration with argument type"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=typeid(TYPE_INVALID)};
        }

        if (expected->next->kind != MATCH_TYPE_IDENTIFIER) {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_COULD_NOT_PATTERN_MATCH_TYPE,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match inferred declaration with argument type"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=typeid(TYPE_INVALID)};
        }

        return (matched_value_t){
            .type=analyzer->ast->type_set.size_t_,
            .word=WORDU(td->as.arr.count),
        };
    }

    case MATCH_TYPE_IDENTIFIER: {
        return (matched_value_t){
            .type=typeid(TYPE_TYPE),
            .word=WORDT(actual),
        };
    }

    default: UNREACHABLE();
    }
}

static bool matched_values_eq(matched_values_t a, matched_values_t b) {
    if (a.count != b.count) return false;

    for (size_t i = 0; i < a.count; ++i) {
        matched_value_t mva = a.items[i];
        matched_value_t mvb = b.items[i];
        if (typeid_nq(mva.type, mvb.type)) return false;
        if (mva.word.as.u != mvb.word.as.u) return false;
    }

    return true;
}

static ast_node_t *find_realized_funcdef_or_null_by_inferred_types(inferred_funcdef_copies_t copies, matched_values_t key) {
    for (size_t i = 0; i < copies.count; ++i) {
        inferred_funcdef_copy_t copy = copies.items[i];
        ASSERT(copy.key.count == key.count, "the keys should be the same size at this point");

        bool found = matched_values_eq(key, copy.key);

        if (found) return copy.funcdef;
    }

    return NULL;
}

static void stan_circular_dependency_error(analyzer_t *analyzer, ast_t *ast, ast_node_t *def, size_t dep_start_index) {
    ast_nodes_t *deps = arena_alloc(ast->arena, sizeof(ast_nodes_t));
    *deps = (ast_nodes_t){.allocator=ast->arena};

    for (size_t j = dep_start_index; j < analyzer->pending_dependencies.count; ++j) {
        ast_node_t *dep = analyzer->pending_dependencies.items[j];
        array_push(deps, dep);
    }

    stan_error(analyzer, OR_ERROR(
        .tag = ERROR_ANALYSIS_COMPILE_TIME_CIRCULAR_DEPENDENCIES,
        .level = ERROR_SOURCE_ANALYSIS,
        .msg = lit2str("attempting to retrieve a constant with recursive compile-time dependencies"),
        .args = ORERR_ARGS(error_arg_node(def), error_arg_ptr(deps)),
        .show_code_lines = ORERR_LINES(0),
    ));
}

static size_t stan_function_is_building(analyzer_t *analyzer, function_t *function) {
    bool passed_through_fold = false;
    for (size_t i = analyzer->pending_dependencies.count; i > 0; --i) {
        ast_node_t *dep = analyzer->pending_dependencies.items[i-1];

        switch (dep->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: passed_through_fold = true; break;
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            function_t *pending_function = (function_t*)dep->expr_val.word.as.p;
            if (passed_through_fold && function == pending_function) {
                return i-1;
            }
            break;
        }

        default: continue;
        }
    }

    return analyzer->pending_dependencies.count;
}

static size_t stan_declaration_is_recursive(analyzer_t *analyzer, ast_node_t *decl) {
    for (size_t i = analyzer->pending_dependencies.count; i > 0; --i) {
        ast_node_t *dep = analyzer->pending_dependencies.items[i-1];
        if (dep->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) break;
        if (dep == decl) return i-1;
    }

    return analyzer->pending_dependencies.count;
}

void resolve_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        type_t implicit_type,
        ast_node_t *expr);

static ast_node_t *stan_realize_inferred_funcdef_or_error_and_null(analyzer_t *analyzer, analysis_state_t state, ast_node_t *call, ast_node_t *inferred_funcdef) {
    scope_t scope = {0};
    scope_init(&scope, analyzer->ast->arena, SCOPE_TYPE_INFERRED_PARAMS, state.scope, call);

    analysis_state_t new_state = state;
    new_state.scope = &scope;

    tmp_arena_t *tmp = allocator_borrow();
    matched_values_t matched_values = {.allocator=tmp->allocator};

    bool had_error = false;

    size_t inferred_arg_start = an_func_def_arg_start(inferred_funcdef);
    size_t inferred_arg_end = an_func_def_arg_end(inferred_funcdef);
    for (size_t i = inferred_arg_start; i < inferred_arg_end; ++i) {
        ast_node_t *decl = inferred_funcdef->children.items[i];
        
        size_t arg_call_start = an_call_arg_start(call);
        ast_node_t *call_arg = call->children.items[i-inferred_arg_start+arg_call_start];
        for (size_t t = 0; t < decl->type_decl_patterns.count; ++t)  {
            type_pattern_t pattern = decl->type_decl_patterns.items[t];

            resolve_expression(analyzer, analyzer->ast, state, typeid(TYPE_UNRESOLVED), call_arg);
            
            matched_value_t matched_value = stan_pattern_match_or_error(analyzer, decl, pattern.expected, call_arg->value_type, call_arg);
            array_push(&matched_values, matched_value);

            if (TYPE_IS_INVALID(matched_value.type)) {
                had_error = true;
                continue;
            }

            ast_node_t *implicit_type_decl = ast_implicit_expr(analyzer->ast, typeid(TYPE_TYPE), WORDT(matched_value.type), token_implicit_at_end(pattern.identifier));
            ast_node_t *implicit_init_expr = ast_implicit_expr(analyzer->ast, matched_value.type, matched_value.word, token_implicit_at_end(pattern.identifier));
            ast_node_t *implicit_constant_decl = ast_decldef(analyzer->ast, pattern.identifier, implicit_type_decl, implicit_init_expr);
            implicit_constant_decl->is_mutable = false;

            declare_definition(analyzer, new_state.scope, implicit_constant_decl);

            resolve_declaration_definition(analyzer, analyzer->ast, new_state, implicit_constant_decl);
        }
    }

    ast_node_t *result = NULL;
    unless (had_error) {
        result = find_realized_funcdef_or_null_by_inferred_types(inferred_funcdef->realized_funcdef_copies, matched_values);
        unless(result) {
            result = ast_node_copy(analyzer->ast, inferred_funcdef);

            {
                matched_values_t matched_values_ = {.allocator=analyzer->ast->arena};
                for (size_t i = 0; i < matched_values.count; ++i) array_push(&matched_values_, matched_values.items[i]);

                inferred_funcdef_copy_t copy = {
                    .key = matched_values_,
                    .funcdef = result,
                };

                array_push(&inferred_funcdef->realized_funcdef_copies, copy);
            }

            resolve_funcdef(analyzer, analyzer->ast, new_state, result);
        }
    }

    allocator_return(tmp);

    return result;
}

static void ast_copy_expr_val_to_memory(ast_t *ast, ast_node_t *expr, void *dest) {
    typedata_t *td = ast_type2td(ast, expr->value_type);

    switch (td->kind) {
    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_FLOAT: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: UNREACHABLE(); break;
            case NUM_SIZE_16: UNREACHABLE(); break;
            case NUM_SIZE_32: {
                f32 val = (f32)expr->expr_val.word.as.d;
                memcpy(dest, &val, sizeof(f32));
                break;
            }

            case NUM_SIZE_64: {
                f64 val = (f64)expr->expr_val.word.as.d;
                memcpy(dest, &val, sizeof(f64));
                break;
            }
            }
            break;
        }

        case NUM_TYPE_SIGNED:
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: {
                u8 val = (u8)expr->expr_val.word.as.u;
                memcpy(dest, &val, sizeof(u8));
                break;
            }

            case NUM_SIZE_16: {
                u16 val = (u16)expr->expr_val.word.as.u;
                memcpy(dest, &val, sizeof(u16));
                break;
            }

            case NUM_SIZE_32: {
                u32 val = (u32)expr->expr_val.word.as.u;
                memcpy(dest, &val, sizeof(u32));
                break;
            }

            case NUM_SIZE_64: {
                u64 val = (u64)expr->expr_val.word.as.u;
                memcpy(dest, &val, sizeof(u64));
                break;
            }
            }
            break;
        }
        }
        break;
    }

    case TYPE_BOOL: {
        u8 val = (u8)expr->expr_val.word.as.u;
        memcpy(dest, &val, sizeof(u8));
        break;
    }

    case TYPE_VOID: break;

    case TYPE_ARRAY:
    case TYPE_STRUCT: {
        if (td->size > WORD_SIZE) {
            void *source = expr->expr_val.word.as.p;
            memcpy(dest, source, td->size);
        } else {
            memcpy(dest, &expr->expr_val.word.as.u, td->size);
        }
        break;
    }

    case TYPE_POINTER:
    case TYPE_INTRINSIC_FUNCTION:
    case TYPE_FUNCTION:
    case TYPE_TYPE:
    case TYPE_INFERRED_FUNCTION: {
        memcpy(dest, &expr->expr_val.word.as.u, WORD_SIZE);
        break;
    }

    case TYPE_STRING: UNREACHABLE(); break; //todo

    case TYPE_COUNT:
    case TYPE_UNREACHABLE:
    case TYPE_UNRESOLVED:
    case TYPE_INVALID: UNREACHABLE(); break;
    }
}

void resolve_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        type_t implicit_type,
        ast_node_t *expr) {
    
    ASSERT(ast_node_type_is_expression(expr->node_type), "should be only expressions");

    if (TYPE_IS_RESOLVED(expr->value_type)) {
        return;
    }

    switch (expr->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: {
            ASSERT(sv_eq(expr->identifier.view, lit2sv("@run")), "only run is implemented right now");

            scope_t scope = {0};
            scope_init(&scope, analyzer->ast->arena, SCOPE_TYPE_FOLD_DIRECTIVE, state.scope, expr);
            analysis_state_t new_state = state;
            new_state.scope = &scope;

            array_push(&analyzer->pending_dependencies, expr);

            ast_node_t *child = expr->children.items[0];

            resolve_expression(analyzer, ast, new_state, implicit_type, child);

            --analyzer->pending_dependencies.count;

            if (!analyzer->had_error && analyzer->run_vm) {
                unless (TYPE_IS_INVALID(child->value_type)) {
                    typedata_t *td = ast_type2td(ast, child->value_type);
                    size_t size = b2w(td->size);
                    ASSERT(size == 1, "for now types can only be as large as a word");

                    word_t result[size];
                    bool success = stan_run(analyzer, analyzer->run_vm, child, result);
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
            resolve_expression(analyzer, ast, state, implicit_type, an_operand(expr));
            expr->lvalue_node = an_operand(expr)->lvalue_node;
            expr->value_type = an_operand(expr)->value_type;
            expr->expr_val = an_operand(expr)->expr_val;
            expr->is_free_number = an_operand(expr)->is_free_number;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: {
            ast_node_t *type_expr = an_array_type_expr(expr);
            resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), type_expr);

            if (!TYPE_IS_TYPE(type_expr->value_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_EXPECTED_TYPE,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("expected 'type' for array but got '$1.$'"),
                    .args = ORERR_ARGS(error_arg_node(type_expr), error_arg_type(type_expr->value_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
                break;
            }

            if (!type_expr->expr_val.is_concrete) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_EXPECTED_CONSTANT,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("expected array type to be a constant"),
                    .args = ORERR_ARGS(error_arg_node(type_expr)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
                break;
            }

            ast_node_t *size_expr = an_array_size_expr(expr);
            if (an_is_notnone(size_expr)) {
                resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), size_expr);

                if (TYPE_IS_INVALID(size_expr->value_type)) {
                    INVALIDATE(expr);
                    break;
                }

                typedata_t *td = ast_type2td(ast, size_expr->value_type);
                if (td->kind != TYPE_NUMBER) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_ARRAY_SIZE_MUST_BE_A_SIGNED_OR_UNSIGNED_INTEGER,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("array size must be signed or unsigned integer but got '$1.$'"),
                        .args = ORERR_ARGS(error_arg_node(size_expr), error_arg_type(size_expr->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }
                
                if (td->as.num == NUM_TYPE_FLOAT) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_ARRAY_SIZE_MUST_BE_A_SIGNED_OR_UNSIGNED_INTEGER,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("array size must be signed or unsigned integer but got '$1.$'"),
                        .args = ORERR_ARGS(error_arg_node(size_expr), error_arg_type(size_expr->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }
                
                if (!size_expr->expr_val.is_concrete) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_EXPECTED_CONSTANT,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("array size must constant"),
                        .args = ORERR_ARGS(error_arg_node(size_expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                if (td->as.num == NUM_TYPE_SIGNED && size_expr->expr_val.word.as.s <= 0) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_ARRAY_SIZE_MUST_BE_POSITIVE,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("array size must be a positve integer"),
                        .args = ORERR_ARGS(error_arg_node(size_expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                type_t array_value_type = type_expr->expr_val.word.as.t;
                size_t array_size = size_expr->expr_val.word.as.u;

                type_t array_type = type_set_fetch_array(&ast->type_set, array_value_type, array_size);

                expr->value_type = typeid(TYPE_TYPE);
                expr->expr_val = ast_node_val_word(WORDT(array_type));
            } else {
                type_t array_value_type = type_expr->expr_val.word.as.t;
                type_t array_type = type_set_fetch_array(&ast->type_set, array_value_type, 0);

                expr->value_type = typeid(TYPE_TYPE);
                expr->expr_val = ast_node_val_word(WORDT(array_type));
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
            ast_node_t *accessee = an_item_accessee(expr);
            resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), accessee);

            ast_node_t *accessor = an_item_accessor(expr);
            resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), accessor);

            if (TYPE_IS_INVALID(accessee->value_type)) {
                INVALIDATE(expr);
                break;
            }

            if (TYPE_IS_INVALID(accessor->value_type)) {
                INVALIDATE(expr);
                break;
            }

            typedata_t *accessee_td = ast_type2td(ast, accessee->value_type);
            if (an_is_notnone(accessee->lvalue_node) || accessee_td->kind == TYPE_POINTER) {
                expr->lvalue_node = expr;
            }

            type_t item_type = typeid(TYPE_INVALID);
            // accessee
            {
                if (accessee_td->kind != TYPE_ARRAY && accessee_td->kind != TYPE_POINTER) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_ACCESSEE_TYPE,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("invalid accessee type: '$1.$'"),
                        .args = ORERR_ARGS(error_arg_node(accessee), error_arg_type(accessee->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                switch (accessee_td->kind) {
                case TYPE_POINTER: {
                    type_t ptr_inner = accessee_td->as.ptr.type;
                    typedata_t *ptr_inner_td = ast_type2td(ast, ptr_inner);

                    if (ptr_inner_td->kind != TYPE_ARRAY) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_INVALID_ACCESSEE_TYPE,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("expected pointer to array type but got '$1.$'"),
                            .args = ORERR_ARGS(error_arg_node(accessee), error_arg_type(accessee->value_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                        break;
                    }

                    item_type = ptr_inner_td->as.arr.type;
                    break;
                }

                case TYPE_ARRAY: {
                    item_type = accessee_td->as.arr.type;
                    break;
                }

                default: UNREACHABLE(); break;
                }
            }

            // accessor
            {
                typedata_t *td = ast_type2td(ast, accessor->value_type);
                unless (td->kind == TYPE_NUMBER && td->as.num != NUM_TYPE_FLOAT) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_ACCESSOR_TYPE,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("expected signed or unsigned integer but got '$1.$'"),
                        .args = ORERR_ARGS(error_arg_node(accessor), error_arg_type(accessor->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }
            }

            expr->value_type = item_type;

            if (accessee->expr_val.is_concrete && accessor->expr_val.is_concrete) {
                // todo
                UNREACHABLE();
            }

            break;
        }

        case AST_NODE_TYPE_EXPRESSION_NIL: {
            expr->value_type = typeid(TYPE_UNRESOLVED);
            break;
        }
        
        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: {
            // inferred type decls are forward scanned and converted to value_defs, and so if they are present anywhere
            // else they are an error
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_INFERRED_TYPE_DECLS_ARE_ONLY_ALLOWED_FOR_FUNCTIONDEFS,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("inferred type declarations are only allowed inside function definition parameters"),
                .args = ORERR_ARGS(error_arg_node(expr)),
                .show_code_lines = ORERR_LINES(0),
            ));
            INVALIDATE(expr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST: {
            ast_node_t *type_expr = an_list_lhs(expr);

            if (an_is_none(type_expr)) {
                if (TYPE_IS_RESOLVED(implicit_type) && !TYPE_IS_INVALID(implicit_type)) {
                    type_expr = ast_implicit_expr(ast, typeid(TYPE_TYPE), WORDT(implicit_type), token_implicit_at_start(expr->start));
                    an_list_lhs(expr) = type_expr;
                } else {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INITIALIZER_LIST_TYPE_CANNOT_BE_INFERRED,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("no implicit type can be inferred for initializer list"),
                        .args = ORERR_ARGS(error_arg_node(expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }
            } else {
                resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), type_expr);
            }

            type_t arg_implicit_type = typeid(TYPE_UNRESOLVED);
            if (TYPE_IS_TYPE(type_expr->value_type) && type_expr->expr_val.is_concrete) {
                type_t t = type_expr->expr_val.word.as.t;
                typedata_t *td = ast_type2td(ast, t);
                switch (td->kind) {
                case TYPE_ARRAY: {
                    arg_implicit_type = td->as.arr.type;
                    break;
                }

                default: break;
                }
            }

            bool invalid_arg = false;
            for (size_t i = an_list_start(expr); i < an_list_end(expr); ++i) {
                ast_node_t *arg = expr->children.items[i];

                resolve_expression(analyzer, ast, state, arg_implicit_type, arg);

                if (TYPE_IS_INVALID(arg->value_type)) {
                    invalid_arg = true;
                }
            }

            if (invalid_arg) {
                break;
            }

            typedata_t *type_expr_td = ast_type2td(ast, type_expr->value_type);
            switch (type_expr_td->kind) {
            case TYPE_TYPE: {
                if (!type_expr->expr_val.is_concrete) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_EXPECTED_CONSTANT,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("expected constant for initializer list"),
                        .args = ORERR_ARGS(error_arg_node(type_expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                type_t init_type = type_expr->expr_val.word.as.t;
                typedata_t *init_type_td = ast_type2td(ast, init_type);

                switch (init_type_td->kind) {
                case TYPE_ARRAY: {
                    type_t array_type = init_type_td->as.arr.type;

                    bool is_constant = true;

                    for (size_t i = an_list_start(expr); i < an_list_end(expr); ++i) {
                        ast_node_t *arg = expr->children.items[i];

                        if (an_is_none(arg)) {
                            arg = ast_nil(ast, init_type, token_implicit_at_end(expr->start));
                            expr->children.items[i] = arg;
                        }

                        arg = cast_implicitly_if_necessary(ast, array_type, arg);
                        expr->children.items[i] = arg;

                        if (!arg->expr_val.is_concrete) {
                            is_constant = false;
                        }

                        if (!typeid_eq(arg->value_type, array_type)) {
                            size_t arg_index = i - an_list_start(expr);
                            stan_error(analyzer, OR_ERROR(
                                .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                                .level = ERROR_SOURCE_ANALYSIS,
                                .msg = lit2str("array expects '$1.$' type elements but item $3.$ is '$2.$'"),
                                .args = ORERR_ARGS(error_arg_node(arg),
                                    error_arg_type(array_type), error_arg_type(arg->value_type), error_arg_sz(arg_index+1)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                            invalid_arg = true;
                        }
                    }

                    if (invalid_arg) break;

                    size_t arg_count = an_list_end(expr) - an_list_start(expr);

                    if (init_type_td->as.arr.count < arg_count) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_TOO_MANY_ARGUMENTS_IN_LIST_INIT,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("array has length of $1.$ but is initialized with $2.$"),
                            .args = ORERR_ARGS(error_arg_node(expr),
                                error_arg_sz(init_type_td->as.arr.count), error_arg_sz(arg_count)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        break;
                    }

                    if (is_constant) {
                        word_t *start;
                        if (init_type_td->size > WORD_SIZE) {
                            start = ast_multiword_value(ast, b2w(init_type_td->size));
                            expr->expr_val = ast_node_val_word(WORDP(start));
                        } else {
                            expr->expr_val.is_concrete = true;
                            start = &expr->expr_val.word;
                        }

                        // constant fold  arguments
                        for (size_t i = an_list_start(expr); i < an_list_end(expr); ++i) {
                            size_t offset = i - an_list_start(expr);
                            ast_node_t *arg = expr->children.items[i];
                            ast_copy_expr_val_to_memory(ast, arg, ((void*)start)+(offset*init_type_td->as.arr.item_size));
                        }
                    }

                    expr->value_type = init_type;
                    break;
                }

                default: UNREACHABLE(); // todo
                }
                break;
            }

            default: {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_INVALID_EXPRESSION_FOR_LIST_INIT,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("invalid expression for initialization list"),
                    .args = ORERR_ARGS(error_arg_node(type_expr)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                break;
            }
            
            }

            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            INVALIDATE(expr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            ast_node_t *left = an_lhs(expr);
            resolve_expression(analyzer, ast, state, implicit_type, left);

            ast_node_t *right = an_rhs(expr);
            resolve_expression(analyzer, ast, state, implicit_type, right);

            an_lhs(expr) = (left = cast_implicitly_if_necessary(ast, right->value_type, left));
            an_rhs(expr) = (right = cast_implicitly_if_necessary(ast, left->value_type, right));


            if (TYPE_IS_INVALID(left->value_type) || TYPE_IS_INVALID(right->value_type)) {
                INVALIDATE(expr);
                break;
            }

            typedata_t *left_td = ast_type2td(ast, left->value_type);
            typedata_t *right_td = ast_type2td(ast, right->value_type);

            #define td_is_s_or_u_int(td) ((td)->as.num == NUM_TYPE_SIGNED || (td)->as.num == NUM_TYPE_UNSIGNED)

            if (right_td->kind == TYPE_POINTER && left_td->kind == TYPE_NUMBER && left->is_free_number && td_is_s_or_u_int(left_td)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_NUMBER_MUST_BE_ON_RHS_FOR_POINTER_ARITHMETIC,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("number must be on right-hand-side for pointer arithmetic"),
                    .args = ORERR_ARGS(error_arg_node(an_lhs(expr))),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
            }

            if (left_td->kind == TYPE_POINTER && right_td->kind == TYPE_NUMBER && right->is_free_number && td_is_s_or_u_int(right_td)) {
                an_rhs(expr) = (right = cast_implicitly_if_necessary(ast, ast->type_set.ptrdiff_t_, right));
            }

            #undef td_is_s_or_u_int

            if (operator_is_arithmetic(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_ARITHMETIC) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_ARITHMETIC_OPERAND_TYPES,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("invalid arithmetic type: '$1.$'"),
                        .args = ORERR_ARGS(error_arg_node(left), error_arg_type(left->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                }

                unless (right_td->capabilities&TYPE_CAP_ARITHMETIC) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_ARITHMETIC_OPERAND_TYPES,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("invalid arithmetic type: '$1.$'"),
                        .args = ORERR_ARGS(error_arg_node(right), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                if (left_td->kind == TYPE_POINTER && typeid_eq(ast->type_set.ptrdiff_t_, right->value_type)) {
                    if (expr->operator.type != TOKEN_PLUS && expr->operator.type != TOKEN_MINUS) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_ONLY_ADD_AND_SUB_ARE_VALID_IN_PTR_ARITHMEIC,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("only '-' and '+' are allowed in pointer arithmetic but got '$0.kind$'"),
                            .args = ORERR_ARGS(error_arg_token(expr->operator)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                    } else {
                        if (expr->operator.type == TOKEN_PLUS && right_td->kind == TYPE_POINTER) {
                            UNREACHABLE();
                        } else {
                            // if right is pointer then its a difference, so type is ptrdiff
                            // if its not a pointer, then it must be a ptrdiff, and the produces a ptr type
                            expr->value_type = right_td->kind == TYPE_POINTER ? ast->type_set.ptrdiff_t_ : left->value_type;
                        }
                    }

                } else {
                    unless (typeid_eq(left->value_type, right->value_type)) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("operation requires explicit cast for types '$1.$' and '$2.$'"),
                            .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(left->value_type), error_arg_type(right->value_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                        break;
                    }

                    if (right_td->kind == TYPE_POINTER && expr->operator.type != TOKEN_MINUS) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_CANNOT_ADD_POINTERS,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("cannot add pointer types"),
                            .args = ORERR_ARGS(error_arg_node(expr)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                    } else {
                        expr->value_type = left->value_type;
                        if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                            word_t word = constant_fold_bin_arithmetic(ast, expr->operator.type, left->value_type, left->expr_val.word, right->expr_val.word);
                            expr->expr_val = ast_node_val_word(word);
                            expr->is_free_number = left->is_free_number && right->is_free_number;
                        }
                    }
                }
            } else if (operator_is_equating(expr->operator.type)) {
                // everything is equatable but they need to be the same type
                unless (typeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("operation requires explicit cast for types '$1.$' and '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(left->value_type), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = typeid(TYPE_BOOL);
                
                // constant fold
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    word_t wordl = left->expr_val.word;
                    word_t wordr = right->expr_val.word;

                    typedata_t *td = ast_type2td(ast, left->value_type);
                    if (td->size > WORD_SIZE) {
                        // todo
                        UNREACHABLE();
                    } else {
                        u8 result = (u8)(memcmp(&wordl, &wordr, WORD_SIZE) == 0);
                        switch (expr->operator.type) {
                        case TOKEN_EQUAL_EQUAL: {
                            expr->expr_val = ast_node_val_word(WORDU(result));
                            break;
                        }

                        case TOKEN_BANG_EQUAL: {
                            expr->expr_val = ast_node_val_word(WORDU(result == 0));
                            break;
                        }

                        default: UNREACHABLE(); break;
                        }
                    }

                    expr->is_free_number = left->is_free_number && right->is_free_number;
                }
            } else if (operator_is_comparing(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_COMPARABLE) {
                    // may be impossible in the end
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_COMPARISON_OPERAND_TYPES,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$1.$' is not a valid type for comparison"),
                        .args = ORERR_ARGS(error_arg_node(left), error_arg_type(left->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                }

                unless (right_td->capabilities&TYPE_CAP_COMPARABLE) {
                    // may be impossible in the end
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_COMPARISON_OPERAND_TYPES,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$1.$' is not a valid type for comparison"),
                        .args = ORERR_ARGS(error_arg_node(right), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                unless (typeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("operation requires explicit cast for types '$1.$' and '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(left->value_type), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
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
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_LOGICAL_OPERAND_TYPES,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$1.$' is not a valid type for logical operations"),
                        .args = ORERR_ARGS(error_arg_node(left), error_arg_type(left->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                unless (right_td->capabilities&TYPE_CAP_LOGICAL) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_LOGICAL_OPERAND_TYPES,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$1.$' is not a valid type for logical operations"),
                        .args = ORERR_ARGS(error_arg_node(right), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
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
            } else if (expr->operator.type == TOKEN_LESS_LESS) {
                // should be in cast branch
                UNREACHABLE();
            } else {
                UNREACHABLE();
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            resolve_expression(analyzer, ast, state, implicit_type, an_operand(expr));

            if (TYPE_IS_INVALID(an_operand(expr)->value_type)) {
                INVALIDATE(expr);
                break;
            }

            if (expr->operator.type == TOKEN_AMPERSAND) {
                ast_node_t *op = an_operand(expr);

                if (TYPE_IS_TYPE(op->value_type) && op->expr_val.is_concrete) {
                    expr->value_type = typeid(TYPE_TYPE);
                    type_t ptr_type = type_set_fetch_pointer(&ast->type_set, op->expr_val.word.as.t);
                    expr->expr_val = ast_node_val_word(WORDT(ptr_type));
                } else {
                    switch (op->lvalue_node->node_type) {
                    case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
                        if (op->lvalue_node->ref_decl->is_mutable) {
                            type_t ptr_type = type_set_fetch_pointer(&ast->type_set, op->lvalue_node->value_type);
                            expr->value_type = ptr_type;
                        } else {
                            stan_error(analyzer, OR_ERROR(
                                .tag = ERROR_ANALYSIS_CANNOT_TAKE_ADDRESS_OF_CONSTANT,
                                .level = ERROR_SOURCE_ANALYSIS,
                                .msg = lit2str("cannot take the address of constant"),
                                .args = ORERR_ARGS(error_arg_node(expr)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                            INVALIDATE(expr);
                        }
                        break;
                    }

                    case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
                        type_t type = type_set_fetch_pointer(&ast->type_set, op->lvalue_node->value_type);
                        expr->value_type = type;
                        break;
                    }

                    default: {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_INVALID_OPERAND_FOR_ADDRESS_OPERATOR,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("only lvalues can have their address taken"),
                            .args = ORERR_ARGS(error_arg_node(an_operand(expr))),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                        break;
                    }
                    }
                }

            } else {
                typedata_t *operand_td = ast_type2td(ast, an_operand(expr)->value_type);

                switch (expr->operator.type) {
                case TOKEN_STAR: {
                    if (operand_td->kind != TYPE_POINTER) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_INVALID_DEREF_OPERAND,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("expected pointer for dereference but got '$1.$'"),
                            .args = ORERR_ARGS(error_arg_node(an_operand(expr)), error_arg_type(an_operand(expr)->value_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                        break;
                    }

                    if (TYPE_IS_VOID(operand_td->as.ptr.type)) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_CANNOT_DEREFERENCE_VOIDPTR,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("cannot dereference a void pointer"),
                            .args = ORERR_ARGS(error_arg_node(an_operand(expr))),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                        break;
                    }

                    expr->value_type = operand_td->as.ptr.type;
                    expr->lvalue_node = expr;
                    break;
                }

                case TOKEN_NOT: {
                    if (operand_td->kind != TYPE_BOOL) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_INVALID_NOT_OPERAND,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("expected a 'bool' for 'not' unary but got '$1.$' instead"),
                            .args = ORERR_ARGS(error_arg_node(an_operand(expr)), error_arg_type(an_operand(expr)->value_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
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
                    if ((operand_td->capabilities&TYPE_CAP_ARITHMETIC) == 0) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_INVALID_MINUS_OPERAND,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("the '-' unary expects a type with arithmetic capabilities"),
                            .args = ORERR_ARGS(error_arg_node(an_operand(expr))),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                        break;
                    }

                    if (operand_td->as.num == NUM_TYPE_UNSIGNED) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = ERROR_ANALYSIS_CANNOT_NEGATE_UNSIGNED_NUMBER,
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("an unsigned number cannot be negated"),
                            .args = ORERR_ARGS(error_arg_node(an_operand(expr))),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                    }

                    ast_node_t *operand = an_operand(expr);
                    expr->value_type = operand->value_type;

                    // constant fold
                    if (an_operand(expr)->expr_val.is_concrete) {
                        switch (operand_td->as.num) {
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

            ast_node_t *decl = get_defval_or_null_by_identifier_and_error(analyzer, ast, state, expr, &def_scope);

            if (decl == NULL) {
                INVALIDATE(expr);
                break;
            }

            expr->ref_decl = decl;

            expr->value_type = decl->value_type;

            if (!decl->is_mutable) {
                expr->expr_val = decl->expr_val;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            UNREACHABLE();
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            ast_node_t *lhs = an_lhs(expr);
            resolve_expression(analyzer, ast, state, implicit_type,lhs);
            ast_node_t *lvalue_node = lhs->lvalue_node;
            if (an_is_none(lvalue_node)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_EXPECTED_LVALUE,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("an lvalue is expected for the left-hand-side of an assignment"),
                    .args = ORERR_ARGS(error_arg_node(lhs)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
                break;
            } 

            ast_node_t *rhs = an_rhs(expr);
            resolve_expression(analyzer, ast, state, lvalue_node->value_type, rhs);

            switch (expr->operator.type) {
            case TOKEN_PLUS_EQUAL:
            case TOKEN_MINUS_EQUAL:
            case TOKEN_STAR_EQUAL:
            case TOKEN_SLASH_EQUAL:
            case TOKEN_PERCENT_EQUAL:
            case TOKEN_PERCENT_PERCENT_EQUAL: {
                typedata_t *td = ast_type2td(ast, lhs->value_type);
                if ((td->capabilities & TYPE_CAP_ARITHMETIC) == 0) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_EXPECTED_LVALUE,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("type '$1.$' does not have arithmetic capabilities"),
                        .args = ORERR_ARGS(error_arg_node(lhs), error_arg_type(lhs->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                }
                break;
            }

            case TOKEN_OR_EQUAL:
            case TOKEN_AND_EQUAL: {
                typedata_t *td = ast_type2td(ast, lhs->value_type);
                if ((td->capabilities & TYPE_CAP_LOGICAL) == 0) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_EXPECTED_LVALUE,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("type '$1.$' does not have logical capabilities"),
                        .args = ORERR_ARGS(error_arg_node(lhs), error_arg_type(lhs->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                }
                break;
            }

            case TOKEN_EQUAL: break;

            default: UNREACHABLE(); break;
            }

            if (TYPE_IS_INVALID(rhs->value_type)) {
                break;
            }

            rhs = cast_implicitly_if_necessary(ast, lhs->value_type, rhs);
            an_rhs(expr) = rhs;

            expr->value_type = lvalue_node->value_type;

            type_t rhs_type = rhs->value_type;
            unless (TYPE_IS_INVALID(rhs_type) || typeid_eq(lvalue_node->value_type, rhs_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("value is of type '$1.$' while definition is of type '$2.$'"),
                    .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(rhs_type), error_arg_type(lvalue_node->value_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            scope_t block_scope;
            scope_init(&block_scope, analyzer->ast->arena, SCOPE_TYPE_BLOCK, state.scope, expr);

            forward_scan_constant_names(analyzer, &block_scope, expr->children);

            analysis_state_t block_state = state;
            block_state.scope = &block_scope;
            resolve_declarations(analyzer, ast, block_state, expr->children);

            size_t last_unreachable_index = expr->children.count;
            for (size_t i = 0; i < expr->children.count; ++i) {
                ast_node_t *decl = expr->children.items[i];
                if (TYPE_IS_UNREACHABLE(decl->value_type)) {
                    last_unreachable_index = i;
                    break;
                }
            }

            bool is_unreachable = last_unreachable_index < expr->children.count;
            // discard everything past an unreachable type
            if (is_unreachable) {
                expr->children.count = last_unreachable_index+1;

                // todo: add a warning for this
            }

            ast_node_t *last_decl = expr->children.items[expr->children.count-1];


            if (!is_unreachable && last_decl->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_BLOCKS_MUST_BE_EMPTY_OR_END_IN_STATEMENT,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("block is expected to be empty or end in a statement"),
                    .args = ORERR_ARGS(error_arg_token(expr->end), error_arg_node(expr->children.items[expr->children.count-1])),
                    .show_code_lines = ORERR_LINES(0, 1),
                ));
                INVALIDATE(expr);
                break;
            }

            expr->last_statement = last_decl;
            expr->value_type = expr->last_statement->value_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            scope_t branch_scope = {0};
            analysis_state_t branch_state = state;
            scope_init(&branch_scope, analyzer->ast->arena, SCOPE_TYPE_JMPABLE, state.scope, expr);
            branch_state.scope = &branch_scope;

            if (an_is_notnone(an_for_decl(expr))) {
                resolve_declaration(analyzer, ast, branch_state, an_for_decl(expr));
            }

            {
                scope_t condition_scope = {0};
                scope_init(&condition_scope, ast->arena, SCOPE_TYPE_CONDITION, branch_state.scope, an_condition(expr));

                analysis_state_t cond_state = state;
                cond_state.scope = &condition_scope;

                resolve_expression(analyzer, ast, cond_state, typeid(TYPE_UNRESOLVED), an_condition(expr));
            }

            if (an_is_notnone(an_for_incr(expr))) {
                resolve_expression(analyzer, ast, branch_state, typeid(TYPE_UNRESOLVED), an_for_incr(expr));
            }

            resolve_expression(analyzer, ast, branch_state, implicit_type, an_then(expr));

            resolve_expression(analyzer, ast, branch_state, implicit_type, an_else(expr));

            type_t branch_type = resolve_block_return_types_or_error(analyzer, expr);
            expr->value_type = branch_type;

            if (TYPE_IS_INVALID(branch_type)) {
                INVALIDATE(expr);
            }

            if (TYPE_IS_INVALID(an_then(expr)->value_type) || TYPE_IS_INVALID(an_else(expr)->value_type)) {
                INVALIDATE(expr);
            }

            if (!TYPE_IS_INVALID(an_condition(expr)->value_type) && !typeid_eq(an_condition(expr)->value_type, typeid(TYPE_BOOL))) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_BLOCKS_TYPE_MISMATCH,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("'$0.kind$' condition must be a 'bool' but got '$1.$'"),
                    .args = ORERR_ARGS(error_arg_token(expr->start), error_arg_type(an_condition(expr)->value_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }

            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            switch (expr->start.type) {
                case TOKEN_RETURN: {
                    scope_t *func_def_scope = NULL;
                    bool success = get_nearest_jmp_scope_in_func_or_error(analyzer, expr, state.scope, SCOPE_TYPE_FUNCDEF, lit2sv(""), &func_def_scope);

                    type_t implicit_type = typeid(TYPE_UNRESOLVED);
                    unless (success) {
                        INVALIDATE(expr);
                    } else {
                        expr->jmp_out_scope_node = func_def_scope->creator;
                        array_push(&func_def_scope->creator->jmp_nodes, expr);

                        typedata_t *sig = ast_type2td(ast, func_def_scope->creator->value_type);
                        implicit_type = sig->as.function.return_type;
                    }

                    resolve_expression(analyzer, ast, state, implicit_type, an_expression(expr));
                    break;
                }

                case TOKEN_CONTINUE:
                case TOKEN_BREAK: {
                    resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), an_expression(expr));

                    scope_t *found_scope = NULL;
                    bool success = get_nearest_jmp_scope_in_func_or_error(analyzer, expr, state.scope, SCOPE_TYPE_JMPABLE, expr->identifier.view, &found_scope);
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
            ast_node_t *callee = an_callee(expr);
            resolve_expression(analyzer, ast, state, implicit_type, callee);

            type_t callee_type = callee->value_type;
            typedata_t *callee_td = ast_type2td(ast, callee_type);

            if (callee_td->kind == TYPE_INFERRED_FUNCTION) {
                // im not sure if this is possible
                unless (callee->expr_val.is_concrete) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_EXPECTED_CONSTANT,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("callee is an inferred function declaration and must be a constant"),
                        .args = ORERR_ARGS(error_arg_node(callee)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                ast_node_t *inferred_funcdef = (ast_node_t*)callee->expr_val.word.as.p;

                ast_node_t *realized_funcdef = stan_realize_inferred_funcdef_or_error_and_null(analyzer, state, expr, inferred_funcdef);
                {
                    function_t *function = (function_t*)realized_funcdef->expr_val.word.as.p;
                    size_t dep_start = stan_function_is_building(analyzer, function);
                    if (dep_start < analyzer->pending_dependencies.count) {
                        stan_circular_dependency_error(analyzer, ast, callee, dep_start);
                    }
                }

                unless (realized_funcdef) {
                    INVALIDATE(expr);
                    break;
                }

                if (TYPE_IS_INVALID(realized_funcdef->value_type)) {
                    INVALIDATE(expr);
                    break;
                }

                callee = ast_implicit_expr(ast, realized_funcdef->value_type, realized_funcdef->expr_val.word, callee->start);
                callee_type = callee->value_type;
                callee_td = ast_type2td(ast, callee_type);
                an_callee(expr) = callee;
            }

            if ((!type_is_function(ast->type_set.types, callee_type) && !type_is_intrinsic_function(ast->type_set.types, callee_type))) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_EXPECTED_CALLABLE,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("expected a callable type but got '$0.type$' instead"),
                    .args = ORERR_ARGS(error_arg_node(callee)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                break;
            }

            bool argument_invalid = false;
            for (size_t i = an_call_arg_start(expr); i < an_call_arg_end(expr); ++i) {
                ast_node_t *argument = expr->children.items[i];

                size_t argi = i - an_call_arg_start(expr);
                type_t arg_implicit_type = callee_td->as.function.argument_types.items[argi];

                resolve_expression(analyzer, ast, state, arg_implicit_type, argument);
                if (TYPE_IS_INVALID(argument->value_type)) {
                    argument_invalid = true;
                }
            }

            if (argument_invalid || TYPE_IS_INVALID(callee->value_type)) {
                INVALIDATE(expr);
                break;
            }

            for (size_t i = an_call_arg_start(expr); i < an_call_arg_end(expr); ++i) {
                ast_node_t *arg = expr->children.items[i];
                if (TYPE_IS_INVALID(arg->value_type)) continue;
                size_t i_= i - an_call_arg_start(expr);

                arg = cast_implicitly_if_necessary(ast, callee_td->as.function.argument_types.items[i_], arg);
                expr->children.items[i] = arg;
            }

            bool success = check_call_on_func(analyzer, ast, expr);
            if (!success) {
                INVALIDATE(expr);
                break;
            }

            expr->value_type = callee_td->as.function.return_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            resolve_funcdef(analyzer, ast, state, expr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            UNREACHABLE();
            break;
        }
        
         case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            bool invalid_parameter = false;
            for (size_t i = an_func_def_arg_start(expr); i < an_func_def_arg_end(expr); ++i) {
                ast_node_t *parameter = expr->children.items[i];
                resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), parameter);

                if (TYPE_IS_INVALID(parameter->value_type)) {
                    invalid_parameter = true;
                }
            }

            resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), an_func_def_return(expr));

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
                expr->value_type = typeid(TYPE_TYPE);

                unless (count == 1) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_TYPEOF_REQUIRES_ONE_ARG,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'typeof' builtin requires exactly 1 argument but got $1.$ instead"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_sz(count)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                ast_node_t *expr_arg = expr->children.items[arg_start];
                resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), expr_arg);

                if (TYPE_IS_INVALID(expr_arg->value_type)) {
                    break;
                }

                expr->expr_val = ast_node_val_word(WORDT(expr_arg->value_type));
                break;
            }
            
            case TOKEN_SIZEOF: {
                expr->value_type = ast->type_set.size_t_;

                unless (count == 1) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_SIZEOF_REQUIRES_ONE_ARG,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'sizeof' builtin requires exactly 1 argument but got $1.$ instead"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_sz(count)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                ast_node_t *expr_arg = expr->children.items[arg_start];
                resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), expr_arg);
                type_t expr_type = expr_arg->value_type;

                if (TYPE_IS_TYPE(expr_type) && expr_arg->expr_val.is_concrete) {
                    expr_arg->value_type = typeid(TYPE_TYPE);
                    expr_arg->expr_val = ast_node_val_word(WORDT(expr_arg->expr_val.word.as.t));
                } else {
                    expr_arg->value_type = typeid(TYPE_TYPE);
                    expr_arg->expr_val = ast_node_val_word(WORDT(expr_type));
                }
                break;
            }

            case TOKEN_LEN: {
                expr->value_type = ast->type_set.size_t_;
                expr->is_free_number = true;

                unless (count == 1) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_LEN_REQUIRES_ONE_ARRAY_ARG,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'len' builtin requires exactly 1 argument but got $1.$ instead"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_sz(count)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                ast_node_t *arg = expr->children.items[arg_start];
                resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), arg);

                type_t arg_type = arg->value_type;
                typedata_t *argtd = ast_type2td(ast, arg_type);

                if (argtd->kind != TYPE_ARRAY) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_LEN_REQUIRES_ONE_ARRAY_ARG,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'len' builtin takes 1 array argument but got '$1.$' instead"),
                        .args = ORERR_ARGS(error_arg_node(arg), error_arg_type(arg->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                typedata_t *array_td = argtd;
                expr->expr_val = ast_node_val_word(WORDU(array_td->as.arr.count));
                break;
            }

            default: UNREACHABLE(); break;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            ast_node_t *cast_expr = an_cast_expr(expr);
            resolve_expression(analyzer, ast, state, implicit_type, cast_expr);
            if (TYPE_IS_INVALID(cast_expr->value_type)) {
                INVALIDATE(expr);
                break;
            }

            ast_node_t *type_expr = an_cast_type(expr);
            resolve_expression(analyzer, ast, state, implicit_type, type_expr);
            if (TYPE_IS_INVALID(type_expr->value_type)) {
                INVALIDATE(expr);
                break;
            }

            typedata_t *typetd = ast_type2td(ast, type_expr->value_type);
            if (typetd->kind == TYPE_TYPE && type_expr->expr_val.is_concrete) {
                if (stan_can_cast(&ast->type_set.types, type_expr->expr_val.word.as.t, cast_expr->value_type)) {
                    expr->value_type = type_expr->expr_val.word.as.t;

                    if (cast_expr->expr_val.is_concrete) {
                        word_t result = constant_fold_cast(ast, cast_expr->expr_val.word, expr->value_type, cast_expr->value_type);
                        expr->expr_val = ast_node_val_word(result);
                        expr->is_free_number = cast_expr->is_free_number;
                    }
                } else {
                    stan_error(analyzer, OR_ERROR(
                        .tag = ERROR_ANALYSIS_INVALID_CAST,
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot cast type '$1.$' to type '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(cast_expr->value_type), error_arg_type(type_expr->expr_val.word.as.t)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                }

            } else {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_EXPECTED_CONSTANT,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("type expression for cast must be a type constant"),
                    .args = ORERR_ARGS(error_arg_node(type_expr)),
                    .show_code_lines = ORERR_LINES(0),
                ));
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
        ast_node_t *previous_decl = (ast_node_t*)def_word.as.p;
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_CANNOT_OVERLOAD_DEFINITION,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("cannot have declarations with the same identifier '$0.$'"),
            .args = ORERR_ARGS(error_arg_token(definition->identifier), error_arg_node(previous_decl)),
            .show_code_lines = ORERR_LINES(0, 1),
        ));
        return;
    }

    add_definition(scope, analyzer->ast->arena, string2sv(identifier), definition);

    allocator_return(tmp);
}

static type_path_t *new_type_path(match_type_t kind, type_path_t *next, arena_t *arena) {
    type_path_t *path = arena_alloc(arena, sizeof(type_path_t));
    *path = zer0(type_path_t);
    path->kind = kind;
    path->next = next;
    return path;
}

static void forward_scan_inferred_types(ast_node_t *decl, ast_node_t *decl_type, arena_t *arena, type_patterns_t *patterns) {
    switch (decl_type->node_type) {
        case AST_NODE_TYPE_NONE:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case AST_NODE_TYPE_DECLARATION_DEFINITION: UNREACHABLE(); break;

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS:
        case AST_NODE_TYPE_EXPRESSION_CAST:
        case AST_NODE_TYPE_EXPRESSION_BINARY:
        case AST_NODE_TYPE_EXPRESSION_DOT:
        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL:
        case AST_NODE_TYPE_EXPRESSION_CALL:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY:
        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE:
        case AST_NODE_TYPE_EXPRESSION_BLOCK:
        case AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_NIL:
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST:
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE:
        case AST_NODE_TYPE_EXPRESSION_JMP: break;

        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: {

            decl_type->node_type = AST_NODE_TYPE_EXPRESSION_DEF_VALUE;
            decl_type->value_type = typeid(TYPE_UNRESOLVED);
            type_path_t *path = new_type_path(MATCH_TYPE_IDENTIFIER, NULL, arena);

            type_pattern_t pattern = {
                .identifier = decl_type->identifier,
                .expected = path,
            };
            array_push(patterns, pattern);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            unless (decl_type->operator.type == TOKEN_AMPERSAND) {
                break;
            }

            forward_scan_inferred_types(decl, an_expression(decl_type), arena, patterns);

            for (size_t i = 0; i < patterns->count; ++i) {
                type_path_t *current = patterns->items[i].expected;
                type_path_t *path = new_type_path(MATCH_TYPE_POINTER, current, arena);
                patterns->items[i].expected = path;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: {
            tmp_arena_t *tmp = allocator_borrow();

            {
                type_patterns_t array_type_patterns = {.allocator=tmp->allocator};
                forward_scan_inferred_types(decl, an_array_type_expr(decl_type), arena, &array_type_patterns);
                for (size_t i = 0; i < array_type_patterns.count; ++i) {
                    type_path_t *current = array_type_patterns.items[i].expected;
                    type_path_t *path = new_type_path(MATCH_TYPE_ARRAY_TYPE, current, arena);
                    array_type_patterns.items[i].expected = path;

                    array_push(patterns, array_type_patterns.items[i]);
                }
            }

            {
                type_patterns_t array_size_patterns = {.allocator=tmp->allocator};
                if (an_is_notnone(an_array_size_expr(decl_type))) {
                    forward_scan_inferred_types(decl, an_array_size_expr(decl_type), arena, &array_size_patterns);
                }

                for (size_t i = 0; i < array_size_patterns.count; ++i) {
                    type_path_t *current = array_size_patterns.items[i].expected;
                    type_path_t *path = new_type_path(MATCH_TYPE_ARRAY_SIZE, current, arena);
                    array_size_patterns.items[i].expected = path;

                    array_push(patterns, array_size_patterns.items[i]);
                }
            }

            allocator_return(tmp);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            UNREACHABLE(); // todo
            break;
        }


        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            forward_scan_inferred_types(decl, an_expression(decl_type), arena, patterns);
            break;
        }
    }
}

static void resolve_declaration_definition(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *decl) {
    array_push(&analyzer->pending_dependencies, decl);

    ast_node_t *decl_type = an_decl_type(decl);
    if (TYPE_IS_UNRESOLVED(decl->value_type)) {
        analysis_state_t new_state = state;
        scope_t type_context = {0};
        scope_init(&type_context, ast->arena, SCOPE_TYPE_TYPE_CONTEXT, state.scope, decl_type);
        new_state.scope = &type_context;

        resolve_expression(analyzer, ast, new_state, typeid(TYPE_UNRESOLVED), decl_type);

        unless (TYPE_IS_TYPE(decl_type->value_type)) {
            if (TYPE_IS_UNRESOLVED(decl_type->value_type)) {
                decl->value_type = typeid(TYPE_UNRESOLVED);
            } else {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_EXPECTED_TYPE,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("expected type expression for declaration"),
                    .args = ORERR_ARGS(error_arg_node(decl_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(decl);
            }
        } else if (TYPE_IS_RESOLVED(decl_type->value_type)) {
            decl->value_type = decl_type->expr_val.word.as.t;
        } 
    }

    ast_node_t *init_expr = an_decl_expr(decl);
    if (TYPE_IS_UNRESOLVED(an_decl_expr(decl)->value_type) && decl->type_decl_patterns.count == 0) {
        resolve_expression(analyzer, ast, state, decl->value_type, init_expr);
    }

    --analyzer->pending_dependencies.count;

    if (TYPE_IS_INVALID(decl->value_type)) {
        return;
    }

    if (TYPE_IS_UNRESOLVED(init_expr->value_type) && TYPE_IS_UNRESOLVED(decl->value_type)) {
        ASSERT(false, "don't thing this is possible??");
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_CANNOT_INFER_NIL_VALUE,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("cannot infer nil value"),
            .args = ORERR_ARGS(error_arg_node(decl)),
            .show_code_lines = ORERR_LINES(0),
        ));
        INVALIDATE(decl);
        INVALIDATE(init_expr);
        INVALIDATE(decl_type);
    } else if (TYPE_IS_RESOLVED(init_expr->value_type) && TYPE_IS_UNRESOLVED(decl->value_type)) {
        decl_type->expr_val.word.as.t = init_expr->value_type;
        decl->value_type = init_expr->value_type;
    } else if (TYPE_IS_UNRESOLVED(init_expr->value_type) && TYPE_IS_RESOLVED(decl->value_type)) {
        init_expr->value_type = decl_type->expr_val.word.as.t;
        init_expr->expr_val = zero_value(ast, init_expr->value_type);
    
        decl->value_type = init_expr->value_type;
    }

    if (TYPE_IS_VOID(decl->value_type)) {
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_CANNOT_STORE_VOID_EXPRESSIONS,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("cannot store void expressions"),
            .args = ORERR_ARGS(error_arg_node(decl_type)),
            .show_code_lines = ORERR_LINES(0),
        ));
        INVALIDATE(decl);
    }

    ASSERT(TYPE_IS_RESOLVED(decl_type->value_type), "must be resolved at this point");

    {
        ast_node_t *decl_expr = an_decl_expr(decl);
        type_t declared_type = decl->value_type;

        ast_node_t *casted_expr = cast_implicitly_if_necessary(ast, declared_type, decl_expr);
        unless (typeid_eq(declared_type, casted_expr->value_type)) {
            unless (TYPE_IS_INVALID(decl_expr->value_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_TYPE_MISMATCH,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("declaration '$1.$' is delcared as a '$2.$' but got '$3.$' as its initial expression type"),
                    .args = ORERR_ARGS(error_arg_node(decl),
                        error_arg_token(decl->identifier), error_arg_type(decl->value_type), error_arg_type(init_expr->value_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
        }

        an_decl_expr(decl) = casted_expr;
    }

    if (an_decl_expr(decl)->expr_val.is_concrete) {
        decl->expr_val = an_decl_expr(decl)->expr_val;
    }
    
    if (decl->is_mutable) {
        declare_definition(analyzer, state.scope, decl);
    }

    // bind it to intrinsic if intrinsic
    {
        if (decl->is_intrinsic) {
            unless (an_is_constant(decl)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_INTRINSIC_MUST_BE_CONSTANT,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("intrinsics must be constant"),
                    .args = ORERR_ARGS(error_arg_node(decl)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(decl);
                return;
            }

            ASSERT(decl->expr_val.is_concrete, "should be there if its a constant");

            unless (TYPE_IS_TYPE(decl->value_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_INTRINSIC_DECLARATIONS_CAN_ONLY_BE_TYPES,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("intrinsic declarations can only be types but got '$1.$'"),
                    .args = ORERR_ARGS(error_arg_node(decl), error_arg_type(decl->value_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(decl);
                return;
            }

            type_t type = decl->expr_val.word.as.t;
            typedata_t *td = ast_type2td(ast, type);

            if (td->kind != TYPE_FUNCTION) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_INTRINSIC_DECLARATIONS_CAN_ONLY_BE_TYPES,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("only intrinsic functions are supported but got '$1.$'"),
                    .args = ORERR_ARGS(error_arg_node(decl), error_arg_type(type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(decl);
                return;
            }

            tmp_arena_t *tmp = allocator_borrow();
            word_t result;
            if (table_get(s2w, ast->intrinsic_fns, sv2string(decl->identifier.view, tmp->allocator), &result)) {
                intrinsic_fn_t func = (intrinsic_fn_t)result.as.p;
                decl->expr_val = ast_node_val_word(WORDP(func));
                decl->value_type = type_set_fetch_intrinsic_function(&ast->type_set, type);
            } else {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_INTRINSIC_DECLARATIONS_CAN_ONLY_BE_TYPES,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("intrinsic name '$1.$' does not exist"),
                    .args = ORERR_ARGS(error_arg_node(decl), error_arg_token(decl->identifier)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(decl);
            }

            allocator_return(tmp);
        } else if (TYPE_IS_INFERRED_FUNCTION(decl->value_type)) {
            if (decl->is_mutable) {
                stan_error(analyzer, OR_ERROR(
                    .tag = ERROR_ANALYSIS_INFERRED_FUNCDEF_CANNOT_BE_SET_TO_MUTABLE_VAR,
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("an inferred function definition cannot be set to an immutable variable"),
                    .args = ORERR_ARGS(error_arg_node(decl)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(decl);
            }
        }
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

static ast_node_t *get_module_or_null(ast_t *ast, string_t moduleid) {
    ast_node_t *module;
    if (table_get(s2n, ast->moduleid2node, moduleid, &module)) {
        return module;
    }

    return NULL;
}

static ast_node_t *get_defval_or_null_by_identifier_and_error(
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
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_DEFINITION_DOES_NOT_EXIST_IN_THE_SAME_RUN_SCOPE,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("declaration for '$2.$' does not exist in the same run scope it's being used in"),
                .args = ORERR_ARGS(error_arg_node(def), error_arg_node(decl),
                    error_arg_token(decl->identifier)),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return NULL;
        }

        if (passed_local_mutable_access_barrier && decl->is_mutable) {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_DEFINITION_DOES_NOT_EXIST_IN_THE_SAME_RUN_SCOPE,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("declaration for '$2.$' does not exist in the same run scope it's being used in"),
                .args = ORERR_ARGS(error_arg_node(def), error_arg_node(decl),
                    error_arg_token(decl->identifier)),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return NULL;
        }
        
        break;
    }
    
    // check core module
    unless (decl) {
        ast_node_t *module = get_module_or_null(ast, str(CORE_MODULE_NAME));
        ASSERT(module, "core should be there");

        for (size_t i = 0; i < module->children.count; ++i) {
            ast_node_t *module_decl = module->children.items[i];
            if (sv_eq(def->identifier.view, module_decl->identifier.view)) {
                decl = module_decl;
                break;
            }
        }
    }

    unless (decl) {
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_DEFINITION_DOES_NOT_EXIST,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("reference to undefined declaration '$1.$'"),
            .args = ORERR_ARGS(error_arg_node(def), error_arg_token(def->identifier)),
            .show_code_lines = ORERR_LINES(0),
        ));
        return NULL;
    }

    MUST(decl->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION);

    if (TYPE_IS_UNRESOLVED(decl->value_type)) {
        analysis_state_t new_state = state;
        new_state.scope = *search_scope;
        size_t dep_start = stan_declaration_is_recursive(analyzer, decl);
        if (dep_start < analyzer->pending_dependencies.count) {
            stan_circular_dependency_error(analyzer, ast, def, dep_start);
            return NULL;
        }

        resolve_declaration_definition(analyzer, ast, new_state, decl);
    }

    #undef NEXT_SCOPE

    typedata_t *td = ast_type2td(ast, decl->value_type);
    switch (td->kind) {
    case TYPE_FUNCTION: {
        ASSERT(decl->expr_val.is_concrete, "should be constant and thus should be concrete");

        function_t *function = (function_t*)decl->expr_val.word.as.p;
        size_t dep_start = stan_function_is_building(analyzer, function);
        if (dep_start < analyzer->pending_dependencies.count) {
            stan_circular_dependency_error(analyzer, ast, def, dep_start);
        }
        break;
    }

    default:  {
        break;
    }
    }

    return decl;
}

static void resolve_funcdef(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *funcdef) {
    ASSERT(funcdef->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, "must be function declaration at this point");

    scope_t funcdef_scope;
    scope_init(&funcdef_scope, ast->arena, SCOPE_TYPE_FUNCDEF, state.scope, funcdef);
    
    tmp_arena_t *tmp = allocator_borrow();
    types_t parameter_types = {.allocator=tmp->allocator};

    // forward scan paramters
    {
        tmp_arena_t *tmp = allocator_borrow();

        ast_nodes_t parameters = {.allocator=tmp->allocator};
        for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
            array_push(&parameters, funcdef->children.items[i]);
        }

        forward_scan_constant_names(analyzer, &funcdef_scope, parameters);

        allocator_return(tmp);
    }

    // forward scan inferred types declarations
    bool is_inferred_function = false;
    {
        for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
            ast_node_t *decl = funcdef->children.items[i];
            ast_node_t *decl_type = an_decl_type(decl);
            type_patterns_t patterns = {.allocator=ast->arena};
            forward_scan_inferred_types(decl, decl_type, ast->arena, &patterns);

            decl->type_decl_patterns.count = 0;
            if (patterns.count > 0) {
                decl->type_decl_patterns = patterns;
                is_inferred_function = true;
            }
        }
    }

    // dip out if it's inferred function, we'll resolve this when its called...
    if (is_inferred_function) {
        type_t function_type = typeid(TYPE_INFERRED_FUNCTION);
        funcdef->value_type = function_type;
        funcdef->expr_val = ast_node_val_word(WORDP(funcdef));
        goto defer;
    }

    bool parameter_invalid = false;

    for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
        analysis_state_t new_state = state;
        new_state.scope = &funcdef_scope;

        ast_node_t *param = funcdef->children.items[i];
        resolve_declaration_definition(analyzer, ast, new_state, param);
        array_push(&parameter_types, param->value_type);
        
        if (TYPE_IS_INVALID(param->value_type)) {
            parameter_invalid = true;
        }
    }

    type_t return_type = typeid(TYPE_VOID);
    {
        resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), an_func_def_return(funcdef));
    }

    ast_node_t *ret_expr = an_func_def_return(funcdef);
    if (ret_expr->is_mutable) {
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_INVALID_RETURN_TYPE,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("return expression must be a constant expression"),
            .args = ORERR_ARGS(error_arg_node(ret_expr)),
            .show_code_lines = ORERR_LINES(0),
        ));
    } else unless (TYPE_IS_TYPE(ret_expr->value_type)) {
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_INVALID_RETURN_TYPE,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("return expression must be a type but got '$1.$' instead"),
            .args = ORERR_ARGS(error_arg_node(ret_expr), error_arg_type(an_func_def_return(funcdef)->value_type)),
            .show_code_lines = ORERR_LINES(0),
        ));
    } else {
        return_type = an_func_def_return(funcdef)->expr_val.word.as.t;
    }

    if (parameter_invalid || TYPE_IS_INVALID(return_type)) {
        INVALIDATE(funcdef);
        goto defer;
    }

    type_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);
    funcdef->value_type = function_type;

    // create empty placeholder function immeidately in case definition is recursive
    function_t *function = NULL;
    unless (TYPE_IS_INVALID(an_func_def_block(funcdef)->value_type)) {
        if (analyzer->run_vm) {
            function = new_function(analyzer->run_vm->program_mem, analyzer->run_vm->arena);
        } else {
            UNREACHABLE();
        }

        funcdef->expr_val = ast_node_val_word(WORDP(function));
    }

    array_push(&analyzer->pending_dependencies, funcdef);

    {
        analysis_state_t new_state = state;

        scope_t function_scope;
        scope_init(&function_scope, ast->arena, SCOPE_TYPE_FUNC_DEF_BODY, &funcdef_scope, funcdef);
        new_state.scope = &function_scope;
        resolve_expression(analyzer, ast, new_state, typeid(TYPE_UNRESOLVED), an_func_def_block(funcdef));
    }

    ast_node_t *funcblock = an_func_def_block(funcdef);
    unless (TYPE_IS_UNREACHABLE(funcblock->value_type) || TYPE_IS_VOID(return_type)) {
        stan_error(analyzer, OR_ERROR(
            .tag = ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES,
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("function does not return on all branches"),
            .args = ORERR_ARGS(error_arg_node(funcdef)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    for (size_t i = 0; i < funcdef->jmp_nodes.count; ++i) {
        ast_node_t *jmp = funcdef->jmp_nodes.items[i];
        
        ast_node_t *ret_expr = an_expression(jmp);

        type_t ret_expr_type = ret_expr->value_type;
        unless (TYPE_IS_INVALID(ret_expr_type) || typeid_eq(ret_expr_type, return_type)) {
            stan_error(analyzer, OR_ERROR(
                .tag = ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES,
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("return value type '$1.$' does not match function return type '$2.$'"),
                .args = ORERR_ARGS(error_arg_node(ret_expr),
                    error_arg_type(ret_expr_type), error_arg_type(return_type)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    --analyzer->pending_dependencies.count;

    if (analyzer->had_error) {
        goto defer;
    }

    unless (analyzer->run_vm) {
        goto defer;
    }

    gen_funcdef(ast, analyzer->run_vm, funcdef);

defer:
    allocator_return(tmp);
}

static void resolve_declaration_statement(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *statement) {
    resolve_expression(analyzer, ast, state, typeid(TYPE_UNRESOLVED), an_expression(statement));
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

void resolve_declarations(analyzer_t* analyzer, ast_t* ast, analysis_state_t state, ast_nodes_t declarations) {
    for (size_t i = 0; i < declarations.count; i++) {
        ast_node_t* declaration = declarations.items[i];
        if (declaration->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION && an_is_constant(declaration)) {
            continue;
        }

        resolve_declaration(analyzer, ast, state, declarations.items[i]);
    }

    for (size_t i = 0; i < declarations.count; i++) {
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

static void analyzer_init(analyzer_t *analyzer, ast_t *ast, arena_t *arena) {
    *analyzer = zer0(analyzer_t);

    analyzer->had_error = false;
    analyzer->run_vm = ast->vm; //vm_default(arena);
    analyzer->ast = ast;

    analyzer->arena = arena;
    analyzer->placeholder = zer0(function_t);
    analyzer->pending_dependencies.allocator = arena;
}

bool resolve_ast(ast_t *ast) {
    analyzer_t analyzer = {0};
    // tmp_arena_t *tmp = allocator_borrow();

    // analyzer_init(&analyzer, ast, tmp->allocator);
    analyzer_init(&analyzer, ast, ast->arena);

    ast_node_t *module;
    kh_foreach_value(ast->moduleid2node, module, {
        scope_t global_scope = {0};
        scope_init(&global_scope, ast->arena, SCOPE_TYPE_MODULE, NULL, module);
        analysis_state_t state = (analysis_state_t) {
            .scope = &global_scope
        };

        forward_scan_constant_names(&analyzer, &global_scope, module->children);
        resolve_declarations(&analyzer, ast, state, module->children);
    });

    ast->resolved = !analyzer.had_error;

    // allocator_return(tmp);
    return ast->resolved;
}

function_t *find_main_or_null(ast_t *ast) {
    string_t moduleid;
    ast_node_t *module;
    kh_foreach(ast->moduleid2node, moduleid, module, ({
        for (size_t i = 0; i < module->children.count; ++i) {
            ast_node_t *decl = module->children.items[i];
            if (sv_eq(decl->identifier.view, lit2sv("main"))) {
                return decl->expr_val.word.as.p;
            }
        }
    }));

    return NULL;
}
