#include "static_analyzer.h"

#include <stdio.h>

#include "type_set.h"
#include "error.h"
#include "vm.h"
#include "codegen.h"
#include "tmp.h"
#include "../nob.h"

#include "intrinsics.h"

#define EXPRESSION_RESOLVED(expression) (!TYPE_IS_UNRESOLVED((expression)->value_type))

#define INVALIDATE(NODE) do {\
    ast_node_t* node = NODE;\
    node->value_type = ortypeid(TYPE_INVALID);\
    node->expr_val = ast_node_val_nil();\
} while(false)

typedef bool (*IsCircularDependencyFunc)(analyzer_t*, ast_node_t*);

typedef struct analysis_state_t {
    scope_t *scope;
} analysis_state_t;

typedef struct sizes_t sizes_t;
struct sizes_t {
    size_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

void scope_init(scope_t *scope, arena_t *allocator, scope_type_t type, scope_t *outer, ast_node_t *creator_expression) {
    scope->outer = outer;
    scope->creator = creator_expression;
    scope->type = type;
    scope->definitions = table_new(s2w, allocator);
}

static void add_definition(scope_t *scope, arena_t *allocator, string_view_t identifier, ast_node_t *decl) {
    table_put(s2w, scope->definitions, sv2string(identifier, allocator), ORWORDP(decl));
}

static ast_node_t *add_builtin_definition(ast_t *ast, string_view_t identifier, ortype_t type, orword_t word) {
    ast_node_t *decl = ast_node_new(ast->arena, AST_NODE_TYPE_DECLARATION_DEFINITION, nil_token);
    decl->value_type = type;
    decl->expr_val = ast_node_val_word(word);
    decl->is_mutable = false;

    table_put(s2w, ast->builtins, sv2string(identifier, ast->arena), ORWORDP(decl));

    return decl;
}

static void stan_error(analyzer_t *analyzer, error_t error) {
    analyzer->had_error = true;
    array_push(&analyzer->ast->errors, error);
}

static ast_node_t *get_defval_or_null_by_identifier_and_error(
        analyzer_t *analyzer,
        ast_t *ast,
        scope_t *scope,
        ast_node_t *def,
        scope_t **found_scope);

static bool is_builtin_type(type_table_t *t, string_view_t identifier, ortype_t *type) {
#define RETURN_IF_TYPE(name, t) \
if (sv_eq(identifier, lit2sv(#name))) {\
    *type = (t); \
    return true; \
}

    RETURN_IF_TYPE(void, t->void_)

    RETURN_IF_TYPE(f32, t->f32_)
    RETURN_IF_TYPE(f64, t->f64_)

    RETURN_IF_TYPE(char, t->char_)
    RETURN_IF_TYPE(uchar, t->uchar_)
    RETURN_IF_TYPE(schar, t->schar_)

    RETURN_IF_TYPE(u8, t->u8_)
    RETURN_IF_TYPE(s8, t->s8_)

    RETURN_IF_TYPE(u16, t->u16_)
    RETURN_IF_TYPE(s16, t->s16_)

    RETURN_IF_TYPE(s32, t->s32_)
    RETURN_IF_TYPE(u32, t->u32_)

    RETURN_IF_TYPE(u64, t->u64_)
    RETURN_IF_TYPE(s64, t->s64_)

    RETURN_IF_TYPE(int, t->int_)
    RETURN_IF_TYPE(uint, t->uint_)
    RETURN_IF_TYPE(size_t, t->size_t_)
    RETURN_IF_TYPE(ptrdiff_t, t->ptrdiff_t_)

    RETURN_IF_TYPE(str8_t, t->str8_t_)

#undef RETURN_IF_TYPE

#define RETURN_IF_TYPE(SYMBOL, TYPE_STRING, TYPE) \
if (sv_eq(identifier, lit2sv(#TYPE_STRING))){\
    *type = ortypeid(TYPE); \
    return true; \
}

    RETURN_IF_TYPE(identifier, void, TYPE_INVALID)
    RETURN_IF_TYPE(identifier, bool, TYPE_BOOL)
    RETURN_IF_TYPE(identifier, type, TYPE_TYPE)
#undef RETURN_IF_TYPE

    return false;

#undef RETURN_IF_TYPE
}

static bool check_call_on_func_or_error(analyzer_t *analyzer, ast_t *ast, ast_node_t *call) {
    ast_node_t *callee = an_callee(call);
    typedata_t *func_td = ast_type2td(ast, callee->value_type);

    ASSERT(func_td->kind == TYPE_FUNCTION, "must be used only on func");

    size_t arg_start = an_call_arg_start(call);
    size_t arg_end = an_call_arg_end(call);
    size_t arg_count = arg_end - arg_start;

    bool errored = false;
    size_t func_type_arg_count = func_td->as.function.argument_types.count;
    if (func_type_arg_count != arg_count) {
        errored = true;
    }

    size_t min_args = func_type_arg_count < arg_count ? func_type_arg_count : arg_count;

    for (size_t i = 0; i < min_args; ++i) {
        ortype_t parameter_type = func_td->as.function.argument_types.items[i];

        ast_node_t *arg = call->children.items[arg_start + i];
        ortype_t argument_type = arg->value_type;
        unless (ortypeid_eq(parameter_type, argument_type) && !TYPE_IS_INVALID(argument_type)) {
            errored = true;
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.type-mismatch.call-check",
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

static bool fold_funcsig_or_error(analyzer_t *analyzer, ast_t *ast, ast_node_t *sig) {
    ASSERT(sig->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");

    unless (an_func_def_return(sig)->expr_val.is_concrete) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.constant.func-sig",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("return type for function signature must be a constant"),
            .args = ORERR_ARGS(error_arg_node(an_func_def_return(sig))),
            .show_code_lines = ORERR_LINES(0),
        ));
        return false;
    }

    bool hit_error = false;

    tmp_arena_t *tmp = allocator_borrow();
    types_t parameter_types = {.allocator=tmp->allocator};

    bool type_is_inferred = false;

    for (size_t i = an_func_def_arg_start(sig); i < an_func_def_arg_end(sig); ++i) {
        ast_node_t *parameter = sig->children.items[i];
        unless (parameter->expr_val.is_concrete) {
            hit_error = true;
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.noconst.sig-param",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("parameter type $1.$ for function signature must be a constant"),
                .args = ORERR_ARGS(error_arg_node(parameter), error_arg_sz(i-an_func_def_arg_start(sig) + 1)),
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
                .tag = "sem.type-mismatch.param-type",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("parameter expression must be type 'type'"),
                .args = ORERR_ARGS(error_arg_node(parameter), error_arg_sz(i-an_func_def_arg_start(sig) + 1)),
                .show_code_lines = ORERR_LINES(0),
            ));
            break;
        }

        ortype_t type = ortypeid(parameter->expr_val.word.as.u);
        array_push(&parameter_types, type);
    }

    bool result = false;

    if (type_is_inferred) {
        sig->expr_val = ast_node_val_word(ORWORDT(ortypeid(TYPE_UNRESOLVED)));

        result = true;
        goto defer;
    }

    if (hit_error) {
        result = false;
        goto defer;
    }

    ast_node_t *return_type_expression = an_func_def_return(sig);
    if (!TYPE_IS_TYPE(return_type_expression->value_type)) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.type-mismatch.expr-type",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("return expression must be type 'type'"),
            .args = ORERR_ARGS(error_arg_node(return_type_expression)),
            .show_code_lines = ORERR_LINES(0),
        ));
        result = false;
        goto defer;
    }

    ortype_t return_type = ortypeid(return_type_expression->expr_val.word.as.u);

    ortype_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);

    orword_t funcsig_word = ORWORDU(function_type.i);

    sig->expr_val = ast_node_val_word(funcsig_word);

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
                .tag = "sem.nojmp.condition",
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
            .tag = "sem.nofunction.return",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("cannot find function to return from"),
            .args = ORERR_ARGS(error_arg_node(jmp_node)),
            .show_code_lines = ORERR_LINES(0),
        ));
    } else {
        if (label.length != 0) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nolabel",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot find jmp label '$0.$'"),
                .args = ORERR_ARGS(error_arg_token(jmp_node->identifier)),
                .show_code_lines = ORERR_LINES(0),
            ));
        } else {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.jmp.no-valid-scope",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("no valid scope to jmp out of"),
                .args = ORERR_ARGS(error_arg_node(jmp_node)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }
    return false;
}
ortype_t resolve_block_return_types_or_error(analyzer_t *analyzer, ast_node_t *block) {
    switch (block->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            ast_node_t *then = an_then(block);
            ast_node_t *else_ = an_else(block);

            tmp_arena_t *tmp = allocator_borrow();
            types_t types = {.allocator=tmp->allocator};
            ast_nodes_t nodes = {.allocator=tmp->allocator};
            
            if (block->branch_type == BRANCH_TYPE_IF) {
                unless (TYPE_IS_UNREACHABLE(then->value_type)) {
                    array_push(&nodes, then);
                    array_push(&types, then->value_type);
                }
            }

            unless (TYPE_IS_UNREACHABLE(else_->value_type)) {
                array_push(&nodes, else_);
                array_push(&types, else_->value_type);
            }

            for (size_t i = 0; i < block->jmp_nodes.count; ++i) {
                ast_node_t *jmp_node = block->jmp_nodes.items[i];
                if (jmp_node->start.type == TOKEN_CONTINUE) continue;

                ast_node_t *jmp_node_expr = an_expression(jmp_node);
                unless (TYPE_IS_UNREACHABLE(jmp_node_expr->value_type)) {
                    array_push(&nodes, jmp_node_expr);
                    array_push(&types, jmp_node_expr->value_type);
                }
            }

            if (types.count == 0) {
                return ortypeid(TYPE_UNREACHABLE);
            }

            ortype_t type = types.items[0];
            ast_node_t *node = nodes.items[0];

            for (size_t i = 1; i < types.count; ++i) {
                ortype_t other_type = types.items[i];
                ast_node_t *other_node = nodes.items[i];

                if (block->is_consumed && !ortypeid_eq(other_type, type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.type-mismatch.branches",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("all '$0.kind$' branches and jmps must match types when consumed but got '$3.$' and '$4.$'"),
                        .args = ORERR_ARGS(error_arg_token(block->start), error_arg_node(node), error_arg_node(other_node), error_arg_type(type), error_arg_type(other_type)),
                        .show_code_lines = ORERR_LINES(1, 2),
                    ));

                    type = ortypeid(TYPE_INVALID);
                    break;
                }
            }

            allocator_return(tmp);

            return type;
        }

        default: UNREACHABLE();
    }

    return ortypeid(TYPE_INVALID);
}

static void resolve_declaration_definition(
    analyzer_t *analyzer,
    ast_t *ast,
    analysis_state_t state,
    ast_node_t *decl);

static size_t resolve_declarations_until_unreachable(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_nodes_t declarations,
        bool is_last_statement_consumed);

static void resolve_declaration(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *declaration_node);

static void resolve_declaration_statement(
    analyzer_t *analyzer,
    ast_t *ast,
    analysis_state_t state,
    ast_node_t *statement,
    bool is_consumed);

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

static orword_t ast_item_get(typedatas_t *types, void *aggregate, ortype_t item_type, size_t byte_offset) {
    typedata_t *td = type2typedata(types, item_type);
    
    void *addr = aggregate + byte_offset;

    switch (td->kind) {
    case TYPE_VOID: return ORWORDU(0);
    case TYPE_BOOL: {
        bool res = *((bool*)addr);
        return ORWORDU(res);
    }

    case TYPE_TYPE: {
        ortype_t res = *((ortype_t*)addr);
        return ORWORDT(res);
    }

    case TYPE_POINTER:
    case TYPE_FUNCTION: {
        void *res = *((void**)addr);
        return ORWORDP(res);
    }

    case TYPE_STRING:
    case TYPE_STRUCT:
    case TYPE_ARRAY: {
        if (td->size > ORWORD_SIZE) {
            return ORWORDP(addr);
        } else {
            orword_t arr = *((orword_t*)addr);
            return arr;
        }
    }

    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: {
                oru8 res = *((oru8*)addr);
                return ORWORDU(res);
            }
            case NUM_SIZE_16: {
                oru16 res = *((oru16*)addr);
                return ORWORDU(res);
            }
            case NUM_SIZE_32: {
                oru32 res = *((oru32*)addr);
                return ORWORDU(res);
            }
            case NUM_SIZE_64: {
                oru64 res = *((oru64*)addr);
                return ORWORDU(res);
            }
            }
        }

        case NUM_TYPE_SIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: {
                ors8 res = *((ors8*)addr);
                return ORWORDI(res);
            }
            case NUM_SIZE_16: {
                ors16 res = *((ors16*)addr);
                return ORWORDI(res);
            }
            case NUM_SIZE_32: {
                ors32 res = *((ors32*)addr);
                return ORWORDI(res);
            }
            case NUM_SIZE_64: {
                ors64 res = *((ors64*)addr);
                return ORWORDI(res);
            }
            }
        }
        case NUM_TYPE_FLOAT: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: UNREACHABLE(); break;
            case NUM_SIZE_16: UNREACHABLE(); break;

            case NUM_SIZE_32: {
                orf32 res = *((orf32*)addr);
                return ORWORDD(res);
            }
            case NUM_SIZE_64: {
                orf64 res = *((orf64*)addr);
                return ORWORDD(res);
            }
            }
        }
        }
    }

    case TYPE_MODULE:
    case TYPE_INVALID:
    case TYPE_UNRESOLVED:
    case TYPE_PARAM_STRUCT:
    case TYPE_INFERRED_FUNCTION:
    case TYPE_UNREACHABLE:
    case TYPE_COUNT: UNREACHABLE(); break;
    }
}

void ast_item_set(ast_t *ast, ortype_t type, void *addr, orword_t value, size_t byte_offset) {
    typedata_t *td = ast_type2td(ast, type);

    addr += byte_offset;

    switch (td->kind) {
    case TYPE_VOID: return;
    case TYPE_BOOL: {
        *((bool*)addr) = orcast(bool, value.as.u);
        return;
    }

    case TYPE_TYPE: {
        *((ortype_t*)addr) = value.as.t;
        break;
    }

    case TYPE_POINTER:
    case TYPE_FUNCTION: {
        *((void**)addr) = value.as.p;
        break;
    }

    case TYPE_STRUCT:
    case TYPE_STRING:
    case TYPE_ARRAY: {
        void *src;
        if (td->size > ORWORD_SIZE) {
            src = value.as.p;
        } else {
            src = &value;
        }

        memcpy(addr, src, td->size);
        break;
    }

    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: {
                *((oru8*)addr) = orcast(oru8, value.as.u);
                return;
            }
            case NUM_SIZE_16: {
                *((oru16*)addr) = orcast(oru16, value.as.u);
                return;
            }
            case NUM_SIZE_32: {
                *((oru32*)addr) = orcast(oru32, value.as.u);
                return;
            }
            case NUM_SIZE_64: {
                *((oru64*)addr) = orcast(oru64, value.as.u);
                return;
            }
            }
        }

        case NUM_TYPE_SIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: {
                *((ors8*)addr) = orcast(ors8, value.as.s);
                return;
            }
            case NUM_SIZE_16: {
                *((ors16*)addr) = orcast(ors16, value.as.s);
                return;
            }
            case NUM_SIZE_32: {
                *((ors32*)addr) = orcast(ors32, value.as.s);
                return;
            }
            case NUM_SIZE_64: {
                *((ors64*)addr) = orcast(ors64, value.as.s);
                return;
            }
            }
        }

        case NUM_TYPE_FLOAT: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: UNREACHABLE(); break;
            case NUM_SIZE_16: UNREACHABLE(); break;

            case NUM_SIZE_32: {
                *((orf32*)addr) = orcast(orf32, value.as.d);
                return;
            }
            case NUM_SIZE_64: {
                *((orf64*)addr) = orcast(orf64, value.as.d);
                return;
            }
            }
        }
        }
    }

    case TYPE_INVALID:
    case TYPE_UNRESOLVED:
    case TYPE_PARAM_STRUCT:
    case TYPE_MODULE:
    case TYPE_INFERRED_FUNCTION:
    case TYPE_UNREACHABLE:
    case TYPE_COUNT: UNREACHABLE(); break;
    }
}

static void *word_as_ptr(orword_t *word, size_t size_bytes) {
    if (size_bytes > ORWORD_SIZE) {
        return word->as.p;
    } else {
        return word;
    }
}

void constant_fold_bin_arithmetic(ast_t *ast, token_type_t operator, ortype_t type, orword_t l, orword_t r, void *result) {
    typedata_t *td = ast_type2td(ast, type);

    #define case_block(type, l, r, v) do { switch (operator) { \
        case TOKEN_PLUS: res = oradd##type(l, r); break; \
        case TOKEN_MINUS: res = orsub##type(l, r); break; \
        case TOKEN_SLASH: res = ordiv##type(l, r); break; \
        case TOKEN_STAR: res = ormul##type(l, r); break; \
        case TOKEN_PERCENT: res = ormod##type(l, r); break; \
        case TOKEN_PERCENT_PERCENT: res = orrem##type(l, r); break; \
        default: UNREACHABLE(); \
        }} while (false)

    switch (td->kind) {
    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_SIGNED: {
            ors64 lhsi = l.as.s;
            ors64 rhsi = r.as.s;
            ors64 res;

            switch((num_size_t)td->size) {
            case NUM_SIZE_8: case_block(s8, lhsi, rhsi, s); break;
            case NUM_SIZE_16: case_block(s16, lhsi, rhsi, s); break;
            case NUM_SIZE_32: case_block(s32, lhsi, rhsi, s); break;
            case NUM_SIZE_64: case_block(s64, lhsi, rhsi, s); break;
            }

            ast_item_set(ast, type, result, ORWORDI(res), 0);
            break;
        }
        
        case NUM_TYPE_UNSIGNED: {
            oru64 lhsu = l.as.u;
            oru64 rhsu = l.as.u;
            oru64 res;

            switch((num_size_t)td->size) {
            case NUM_SIZE_8: case_block(u8, lhsu, rhsu, u); break;
            case NUM_SIZE_16: case_block(u16, lhsu, rhsu, u); break;
            case NUM_SIZE_32: case_block(u32, lhsu, rhsu, u); break;
            case NUM_SIZE_64: case_block(u64, lhsu, rhsu, u); break;
            }

            ast_item_set(ast, type, result, ORWORDU(res), 0);
            break;
        }

        case NUM_TYPE_FLOAT: {
            orf64 lhsf = l.as.d;
            orf64 rhsf = r.as.d;
            orf64 res;

            switch((num_size_t)td->size) {
            case NUM_SIZE_32: case_block(f, lhsf, rhsf, d); break;
            case NUM_SIZE_64: case_block(d, lhsf, rhsf, d); break;
            default: UNREACHABLE(); break;
            }

            ast_item_set(ast, type, result, ORWORDD(res), 0);
            break;
        }
        }
        break;
    }

    case TYPE_ARRAY: {
        void *array_addr = result;

        typedata_t *inner_td = ast_type2td(ast, td->as.arr.type);

        size_t aligned_size = td_align(inner_td->size, inner_td->alignment);
        for (size_t i = 0; i < td->as.arr.count; ++i) {
            orword_t item_l = ast_item_get(&ast->type_set.types, word_as_ptr(&l, td->size), td->as.arr.type, i*aligned_size);
            orword_t item_r = ast_item_get(&ast->type_set.types, word_as_ptr(&r, td->size), td->as.arr.type, i*aligned_size);

            void *addr = array_addr + (i*aligned_size);

            constant_fold_bin_arithmetic(ast, operator, td->as.arr.type, item_l, item_r, addr);
        }
        break;
    }

    case TYPE_STRUCT: {
        void *struct_addr = result;

        for (size_t i = 0; i < td->as.struct_.fields.count; ++i) {
            struct_field_t field = td->as.struct_.fields.items[i];
            orword_t item_l = ast_item_get(&ast->type_set.types, word_as_ptr(&l, td->size), field.type, field.offset);
            orword_t item_r = ast_item_get(&ast->type_set.types, word_as_ptr(&r, td->size), field.type, field.offset);

            void *addr = struct_addr + field.offset;
            constant_fold_bin_arithmetic(ast, operator, field.type, item_l, item_r, addr);
        }
        break;
    }

    default: UNREACHABLE(); break;
    }

    #undef case_block
}

orword_t constant_fold_bin_comparison(ast_t *ast, token_type_t operator, ortype_t type, orword_t a, orword_t b) {
    typedata_t *typedata = ast_type2td(ast, type);

    switch (typedata->kind) {
    case TYPE_NUMBER: {
        switch (typedata->as.num) {
        case NUM_TYPE_SIGNED: {
            ors64 lhsi = a.as.s;
            ors64 rhsi = b.as.s;
            bool result = 0;

            switch (operator) {
            case TOKEN_GREATER: result = (lhsi > rhsi); break;
            case TOKEN_GREATER_EQUAL: result = (lhsi >= rhsi); break;
            case TOKEN_LESS: result = (lhsi < rhsi); break;
            case TOKEN_LESS_EQUAL: result = (lhsi <= rhsi); break;
            default: UNREACHABLE();
            }

            return ORWORDU((oru64)result);
        }
        
        case NUM_TYPE_UNSIGNED: {
            oru64 lhsu = a.as.u;
            oru64 rhsu = b.as.u;
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

            return ORWORDU((oru64)result);
        }

        case NUM_TYPE_FLOAT: {
            orf64 lhsf = a.as.d;
            orf64 rhsf = b.as.d;
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

            return ORWORDU((oru64)result);
        }
        }
    }

    case TYPE_STRUCT: UNREACHABLE(); /*todo*/ return (orword_t){0};

    default: UNREACHABLE(); return (orword_t){0};
    }
}

static bool stan_can_cast(typedatas_t *types, ortype_t dst, ortype_t src) {
    typedata_t *srctd = type2typedata(types, src);
    typedata_t *dsttd = type2typedata(types, dst);

    if (srctd->kind == TYPE_NUMBER && dsttd->kind == TYPE_NUMBER) return true;
    if (srctd->kind == TYPE_POINTER && dsttd->kind == TYPE_POINTER) {
        if (TYPE_IS_VOID(dsttd->as.ptr.type)) return true;
        if (TYPE_IS_VOID(srctd->as.ptr.type)) return true;

        typedata_t *src_inner_td = type2typedata(types, srctd->as.ptr.type);
        typedata_t *dst_inner_td = type2typedata(types, dsttd->as.ptr.type);

        if (src_inner_td->kind == dst_inner_td->kind && src_inner_td->kind == TYPE_ARRAY) {
            if (ortypeid_eq(src_inner_td->as.arr.type, dst_inner_td->as.arr.type)) {
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

static orword_t constant_fold_cast(ast_t *ast, orword_t in, ortype_t dst, ortype_t src) {
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

        orword_t result = {0};
        if (CASTING(SIGNED, SIGNED)) {
            if (DSIZE(8)) result.as.s = orcast(ors8, VALI);
            else if (DSIZE(16)) result.as.s = orcast(ors16, VALI);
            else if (DSIZE(32)) result.as.s = orcast(ors32, VALI);
            else result.as.s = VALI;
        } else if (CASTING(SIGNED, UNSIGNED)) {
            if (DSIZE(8)) result.as.u = orcast(oru8, orcast(oru64, VALI));
            else if (DSIZE(16)) result.as.u = orcast(oru16, orcast(oru64, VALI));
            else if (DSIZE(32)) result.as.u = orcast(oru32, orcast(oru64, VALI));
            else if (DSIZE(64)) result.as.u = orcast(oru64, VALI);
            else UNREACHABLE();
        } else if (CASTING(SIGNED, FLOAT)) {
            if (DSIZE(32)) result.as.d = orcast(orf32, VALI);
            else if (DSIZE(64)) result.as.d = orcast(orf64, VALI);
            else UNREACHABLE();
        } else if (CASTING(UNSIGNED, UNSIGNED)) {
            if (DSIZE(8)) result.as.u = orcast(oru8, VALU);
            else if (DSIZE(16)) result.as.u = orcast(oru16, VALU);
            else if (DSIZE(32)) result.as.u = orcast(oru32, VALU);
            else result.as.u = VALU;
        } else if (CASTING(UNSIGNED, SIGNED)) {
            if (DSIZE(8)) result.as.s = orcast(ors8, orcast(ors64, VALU));
            else if (DSIZE(16)) result.as.s = orcast(ors16, orcast(ors64, VALU));
            else if (DSIZE(32)) result.as.s = orcast(ors32, orcast(ors64, VALU));
            else if (DSIZE(64)) result.as.s = orcast(ors64, VALU);
            else UNREACHABLE();
        } else if (CASTING(UNSIGNED, FLOAT)) {
            if (DSIZE(32)) result.as.d = orcast(orf32, VALU);
            else if (DSIZE(64)) result.as.d = orcast(orf64, VALU);
            else UNREACHABLE();
        } else if (CASTING(FLOAT, FLOAT)) {
            if (DSIZE(32)) result.as.d = orcast(orf32, VALF);
            else result.as.d = VALF;
        } else if (CASTING(FLOAT, SIGNED)) {
            if (DSIZE(8)) result.as.u = orcast(oru8, orcast(oru64, VALF));
            else if (DSIZE(16)) result.as.u = orcast(oru16, orcast(oru64, VALF));
            else if (DSIZE(32)) result.as.u = orcast(oru32, orcast(oru64, VALF));
            else if (DSIZE(64)) result.as.u = orcast(oru64, VALF);
            else UNREACHABLE();
        } else if (CASTING(FLOAT, SIGNED)) {
            if (DSIZE(8)) result.as.s = orcast(ors8, orcast(ors64, VALF));
            else if (DSIZE(16)) result.as.s = orcast(ors16, orcast(ors64, VALF));
            else if (DSIZE(32)) result.as.s = orcast(ors32, orcast(ors64, VALF));
            else if (DSIZE(64)) result.as.s = orcast(ors64, VALF);
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

static ast_node_t *ast_implicit_cast(ast_t *ast, ast_node_t *expr, ortype_t dst_type) {
    ast_node_t *inferred_type = ast_inferred_type_decl(ast, token_implicit_at_start(expr->start), token_implicit_at_start(expr->start));
    inferred_type->value_type = ortypeid(TYPE_TYPE);
    inferred_type->expr_val = ast_node_val_word(ORWORDT(dst_type));

    ast_node_t *cast = ast_cast(ast, inferred_type, expr);
    cast->value_type = dst_type;

    if (expr->expr_val.is_concrete) {
        orword_t result = constant_fold_cast(ast, expr->expr_val.word, dst_type, expr->value_type);
        cast->expr_val = ast_node_val_word(result);
        cast->is_free_number = expr->is_free_number;
    }

    return cast;
}
bool type_size_is_platform_specific(type_table_t *type_table, ortype_t type) {
    #define IS(t) ortypeid_eq(type, type_table->t##_)
    if (IS(s8) || IS(s16) || IS(s32) || IS(s64) ||
        IS(u8) || IS(u16) || IS(u32) || IS(u64) ||
        IS(f64) || IS(f32)) return false;
    #undef IS

    return true;
}

static ast_node_t *cast_implicitly_if_necessary(ast_t *ast, ortype_t dst_type, ast_node_t *expr) {
    ASSERT(TYPE_IS_RESOLVED(expr->value_type), "expression must be resolved already");

    if (ortypeid_eq(dst_type, expr->value_type)) return expr;

    unless (stan_can_cast(&ast->type_set.types, dst_type, expr->value_type)) return expr;

    typedata_t *dsttd = ast_type2td(ast, dst_type);
    typedata_t *exprtd = ast_type2td(ast, expr->value_type);

    // for now only numbers can be cast/promoted implicitly
    if (dsttd->kind == TYPE_NUMBER && exprtd->kind == TYPE_NUMBER) {
        num_type_t dst_num = dsttd->as.num;
        num_type_t expr_num = exprtd->as.num;

        // if we now what the value of the number we just implicitly cast it...
        if (expr->expr_val.is_concrete && expr->is_free_number) {
            orword_t w = expr->expr_val.word;
            switch (dst_num) {
            case NUM_TYPE_SIGNED:
                switch ((num_size_t)(dsttd->size)) {
                case NUM_SIZE_8:
                    switch(expr_num) {
                    case NUM_TYPE_SIGNED: if (w.as.s < INT8_MIN || w.as.s > INT8_MAX) return expr; break;
                    case NUM_TYPE_UNSIGNED: if (w.as.u > INT8_MAX) return expr; break;
                    case NUM_TYPE_FLOAT: if (w.as.d < INT8_MIN || w.as.d > INT8_MAX || w.as.d != ((ors8)w.as.d)) return expr; break;
                    }
                    break;
                
                case NUM_SIZE_16:
                    switch(expr_num) {
                    case NUM_TYPE_SIGNED: if (w.as.s < INT16_MIN || w.as.s > INT16_MAX) return expr; break;
                    case NUM_TYPE_UNSIGNED: if (w.as.u > INT16_MAX) return expr; break;
                    case NUM_TYPE_FLOAT: if (w.as.d < INT16_MIN || w.as.d > INT16_MAX || w.as.d != ((ors16)w.as.d)) return expr; break;
                    }
                    break;
                
                case NUM_SIZE_32:
                    switch(expr_num) {
                    case NUM_TYPE_SIGNED: if (w.as.s < INT32_MIN || w.as.s > INT32_MAX) return expr; break;
                    case NUM_TYPE_UNSIGNED: if (w.as.u > INT32_MAX)  return expr; break;
                    case NUM_TYPE_FLOAT: if (w.as.d < INT32_MIN || w.as.d > INT32_MAX || w.as.d != ((ors32)w.as.d)) return expr; break;
                    }
                    break;
                
                case NUM_SIZE_64: break;
                    switch(expr_num) {
                    case NUM_TYPE_SIGNED: break;
                    case NUM_TYPE_UNSIGNED: if (w.as.u > INT64_MAX)  return expr; break;
                    case NUM_TYPE_FLOAT: if (w.as.d < INT64_MIN || w.as.d > INT64_MAX || w.as.d != ((ors64)w.as.d)) return expr; break;
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
                    case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT8_MAX || w.as.d != ((oru8)w.as.d)) return expr; break;
                    }
                    break;
                
                case NUM_SIZE_16:
                    switch(expr_num) {
                    case NUM_TYPE_SIGNED: if (w.as.s < 0 || w.as.s > UINT16_MAX) return expr; break;
                    case NUM_TYPE_UNSIGNED: if (w.as.u > UINT16_MAX) return expr; break;
                    case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT16_MAX || w.as.d != ((oru16)w.as.d)) return expr; break;
                    }
                    break;
                
                case NUM_SIZE_32:
                    switch(expr_num) {
                    case NUM_TYPE_SIGNED: if (w.as.s < 0 || w.as.s > UINT32_MAX) return expr; break;
                    case NUM_TYPE_UNSIGNED: if (w.as.u > UINT32_MAX)  return expr; break;
                    case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT32_MAX || w.as.d != ((oru32)w.as.d)) return expr; break;
                    }
                    break;
                
                case NUM_SIZE_64: break;
                    switch(expr_num) {
                    case NUM_TYPE_SIGNED: if (w.as.s < 0) return expr; break;
                    case NUM_TYPE_UNSIGNED: break;
                    case NUM_TYPE_FLOAT: if (w.as.d < 0 || w.as.d > UINT64_MAX || w.as.d != ((oru64)w.as.d)) return expr; break;
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

static bool stan_run(analyzer_t *analyzer, vm_t *vm, ast_node_t *expr, orword_t *out_result) {
    tmp_arena_t *tmp = allocator_borrow();

    function_t *function = new_function(vm->program_mem, tmp->allocator);
    compile_expr_to_function(function, analyzer->ast, expr, vm->program_mem, vm->arena);

    vm_fresh_run(analyzer->run_vm, function);

    orword_t result = analyzer->run_vm->registers[REG_RESULT];
    *out_result = result;

    allocator_return(tmp);
    return true;
}

bool ast_word_eq(type_table_t *type_set, ortype_t type, orword_t a, orword_t b) {
    typedata_t *td = type2typedata(&type_set->types, type);

    void *ap = td->size > ORWORD_SIZE ? a.as.p : &a;
    void *bp = td->size > ORWORD_SIZE ? b.as.p : &b;

    bool same = memcmp(ap, bp, td->size) == 0;
    return same;
}

static matched_value_t stan_pattern_match_or_error(analyzer_t *analyzer, ast_node_t *decl, type_path_t *expected, ortype_t actual, ast_node_t *arg) {
    if (TYPE_IS_INVALID(actual)) {
        return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
    }

    typedata_t *td = ast_type2td(analyzer->ast, actual);
    switch (expected->kind) {
    case MATCH_TYPE_POINTER: {
        if (td->kind != TYPE_POINTER) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nomatch.ptr",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match to pointer type"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
        }

        return stan_pattern_match_or_error(analyzer, decl, expected->next, td->as.ptr.type, arg);
    }

    case MATCH_TYPE_ARRAY_TYPE: {
        if (td->kind != TYPE_ARRAY) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nomatch.arr-type",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match to array type"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
        }

        return stan_pattern_match_or_error(analyzer, decl, expected->next, td->as.arr.type, arg);
    }

    case MATCH_TYPE_ARRAY_SIZE: {
        if (td->kind != TYPE_ARRAY || expected->next->kind != MATCH_TYPE_IDENTIFIER) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nomatch.arr-size",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match to array size"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
        }

        return (matched_value_t){
            .type=analyzer->ast->type_set.size_t_,
            .word=ORWORDU(td->as.arr.count),
        };
    }

    case MATCH_TYPE_IDENTIFIER: {
        return (matched_value_t){
            .type=ortypeid(TYPE_TYPE),
            .word=ORWORDT(actual),
        };
    }

    case MATCH_TYPE_SIG_ARG: {
        size_t arg_index = expected->index;
        if (td->kind != TYPE_FUNCTION || arg_index >= td->as.function.argument_types.count) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nomatch.sig-arg",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match to argument in signature"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
        }

        ortype_t arg_type = td->as.function.argument_types.items[arg_index];

        return stan_pattern_match_or_error(analyzer, decl, expected->next, arg_type, arg);
    }

    case MATCH_TYPE_STRUCT_PARAM: {
        size_t arg_index = expected->index;
        if (td->kind != TYPE_STRUCT || arg_index >= td->as.struct_.params.count) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nomatch.struct-param",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match to struct parameter"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
        }

        ortype_t param_type = td->as.struct_.params.items[arg_index].type;
        orword_t arg_word = td->as.struct_.params.items[arg_index].default_value;
        if (TYPE_IS_TYPE(param_type)) {
            return stan_pattern_match_or_error(analyzer, decl, expected->next, arg_word.as.t, arg);
        }

        if (expected->next->kind != MATCH_TYPE_IDENTIFIER) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nomatch.expected-value",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("expected '$2.$' for struct parameter but got '$3.$'"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl)),
                        error_arg_type(param_type), error_arg_type(actual)),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
        }

        return (matched_value_t){
            .type=param_type,
            .word=arg_word,
        };
    }

    case MATCH_TYPE_SIG_RET: {
        if (td->kind != TYPE_FUNCTION) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nomatch.sig-return",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot match to return in signature"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_node(an_decl_type(decl))),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return (matched_value_t){.type=ortypeid(TYPE_INVALID)};
        }

        ortype_t ret_type = td->as.function.return_type;

        return stan_pattern_match_or_error(analyzer, decl, expected->next, ret_type, arg);
    }
    }
}

static bool matched_values_eq(ast_t *ast, matched_values_t a, matched_values_t b) {
    if (a.count != b.count) return false;

    for (size_t i = 0; i < a.count; ++i) {
        matched_value_t mva = a.items[i];
        matched_value_t mvb = b.items[i];
        if (ortypeid_nq(mva.type, mvb.type)) return false;
        bool same = ast_word_eq(&ast->type_set, mva.type, mva.word, mvb.word);
        return same;
    }

    return true;
}

static ast_node_t *find_realized_node_or_null_by_inferred_values(ast_t *ast, inferred_copies_t copies, matched_values_t key) {
    for (size_t i = 0; i < copies.count; ++i) {
        inferred_copy_t copy = copies.items[i];
        ASSERT(copy.key.count == key.count, "the keys should be the same size at this point");

        bool found = matched_values_eq(ast, key, copy.key);

        if (found) return copy.copy;
    }

    return NULL;
}

static size_t stan_is_struct_circular(analyzer_t *analyzer, ast_node_t *expr) {
    size_t index;
    for (size_t i = analyzer->pending_dependencies.count; i > 0; --i) {
        index = i-1;
        ast_node_t *dep = analyzer->pending_dependencies.items[index];
        if (dep == expr) {
            return index;
        }
    }

    return analyzer->pending_dependencies.count;
}

static void stan_circular_dependency_error(analyzer_t *analyzer, ast_t *ast, ast_node_t *def, size_t dep_start_index) {
    ast_nodes_t *deps = arena_alloc(ast->arena, sizeof(ast_nodes_t));
    *deps = (ast_nodes_t){.allocator=ast->arena};

    for (size_t j = dep_start_index; j < analyzer->pending_dependencies.count; ++j) {
        ast_node_t *dep = analyzer->pending_dependencies.items[j];
        array_push(deps, dep);
    }

    stan_error(analyzer, OR_ERROR(
        .tag = "sem.circ-dep",
        .level = ERROR_SOURCE_ANALYSIS,
        .msg = lit2str("attempting to retrieve a constant with recursive compile-time dependencies"),
        .args = ORERR_ARGS(error_arg_node(def), error_arg_ptr(deps)),
        .show_code_lines = ORERR_LINES(0),
    ));
}

static void resolve_struct(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *struct_def);

static void retry_struct_resolve(analyzer_t *analyzer, ast_node_t *dep_expr, ortype_t struct_type) {
    ast_node_t *struct_decl = NULL;
    for (size_t i = analyzer->pending_dependencies.count; i > 0; --i) {
        ast_node_t *dep = analyzer->pending_dependencies.items[i-1];
        if (dep->node_type != AST_NODE_TYPE_EXPRESSION_STRUCT) continue;
        if (TYPE_IS_UNRESOLVED(dep->value_type)) continue;
        unless (dep->expr_val.is_concrete) continue;

        unless (TYPE_IS_TYPE(dep->value_type)) continue;

        if (ortypeid_eq(dep->expr_val.word.as.t, struct_type)) {
            struct_decl = dep;
            break;
        }
    }

    MUST(struct_decl);

    {
        array_push(&analyzer->pending_dependencies, dep_expr);

        scope_t *scope = &struct_decl->defined_scope;
        analysis_state_t state = {0};
        state.scope = scope;
        resolve_struct(analyzer, analyzer->ast, state, struct_decl);

        --analyzer->pending_dependencies.count;
    }
}

static bool retry_resolve_possible_struct_or_error(analyzer_t *analyzer, ast_node_t *dep_expr, ortype_t struct_type) {
    typedata_t *td = ast_type2td(analyzer->ast, struct_type);
    if (td->kind != TYPE_STRUCT || td->as.struct_.status == STRUCT_STATUS_COMPLETE) {
        return true;
    }

    if (td->as.struct_.status == STRUCT_STATUS_INVALID) {
        return false;
    }

    size_t dep_index  = stan_is_struct_circular(analyzer, dep_expr);

    if (dep_index < analyzer->pending_dependencies.count) {
        stan_circular_dependency_error(analyzer, analyzer->ast, dep_expr, dep_index);
        return false;
    }

    array_push(&analyzer->pending_dependencies, dep_expr);

    retry_struct_resolve(analyzer, dep_expr, struct_type);

    --analyzer->pending_dependencies.count;

    bool is_invalid = td->as.struct_.status == STRUCT_STATUS_INVALID;
    return !is_invalid;
}

static size_t stan_function_is_building(analyzer_t *analyzer, function_t *function, bool *passed_through_fold) {
    *passed_through_fold = false;

    for (size_t i = analyzer->pending_dependencies.count; i > 0; --i) {
        ast_node_t *dep = analyzer->pending_dependencies.items[i-1];

        switch (dep->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: *passed_through_fold = true; break;
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            function_t *pending_function = (function_t*)dep->expr_val.word.as.p;
            if (*passed_through_fold && function == pending_function) {
                return i-1;
            }
            break;
        }

        default: continue;
        }
    }

    return analyzer->pending_dependencies.count;
}

size_t stan_struct_is_building(analyzer_t *analyzer, ortype_t struct_type) {
    bool passed_through_fold = false;
    for (size_t i = analyzer->pending_dependencies.count; i > 0; --i) {
        ast_node_t *dep = analyzer->pending_dependencies.items[i-1];

        switch (dep->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: passed_through_fold = true; break;
        case AST_NODE_TYPE_EXPRESSION_STRUCT: {
            ortype_t t = dep->expr_val.word.as.t;
            if (passed_through_fold && ortypeid_eq(t, struct_type)) {
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
        if (dep->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) break; break;
        if (dep->node_type == AST_NODE_TYPE_EXPRESSION_STRUCT) break;
        if (dep == decl) return i-1;
    }

    return analyzer->pending_dependencies.count;
}

void resolve_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ortype_t implicit_type,
        ast_node_t *expr,
        bool is_consumed);

static ast_node_t *stan_realize_inferred_funcdefcall_or_errornull(analyzer_t *analyzer, analysis_state_t call_state, ast_node_t *call, ast_node_t *inferred_funcdef) {
    tmp_arena_t *tmp = allocator_borrow();
    matched_values_t matched_values = {.allocator=tmp->allocator};

    // todo: im pretty sure i can reuse the scope over and over if i clear it
    // that way im not allocating that much more...
    analysis_state_t inferred_state = call_state;
    scope_t inferred_scope = {0};
    scope_init(&inferred_scope, tmp->allocator, SCOPE_TYPE_INFERRED_PARAMS, inferred_funcdef->defined_scope.outer, call);
    inferred_state.scope = &inferred_scope;

    bool had_error = false;

    size_t inferred_arg_start = an_func_def_arg_start(inferred_funcdef);
    size_t inferred_arg_end = an_func_def_arg_end(inferred_funcdef);
    for (size_t i = inferred_arg_start; i < inferred_arg_end; ++i) {
        ast_node_t *decl = inferred_funcdef->children.items[i];
        
        size_t arg_call_start = an_call_arg_start(call);
        ast_node_t *call_arg = call->children.items[i-inferred_arg_start+arg_call_start];

        if (decl->type_decl_patterns.count > 0) {
            if (an_is_none(call_arg)) {
                had_error = true;
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.call.arg-required",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("inferred call argument must not be omitted"),
                    .args = ORERR_ARGS(error_arg_node(call_arg)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                continue;
            }

            resolve_expression(analyzer, analyzer->ast, call_state, ortypeid(TYPE_UNRESOLVED), call_arg, true);
        }

        for (size_t t = 0; t < decl->type_decl_patterns.count; ++t)  {
            type_pattern_t pattern = decl->type_decl_patterns.items[t];
            
            matched_value_t matched_value = stan_pattern_match_or_error(analyzer, decl, pattern.expected, call_arg->value_type, call_arg);
            array_push(&matched_values, matched_value);

            if (TYPE_IS_INVALID(matched_value.type)) {
                had_error = true;
                continue;
            }

            ast_node_t *implicit_type_decl = ast_implicit_expr(analyzer->ast, ortypeid(TYPE_TYPE), ORWORDT(matched_value.type), token_implicit_at_end(pattern.identifier));
            ast_node_t *implicit_init_expr = ast_implicit_expr(analyzer->ast, matched_value.type, matched_value.word, token_implicit_at_end(pattern.identifier));
            ast_node_t *implicit_constant_decl = ast_decldef(analyzer->ast, pattern.identifier, implicit_type_decl, implicit_init_expr);
            implicit_constant_decl->is_mutable = false;

            declare_definition(analyzer, inferred_state.scope, implicit_constant_decl);

            resolve_declaration_definition(analyzer, analyzer->ast, inferred_state, implicit_constant_decl);
        }
    }

    ast_node_t *result = NULL;
    unless (had_error) {
        // resolve compile time params
        for (size_t i = an_func_def_arg_end(inferred_funcdef); i > an_func_def_arg_start(inferred_funcdef); --i) {
            ast_node_t *param = inferred_funcdef->children.items[i-1];
            if (param->is_compile_time_param) {
                size_t arg_index = i-1-an_func_def_arg_start(inferred_funcdef) + an_call_arg_start(inferred_funcdef);
                ast_node_t *arg = call->children.items[arg_index];
                if (TYPE_IS_UNRESOLVED(arg->value_type)) {
                    resolve_expression(analyzer, analyzer->ast, call_state, param->value_type, arg, true);
                }

                // todo: instead of copying the parameter, i can resolve the actual parameter, and then unresolve it after
                param = ast_node_copy(analyzer->ast->arena, param);
                MUST(param->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION);
                an_decl_expr(param) = arg;

                param->is_mutable = false;
                declare_definition(analyzer, inferred_state.scope, param);
                resolve_declaration_definition(analyzer, analyzer->ast, inferred_state, param);

                if (TYPE_IS_INVALID(param->value_type)) {
                    had_error = true;
                    break;
                }

                if (arg->expr_val.is_concrete) {
                    matched_value_t value = {
                        .type = arg->value_type,
                        .word = arg->expr_val.word,
                    };

                    array_push(&matched_values, value);
                } else {
                    had_error = true;
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noconst.call-arg",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("call argument '$1.$' must be a constant"),
                        .args = ORERR_ARGS(error_arg_node(arg), error_arg_token(param->identifier)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }
            }
        }

        result = find_realized_node_or_null_by_inferred_values(analyzer->ast, inferred_funcdef->realized_copies, matched_values);
        unless(result) {
            // todo: instead of copying the the definition wholesale,
            // i remove the parameters i've already copied, copy the function
            // then i add back in the old parameters to the original
            // that prevents more unnecessary allocations
            result = ast_node_copy(analyzer->ast->arena, inferred_funcdef);

            // extract compile time params and respective call args
            for (size_t i = an_func_def_arg_end(result); i > an_func_def_arg_start(result); --i) {
                ast_node_t *param = result->children.items[i-1];
                if (param->is_compile_time_param) {
                    size_t arg_index = i-1-an_func_def_arg_start(result) + an_call_arg_start(call);
                    array_remove(&result->children, i-1);
                    array_remove(&call->children, arg_index);
                }
            }

            matched_values_t matched_values_ = {.allocator=analyzer->ast->arena};
            for (size_t i = 0; i < matched_values.count; ++i) array_push(&matched_values_, matched_values.items[i]);

            inferred_copy_t copy = {
                .key = matched_values_,
                .copy = result,
            };

            array_push(&inferred_funcdef->realized_copies, copy);

            resolve_funcdef(analyzer, analyzer->ast, inferred_state, result);

            if (TYPE_IS_INVALID(result->value_type)) {
                had_error = true;
            }
        }
    }

    allocator_return(tmp);

    if (had_error) return NULL;

    return result;
}

static void ast_copy_expr_val_to_memory(ast_t *ast, ortype_t type, orword_t src, void *dest) {
    typedata_t *td = ast_type2td(ast, type);

    switch (td->kind) {
    case TYPE_NUMBER: {
        switch (td->as.num) {
        case NUM_TYPE_FLOAT: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: UNREACHABLE(); break;
            case NUM_SIZE_16: UNREACHABLE(); break;
            case NUM_SIZE_32: {
                orf32 val = (orf32)src.as.d;
                memcpy(dest, &val, sizeof(orf32));
                break;
            }

            case NUM_SIZE_64: {
                orf64 val = (orf64)src.as.d;
                memcpy(dest, &val, sizeof(orf64));
                break;
            }
            }
            break;
        }

        case NUM_TYPE_SIGNED:
        case NUM_TYPE_UNSIGNED: {
            switch ((num_size_t)td->size) {
            case NUM_SIZE_8: {
                oru8 val = (oru8)src.as.u;
                memcpy(dest, &val, sizeof(oru8));
                break;
            }

            case NUM_SIZE_16: {
                oru16 val = (oru16)src.as.u;
                memcpy(dest, &val, sizeof(oru16));
                break;
            }

            case NUM_SIZE_32: {
                oru32 val = (oru32)src.as.u;
                memcpy(dest, &val, sizeof(oru32));
                break;
            }

            case NUM_SIZE_64: {
                oru64 val = (oru64)src.as.u;
                memcpy(dest, &val, sizeof(oru64));
                break;
            }
            }
            break;
        }
        }
        break;
    }

    case TYPE_BOOL: {
        bool val = (bool)src.as.u;
        memcpy(dest, &val, sizeof(bool));
        break;
    }

    case TYPE_VOID: break;

    case TYPE_STRING:
    case TYPE_ARRAY:
    case TYPE_STRUCT: {
        if (td->size > ORWORD_SIZE) {
            void *source = src.as.p;
            memcpy(dest, source, td->size);
        } else {
            memcpy(dest, &src.as.u, td->size);
        }
        break;
    }

    case TYPE_MODULE:
    case TYPE_PARAM_STRUCT:
    case TYPE_POINTER:
    case TYPE_FUNCTION:
    case TYPE_TYPE:
    case TYPE_INFERRED_FUNCTION: {
        memcpy(dest, &src.as.u, ORWORD_SIZE);
        break;
    }

    case TYPE_COUNT:
    case TYPE_UNREACHABLE:
    case TYPE_UNRESOLVED:
    case TYPE_INVALID: UNREACHABLE(); break;
    }
}

static struct_field_t ast_struct_field_from_decl(ast_t *ast, ast_node_t *decl, arena_t *arena) {
    struct_field_t field = {0};
    field.name = sv2string(decl->identifier.view, arena);
    field.type = decl->value_type;

    orword_t word = an_decl_expr(decl)->expr_val.word;
    typedata_t *td = ast_type2td(ast, field.type);
    if (td->size > ORWORD_SIZE) {
        void *data = arena_alloc(arena, td->size);
        memcpy(data, word.as.p, td->size);
        field.default_value = ORWORDP(data);
    } else {
        field.default_value = word;
    }
    return field;
}

static void resolve_struct(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *struct_def) {
    analysis_state_t new_state = state;
    if (struct_def->defined_scope.creator == struct_def) {
        new_state.scope = &struct_def->defined_scope;
    } else {
        scope_init(&struct_def->defined_scope, analyzer->ast->arena, SCOPE_TYPE_STRUCT, state.scope, struct_def);
        new_state.scope = &struct_def->defined_scope;
    }

    if (struct_def->param_end > 0) {
        scope_init(&struct_def->defined_scope, analyzer->ast->arena, SCOPE_TYPE_INFERRED_PARAMS, state.scope, struct_def);
        new_state.scope = &struct_def->defined_scope;

        struct_def->value_type = ortypeid(TYPE_PARAM_STRUCT);
        struct_def->expr_val = ast_node_val_word(ORWORDP(struct_def));
        return;
    }

    // forward declare struct constants
    for (size_t i = an_struct_start(struct_def); i < an_struct_end(struct_def); ++i) {
        ast_node_t *decl = struct_def->children.items[i];
        if (decl->is_mutable) continue;

        declare_definition(analyzer, new_state.scope, decl);
    }

    ortype_t struct_type;
    if (TYPE_IS_UNRESOLVED(struct_def->value_type)) {
        struct_type = type_set_fetch_anonymous_incomplete_struct(&ast->type_set);
        struct_def->value_type = ortypeid(TYPE_TYPE);
        struct_def->expr_val = ast_node_val_word(ORWORDT(struct_type));
    } else {
        struct_type = struct_def->expr_val.word.as.t;
    }
    typedata_t *td = ast_type2td(ast, struct_type);
    MUST(td->kind == TYPE_STRUCT && td->as.struct_.status == STRUCT_STATUS_INCOMPLETE);

    bool invalid_type = false;

    array_push(&analyzer->pending_dependencies, struct_def);

    for (size_t i = an_struct_start(struct_def); i < an_struct_end(struct_def); ++i) {
        ast_node_t *decl = struct_def->children.items[i];
        if (!decl->is_mutable) continue;

        if (TYPE_IS_UNRESOLVED(decl->value_type)) {
            resolve_declaration_definition(analyzer, ast, new_state, decl);

            ast_node_t *init_expr = an_decl_expr(decl);
            unless (init_expr->expr_val.is_concrete) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.noconst.struct-fields",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("initial expression for fields must be compile-time constants"),
                    .args = ORERR_ARGS(error_arg_node(init_expr)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                continue;
            }

            if (TYPE_IS_INVALID(decl->value_type)) {
                invalid_type = true;
            }
        }
    }

    for (size_t i = an_struct_start(struct_def); i < an_struct_end(struct_def); ++i) {
        ast_node_t *decl = struct_def->children.items[i];
        if (decl->is_mutable) continue;

        if (TYPE_IS_UNRESOLVED(decl->value_type)) {
            resolve_declaration_definition(analyzer, ast, new_state, decl);

            if (TYPE_IS_INVALID(decl->value_type)) {
                invalid_type = true;
            }
        }
    }

    --analyzer->pending_dependencies.count;

    if (td->as.struct_.status != STRUCT_STATUS_INCOMPLETE) {
        return;
    }

    if (invalid_type) {
        type_set_invalid_struct(&ast->type_set, struct_type);
        INVALIDATE(struct_def);
        return;
    }

    tmp_arena_t *tmp = allocator_borrow();

    struct_fields_t fields = {.allocator=tmp->allocator};
    struct_fields_t consts = {.allocator=tmp->allocator};

    bool has_error = false;
    for (size_t i = an_struct_start(struct_def); i < an_struct_end(struct_def); ++i) {
        ast_node_t *decl = struct_def->children.items[i];
        struct_field_t field = ast_struct_field_from_decl(ast, decl, ast->arena);

        if (TYPE_IS_INVALID(decl->value_type)) {
            has_error = true;
            break;
        }

        array_push(&analyzer->pending_dependencies, struct_def);

        if (!retry_resolve_possible_struct_or_error(analyzer, decl, field.type)) {
            has_error = true;
        }

        --analyzer->pending_dependencies.count;

        if (decl->is_mutable) {
            array_push(&fields, field);
        } else {
            array_push(&consts, field);
        }
    }

    if (has_error) {
        type_set_invalid_struct(&ast->type_set, struct_type);
        INVALIDATE(struct_def);
    }  else {
        type_set_complete_struct(&ast->type_set, struct_type, fields, consts);
    }

    allocator_return(tmp);
}

bool ast_find_field_by_name(struct_fields_t fields, string_view_t name, struct_field_t *field, size_t *index) {
    for (size_t i = 0; i < fields.count; ++i) {
        struct_field_t field_ = fields.items[i];
        if (sv_eq(string2sv(field_.name), name)) {
            *field = field_;
            *index = i;
            return true;
        }
    }

    return false;
}

bool ast_find_struct_field_by_name(ast_t *ast, ortype_t struct_type, string_view_t name, struct_field_t *field, size_t *index) {
    typedata_t *td = ast_type2td(ast, struct_type);
    MUST(td->kind == TYPE_STRUCT || td->kind == TYPE_STRING);
    return ast_find_field_by_name(td->as.struct_.fields, name, field, index);
}

static void patch_call_argument_gaps(analyzer_t *analyzer, ast_t *ast, ast_node_t *call, struct_fields_t arg_defaults, bools_t has_defaults) {
    ast_node_t *callee = an_callee(call);
    tmp_arena_t *tmp = allocator_borrow();
    ast_nodes_t new_args = {.allocator=tmp->allocator};

    size_t arg_start = an_call_arg_start(call);
    size_t arg_end = an_call_arg_end(call);
    size_t arg_count = arg_end - arg_start;

    size_t param_index = 0;
    size_t arg_index = 0;

    bool is_invalid = false;

    token_t arg_where = callee->end;
    for (size_t i = 0; i < arg_defaults.count; ++i) {
        bool has_default = has_defaults.items[i];
        ast_node_t *arg_or_null = NULL;
        if (arg_index < arg_count && (arg_or_null = call->children.items[arg_start + arg_index])) {
            ++arg_index;
            arg_where = arg_or_null->start;

            if (arg_or_null->label.view.length > 0) {
                size_t next_param_index;
                struct_field_t field;
                bool found_param = ast_find_field_by_name(arg_defaults, arg_or_null->label.view, &field, &next_param_index);
                if (!found_param) {
                    is_invalid = true;
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nolabel.call-args",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot find argument '$0.$' for function"),
                        .args = ORERR_ARGS(error_arg_token(arg_or_null->label)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                if (next_param_index < param_index) {
                    is_invalid = true;
                    struct_field_t next_field = arg_defaults.items[next_param_index];
                    struct_field_t field = arg_defaults.items[param_index];
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noorder.call-args",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("arguments must be in function parameter order but got argument '$1.$' after argument '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(arg_or_null), error_arg_str(ast, next_field.name), error_arg_str(ast, field.name)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                param_index = next_param_index;
            } else {
                param_index = i;
            }

            arg_or_null = an_is_none(arg_or_null) ? NULL : arg_or_null;
        }

        #define error_arg_must_be_provided(index, n) do {\
            struct_field_t param_field = arg_defaults.items[index]; \
            stan_error(analyzer, OR_ERROR(\
                .tag = "sem.requires-default.func-arg."#n,\
                .level = ERROR_SOURCE_ANALYSIS,\
                .msg = lit2str("function argument '$1.$' has no default value and must be provided"),\
                .args = ORERR_ARGS(error_arg_node(call), error_arg_str(ast, param_field.name)),\
                .show_code_lines = ORERR_LINES(0),\
            ));\
        } while(false)

        if (arg_or_null == NULL && !has_default) {
            is_invalid = true;
            error_arg_must_be_provided(i, 0);
        } else {
            if (param_index > i) {
                for (size_t j = i; j < param_index; ++j) {
                    if (!has_defaults.items[j]) {
                        is_invalid = true;
                        error_arg_must_be_provided(j, 1);
                    }

                    ast_node_t *unique_nil = ast_node_new(ast->arena, AST_NODE_TYPE_NONE, arg_where);
                    array_push(&new_args, unique_nil);
                }

                i = param_index;
            }

            if (arg_or_null) {
                array_push(&new_args, arg_or_null);
            } else {
                ast_node_t *unique_nil = ast_node_new(ast->arena, AST_NODE_TYPE_NONE, arg_where);
                array_push(&new_args, unique_nil);
            }
        }

        #undef error_arg_must_be_provided
    }


    allocator_return(tmp);

    if (is_invalid) return;

    if (arg_index < arg_count) {
        struct_field_t field = arg_defaults.items[param_index];
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.extra-args.call-args",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("there are extra arguments after '$1.$'"),
            .args = ORERR_ARGS(error_arg_node(call->children.items[arg_start+arg_index]), error_arg_str(ast, field.name)),
            .show_code_lines = ORERR_LINES(0),
        ));
        return;
    }

    size_t arg_diff = arg_defaults.count - arg_count;
    for (size_t i = 0; i < arg_diff; ++i) {
        array_push(&call->children, &nil_node);
    }

    arg_end = an_call_arg_end(call);
    MUST(arg_end - arg_start == arg_defaults.count);

    for (size_t i = arg_start; i < arg_end; ++i) {
        call->children.items[i] = new_args.items[i-arg_start];
    }
}

typedef struct ast_inferred_function_t ast_inferred_function_t;
struct ast_inferred_function_t {
    struct_fields_t arg_defaults;
    bools_t has_defaults;
    ast_node_t *funcdef;
};

ast_inferred_function_t *ast_inferred_function_from_funcdef(ast_t *ast, ast_node_t *funcdef, arena_t *arena) {
    ast_inferred_function_t *func = arena_alloc(arena, sizeof(ast_inferred_function_t));
    func->funcdef = funcdef;
    func->arg_defaults = (struct_fields_t){.allocator=arena};
    func->has_defaults = (bools_t){.allocator=arena};

    for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
        ast_node_t *decl = funcdef->children.items[i];
        struct_field_t field = ast_struct_field_from_decl(ast, decl, arena);
        array_push(&func->arg_defaults, field);
        array_push(&func->has_defaults, decl->has_default_value);
    }

    return func;
}

orstring_t parse_token_as_str8(string_view_t str, arena_t *arena) {
    ++str.data;
    str.length -= 2;

    orstring_t s = sv2string(str, arena);
    return s;
}

orword_t ast_struct_item_get(typedatas_t *types, ortype_t struct_type, string_view_t field_name, void *struct_) {
    typedata_t *td = type2typedata(types, struct_type);
    MUST(td->kind == TYPE_STRING || td->kind == TYPE_STRUCT);

    struct_field_t field;
    size_t field_index;
    bool success = ast_find_field_by_name(td->as.struct_.fields, field_name, &field, &field_index);
    MUST(success);

    orword_t ret = ast_item_get(types, struct_, field.type, field.offset);
    return ret;
}

void ast_struct_item_set(ast_t *ast, ortype_t struct_type, string_view_t field_name, orword_t *struct_, orword_t value) {
    typedata_t *td = ast_type2td(ast, struct_type);
    MUST(td->kind == TYPE_STRING || td->kind == TYPE_STRUCT);

    void *addr = NULL;
    if (td->size > ORWORD_SIZE) {
        addr = struct_->as.p;
    } else {
        addr = struct_;
    }

    struct_field_t field;
    size_t field_index;
    bool success = ast_find_field_by_name(td->as.struct_.fields, field_name, &field, &field_index);
    MUST(success);

    ast_item_set(ast, field.type, addr+field.offset, value, 0);
}

static void stan_realize_parameterized_struct(analyzer_t *analyzer, analysis_state_t state, ast_node_t *param_struct_call) {
    ast_node_t *param_struct_callee = an_callee(param_struct_call);
    typedata_t *td = ast_type2td(analyzer->ast, param_struct_callee->value_type);
    MUST(td->kind == TYPE_PARAM_STRUCT);

    ast_node_t *param_struct = (ast_node_t*)param_struct_callee->expr_val.word.as.p;
    size_t param_count = an_struct_param_end(param_struct) - an_struct_param_start(param_struct);

    size_t arg_start = an_call_arg_start(param_struct_call);
    size_t arg_end = an_call_arg_end(param_struct_call);
    size_t arg_count = arg_end - arg_start;

    if (param_count != arg_count) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.arg-count-mismatch.param-struct",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("parameterized struct requires '$1.$' argument(s) but got '$2.$'"),
            .args = ORERR_ARGS(error_arg_node(param_struct_call), error_arg_sz(param_count), error_arg_sz(arg_count)),
            .show_code_lines = ORERR_LINES(0),
        ));
        INVALIDATE(param_struct_call);
        return;
    }


    tmp_arena_t *tmp = allocator_borrow();
    matched_values_t values = {.allocator=tmp->allocator};

    scope_t inferred_scope = {0};
    scope_init(&inferred_scope, analyzer->ast->arena, SCOPE_TYPE_INFERRED_PARAMS, param_struct->defined_scope.outer, param_struct_call);

    ast_nodes_t params = {.allocator=tmp->allocator};
    for (size_t i = an_struct_param_start(param_struct); i < an_struct_param_end(param_struct); ++i) {
        ast_node_t *p = ast_node_copy(analyzer->ast->arena, param_struct->children.items[i]);

        array_push(&params, p);

        declare_definition(analyzer, &inferred_scope, p);
    }

    bool is_invalid = false;
    for (size_t i = 0; i < params.count; ++i) {
        ast_node_t *param = params.items[i];

        resolve_declaration_definition(analyzer, analyzer->ast, (analysis_state_t){.scope=&inferred_scope}, param);
        param->is_mutable = false;

        ortype_t implicit_type = TYPE_IS_INVALID(param->value_type) ? ortypeid(TYPE_UNRESOLVED) : param->value_type;

        size_t arg_index = i+an_call_arg_start(param_struct_call);
        ast_node_t *arg = param_struct_call->children.items[arg_index];
        resolve_expression(analyzer, analyzer->ast, state, implicit_type, arg, true);

        param->expr_val = arg->expr_val;

        if (!ortypeid_eq(param->value_type, arg->value_type)) {
            is_invalid = true;
            if (!TYPE_IS_INVALID(param->value_type) && !TYPE_IS_INVALID(arg->value_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.type-mismatch.struct-param",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("parameterized struct arguments require exact type matches; struct parameter '$1.$' is type '$2.$' but argument is '$3.$'"),
                    .args = ORERR_ARGS(error_arg_node(arg),
                            error_arg_token(param->identifier),
                            error_arg_type(param->value_type), error_arg_type(arg->value_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }

            INVALIDATE(param_struct_call);
            break;
        }

        if (!arg->expr_val.is_concrete) {
            is_invalid = true;
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.noconst.struct-args",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("parameterized struct arugments must be compile-time constants"),
                .args = ORERR_ARGS(error_arg_node(arg)),
                .show_code_lines = ORERR_LINES(0),
            ));
            INVALIDATE(param_struct_call);
            break;
        }

        matched_value_t val = {
            .type = arg->value_type,
            .word = arg->expr_val.word,
        };

        array_push(&values, val);
    }

    if (is_invalid) goto defer;

    ast_node_t *realized_struct = find_realized_node_or_null_by_inferred_values(analyzer->ast, param_struct->realized_copies, values);
    if (!realized_struct) {
        // copy param struct without params...
        {
            ast_node_t **children = param_struct->children.items;
            param_struct->children.items += an_struct_start(param_struct);
            size_t child_count = param_struct->children.count;
            param_struct->children.count = an_struct_end(param_struct) - an_struct_start(param_struct);

            realized_struct = ast_node_copy(analyzer->ast->arena, param_struct);
            realized_struct->defined_scope.creator = NULL;
            realized_struct->value_type = ortypeid(TYPE_UNRESOLVED);
            realized_struct->expr_val = ast_node_val_nil();
            realized_struct->param_end = 0;

            param_struct->children.items = children;
            param_struct->children.count = child_count;
        }

        matched_values_t key = {.allocator=analyzer->ast->arena};
        for (size_t i = 0; i < values.count; ++i) {
            array_push(&key, values.items[i]);
        }

        inferred_copy_t copy = {
            .key = key,
            .copy = realized_struct,
        };

        array_push(&param_struct->realized_copies, copy);

        {
            analysis_state_t new_state = {.scope=&inferred_scope};

            resolve_expression(analyzer, analyzer->ast, new_state, ortypeid(TYPE_UNRESOLVED), realized_struct, true);
        }

        if (!TYPE_IS_INVALID(realized_struct->value_type)) {
            ortype_t struct_type = realized_struct->expr_val.word.as.t;

            struct_fields_t params = {.allocator=tmp->allocator};
            for (size_t i = 0; i < values.count; ++i) {
                struct_field_t field = {0};
                field.type = values.items[i].type;
                field.default_value = values.items[i].word;

                array_push(&params, field);
            }

            type_set_attach_params_to_struct_type(&analyzer->ast->type_set, struct_type, params);
        }
    }

    if (realized_struct == NULL || TYPE_IS_INVALID(realized_struct->value_type)) {
        INVALIDATE(param_struct_call);
        goto defer;
    }

    MUST(TYPE_IS_TYPE(realized_struct->value_type));
    MUST(realized_struct->expr_val.is_concrete);
    param_struct_call->value_type = ortypeid(TYPE_TYPE);
    param_struct_call->expr_val = realized_struct->expr_val;

defer: 
    allocator_return(tmp);
}

static void resolve_call(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *call, ortype_t implicit_type) {
    ast_node_t *callee = an_callee(call);
    resolve_expression(analyzer, ast, state, implicit_type, callee, true);

    ortype_t callee_type = callee->value_type;
    typedata_t *callee_td = ast_type2td(ast, callee_type);

    call->value_type = ortypeid(TYPE_INVALID);

    if (callee_td->kind == TYPE_FUNCTION || callee_td->kind == TYPE_INFERRED_FUNCTION) {
        size_t arg_start = an_call_arg_start(call);
        size_t arg_end = an_call_arg_end(call);
        size_t arg_count = arg_end - arg_start;
        // cannot use labelled arguments on variables
        if (!callee->expr_val.is_concrete) {
            if (arg_count != callee_td->as.function.argument_types.count) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.all-args-required.nonconst-funcs",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("for calls on non-constant functions, all arguments are required; got '$1.$' arguments instead of '$2.$'"),
                    .args = ORERR_ARGS(error_arg_node(call), error_arg_sz(arg_count), error_arg_sz(callee_td->as.function.argument_types.count)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                return;
            }

            for (size_t i = 0; i < arg_count; ++i) {
                size_t argi = arg_start + i;
                ast_node_t *arg = call->children.items[argi];
                if (arg->label.view.length > 0) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nolabel.nonconst-func",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot use labels on arguments for calls on non-constant functions"),
                        .args = ORERR_ARGS(error_arg_token(arg->label)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    return;
                }

                if (an_is_none(arg)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.args-required.nonconst.func",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot omit arguments for a call on a non-constant function"),
                        .args = ORERR_ARGS(error_arg_node(arg)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    return;
                }
            }
        // reconstruct/fill arguments for constant calls 
        } else {
            if (callee_td->kind == TYPE_INFERRED_FUNCTION) {
                ast_inferred_function_t *inferred_decl = callee->expr_val.word.as.p;
                patch_call_argument_gaps(analyzer, ast, call, inferred_decl->arg_defaults, inferred_decl->has_defaults);
            } else {
                function_t *function = (function_t*)callee->expr_val.word.as.p;
                patch_call_argument_gaps(analyzer, ast, call, function->arg_defaults, function->has_defaults);
            }
            arg_end = an_call_arg_end(call);
            arg_count = arg_end-arg_start;
        }

        if (callee_td->kind == TYPE_INFERRED_FUNCTION) {
            // im not sure if this is possible
            unless (callee->expr_val.is_concrete) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.noconst.inferred-func-decl-callee",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("callee is an inferred function declaration and must be a constant"),
                    .args = ORERR_ARGS(error_arg_node(callee)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(call);
                return;
            }

            ast_inferred_function_t *inferred_funcdef = (ast_inferred_function_t*)callee->expr_val.word.as.p;

            ast_node_t *realized_funcdef = stan_realize_inferred_funcdefcall_or_errornull(analyzer, state, call, inferred_funcdef->funcdef);

            unless (realized_funcdef) {
                INVALIDATE(call);
                return;
            }

            if (TYPE_IS_INVALID(realized_funcdef->value_type)) {
                INVALIDATE(call);
                return;
            }

            {
                function_t *function = (function_t*)realized_funcdef->expr_val.word.as.p;
                bool passed_through_fold = false;
                size_t dep_start = stan_function_is_building(analyzer, function, &passed_through_fold);
                if (dep_start < analyzer->pending_dependencies.count) {
                    stan_circular_dependency_error(analyzer, ast, callee, dep_start);
                }
            }

            callee = ast_implicit_expr(ast, realized_funcdef->value_type, realized_funcdef->expr_val.word, callee->start);
            callee_type = callee->value_type;
            callee_td = ast_type2td(ast, callee_type);
            an_callee(call) = callee;

            arg_end = an_call_arg_end(call);
            arg_count = arg_end-arg_start;
        }

        if (!TYPE_IS_INVALID(callee_type)) {
            call->value_type = callee_td->as.function.return_type;
        }

        if (callee->expr_val.is_concrete) {
            function_t *function = (function_t*)callee->expr_val.word.as.p;
            for (size_t i = arg_start; i < arg_end; ++i) {
                ast_node_t *arg = call->children.items[i];
                if (an_is_none(arg)) {
                    struct_field_t field = function->arg_defaults.items[i - arg_start];
                    arg = ast_implicit_expr(ast, field.type, field.default_value, arg->start);
                    call->children.items[i] = arg;
                }
            }
        }

        bool argument_invalid = false;
        for (size_t i = an_call_arg_start(call); i < an_call_arg_end(call); ++i) {
            ast_node_t *argument = call->children.items[i];

            size_t argi = i - an_call_arg_start(expr);
            
            ortype_t arg_implicit_type = ortypeid(TYPE_UNRESOLVED);
            if (argi < callee_td->as.function.argument_types.count) {
                arg_implicit_type = callee_td->as.function.argument_types.items[argi];
            }

            resolve_expression(analyzer, ast, state, arg_implicit_type, argument, true);
            if (TYPE_IS_INVALID(argument->value_type)) {
                argument_invalid = true;
            }
        }

        if (argument_invalid || TYPE_IS_INVALID(callee->value_type)) {
            return;
        }

        for (size_t i = an_call_arg_start(call); i < an_call_arg_end(call); ++i) {
            ast_node_t *arg = call->children.items[i];
            if (TYPE_IS_INVALID(arg->value_type)) continue;
            size_t i_= i - an_call_arg_start(expr);

            if (i_ < callee_td->as.function.argument_types.count) {
                arg = cast_implicitly_if_necessary(ast, callee_td->as.function.argument_types.items[i_], arg);
                call->children.items[i] = arg;
            }
        }

        check_call_on_func_or_error(analyzer, ast, call);
    } else if (callee_td->kind == TYPE_PARAM_STRUCT) {
        MUST(callee->expr_val.is_concrete);
        stan_realize_parameterized_struct(analyzer, state, call);

    } else {
        unless (TYPE_IS_INVALID(callee_type)) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.nocallable.call",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("expected a function or parameterized struct but got '$0.type$' instead"),
                .args = ORERR_ARGS(error_arg_node(callee)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
        return;
    }
}

orstring_t ast_orstr2str(type_table_t *type_set, void *start) {
    MUST(type2typedata(&type_set->types, type_set->str8_t_)->size > ORWORD_SIZE);

    orword_t cstr = ast_struct_item_get(&type_set->types, type_set->str8_t_, lit2sv("cstr"), start);
    orword_t length = ast_struct_item_get(&type_set->types, type_set->str8_t_, lit2sv("length"), start);
    orstring_t s = {
        .cstr = cstr.as.p,
        .length = (size_t)length.as.u
    };

    return s;
}

static ffi_t *make_ffi(ast_t *ast) {
    ffi_t *ffi = arena_alloc(ast->arena, sizeof(ffi_t));
    ffi->arg_types = (types_t){.allocator=ast->arena};
    ffi->funcname = lit2str("");
    ffi->libpath = lit2str("");
    ffi->callconv = lit2str("");
    ffi->node = &nil_node;
    ffi->return_type = ortypeid(TYPE_INVALID);
    return ffi;
}


orstring_t ast_generate_moduleid(orstring_t file_path, arena_t *arena) {
    tmp_arena_t *tmp = allocator_borrow();

    orstring_t absolute_path;
    bool success = core_abspath(file_path, tmp->allocator, &absolute_path);
    MUST(success);

    string_builder_t sb = {.allocator=tmp->allocator};
    success = core_fileid(absolute_path, &sb);
    MUST(success);

    orstring_t id = bytes2alphanum(sb.items, sb.count, arena);

    allocator_return(tmp);

    return id;
}

static ast_node_t *stan_find_owning_module_or_null(scope_t *scope) {
    while (scope && scope->type != SCOPE_TYPE_MODULE) {
        scope = scope->outer;
    }

    if (scope) {
        return scope->creator;
    } else {
        return NULL;
    }
}

static void resolve_module(analyzer_t *analyzer, ast_t *ast, ast_node_t *module) {
    scope_init(&module->defined_scope, ast->arena, SCOPE_TYPE_MODULE, NULL, module);
    analysis_state_t state = (analysis_state_t) {
        .scope = &module->defined_scope
    };

    forward_scan_constant_names(analyzer, &module->defined_scope, module->children);
    size_t last_decl = resolve_declarations_until_unreachable(analyzer, ast, state, module->children, false);
    if (last_decl < module->children.count) {
        module->children.count = last_decl+1;
    }
}

static bool resolve_directive_argument_or_error(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *directive, ast_node_t *arg, size_t arg_zero_based_pos, ortype_t expected_type, bool arg_must_be_compile_time) {
    resolve_expression(analyzer, ast, state, ortypeid(TYPE_STRING), arg, true);
    if (!ortypeid_eq(expected_type, arg->value_type)) {
        if (!TYPE_IS_INVALID(arg->value_type)) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.type-mismatch.directive",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("'$4.$' argument $3.$ requires a '$1.$' but got '$2.$'"),
                .args = ORERR_ARGS(error_arg_node(arg), error_arg_type(expected_type), error_arg_type(arg->value_type), error_arg_sz(arg_zero_based_pos + 1), error_arg_node(directive)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
        return false;
    }

    if (arg_must_be_compile_time && !arg->expr_val.is_concrete) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.noconst.directive",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("'$2.$' argument $1.$ must be a constant"),
            .args = ORERR_ARGS(error_arg_node(arg), error_arg_sz(arg_zero_based_pos+1), error_arg_node(directive)),
            .show_code_lines = ORERR_LINES(0),
        ));
        return false;
    }

    return true;
}

static ast_node_t *stan_load_module_or_errornull(analyzer_t *analyzer, ast_t *ast, ast_node_t *arg_ref, string_view_t module_path) {
    tmp_arena_t *tmp = allocator_borrow();
    orstring_t s = sv2string(module_path, tmp->allocator);

    ast_node_t *result = NULL;
    Nob_String_Builder sb = {0};
    
    orstring_t moduleid = ast_generate_moduleid(s, tmp->allocator);

    ast_node_t *module;
    if (table_get(s2n, ast->moduleid2node, moduleid, &module)) {
        nob_return_defer(module);
    }

    orstring_t source;
    if (!nob_read_entire_file(s.cstr, &sb)) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.nomodule.loadmodule",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("cannot find module: '$1.$'"),
            .args = ORERR_ARGS(error_arg_node(arg_ref)),
            .show_code_lines = ORERR_LINES(0),
        ));
        nob_return_defer(NULL);
    }

    source = cstrn2string(sb.items, sb.count, ast->arena);

    module = parse_source_into_module(ast, s, string2sv(source));

    ast_add_module(ast, module, moduleid);

    resolve_module(analyzer, ast, module);
    
    nob_return_defer(module);

defer:
    nob_sb_free(sb);
    allocator_return(tmp);
    return result;
}

bool check_dir_or_intr_params_or_error(analyzer_t *analyzer, analysis_state_t state, ast_t *ast, ast_node_t *call, size_t start, size_t count, orintrinsic_fn_t fn, bool args_must_be_compile_time) {
    size_t expected_arg_count = fn.arg_types.count;
    if (fn.has_varargs) {
        if (count < expected_arg_count) {
            orstring_t directive_name = sv2string(call->identifier.view, ast->arena);
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.directive.not-enough-args",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("'$1.$' requires at least $2.$ arguments but got $3.$"),
                .args = ORERR_ARGS(error_arg_node(call), error_arg_str(ast, directive_name),
                        error_arg_sz(expected_arg_count), error_arg_sz(count)),
                .show_code_lines = ORERR_LINES(0),
            ));

            return false;
        }
    } else {
        if (count != expected_arg_count) {
            orstring_t directive_name = sv2string(call->identifier.view, ast->arena);
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.directive.not-enough-args",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("'$1.$' requires exactly $2.$ arguments but got $3.$"),
                .args = ORERR_ARGS(error_arg_node(call), error_arg_str(ast, directive_name),
                        error_arg_sz(expected_arg_count), error_arg_sz(count)),
                .show_code_lines = ORERR_LINES(0),
            ));

            return false;
        }
    }

    for (size_t i = 0; i < expected_arg_count; ++i) {
        size_t argi = start + i;
        ast_node_t *arg = call->children.items[argi];
        ortype_t expected_type = fn.arg_types.items[i];

        if (!resolve_directive_argument_or_error(analyzer, ast, state, call, arg, i, expected_type, args_must_be_compile_time)) {
            return false;
        }
    }

    return true;
}
void resolve_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ortype_t implicit_type,
        ast_node_t *expr,
        bool is_consumed) {
    
    ASSERT(ast_node_type_is_expression(expr->node_type), "should be only expressions");

    expr->is_consumed = is_consumed;

    if (TYPE_IS_RESOLVED(expr->value_type)) {
        return;
    }

    switch (expr->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE: {
            orintrinsic_fn_t fn = {0};
            {
                string_view_t directive_name = expr->identifier.view;
                directive_name.data += 1;
                --directive_name.length;
                if (!ast_find_intrinsic_funcname(ast->directives, directive_name, &fn)) {
                    orstring_t directive_name = sv2string(expr->identifier.view, ast->arena);
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nodirecive.directivecall",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$1.$' is not a valid directive"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_str(ast, directive_name)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }
            }

            if (sv_eq(expr->identifier.view, lit2sv("@run"))) {
                scope_t scope = {0};
                scope_init(&scope, analyzer->ast->arena, SCOPE_TYPE_FOLD_DIRECTIVE, state.scope, expr);
                analysis_state_t new_state = state;
                new_state.scope = &scope;

                array_push(&analyzer->pending_dependencies, expr);

                ast_node_t *child = expr->children.items[0];
                resolve_expression(analyzer, ast, new_state, implicit_type, child, true);

                --analyzer->pending_dependencies.count;

                if (!analyzer->had_error && analyzer->run_vm) {
                    for (size_t i = 0; i < analyzer->run_required_uncompiled_funcdefs.count; ++i) {
                        ast_node_t *funcdef = analyzer->run_required_uncompiled_funcdefs.items[i];
                        gen_funcdef(ast, analyzer->run_vm, funcdef);
                    }

                    analyzer->run_required_uncompiled_funcdefs.count = 0;

                    unless (TYPE_IS_INVALID(child->value_type)) {
                        typedata_t *td = ast_type2td(ast, child->value_type);

                        orword_t result;
                        bool success = stan_run(analyzer, analyzer->run_vm, child, &result);
                        if (td->size > ORWORD_SIZE) {
                            void *word = ast_multiword_value(ast, orb2w2b(td->size));
                            ast_copy_expr_val_to_memory(ast, child->value_type, result, word);
                            result.as.p = word;
                        }

                        if (success) {
                            expr->expr_val = ast_node_val_word(result);
                            expr->value_type = child->value_type;
                        } else {
                            INVALIDATE(expr);
                        }
                    } else {
                        INVALIDATE(expr);
                    }
                } else {
                    // make sure we cannot compile
                    analyzer->had_error = true;

                    // todo: output a warning that is not buildable or something
                    expr->value_type = child->value_type;
                    expr->expr_val = child->expr_val;
                }

            } else {
                size_t dir_arg_count = an_dir_arg_end(expr) - an_dir_arg_start(expr);
                if (!check_dir_or_intr_params_or_error(analyzer, state, ast, expr, an_dir_arg_start(expr), dir_arg_count, fn, true)) {
                    INVALIDATE(expr);
                    break;
                }

                if (sv_eq(expr->identifier.view, lit2sv("@load"))) {
                    ast_node_t *owning_module = stan_find_owning_module_or_null(state.scope);
                    MUST(owning_module);

                    ast_node_t *module_path_node = expr->children.items[an_dir_arg_start(expr)];
                    orstring_t module_path = ast_orstr2str(&ast->type_set, module_path_node->expr_val.word.as.p);

                    ast_node_t *module = stan_load_module_or_errornull(analyzer, ast, module_path_node, string2sv(module_path));
                    expr->expr_val = ast_node_val_word(ORWORDP(module));
                    expr->value_type = ortypeid(TYPE_MODULE);

                    array_push(&owning_module->module_deps, module);
                } else if (sv_eq(expr->identifier.view, lit2sv("@fficall"))) {
                    tmp_arena_t *tmp = allocator_borrow();
                    ortype_t fficall_return_type = ortypeid(TYPE_INVALID);

                    ast_node_t *libpath_node = an_fficall_libpath(expr);
                    orstring_t libpath = ast_orstr2str(&ast->type_set, libpath_node->expr_val.word.as.p);
                    orstring_t abslibpath;
                    if (!core_abspath(libpath, tmp->allocator, &abslibpath)) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.inarg.fficall-libpath",
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("'$1.$' cannot get library absolute path"),
                            .args = ORERR_ARGS(error_arg_node(expr->children.items[0]), error_arg_str(ast, libpath)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                    }

                    ast_node_t *callconv_node = an_fficall_callconv(expr);
                    ast_node_t *retarg_node = an_fficall_rettype(expr);
                    fficall_return_type = retarg_node->expr_val.word.as.t;

                    ast_node_t *funcname_node = an_fficall_funcname(expr);

                    size_t arg_count = an_fficall_arg_end(expr) - an_fficall_arg_start(expr);

                    types_t types = {.allocator=tmp->allocator};

                    for (size_t i = an_fficall_arg_start(expr); i < an_fficall_arg_end(expr); ++i) {
                        ast_node_t *arg = expr->children.items[i];
                        resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), arg, true);
                        if (!TYPE_IS_INVALID(arg->value_type) && TYPE_IS_RESOLVED(arg->value_type)) {
                            array_push(&types,arg->value_type);
                        }
                    }

                    orstring_t funcname = ast_orstr2str(&ast->type_set, funcname_node->expr_val.word.as.p);
                    orstring_t key = string_format("%s:%s", tmp->allocator, abslibpath.cstr, funcname.cstr);
                    ffi_t *ffi;
                    if (!table_get(s2fis, ast->ffis, key, &ffi)) {
                        ffi = make_ffi(ast);
                        ffi->funcname = string_copy(funcname, ast->arena);
                        ffi->libpath = string_copy(abslibpath, ast->arena);
                        ffi->callconv = ast_orstr2str(&ast->type_set, callconv_node->expr_val.word.as.p);
                        ffi->return_type = fficall_return_type;
                        ffi->node = expr;
                        for (size_t i = 0; i < types.count; ++i) {
                            ortype_t arg_type = types.items[i];
                            array_push(&ffi->arg_types, arg_type);
                        }

                        key = string_copy(key, ast->arena);
                        table_put(s2fis, ast->ffis, key, ffi);
                    } else {
                        if (!ortypeid_eq(ffi->return_type, fficall_return_type)) {
                            stan_error(analyzer, OR_ERROR(
                                .tag = "sem.fficall-mismatch.return-type",
                                .level = ERROR_SOURCE_ANALYSIS,
                                .msg = lit2str("this fficall uses a '$1.$' return type but the first fficall analyzed uses '$2.$'"),
                                .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(fficall_return_type), error_arg_type(ffi->return_type),
                                        error_arg_node(ffi->node)),
                                .show_code_lines = ORERR_LINES(0, 3),
                            ));
                            goto defer;
                        }

                        if (arg_count != ffi->arg_types.count) {
                            stan_error(analyzer, OR_ERROR(
                                .tag = "sem.fficall-mismatch.arg",
                                .level = ERROR_SOURCE_ANALYSIS,
                                .msg = lit2str("this fficall has '$1.$' argument(s) but the first fficall analyzed uses '$2.$'"),
                                .args = ORERR_ARGS(error_arg_node(expr), error_arg_sz(arg_count), error_arg_sz(ffi->arg_types.count),
                                        error_arg_node(ffi->node)),
                                .show_code_lines = ORERR_LINES(0, 3),
                            ));
                            goto defer;
                        }

                        bool match = true;
                        for (size_t i = 0; i < arg_count; ++i) {
                            ortype_t expected = ffi->arg_types.items[i];
                            ortype_t actual = types.items[i];
                            if (!ortypeid_eq(expected, actual)) {
                                match = false;
                                stan_error(analyzer, OR_ERROR(
                                    .tag = "sem.fficall-mismatch.arg-type-mismatch",
                                    .level = ERROR_SOURCE_ANALYSIS,
                                    .msg = lit2str("this fficall uses a '$1.$' type for argument '$2.$' but the first fficall analyzed uses '$3.$' for the same argument"),
                                    .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(actual), error_arg_sz(i+1), error_arg_type(expected),
                                            error_arg_node(ffi->node)),
                                    .show_code_lines = ORERR_LINES(0, 4),
                                ));
                            }
                        }

                        if (!match) {
                            goto defer;
                        }
                    }

                    expr->ffi_or_null = ffi;

                defer:
                    allocator_return(tmp);

                    expr->value_type = fficall_return_type;
                } else if (sv_eq(expr->identifier.view, lit2sv("@icall"))) {
                    ast_node_t *function_name_node = expr->children.items[0];
                    orstring_t fn_name = ast_orstr2str(&ast->type_set, function_name_node->expr_val.word.as.p);

                    orintrinsic_fn_t in = {0};
                    if (!ast_find_intrinsic_funcname(ast->intrinsics, string2sv(fn_name), &in)) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.nodirecive.directivecall",
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("'$1.$' is not a valid intrinsic function name"),
                            .args = ORERR_ARGS(error_arg_node(expr), error_arg_str(ast, fn_name)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        INVALIDATE(expr);
                        break;
                    }

                    expr->value_type = in.ret_type;

                    size_t icall_count = expr->children.count - 1;
                    check_dir_or_intr_params_or_error(analyzer, state, ast, expr, 1, icall_count, in, false);
                    expr->intrinsic_fn = in;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            resolve_expression(analyzer, ast, state, implicit_type, an_operand(expr), is_consumed);
            expr->lvalue_node = an_operand(expr)->lvalue_node;
            expr->value_type = an_operand(expr)->value_type;
            expr->expr_val = an_operand(expr)->expr_val;
            expr->is_free_number = an_operand(expr)->is_free_number;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_TYPE: {
            ast_node_t *type_expr = an_array_type_expr(expr);
            resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), type_expr, true);

            if (!TYPE_IS_TYPE(type_expr->value_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.type-mismatch.arr-type",
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
                    .tag = "sem.noconst.arr-type",
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
                resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), size_expr, true);

                if (TYPE_IS_INVALID(size_expr->value_type)) {
                    INVALIDATE(expr);
                    break;
                }

                typedata_t *td = ast_type2td(ast, size_expr->value_type);
                if (td->kind != TYPE_NUMBER) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nonumber.arr-size",
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
                        .tag = "sem.float.arr-size",
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
                        .tag = "sem.noconst.arr-size",
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
                        .tag = "sem.nopos.arr-size",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("array size must be a positve integer"),
                        .args = ORERR_ARGS(error_arg_node(size_expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }
            } else {
                MUST(false); // todo: error maybe or inferred idk
            }

            ortype_t array_value_type = type_expr->expr_val.word.as.t;
            if (!retry_resolve_possible_struct_or_error(analyzer, type_expr, array_value_type)) {
                INVALIDATE(expr);
                break;
            }

            ortype_t array_type = type_set_fetch_array(&ast->type_set, array_value_type, size_expr->expr_val.word.as.u);

            expr->value_type = ortypeid(TYPE_TYPE);
            expr->expr_val = ast_node_val_word(ORWORDT(array_type));
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ARRAY_ITEM_ACCESS: {
            ast_node_t *accessee = an_item_accessee(expr);
            resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), accessee, true);

            ast_node_t *accessor = an_item_accessor(expr);
            resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), accessor, true);

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

            ortype_t item_type = ortypeid(TYPE_INVALID);
            // accessee
            {
                if (accessee_td->kind != TYPE_ARRAY && accessee_td->kind != TYPE_POINTER) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.inarg.accessee-type",
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
                    ortype_t ptr_inner = accessee_td->as.ptr.type;
                    typedata_t *ptr_inner_td = ast_type2td(ast, ptr_inner);

                    if (ptr_inner_td->kind != TYPE_ARRAY) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.noptr-to-array.accessee-type",
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
                        .tag = "sem.noint.accessor",
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
                // UNREACHABLE();
            }

            break;
        }

        case AST_NODE_TYPE_EXPRESSION_NIL: {
            expr->value_type = ortypeid(TYPE_UNRESOLVED);
            break;
        }
        
        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: {
            // inferred type decls are forward scanned and converted to value_defs, and so if they are present anywhere
            // else they are an error
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.inferred-def.outside-funcdef",
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
                    type_expr = ast_implicit_expr(ast, ortypeid(TYPE_TYPE), ORWORDT(implicit_type), token_implicit_at_start(expr->start));
                    an_list_lhs(expr) = type_expr;
                } else {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noinfer.init-list",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("no implicit type can be inferred for initializer list"),
                        .args = ORERR_ARGS(error_arg_node(expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }
            } else {
                resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), type_expr, true);
            }

            typedata_t *type_expr_td = ast_type2td(ast, type_expr->value_type);
            switch (type_expr_td->kind) {
            case TYPE_TYPE: {
                if (!type_expr->expr_val.is_concrete) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noconst.init-list",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("expected constant for initializer list"),
                        .args = ORERR_ARGS(error_arg_node(type_expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                ortype_t init_type = type_expr->expr_val.word.as.t;
                typedata_t *init_type_td = ast_type2td(ast, init_type);

                expr->value_type = init_type;

                switch (init_type_td->kind) {
                case TYPE_ARRAY: {
                    ortype_t arg_implicit_type;
                    {
                        ortype_t t = type_expr->expr_val.word.as.t;
                        typedata_t *td = ast_type2td(ast, t);
                        arg_implicit_type = td->as.arr.type;
                    }

                    bool invalid_arg = false;
                    for (size_t i = an_list_start(expr); i < an_list_end(expr); ++i) {
                        ast_node_t *arg = expr->children.items[i];

                        resolve_expression(analyzer, ast, state, arg_implicit_type, arg, true);

                        if (TYPE_IS_INVALID(arg->value_type)) {
                            invalid_arg = true;
                        }
                    }

                    if (invalid_arg) {
                        break;
                    }

                    ortype_t array_type = init_type_td->as.arr.type;

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

                        if (!ortypeid_eq(arg->value_type, array_type)) {
                            size_t arg_index = i - an_list_start(expr);
                            stan_error(analyzer, OR_ERROR(
                                .tag = "sem.type-mismatch.arr-ele",
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
                            .tag = "sem.no-many-args.init-list",
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("array has length of $1.$ but is initialized with $2.$"),
                            .args = ORERR_ARGS(error_arg_node(expr),
                                error_arg_sz(init_type_td->as.arr.count), error_arg_sz(arg_count)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                        break;
                    }

                    if (is_constant) {
                        orword_t *start;
                        if (init_type_td->size > ORWORD_SIZE) {
                            start = ast_multiword_value(ast, orb2w(init_type_td->size));
                            expr->expr_val = ast_node_val_word(ORWORDP(start));
                        } else {
                            expr->expr_val.is_concrete = true;
                            start = &expr->expr_val.word;
                        }

                        // constant fold  arguments
                        for (size_t i = an_list_start(expr); i < an_list_end(expr); ++i) {
                            size_t offset = i - an_list_start(expr);
                            ast_node_t *arg = expr->children.items[i];
                            
                            typedata_t *itemtd = ast_type2td(ast, init_type_td->as.arr.type);
                            size_t size_aligned = td_align(itemtd->size, itemtd->alignment);
                            ast_copy_expr_val_to_memory(ast, arg->value_type, arg->expr_val.word, ((void*)start)+(offset*size_aligned));
                        }
                    }
                    break;
                }

                case TYPE_STRING:
                case TYPE_STRUCT: {
                    tmp_arena_t *tmp = allocator_borrow();

                    bool is_invalid = false;

                    sizes_t used_field_indices = {.allocator=tmp->allocator};
                    sizes_t arg_indices = {.allocator=tmp->allocator};
                    {
                        size_t current_field_index = 0;
                        for (size_t i = an_list_start(expr); i < an_list_end(expr); ++i) {
                            ast_node_t *arg = expr->children.items[i];

                            if (current_field_index >= init_type_td->as.struct_.fields.count) {
                                is_invalid = true;
                                stan_error(analyzer, OR_ERROR(
                                    .tag = "sem.too-many-args.init-list.2",
                                    .level = ERROR_SOURCE_ANALYSIS,
                                    .msg = lit2str("too many arguments; expected no more than '$1.$' but initializer is` at argument '$2.$'"),
                                    .args = ORERR_ARGS(error_arg_node(arg),
                                                    error_arg_sz(init_type_td->as.struct_.fields.count),
                                                    error_arg_sz(current_field_index+1)),
                                    .show_code_lines = ORERR_LINES(0),
                                ));
                                break;
                            }

                            if (an_is_notnone(arg)) {
                                array_push(&arg_indices, i);
                                if (arg->label.view.length > 0) {
                                    struct_field_t field;
                                    size_t field_index;
                                    bool success = ast_find_struct_field_by_name(ast, init_type, arg->label.view, &field, &field_index);
                                    if (!success) {
                                        is_invalid = true;
                                        stan_error(analyzer, OR_ERROR(
                                            .tag = "sem.nomember.struct-init",
                                            .level = ERROR_SOURCE_ANALYSIS,
                                            .msg = lit2str("cannot find field '$0.$' in struct"),
                                            .args = ORERR_ARGS(error_arg_token(arg->label)),
                                            .show_code_lines = ORERR_LINES(0),
                                        ));
                                        break;
                                    }

                                    if (field_index < current_field_index) {
                                        is_invalid = true;
                                        stan_error(analyzer, OR_ERROR(
                                            .tag = "sem.noorder.struct-init",
                                            .level = ERROR_SOURCE_ANALYSIS,
                                            .msg = lit2str("field '$0.$' is argument '$1.$' but is placed after argument '$2.$'"),
                                            .args = ORERR_ARGS(error_arg_token(arg->label), error_arg_sz(field_index), error_arg_sz(current_field_index-1)),
                                            .show_code_lines = ORERR_LINES(0),
                                        ));
                                        break;
                                    }

                                    array_push(&used_field_indices, field_index);
                                    current_field_index = field_index+1;

                                } else {
                                    array_push(&used_field_indices, current_field_index);
                                    ++current_field_index;
                                }
                            } else {
                                ++current_field_index;
                            }
                        }
                    }

                    if (is_invalid) {
                        allocator_return(tmp);
                        break;
                    }

                    bool is_constant = true;
                    size_t used_field_index = 0;
                    for (size_t i = 0; i < init_type_td->as.struct_.fields.count; ++i) {
                        struct_field_t field = init_type_td->as.struct_.fields.items[i];
                        if (used_field_index < used_field_indices.count && i == used_field_indices.items[used_field_index]) {
                            size_t arg_index = arg_indices.items[used_field_index];
                            ++used_field_index;

                            ast_node_t *arg = expr->children.items[arg_index];
                            resolve_expression(analyzer, ast, state, field.type, arg, true);

                            arg = cast_implicitly_if_necessary(ast, field.type, arg);
                            expr->children.items[arg_index] = arg;

                            arg->arg_index = i;
                            arg->value_offset = field.offset;

                            if (!ortypeid_eq(arg->value_type, field.type)) {
                                is_invalid = true;
                                stan_error(analyzer, OR_ERROR(
                                    .tag = "sem.type-mismatch.struct-init",
                                    .level = ERROR_SOURCE_ANALYSIS,
                                    .msg = lit2str("setting struct field requires explicit cast from '$1.$' to '$2.$'"),
                                    .args = ORERR_ARGS(error_arg_node(arg),
                                        error_arg_type(arg->value_type), error_arg_type(field.type)),
                                    .show_code_lines = ORERR_LINES(0),
                                ));
                                break;
                            }

                            unless (arg->expr_val.is_concrete) {
                                is_constant = false;
                            }
                        }
                    }

                    if (is_invalid) {
                        allocator_return(tmp);
                        break;
                    }

                    if (is_constant) {
                        orword_t *start;
                        if (init_type_td->size > ORWORD_SIZE) {
                            start = ast_multiword_value(ast, orb2w(init_type_td->size));
                            expr->expr_val = ast_node_val_word(ORWORDP(start));
                        } else {
                            expr->expr_val.is_concrete = true;
                            start = &expr->expr_val.word;
                        }

                        used_field_index = 0;
                        for (size_t i = 0; i < init_type_td->as.struct_.fields.count; ++i) {
                            struct_field_t field = init_type_td->as.struct_.fields.items[i];
                            size_t offset = field.offset;
                            if (used_field_index < used_field_indices.count && i == used_field_indices.items[used_field_index]) {
                                size_t arg_index = arg_indices.items[used_field_index];
                                ++used_field_index;

                                ast_node_t *arg = expr->children.items[arg_index];
                                ast_copy_expr_val_to_memory(ast, arg->value_type, arg->expr_val.word, ((void*)start) + offset);
                            } else {
                                ast_copy_expr_val_to_memory(ast, field.type, field.default_value, ((void*)start) + offset);
                            }

                        }
                    }

                    allocator_return(tmp);
                    break;
                }

                case TYPE_BOOL:
                case TYPE_TYPE: 
                case TYPE_NUMBER:
                case TYPE_FUNCTION:
                case TYPE_POINTER: {
                    size_t arg_count = an_list_end(expr) - an_list_start(expr);
                    if (arg_count == 0) {
                        expr->expr_val = zero_value(ast, type_expr->value_type);
                    } else if (arg_count == 1) {
                        ast_node_t *arg = expr->children.items[an_list_start(expr)];
                        resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), arg, true);
                        *expr = *cast_implicitly_if_necessary(ast, init_type, arg);
                        if (!TYPE_IS_INVALID(expr->value_type) && !ortypeid_eq(arg->value_type, expr->value_type)) {
                            stan_error(analyzer, OR_ERROR(
                                .tag = "sem.type-mismatch.type-init",
                                .level = ERROR_SOURCE_ANALYSIS,
                                .msg = lit2str("initialization list item requires explicit cast from '$1.$' to '$2.$'"),
                                .args = ORERR_ARGS(error_arg_node(arg),
                                    error_arg_type(arg->value_type), error_arg_type(expr->value_type)),
                                .show_code_lines = ORERR_LINES(0),
                            ));
                        }
                    } else {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.too-many-args.primitive-init",
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("initialization list for '$1.$' can only have up to 1 item"),
                            .args = ORERR_ARGS(error_arg_node(expr),
                                error_arg_type(init_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                    }
                    break;
                }

                case TYPE_VOID: break;

                case TYPE_COUNT:
                case TYPE_INVALID:
                case TYPE_UNRESOLVED:
                case TYPE_MODULE:
                case TYPE_PARAM_STRUCT:
                case TYPE_INFERRED_FUNCTION:
                case TYPE_UNREACHABLE: UNREACHABLE(); break;
                }
                break;
            }

            default: {
                unless (TYPE_IS_INVALID(type_expr->value_type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.inexpr.init-list",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("invalid expression for initialization list"),
                        .args = ORERR_ARGS(error_arg_node(type_expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                }
                INVALIDATE(expr);
                break;
            }
            
            }

            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            if (expr->start.type == TOKEN_STRING) {
                expr->value_type = ast->type_set.str8_t_;
                expr->expr_val = ast_node_val_word(ORWORDU(0));

                typedata_t *td = ast_type2td(ast, ast->type_set.str8_t_);
                if (td->size > ORWORD_SIZE) {
                    void *w = ast_multiword_value(ast, orb2w(td->size));
                    expr->expr_val = ast_node_val_word(ORWORDP(w));
                }

                orstring_t value = parse_token_as_str8(expr->start.view, ast->arena);
                ast_struct_item_set(ast, expr->value_type, lit2sv("cstr"), &expr->expr_val.word, ORWORDP((void*)value.cstr));
                ast_struct_item_set(ast, expr->value_type, lit2sv("length"), &expr->expr_val.word, ORWORDI((ors64)value.length));
            } else {
                INVALIDATE(expr);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            ast_node_t *left = an_lhs(expr);
            resolve_expression(analyzer, ast, state, implicit_type, left, true);

            ast_node_t *right = an_rhs(expr);
            resolve_expression(analyzer, ast, state, implicit_type, right, true);

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
                    .tag = "sem.nonumber-on-right.ptr-arith",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("number must be on right-hand-side for pointer arithmetic"),
                    .args = ORERR_ARGS(error_arg_node(an_lhs(expr))),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
                break;
            }

            if (left_td->kind == TYPE_POINTER && right_td->kind == TYPE_NUMBER && right->is_free_number && td_is_s_or_u_int(right_td)) {
                an_rhs(expr) = (right = cast_implicitly_if_necessary(ast, ast->type_set.ptrdiff_t_, right));
            }

            #undef td_is_s_or_u_int

            if (
                left_td->kind == TYPE_POINTER &&
                ortypeid_eq(ast->type_set.ptrdiff_t_, right->value_type) &&
                operator_is_arithmetic(expr->operator.type)) {

                if (expr->operator.type != TOKEN_PLUS && expr->operator.type != TOKEN_MINUS) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.inop.ptr-arith",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("only '-' and '+' are allowed in pointer arithmetic but got '$0.kind$'"),
                        .args = ORERR_ARGS(error_arg_token(expr->operator)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = right_td->kind == TYPE_POINTER ? ast->type_set.ptrdiff_t_ : left->value_type;
            }  else if (
                left_td->kind == TYPE_POINTER &&
                right_td->kind == TYPE_POINTER &&
                ortypeid_eq(left->value_type, right->value_type) &&
                operator_is_arithmetic(expr->operator.type)) {

                if (right_td->kind == TYPE_POINTER && expr->operator.type != TOKEN_MINUS) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noadd.ptr-arith",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot add pointer types"),
                        .args = ORERR_ARGS(error_arg_node(expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                }
            } else if (operator_is_arithmetic(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_ARITHMETIC) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noarith.arith",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("invalid arithmetic type: '$1.$'"),
                        .args = ORERR_ARGS(error_arg_node(left), error_arg_type(left->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                }

                unless (ortypeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.type-mismatch.arith",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("operation requires explicit cast for types '$1.$' and '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(left->value_type), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = left->value_type;
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    typedata_t *exprtd = ast_type2td(ast, expr->value_type);
                    orword_t result;
                    void *result_addr;
                    if (exprtd->size > ORWORD_SIZE) {
                        result_addr = ast_multiword_value(ast, exprtd->size);
                    } else {
                        result_addr = &result;
                    }

                    constant_fold_bin_arithmetic(ast, expr->operator.type, left->value_type, left->expr_val.word, right->expr_val.word, result_addr);

                    expr->is_free_number = left->is_free_number && right->is_free_number;

                    if (exprtd->size > ORWORD_SIZE) {
                        expr->expr_val = ast_node_val_word(ORWORDP(result_addr));
                    } else {
                        orword_t val = ast_item_get(&ast->type_set.types, result_addr, expr->value_type, 0);
                        expr->expr_val = ast_node_val_word(val);
                    }
                }
            } else if (operator_is_comparing(expr->operator.type)) {
                // everything is equatable but they need to be the same type
                unless (ortypeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nocompare.compare",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("operation requires explicit cast for types '$1.$' and '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(left->value_type), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = ortypeid(TYPE_BOOL);
                
                // constant fold
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    orword_t wordl = left->expr_val.word;
                    orword_t wordr = right->expr_val.word;

                    bool result = ast_word_eq(&ast->type_set, left->value_type, wordl, wordr);
                    switch (expr->operator.type) {
                    case TOKEN_EQUAL_EQUAL: {
                        expr->expr_val = ast_node_val_word(ORWORDU((oru64)result));
                        break;
                    }

                    case TOKEN_BANG_EQUAL: {
                        expr->expr_val = ast_node_val_word(ORWORDU((oru64)(result == 0)));
                        break;
                    }

                    default: UNREACHABLE(); break;
                    }

                    typedata_t *td = ast_type2td(ast, left->value_type);
                    if (td->size > ORWORD_SIZE) {
                        // todo
                        UNREACHABLE();
                    } else {
                        oru8 result = (oru8)(memcmp(&wordl, &wordr, ORWORD_SIZE) == 0);
                        switch (expr->operator.type) {
                        case TOKEN_EQUAL_EQUAL: {
                            expr->expr_val = ast_node_val_word(ORWORDU(result));
                            break;
                        }

                        case TOKEN_BANG_EQUAL: {
                            expr->expr_val = ast_node_val_word(ORWORDU(result == 0));
                            break;
                        }

                        default: UNREACHABLE(); break;
                        }
                    }

                    expr->is_free_number = left->is_free_number && right->is_free_number;
                }
            } else if (operator_is_ordering(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_ORDERABLE) {
                    // may be impossible in the end
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noorder.compare",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$1.$' is not a valid type for comparison"),
                        .args = ORERR_ARGS(error_arg_node(left), error_arg_type(left->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                }

                unless (ortypeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noorder.order",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("operation requires explicit cast for types '$1.$' and '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(left->value_type), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = ortypeid(TYPE_BOOL);
                if (left->expr_val.is_concrete && right->expr_val.is_concrete) {
                    orword_t word = constant_fold_bin_comparison(ast, expr->operator.type, left->value_type, left->expr_val.word, right->expr_val.word);
                    expr->expr_val = ast_node_val_word(word);
                    expr->is_free_number = left->is_free_number && right->is_free_number;
                }
            } else if (operator_is_logical(expr->operator.type)) {
                unless (left_td->capabilities&TYPE_CAP_LOGICAL) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.inarg.logical",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$1.$' is not a valid type for logical operations"),
                        .args = ORERR_ARGS(error_arg_node(left), error_arg_type(left->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                unless (ortypeid_eq(left->value_type, right->value_type)) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.needscast.logical",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("operation requires explicit cast for types '$1.$' and '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(left->value_type), error_arg_type(right->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                MUST(left_td->kind == TYPE_BOOL);
                
                expr->value_type = ortypeid(TYPE_BOOL);

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

                    expr->expr_val = ast_node_val_word(ORWORDU((oru64)result));
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
            resolve_expression(analyzer, ast, state, implicit_type, an_operand(expr), true);

            if (TYPE_IS_INVALID(an_operand(expr)->value_type)) {
                INVALIDATE(expr);
                break;
            }

            if (expr->operator.type == TOKEN_AMPERSAND) {
                ast_node_t *op = an_operand(expr);

                if (TYPE_IS_TYPE(op->value_type) && op->expr_val.is_concrete) {
                    expr->value_type = ortypeid(TYPE_TYPE);
                    ortype_t ptr_type = type_set_fetch_pointer(&ast->type_set, op->expr_val.word.as.t);
                    expr->expr_val = ast_node_val_word(ORWORDT(ptr_type));
                } else {
                    switch (op->lvalue_node->node_type) {
                    case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
                        if (op->lvalue_node->ref_decl->is_mutable) {
                            ortype_t ptr_type = type_set_fetch_pointer(&ast->type_set, op->lvalue_node->value_type);
                            expr->value_type = ptr_type;
                        } else {
                            stan_error(analyzer, OR_ERROR(
                                .tag = "sem.noaddr.const",
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
                        ortype_t type = type_set_fetch_pointer(&ast->type_set, op->lvalue_node->value_type);
                        expr->value_type = type;
                        break;
                    }

                    case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS: {
                        ortype_t type = type_set_fetch_pointer(&ast->type_set, op->lvalue_node->value_type);
                        expr->value_type = type;
                        break;
                    }

                    default: {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.inarg.address",
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
                            .tag = "sem.noptr.deref",
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
                            .tag = "sem.void.deref",
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
                            .tag = "sem.nobool.not",
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
                        expr->expr_val = ast_node_val_word(ORWORDU((oru64)value));
                    }
                    break;
                }

                case TOKEN_MINUS: {
                    if ((operand_td->capabilities&TYPE_CAP_ARITHMETIC) == 0) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.noarith.minus",
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
                            .tag = "sem.nosigned.minus",
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
                            ors64 val = operand->expr_val.word.as.s;
                            val = -val;
                            expr->expr_val = ast_node_val_word(ORWORDI(val));
                            break;
                        }

                        case NUM_TYPE_FLOAT: {
                            orf64 val = operand->expr_val.word.as.d;
                            val = -val;
                            expr->expr_val = ast_node_val_word(ORWORDD(val));
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

            ast_node_t *decl = get_defval_or_null_by_identifier_and_error(analyzer, ast, state.scope, expr, &def_scope);

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

        case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS: {
            ortype_t lhs_type = implicit_type;
            ast_node_t *lhs = an_dot_lhs(expr);
            if (an_is_notnone(lhs)) {
                resolve_expression(analyzer, ast, state, implicit_type, lhs, true);
                lhs_type = lhs->value_type;
            }

            if (TYPE_IS_INVALID(lhs_type)) {
                INVALIDATE(expr);
                break;
            }

            if (TYPE_IS_UNRESOLVED(lhs_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.noinfer.dot-access",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("no implicit type can be inferred for '.' access"),
                    .args = ORERR_ARGS(error_arg_node(expr)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
                break;
            }

            typedata_t *lhstd = ast_type2td(ast, lhs_type);
            switch (lhstd->kind){
            case TYPE_POINTER:
            case TYPE_STRING:
            case TYPE_STRUCT: {
                if (lhstd->kind == TYPE_POINTER) {
                    typedata_t *innertd = ast_type2td(ast, lhstd->as.ptr.type);
                    if (innertd->kind == TYPE_STRUCT) {
                        lhs_type = lhstd->as.ptr.type;
                        lhstd = innertd;
                    }
                }

                if (!retry_resolve_possible_struct_or_error(analyzer, lhs, lhs_type)) {
                    INVALIDATE(expr);
                    break;
                }

                struct_field_t field = {0};
                size_t field_index;
                bool success = ast_find_struct_field_by_name(ast, lhs_type, expr->identifier.view, &field, &field_index);

                unless (success) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nomember.dot-access",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot find '$0.$' in '$1.$'"),
                        .args = ORERR_ARGS(error_arg_token(expr->identifier), error_arg_type(lhs_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_offset = field.offset;
                expr->arg_index = field_index;
                expr->value_type = field.type;

                if (lhstd->kind != TYPE_POINTER && an_is_notnone(lhs->lvalue_node)) {
                    expr->lvalue_node = expr;
                }

                if (lhs->expr_val.is_concrete) {
                    void *start;
                    if (lhstd->size > ORWORD_SIZE) {
                        start = lhs->expr_val.word.as.p;
                    } else {
                        start = &lhs->expr_val.word;
                    }

                    typedata_t *fieldtd = ast_type2td(ast, field.type);
                    start += fieldtd->size;

                    orword_t result = ast_mem2word(ast, start, field.type);
                    expr->expr_val = ast_node_val_word(result);
                }
                break;
            }

            case TYPE_TYPE: {
                if (!lhs->expr_val.is_concrete) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noconst.dot-access-type",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'.' access on types requires a compile-time value"),
                        .args = ORERR_ARGS(error_arg_node(expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                ortype_t struct_type = lhs->expr_val.word.as.t;
                typedata_t *td = ast_type2td(ast, struct_type);
                if (td->kind != TYPE_STRUCT) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nostructtype.dot-access",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'.' access on types requires a struct definition type"),
                        .args = ORERR_ARGS(error_arg_node(expr)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                if (!retry_resolve_possible_struct_or_error(analyzer, lhs, struct_type)) {
                    INVALIDATE(expr);
                    break;
                }

                struct_field_t field = {0};
                size_t field_index;
                bool success = ast_find_field_by_name(td->as.struct_.constants, expr->identifier.view, &field, &field_index);

                unless (success) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.member.struct-access",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot find '$0.$' in '$1.$'"),
                        .args = ORERR_ARGS(error_arg_token(expr->identifier), error_arg_type(lhs_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = field.type;
                expr->expr_val = ast_node_val_word(field.default_value);
                break;
            }

            case TYPE_MODULE: {
                if (!lhs->expr_val.is_concrete) {
                    // todo: remove
                    UNREACHABLE();
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.noconst.module",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("expected a constant module"),
                        .args = ORERR_ARGS(error_arg_node(lhs)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    INVALIDATE(expr);
                    break;
                }

                ast_node_t *module = (ast_node_t*)lhs->expr_val.word.as.p;
                scope_t *found_scope;
                ast_node_t *defval = get_defval_or_null_by_identifier_and_error(analyzer, ast, &module->defined_scope, expr, &found_scope);
                if (defval == NULL) {
                    INVALIDATE(expr);
                    break;
                }

                expr->node_type = AST_NODE_TYPE_EXPRESSION_DEF_VALUE;
                expr->ref_decl = defval;
                expr->value_type = defval->value_type;
                expr->expr_val = defval->expr_val;
                break;
            }

            default: {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.inarg.dot-access",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("invalid '.' access expression, only structs, struct types, or modules"),
                    .args = ORERR_ARGS(error_arg_node(expr)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
                break;
            }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            ast_node_t *lhs = an_lhs(expr);
            resolve_expression(analyzer, ast, state, implicit_type,lhs, true);
            ast_node_t *lvalue_node = lhs->lvalue_node;
            if (an_is_none(lvalue_node)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.nolvalue.assignment",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("an lvalue is expected for the left-hand-side of an assignment"),
                    .args = ORERR_ARGS(error_arg_node(lhs)),
                    .show_code_lines = ORERR_LINES(0),
                ));
                INVALIDATE(expr);
                break;
            } 

            ast_node_t *rhs = an_rhs(expr);
            resolve_expression(analyzer, ast, state, lvalue_node->value_type, rhs, true);

            switch (expr->operator.type) {
            case TOKEN_PLUS_EQUAL:
            case TOKEN_MINUS_EQUAL:
            case TOKEN_STAR_EQUAL:
            case TOKEN_SLASH_EQUAL:
            case TOKEN_PERCENT_EQUAL:
            case TOKEN_PERCENT_PERCENT_EQUAL: {
                typedata_t *td = ast_type2td(ast, lhs->value_type);
                if ((td->capabilities & TYPE_CAP_ARITHMETIC) == 0) {
                    unless (TYPE_IS_INVALID(lhs->value_type)) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.noarith.compound-assign",
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("type '$1.$' does not have arithmetic capabilities"),
                            .args = ORERR_ARGS(error_arg_node(lhs), error_arg_type(lhs->value_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                    }
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

            ortype_t rhs_type = rhs->value_type;
            unless (TYPE_IS_INVALID(rhs_type) || TYPE_IS_INVALID(lhs->value_type) || ortypeid_eq(lvalue_node->value_type, rhs_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.type-mismatch.decl-and-type",
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
            size_t last_unreachable_index = resolve_declarations_until_unreachable(analyzer, ast, block_state, expr->children, is_consumed);

            bool is_unreachable = last_unreachable_index < expr->children.count;
            // discard everything past an unreachable type
            if (is_unreachable) {
                expr->children.count = last_unreachable_index+1;

                // todo: add a warning for this
            }

            ast_node_t *last_decl = expr->children.items[expr->children.count-1];
            last_decl->is_consumed = is_consumed;


            if (!is_unreachable && last_decl->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.no-end-statement.block",
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
            switch (expr->branch_type) {
            case BRANCH_TYPE_FOR:
            case BRANCH_TYPE_IF:
            case BRANCH_TYPE_WHILE: {
                scope_t branch_scope = {0};
                analysis_state_t branch_state = state;
                if (expr->branch_type != BRANCH_TYPE_IF) {
                    scope_init(&branch_scope, analyzer->ast->arena, SCOPE_TYPE_JMPABLE, state.scope, expr);
                    branch_state.scope = &branch_scope;
                }

                if (an_is_notnone(an_for_decl(expr))) {
                    resolve_declaration(analyzer, ast, branch_state, an_for_decl(expr));
                }

                {
                    scope_t condition_scope = {0};
                    scope_init(&condition_scope, ast->arena, SCOPE_TYPE_CONDITION, branch_state.scope, an_condition(expr));

                    analysis_state_t cond_state = state;
                    cond_state.scope = &condition_scope;

                    resolve_expression(analyzer, ast, cond_state, ortypeid(TYPE_UNRESOLVED), an_condition(expr), true);
                }

                if (an_is_notnone(an_for_incr(expr))) {
                    resolve_expression(analyzer, ast, branch_state, ortypeid(TYPE_UNRESOLVED), an_for_incr(expr), false);
                }

                resolve_expression(analyzer, ast, branch_state, implicit_type, an_then(expr), is_consumed);

                resolve_expression(analyzer, ast, branch_state, implicit_type, an_else(expr), is_consumed);

                ortype_t branch_type = resolve_block_return_types_or_error(analyzer, expr);
                expr->value_type = branch_type;

                if (TYPE_IS_INVALID(branch_type)) {
                    INVALIDATE(expr);
                }

                if (TYPE_IS_INVALID(an_then(expr)->value_type) || TYPE_IS_INVALID(an_else(expr)->value_type)) {
                    INVALIDATE(expr);
                }

                if (!TYPE_IS_INVALID(an_condition(expr)->value_type) && !ortypeid_eq(an_condition(expr)->value_type, ortypeid(TYPE_BOOL))) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nobool.condition",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'$0.kind$' condition must be a 'bool' but got '$1.$'"),
                        .args = ORERR_ARGS(error_arg_token(expr->start), error_arg_type(an_condition(expr)->value_type)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                }
                break;
            }

            case BRANCH_TYPE_DO: {
                scope_t branch_scope = {0};
                analysis_state_t branch_state = state;
                scope_init(&branch_scope, analyzer->ast->arena, SCOPE_TYPE_JMPABLE, state.scope, expr);
                branch_state.scope = &branch_scope;

                ast_node_t *do_block = an_expression(expr);
                resolve_expression(analyzer, ast, branch_state, implicit_type, do_block, is_consumed);

                tmp_arena_t *tmp = allocator_borrow();
                ast_nodes_t nodes = {.allocator=tmp->allocator};
                types_t types = {.allocator=tmp->allocator};

                unless (TYPE_IS_UNREACHABLE(do_block->value_type)) {
                    array_push(&nodes, do_block);
                    array_push(&types, do_block->value_type);
                }

                for (size_t i = 0; i < expr->jmp_nodes.count; ++i) {
                    ast_node_t *jmp = expr->jmp_nodes.items[i];

                    ast_node_t *e = an_expression(jmp);
                    unless (TYPE_IS_UNREACHABLE(e->value_type)) {
                        array_push(&nodes, e);
                        array_push(&types, e->value_type);
                    }
                }

                if (expr->is_consumed) {
                    if (types.count > 0) {
                        ortype_t check_type = types.items[0];
                            ast_node_t *check_node = nodes.items[0];
                        for (size_t i = 1; i < types.count; ++i) {
                            ortype_t other_type = types.items[i];
                            unless (ortypeid_eq(check_type, other_type)) {
                                ast_node_t *other_node = nodes.items[i];
                                stan_error(analyzer, OR_ERROR(
                                    .tag = "sem.type-mismatch.block-jmps",
                                    .level = ERROR_SOURCE_ANALYSIS,
                                    .msg = lit2str("all 'break' and `do` expression types must match when consumed but got '$0.$' and '$1.$'"),
                                    .args = ORERR_ARGS(error_arg_type(check_type), error_arg_type(other_type),
                                                    error_arg_node(check_node), error_arg_node(other_node)),
                                    .show_code_lines = ORERR_LINES(2, 3),
                                ));
                            }
                        }

                        expr->value_type = check_type;
                    } else {
                        expr->value_type = ortypeid(TYPE_UNREACHABLE);
                    }
                } else {
                    expr->value_type = ortypeid(TYPE_UNREACHABLE);
                }

                allocator_return(tmp);
                break;
            }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            switch (expr->start.type) {
                case TOKEN_RETURN: {
                    scope_t *func_def_scope = NULL;
                    bool success = get_nearest_jmp_scope_in_func_or_error(analyzer, expr, state.scope, SCOPE_TYPE_FUNCDEF, lit2sv(""), &func_def_scope);

                    ortype_t implicit_type = ortypeid(TYPE_UNRESOLVED);
                    unless (success) {
                        INVALIDATE(expr);
                    } else {
                        expr->jmp_out_scope_node = func_def_scope->creator;
                        array_push(&func_def_scope->creator->jmp_nodes, expr);

                        typedata_t *sig = ast_type2td(ast, func_def_scope->creator->value_type);
                        implicit_type = sig->as.function.return_type;
                    }

                    resolve_expression(analyzer, ast, state, implicit_type, an_expression(expr), true);
                    break;
                }

                case TOKEN_CONTINUE:
                case TOKEN_BREAK: {
                    resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), an_expression(expr), true);

                    scope_t *found_scope = NULL;
                    bool success = get_nearest_jmp_scope_in_func_or_error(analyzer, expr, state.scope, SCOPE_TYPE_JMPABLE, expr->identifier.view, &found_scope);
                    if (success) {
                        if (found_scope->creator->branch_type == BRANCH_TYPE_DO && expr->start.type == TOKEN_CONTINUE) {
                            stan_error(analyzer, OR_ERROR(
                                .tag = "sem.incontinue.branch",
                                .level = ERROR_SOURCE_ANALYSIS,
                                .msg = lit2str("'continue' expression is only valid in 'for' or 'while/until' expressions"),
                                .args = ORERR_ARGS(error_arg_node(expr), error_arg_node(found_scope->creator)),
                                .show_code_lines = ORERR_LINES(0, 1),
                            ));
                            break;
                        }

                        expr->jmp_out_scope_node = found_scope->creator;
                        array_push(&found_scope->creator->jmp_nodes, expr);
                        ASSERT(found_scope->creator->node_type == AST_NODE_TYPE_EXPRESSION_BRANCHING, "only supports branches rn");
                    }
                    break;
                }

                default: UNREACHABLE();
            }


            expr->value_type = ortypeid(TYPE_UNREACHABLE);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            resolve_call(analyzer, ast, state, expr, implicit_type);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            resolve_funcdef(analyzer, ast, state, expr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT: {
            resolve_struct(analyzer, ast, state, expr);
            break;
        }
        
         case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            bool invalid_parameter = false;
            for (size_t i = an_func_def_arg_start(expr); i < an_func_def_arg_end(expr); ++i) {
                ast_node_t *parameter = expr->children.items[i];
                resolve_expression(analyzer, ast, state, ortypeid(TYPE_TYPE), parameter, true);

                if (TYPE_IS_INVALID(parameter->value_type)) {
                    invalid_parameter = true;
                }
            }

            resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), an_func_def_return(expr), true);

            if (invalid_parameter || TYPE_IS_INVALID(an_func_def_return(expr)->value_type)) {
                INVALIDATE(expr);
                break;
            }

            bool success = fold_funcsig_or_error(analyzer, ast, expr);
            unless (success) {
                INVALIDATE(expr);
                break;
            }

            expr->value_type = ortypeid(TYPE_TYPE);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL: {
            size_t arg_start = an_bcall_arg_start(expr);
            size_t arg_end = an_bcall_arg_end(expr);
            size_t count = arg_end - arg_start;
            switch (expr->identifier.type) {
            case TOKEN_TYPEOF: {
                expr->value_type = ortypeid(TYPE_TYPE);

                unless (count == 1) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.inarg.typeof",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'typeof' builtin requires exactly 1 argument but got $1.$ instead"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_sz(count)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                ast_node_t *expr_arg = expr->children.items[arg_start];
                resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), expr_arg, true);

                if (TYPE_IS_INVALID(expr_arg->value_type)) {
                    break;
                }

                expr->expr_val = ast_node_val_word(ORWORDT(expr_arg->value_type));
                break;
            }
            
            case TOKEN_SIZEOF: {
                expr->value_type = ast->type_set.size_t_;

                unless (count == 1) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.inarg.sizeof",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'sizeof' builtin requires exactly 1 argument but got $1.$ instead"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_sz(count)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                ast_node_t *expr_arg = expr->children.items[arg_start];
                resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), expr_arg, true);
                ortype_t expr_type = expr_arg->value_type;

                if (TYPE_IS_TYPE(expr_type) && expr_arg->expr_val.is_concrete) {
                    expr_arg->value_type = ortypeid(TYPE_TYPE);
                    expr_arg->expr_val = ast_node_val_word(ORWORDT(expr_arg->expr_val.word.as.t));
                } else {
                    expr_arg->value_type = ortypeid(TYPE_TYPE);
                    expr_arg->expr_val = ast_node_val_word(ORWORDT(expr_type));
                }
                break;
            }

            case TOKEN_LEN: {
                expr->value_type = ast->type_set.size_t_;
                expr->is_free_number = true;
                expr->expr_val = ast_node_val_word(ORWORDU(0)); // default for error case

                unless (count == 1) {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.inarg.len",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("'len' builtin requires exactly 1 argument but got $1.$ instead"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_sz(count)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                    break;
                }

                ast_node_t *arg = expr->children.items[arg_start];
                resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), arg, true);

                ortype_t arg_type = arg->value_type;
                typedata_t *argtd = ast_type2td(ast, arg_type);

                if (argtd->kind != TYPE_ARRAY) {
                    unless (TYPE_IS_INVALID(arg_type)) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.too-many-args.len",
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("'len' builtin takes 1 array argument but got '$1.$' instead"),
                            .args = ORERR_ARGS(error_arg_node(arg), error_arg_type(arg->value_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                    }
                    break;
                }

                typedata_t *array_td = argtd;
                expr->expr_val = ast_node_val_word(ORWORDU(array_td->as.arr.count));
                break;
            }

            default: UNREACHABLE(); break;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST: {
            ast_node_t *cast_expr = an_cast_expr(expr);
            resolve_expression(analyzer, ast, state, implicit_type, cast_expr, true);
            if (TYPE_IS_INVALID(cast_expr->value_type)) {
                INVALIDATE(expr);
                break;
            }

            ast_node_t *type_expr = an_cast_type(expr);
            resolve_expression(analyzer, ast, state, implicit_type, type_expr, true);
            if (TYPE_IS_INVALID(type_expr->value_type)) {
                INVALIDATE(expr);
                break;
            }

            typedata_t *typetd = ast_type2td(ast, type_expr->value_type);
            if (typetd->kind == TYPE_TYPE && type_expr->expr_val.is_concrete) {
                if (stan_can_cast(&ast->type_set.types, type_expr->expr_val.word.as.t, cast_expr->value_type)) {
                    expr->value_type = type_expr->expr_val.word.as.t;

                    if (cast_expr->expr_val.is_concrete) {
                        orword_t result = constant_fold_cast(ast, cast_expr->expr_val.word, expr->value_type, cast_expr->value_type);
                        expr->expr_val = ast_node_val_word(result);
                        expr->is_free_number = cast_expr->is_free_number;
                    }
                } else {
                    stan_error(analyzer, OR_ERROR(
                        .tag = "sem.nocast.cast",
                        .level = ERROR_SOURCE_ANALYSIS,
                        .msg = lit2str("cannot cast type '$1.$' to type '$2.$'"),
                        .args = ORERR_ARGS(error_arg_node(expr), error_arg_type(cast_expr->value_type), error_arg_type(type_expr->expr_val.word.as.t)),
                        .show_code_lines = ORERR_LINES(0),
                    ));
                }

            } else {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.noconst.cast-type",
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
    orword_t def_word;
    
    tmp_arena_t *tmp = allocator_borrow();
    orstring_t identifier = sv2string(definition->identifier.view, tmp->allocator);

    bool defined_through_some_recursive_definition = false;
    if (table_get(s2w, scope->definitions, identifier, &def_word)) {
        ast_node_t *previous_decl = (ast_node_t*)def_word.as.p;
        if (previous_decl != definition) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.overload.decl",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("cannot have declarations with the same identifier '$0.$'"),
                .args = ORERR_ARGS(error_arg_token(definition->identifier), error_arg_node(previous_decl)),
                .show_code_lines = ORERR_LINES(0, 1),
            ));

            allocator_return(tmp);
            return;
        }

        // i feel so fucking smart for thinking of this
        // it's possible for the same definition to be defined
        // through recursive calls (when structs try to resolve themselves)
        // it's easy to just check if this is the exact same definition 
        // or it's a different one defined somewhere else just by checking
        // if the definitions ast nodes are the same
        defined_through_some_recursive_definition = true;
    }

    if (!defined_through_some_recursive_definition) {
        add_definition(scope, analyzer->ast->arena, string2sv(identifier), definition);
    }

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
        case AST_NODE_TYPE_EXPRESSION_DOT_ACCESS:
        case AST_NODE_TYPE_EXPRESSION_BUILTIN_CALL:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY:
        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE:
        case AST_NODE_TYPE_EXPRESSION_BLOCK:
        case AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_NIL:
        case AST_NODE_TYPE_EXPRESSION_STRUCT:
        case AST_NODE_TYPE_EXPRESSION_INITIALIZER_LIST:
        case AST_NODE_TYPE_EXPRESSION_DIRECTIVE:
        case AST_NODE_TYPE_EXPRESSION_JMP: break;

        case AST_NODE_TYPE_EXPR_INFERRED_TYPE_DECL: {

            decl_type->node_type = AST_NODE_TYPE_EXPRESSION_DEF_VALUE;
            decl_type->value_type = ortypeid(TYPE_UNRESOLVED);
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

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            tmp_arena_t *tmp = allocator_borrow();

            size_t arg_count = an_call_arg_end(decl_type) - an_call_arg_start(decl_type);

            for (size_t i = 0; i < arg_count; ++i) {
                size_t arg_index = i;
                ast_node_t *arg = decl_type->children.items[i + an_call_arg_start(decl_type)];

                type_patterns_t pat = (type_patterns_t){.allocator=tmp->allocator};

                forward_scan_inferred_types(decl, arg, arena, &pat);

                for (size_t i = 0; i < pat.count; ++i) {
                    type_path_t *current = pat.items[i].expected;
                    type_path_t *path = new_type_path(MATCH_TYPE_STRUCT_PARAM, current, arena);
                    path->index = arg_index;
                    pat.items[i].expected = path;

                    array_push(patterns, pat.items[i]);
                }
            }

            allocator_return(tmp);
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
            tmp_arena_t *tmp = allocator_borrow();

            for (size_t i = an_func_def_arg_start(decl_type); i < an_func_def_arg_end(decl_type); ++i) {
                ast_node_t *type_expr = decl_type->children.items[i];
                type_patterns_t pats = {.allocator=tmp->allocator};
                forward_scan_inferred_types(decl, type_expr, arena, &pats);

                for (size_t j = 0; j < pats.count; ++j) {
                    type_path_t *current = pats.items[j].expected;
                    type_path_t *path = new_type_path(MATCH_TYPE_SIG_ARG, current, arena);
                    path->index = i-an_func_def_arg_start(decl_type);
                    pats.items[j].expected = path;

                    array_push(patterns, pats.items[j]);
                }
            }

            {
                ast_node_t *ret = an_func_def_return(decl_type);
                type_patterns_t pats = {.allocator=tmp->allocator};
                forward_scan_inferred_types(decl, ret, arena, &pats);

                for (size_t i = 0; i < pats.count; ++i) {
                    type_path_t *current = pats.items[i].expected;
                    type_path_t *path = new_type_path(MATCH_TYPE_SIG_RET, current, arena);
                    pats.items[i].expected = path;

                    array_push(patterns, pats.items[i]);
                }
            }

            allocator_return(tmp);
            break;
        }


        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            forward_scan_inferred_types(decl, an_expression(decl_type), arena, patterns);
            break;
        }
    }
}

static void resolve_declaration_definition(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *decl) {
    if (TYPE_IS_RESOLVED(decl->value_type)) return;

    array_push(&analyzer->pending_dependencies, decl);

    ast_node_t *decl_type = an_decl_type(decl);
    if (TYPE_IS_UNRESOLVED(decl->value_type)) {
        analysis_state_t new_state = state;
        scope_t type_context = {0};
        scope_init(&type_context, ast->arena, SCOPE_TYPE_TYPE_CONTEXT, state.scope, decl_type);
        new_state.scope = &type_context;

        resolve_expression(analyzer, ast, new_state, ortypeid(TYPE_UNRESOLVED), decl_type, true);
        if (!decl_type->expr_val.is_concrete) {
            unless (TYPE_IS_INVALID(decl_type->value_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.noconst.type-decl",
                    .level = ERROR_SOURCE_ANALYSIS,
                    .msg = lit2str("expected type expression to be a constant"),
                    .args = ORERR_ARGS(error_arg_node(decl_type)),
                    .show_code_lines = ORERR_LINES(0),
                ));
            }
            INVALIDATE(decl);
        } else {
            unless (TYPE_IS_TYPE(decl_type->value_type)) {
                if (TYPE_IS_UNRESOLVED(decl_type->value_type)) {
                    decl->value_type = ortypeid(TYPE_UNRESOLVED);
                } else {
                    unless (TYPE_IS_INVALID(decl_type->value_type)) {
                        stan_error(analyzer, OR_ERROR(
                            .tag = "sem.noexpr.decl",
                            .level = ERROR_SOURCE_ANALYSIS,
                            .msg = lit2str("expected type expression for declaration"),
                            .args = ORERR_ARGS(error_arg_node(decl_type)),
                            .show_code_lines = ORERR_LINES(0),
                        ));
                    }
                    INVALIDATE(decl);
                }
            } else if (TYPE_IS_RESOLVED(decl_type->value_type)) {
                decl->value_type = decl_type->expr_val.word.as.t;
            } 
        }
    }

    ast_node_t *init_expr = an_decl_expr(decl);
    if (TYPE_IS_UNRESOLVED(an_decl_expr(decl)->value_type) && decl->type_decl_patterns.count == 0) {
        resolve_expression(analyzer, ast, state, decl->value_type, init_expr, true);
    }

    --analyzer->pending_dependencies.count;
    
    if (decl->is_mutable) {
        declare_definition(analyzer, state.scope, decl);
    }

    if (state.scope->type == SCOPE_TYPE_MODULE) {
        decl->is_global = true;
        if (decl->is_mutable) {
            array_push(&ast->global_decls_in_resolution_order, decl);
        }
    }

    if (TYPE_IS_INVALID(decl->value_type)) {
        return;
    }

    if (TYPE_IS_UNRESOLVED(init_expr->value_type) && TYPE_IS_UNRESOLVED(decl->value_type)) {
        // ASSERT(false, "don't thing this is possible??");
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.noinfer.nil",
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
            .tag = "sem.void.decl",
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
        ortype_t declared_type = decl->value_type;

        ast_node_t *casted_expr = cast_implicitly_if_necessary(ast, declared_type, decl_expr);
        unless (ortypeid_eq(declared_type, casted_expr->value_type)) {
            unless (TYPE_IS_INVALID(decl_expr->value_type)) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.type-mismatch.decl-and-type",
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

    if (!decl->is_mutable) {
        decl->expr_val = init_expr->expr_val;
        if (!decl->expr_val.is_concrete) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.noconst.const-decl",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("expected constant expression for compile-time value declaration"),
                .args = ORERR_ARGS(error_arg_node(init_expr)),
                .show_code_lines = ORERR_LINES(0),
            ));
        } else {
            typedata_t *td = ast_type2td(ast, init_expr->value_type);
            if (td->kind == TYPE_FUNCTION) {
                function_t *fn = decl->expr_val.word.as.p;
                if (fn->name.length == 0) {
                    // todo: give function proper arena?? figure out memory model better??
                    fn->name = sv2string(decl->identifier.view, fn->code.allocator);
                }
            }
        }
    } else {
        if (TYPE_IS_INFERRED_FUNCTION(decl->value_type)) {
            if (decl->is_mutable) {
                stan_error(analyzer, OR_ERROR(
                    .tag = "sem.noconst.inferred-funcdef",
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
    orstring_t identifier_ = sv2string(identifier, tmp->allocator);

    ast_node_t *decl;
    orword_t def_slot;
    unless (table_get(s2w, ast->builtins, identifier_, &def_slot)) {
        ortype_t type;
        // native_function_t *function;
        bool has_value = false;
        ortype_t value_type;
        orword_t value_slot;
        if (is_builtin_type(&ast->type_set, identifier, &type)) {
            has_value = true;
            value_slot = ORWORDT(type);
            value_type = ortypeid(TYPE_TYPE);
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

// static ast_node_t *get_module_or_null(ast_t *ast, string_t moduleid) {
//     ast_node_t *module;
//     if (table_get(s2n, ast->moduleid2node, moduleid, &module)) {
//         return module;
//     }

//     return NULL;
// }

static ast_node_t *get_defval_or_null_by_identifier_and_error(
        analyzer_t *analyzer,
        ast_t *ast,
        scope_t *scope,
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

    orword_t def_slot;
    *search_scope = scope;

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
            orstring_t identifier_ = sv2string(def->identifier.view, tmp->allocator);

            unless (table_get(s2w, (*search_scope)->definitions, identifier_, &def_slot)) {
                NEXT_SCOPE();
                continue;
            }

            allocator_return(tmp);
        }


        decl = (ast_node_t*)def_slot.as.p;

        if (passed_local_fold_scope && decl->is_mutable) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.noscope.run",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("declaration for '$2.$' does not exist in the same run scope it's being used in"),
                .args = ORERR_ARGS(error_arg_node(def), error_arg_node(decl),
                    error_arg_token(decl->identifier)),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return NULL;
        }

        if (passed_local_mutable_access_barrier && decl->is_mutable && (*search_scope)->type != SCOPE_TYPE_MODULE) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.noscope-local.run",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("declaration for '$2.$' does not exist in the same local scope it's being used in"),
                .args = ORERR_ARGS(error_arg_node(def), error_arg_node(decl),
                    error_arg_token(decl->identifier)),
                .show_code_lines = ORERR_LINES(0, 1),
            ));
            return NULL;
        }
        
        break;
    }
    
    // check core module
    if (!decl && ast->core_module_or_null) {
        ast_node_t *module = ast->core_module_or_null;
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
            .tag = "sem.nodef",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("reference to undefined declaration '$1.$'"),
            .args = ORERR_ARGS(error_arg_node(def), error_arg_token(def->identifier)),
            .show_code_lines = ORERR_LINES(0),
        ));
        return NULL;
    }

    MUST(decl->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION);

    if (TYPE_IS_UNRESOLVED(decl->value_type)) {
        analysis_state_t new_state = {0};
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
        if (!decl->is_mutable) {
            function_t *function = (function_t*)decl->expr_val.word.as.p;
            bool passed_through_fold = false;
            size_t dep_start = stan_function_is_building(analyzer, function, &passed_through_fold);
            if (dep_start < analyzer->pending_dependencies.count) {
                stan_circular_dependency_error(analyzer, ast, def, dep_start);
            }

            if (passed_through_fold) {
                ast_node_t *funcdef;
                bool success = table_get(fn2an, ast->fn2an, function, &funcdef);
                ORUNUSED(success);
                MUST(success);

                array_push(&analyzer->run_required_uncompiled_funcdefs, funcdef);
            }
        }
        break;
    }

    default: break;
    }

    if (!decl->is_mutable && TYPE_IS_TYPE(decl->value_type)) {
        MUST(decl->expr_val.is_concrete);
        ortype_t type = decl->expr_val.word.as.t;
        typedata_t *td = ast_type2td(ast, type);
        if (td->kind == TYPE_STRUCT) {
            size_t dep_start = stan_struct_is_building(analyzer, type);
            if (dep_start < analyzer->pending_dependencies.count) {
                stan_circular_dependency_error(analyzer, ast, def, dep_start);
            }
        }

    }

    return decl;
}

static void resolve_funcdef(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *funcdef) {
    ASSERT(funcdef->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, "must be function declaration at this point");

    // todo: is it possible to do this at parse time?
    {
        ast_node_t *module = stan_find_owning_module_or_null(state.scope);
        MUST(module);
        array_push(&module->owned_funcdefs, funcdef);
    }

    scope_init(&funcdef->defined_scope, ast->arena, SCOPE_TYPE_FUNCDEF, state.scope, funcdef);
    
    tmp_arena_t *tmp = allocator_borrow();
    types_t parameter_types = {.allocator=tmp->allocator};

    // forward scan paramters
    {
        tmp_arena_t *tmp = allocator_borrow();

        ast_nodes_t parameters = {.allocator=tmp->allocator};
        for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
            array_push(&parameters, funcdef->children.items[i]);
        }

        forward_scan_constant_names(analyzer, &funcdef->defined_scope, parameters);

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

            if (decl->is_compile_time_param) {
                is_inferred_function = true;
            }
        }
    }

    // dip out if it's inferred function, we'll resolve this when its called...
    if (is_inferred_function) {
        ortype_t function_type = ortypeid(TYPE_INFERRED_FUNCTION);
        funcdef->value_type = function_type;
        ast_inferred_function_t *inferred_func = ast_inferred_function_from_funcdef(ast, funcdef, ast->arena);
        funcdef->expr_val = ast_node_val_word(ORWORDP(inferred_func));
        goto defer;
    }

    bool parameter_invalid = false;

    for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
        analysis_state_t new_state = state;
        new_state.scope = &funcdef->defined_scope;

        ast_node_t *param = funcdef->children.items[i];
        resolve_declaration_definition(analyzer, ast, new_state, param);
        array_push(&parameter_types, param->value_type);
        
        if (TYPE_IS_INVALID(param->value_type)) {
            parameter_invalid = true;
        }
    }

    ortype_t return_type = ortypeid(TYPE_VOID);
    {
        resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), an_func_def_return(funcdef), true);
    }

    ast_node_t *ret_expr = an_func_def_return(funcdef);
    if (ret_expr->is_mutable) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.noconst.return-type",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("return expression must be a constant expression"),
            .args = ORERR_ARGS(error_arg_node(ret_expr)),
            .show_code_lines = ORERR_LINES(0),
        ));
    } else unless (TYPE_IS_TYPE(ret_expr->value_type)) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.notype.return-type",
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

    ortype_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);
    funcdef->value_type = function_type;

    // create empty placeholder function immeidately in case definition is recursive
    function_t *function = NULL;
    unless (TYPE_IS_INVALID(an_func_def_block(funcdef)->value_type)) {
        if (analyzer->run_vm) {
            function = new_function(analyzer->run_vm->program_mem, analyzer->run_vm->arena);
            for (size_t i = an_func_def_arg_start(funcdef); i < an_func_def_arg_end(funcdef); ++i) {
                ast_node_t *param = funcdef->children.items[i];

                struct_field_t field = ast_struct_field_from_decl(ast, param, function->arg_defaults.allocator);
                bool has_default = param->has_default_value;
                array_push(&function->arg_defaults, field);
                array_push(&function->has_defaults, has_default);
            }
        } else {
            UNREACHABLE();
        }

        funcdef->expr_val = ast_node_val_word(ORWORDP(function));
    }

    array_push(&analyzer->pending_dependencies, funcdef);
    table_put(fn2an, ast->fn2an, function, funcdef);

    {
        analysis_state_t new_state = state;

        scope_t function_scope;
        scope_init(&function_scope, ast->arena, SCOPE_TYPE_FUNC_DEF_BODY, &funcdef->defined_scope, funcdef);
        new_state.scope = &function_scope;
        resolve_expression(analyzer, ast, new_state, ortypeid(TYPE_UNRESOLVED), an_func_def_block(funcdef), false);
    }

    ast_node_t *funcblock = an_func_def_block(funcdef);
    unless (TYPE_IS_UNREACHABLE(funcblock->value_type) || TYPE_IS_VOID(return_type)) {
        stan_error(analyzer, OR_ERROR(
            .tag = "sem.noreturn-all-branches.funcdef",
            .level = ERROR_SOURCE_ANALYSIS,
            .msg = lit2str("function does not return on all branches"),
            .args = ORERR_ARGS(error_arg_node(funcdef)),
            .show_code_lines = ORERR_LINES(0),
        ));
    }

    for (size_t i = 0; i < funcdef->jmp_nodes.count; ++i) {
        ast_node_t *jmp = funcdef->jmp_nodes.items[i];
        
        ast_node_t *ret_expr = an_expression(jmp);

        ortype_t ret_expr_type = ret_expr->value_type;
        unless (TYPE_IS_INVALID(ret_expr_type) || ortypeid_eq(ret_expr_type, return_type)) {
            stan_error(analyzer, OR_ERROR(
                .tag = "sem.type-mismatch.return-funcdef",
                .level = ERROR_SOURCE_ANALYSIS,
                .msg = lit2str("return value type '$1.$' does not match function return type '$2.$'"),
                .args = ORERR_ARGS(error_arg_node(ret_expr),
                    error_arg_type(ret_expr_type), error_arg_type(return_type)),
                .show_code_lines = ORERR_LINES(0),
            ));
        }
    }

    --analyzer->pending_dependencies.count;

defer:
    allocator_return(tmp);
}

static void resolve_declaration_statement(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *statement,
        bool is_consumed) {
    resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), an_expression(statement), is_consumed);
    statement->value_type = an_expression(statement)->value_type;
    statement->expr_val = an_expression(statement)->expr_val;
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
            resolve_declaration_statement(analyzer, ast, state, declaration_node, false);
            break;
        }

        default: UNREACHABLE();
    }
}

size_t resolve_declarations_until_unreachable(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_nodes_t declarations, bool is_last_statement_consumed) {
    size_t last_decl = declarations.count;
    for (size_t i = 0; i < declarations.count; i++) {
        ast_node_t *declaration = declarations.items[i];
        if (declaration->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION && an_is_constant(declaration)) {
            continue;
        }

        if (i == declarations.count-1 && declaration->node_type == AST_NODE_TYPE_DECLARATION_STATEMENT) {
            resolve_expression(analyzer, ast, state, ortypeid(TYPE_UNRESOLVED), an_expression(declaration), is_last_statement_consumed);
            resolve_declaration_statement(analyzer, ast, state, declaration, is_last_statement_consumed);
        } else {
            resolve_declaration(analyzer, ast, state, declarations.items[i]);
        }

        if (TYPE_IS_UNREACHABLE(declarations.items[i]->value_type)) {
            last_decl = i;
            break;
        }
    }

    // finds unused constants
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

    return last_decl;
}

static void analyzer_init(analyzer_t *analyzer, ast_t *ast, arena_t *arena) {
    *analyzer = zer0(analyzer_t);

    analyzer->had_error = false;
    analyzer->run_vm = ast->vm; //vm_default(arena);
    analyzer->ast = ast;

    analyzer->arena = arena;
    analyzer->placeholder = zer0(function_t);
    analyzer->run_required_uncompiled_funcdefs = (ast_nodes_t){.allocator=arena};
    analyzer->pending_dependencies.allocator = arena;
}

bool resolve_ast(ast_t *ast) {
    analyzer_t analyzer = {0};

    analyzer_init(&analyzer, ast, ast->arena);

    resolve_module(&analyzer, ast, ast->core_module_or_null);

    for (size_t i = 0; i < ast->core_module_or_null->children.count; ++i) {
        ast_node_t *decl = ast->core_module_or_null->children.items[i];
        if (sv_eq(decl->identifier.view, lit2sv("str8_t"))) {
            typedata_t *decltd = ast_type2td(ast, decl->expr_val.word.as.t);
            typedata_t *str8td = ast_type2td(ast, ast->type_set.str8_t_);
            orstring_t name = str8td->name;
            *str8td = *decltd;
            str8td->name = name;
            str8td->kind = TYPE_STRING;
            break;
        }
    }

    ast_node_t *module;
    kh_foreach_value(ast->moduleid2node, module, {
        resolve_module(&analyzer, ast, module);
    });

    ast->resolved = !analyzer.had_error;

    return ast->resolved;
}

function_t *find_main_or_null(ast_node_t *module) {
    for (size_t i = 0; i < module->children.count; ++i) {
        ast_node_t *decl = module->children.items[i];
        if (sv_eq(decl->identifier.view, lit2sv("main"))) {
            return decl->expr_val.word.as.p;
        }
    }

    return NULL;
}
