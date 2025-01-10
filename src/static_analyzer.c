#include "static_analyzer.h"

#include <stdio.h>

#include "mathutils.h"
#include "type_set.h"
#include "error.h"
#include "vm.h"
#include "tmp.h"

#include <time.h>

#define EXPRESSION_RESOLVED(expression) (!TYPE_IS_UNRESOLVED((expression)->value_type))

#define INVALIDATE(NODE) do {\
    ast_node_t* node = NODE;\
    node->value_type = typeid(TYPE_INVALID);\
    node->value_index = value_index_nil();\
} while(false)

typedef bool (*IsCircularDependencyFunc)(analyzer_t*, ast_node_t*);

// static void clock_native(word_t *arguments, word_t *result) {
//     (void)arguments;
//     result[0] = WORDD((double)clock() / CLOCKS_PER_SEC);
// }

typedef enum folding_mode_t {
    MODE_RUNTIME = 0x1,
    MODE_CONSTANT_TIME = 0x2,
    MODE_FOLDING_TIME = 0x4,
} folding_mode_t;

typedef struct analysis_state_t {
    i32 fold_level;
    folding_mode_t mode;
    scope_t *scope;
} analysis_state_t;

// #define def_type(e) (((e)->node->node_type == AST_NODE_TYPE_NONE) ? ((e)->declared_type) : ((e)->node->value_type))
#define def_type(e) ((e)->declared_type)

static void scope_init(scope_t *scope, arena_t *allocator, scope_type_t type, scope_t *outer, ast_node_t *creator_expression) {
    scope->outer = outer;
    scope->creator = creator_expression;
    scope->type = type;
    scope->definitions = table_new(s2w, allocator);
}

static void add_definition(scope_t *scope, arena_t *allocator, string_view_t identifier, ast_node_t *decl) {
    table_put(s2w, scope->definitions, sv2string(identifier, allocator), WORDP(decl));
}

static ast_node_t *add_builtin_definition(ast_t *ast, string_view_t identifier, type_t type, value_index_t value_index) {
    ast_node_t *decl = ast_node_new(ast, AST_NODE_TYPE_DECLARATION_DEFINITION, nil_token);
    decl->value_type = type;
    decl->value_index = value_index;
    decl->is_mutable = false;

    table_put(s2w, ast->builtins, sv2string(identifier, &ast->allocator), WORDP(decl));

    return decl;
}

// static void function_dependencies_cannot_be_compiled(analyzer_t *analyzer) {
//     for (size_t i = 0; i < analyzer->dependencies.count; ++i) {
//         analysis_dependency_t *dependency = &analyzer->dependencies.items[i];
//         ast_node_t *node = dependency->ast_node;
//         if (node->node_type != AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) {
//             continue;
//         }

//         node->as.function.compilable = false;
//     }
// }

static void stan_error(analyzer_t *analyzer, error_t error) {
    analyzer->had_error = true;
    if (analyzer->error_fn) analyzer->error_fn(analyzer->ast, error);
}

static type_t resolve_unary_type(ast_t* ast, token_type_t operator, type_t operand_id) {
    if (TYPE_IS_INVALID(operand_id)) {
        return typeid(TYPE_INVALID);
    }

    if (TYPE_IS_UNRESOLVED(operand_id)) {
        return typeid(TYPE_UNRESOLVED);
    }

    type_info_t *operand = ast->type_set.types.items[operand_id.i];

    switch (operator) {
        case TOKEN_MINUS: {
            if (type_is_number(operand, false) && operand->data.num != NUM_TYPE_UNSIGNED) {
                return operand_id;
            } else {
                return typeid(TYPE_INVALID);
            }
        }
        case TOKEN_NOT: {
            if (typeid_eq(typeid(TYPE_BOOL), operand_id)) {
                return typeid(TYPE_BOOL);
            } else {
                return typeid(TYPE_INVALID);
            }
        }

        case TOKEN_PLUS_PLUS:
        case TOKEN_MINUS_MINUS: {
            return typeid(TYPE_INVALID);
            // if (type_is_number(operand, false)) {
            //     return operand_id;
            // }
        }

        case TOKEN_AMPERSAND: {
            type_t type = type_set_fetch_pointer(&ast->type_set, operand_id);
            return type;
        }

        default: return typeid(TYPE_INVALID);
    }
}

static ast_node_t *implicit_cast(ast_t *ast, ast_node_t *operand, type_t value_type) {
    ast_node_t *implicit_cast = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT, operand->start);
    implicit_cast->end = operand->end;

    implicit_cast->value_type = value_type;
    an_operand(implicit_cast) = operand;
    implicit_cast->fold = false;
    implicit_cast->foldable = true;
    implicit_cast->value_index = value_index_nil();

    return implicit_cast;
}

#define IS_FOLDED(EXPRESSION_PTR) (EXPRESSION_PTR->value_index.exists)

typedef enum {
    QUERY_FLAG_MATCH_ANY = 0x1,
    QUERY_FLAG_MATCH_ONLY_IN_GIVEN_SCOPE = 0x2,
    QUERY_FLAG_MATCH_TYPE = 0x4,
    QUERY_FLAG_MACH_FUNCTION = 0x8,
} QueryFlags;

typedef struct def_query_t def_query_t;
struct def_query_t  {
    QueryFlags flags;
    type_t search_type;
    bool skip_mutable;
};

static ast_node_t *get_resolved_def_by_identifier(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        token_t identifier_token,
        def_query_t *query,
        scope_t **found_scope);

static bool is_builtin_type(type_table_t *t, string_view_t identifier, type_t *type) {
#define RETURN_IF_TYPE(SYMBOL, TYPE_STRING, TYPE) \
if (sv_eq(identifier, lit2sv(#TYPE_STRING))) {\
    *type = (TYPE); \
    return true; \
}

    RETURN_IF_TYPE(identifier, i32, t->i32_)
    RETURN_IF_TYPE(identifier, i64, t->i64_)
    RETURN_IF_TYPE(identifier, f32, t->f32_)
    RETURN_IF_TYPE(identifier, f64, t->f64_)
#undef RETURN_IF_TYPE

#define RETURN_IF_TYPE(SYMBOL, TYPE_STRING, TYPE) \
if (sv_eq(identifier, lit2sv(#TYPE_STRING))){\
    *type = typeid(TYPE); \
    return true; \
}

    RETURN_IF_TYPE(identifier, void, TYPE_INVALID)
    RETURN_IF_TYPE(identifier, bool, TYPE_BOOL)
    RETURN_IF_TYPE(identifier, string, TYPE_STRING)
    RETURN_IF_TYPE(identifier, type, TYPE_TYPE)
#undef RETURN_IF_TYPE

    return false;

#undef RETURN_IF_TYPE
}

static bool can_call(type_infos_t types, type_t type, ast_nodes_t arguments, size_t arg_start, size_t arg_end) {
    ASSERT(type_is_function(types, type), "must be a function type");

    type_info_t *type_info = get_type_info(&types, type);

    if (type_info->data.function.argument_types.count != (arg_end - arg_start)) {
        return false;
    }

    for (size_t i = 0; i < type_info->data.function.argument_types.count; ++i) {
        type_t parameter_type = type_info->data.function.argument_types.items[i];
        type_t argument_type = arguments.items[arg_start+i]->value_type;
        unless (typeid_eq(parameter_type, argument_type)) {
            return false;
        }
    }

    return true;
}

static value_index_t evaluate_expression(analyzer_t *analyzer, ast_t *ast, bool is_folding_time, ast_node_t *expression) {
    UNUSED(analyzer);
    UNUSED(ast);
    UNUSED(is_folding_time);
    UNUSED(expression);
    return value_index_nil();
}

static bool is_declaration_resolved(ast_node_t *definition) {
    if (TYPE_IS_INVALID(definition->value_type)) {
        return true;
    }

    return (definition->value_index.exists || (an_decl_expr(definition)->node_type != AST_NODE_TYPE_NONE && !TYPE_IS_UNRESOLVED(an_decl_expr(definition)->value_type))) && !TYPE_IS_UNRESOLVED(definition->value_type);
}

static bool is_block_compile_time_foldable(ast_nodes_t block) {
    for (size_t i = 0; i < block.count; i++) {
        ast_node_t *declaration = block.items[i];
        switch (declaration->node_type) {
            case AST_NODE_TYPE_DECLARATION_DEFINITION: {
                if (!is_declaration_resolved(declaration)) {
                    return false;
                }
                break;
            }
            
            case AST_NODE_TYPE_DECLARATION_STATEMENT: {
                if (!EXPRESSION_RESOLVED(an_operand(declaration)) || TYPE_IS_INVALID(an_operand(declaration)->value_type)) {
                    return false;
                }
                break;
            }
            
            case AST_NODE_TYPE_NONE:
            case AST_NODE_TYPE_MODULE:
            case AST_NODE_TYPE_EXPRESSION_CASE: UNREACHABLE();
        }
    }

    return true;
}

static void fold_constants_via_runtime(
        analyzer_t* analyzer,
        ast_t* ast,
        analysis_state_t state,
        ast_node_t* expression);

static void resolve_foldable(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *expression) {
    UNUSED(analyzer);

    bool foldable = false;
    value_index_t folded_index = value_index_nil();

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            foldable = an_operand(expression)->foldable;
            folded_index = an_operand(expression)->value_index;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_NIL: {
            foldable = true;
            folded_index = zero_value(ast, expression->value_type);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            if (expression->as.initiailizer.arguments.count == 0) {
                foldable = true;
                type_t type = get_folded_type(ast, expression->as.initiailizer.type->value_index);
                value_index_t value_index = zero_value(ast, type);
                folded_index = value_index;
            } else {
                foldable = true;
                for (size_t i = 0; i < expression->as.initiailizer.arguments.count; ++i) {
                    ast_node_t *arg = expression->as.initiailizer.arguments.items[i];
                    unless (arg) continue;
                    foldable &= arg->foldable;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            ast_node_t *left = an_lhs(expression);
            ast_node_t *right = an_rhs(expression);
            foldable = left->foldable && right->foldable;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            bool condition_is_foldable = an_condition(expression)->foldable;
            bool then_is_foldable = an_then(expression)->foldable;
            bool else_is_foldable = an_is_notnone(an_else(expression)) ? an_else(expression)->foldable : true;

            foldable = condition_is_foldable && then_is_foldable && else_is_foldable;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            foldable = an_operand(expression)->foldable;

            if (expression->operator.type == TOKEN_AMPERSAND) {
                // todo: check if inside type context
                bool is_inside_type_context = false;
                if (is_inside_type_context) {
                    type_t type = get_folded_type(ast, an_operand(expression)->value_index);

                    type_t pointer_type = type_set_fetch_pointer(&ast->type_set, type);

                    expression->value_type = typeid(TYPE_TYPE);

                    word_t type_slot = WORDU(pointer_type.i);
                    
                    folded_index = add_value_to_ast_constant_stack(ast, &type_slot, typeid(TYPE_TYPE));
                } else {
                    foldable = false;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            ast_node_t *referencing_declaration = expression->ref_decl;

            // this happens when the referencing declaration cannot be found OR for a builtin type
            if (an_is_implicit(referencing_declaration)) {
                ASSERT(expression->value_index.exists, "must already be folded");

                foldable = expression->foldable;
                folded_index = expression->value_index;
                break;
            }

            if (referencing_declaration->is_mutable) {
                foldable = false;
                break;
            }

            if (an_decl_expr(referencing_declaration)->node_type == AST_NODE_TYPE_NONE) {
                foldable = an_decl_expr(referencing_declaration)->foldable;
                folded_index = an_decl_expr(referencing_declaration)->value_index;

                unless (TYPE_IS_INVALID(an_decl_expr(referencing_declaration)->value_type)) {
                    ASSERT(folded_index.exists, "since the definition is a constant, it should have a folded value already");
                }
            } else {
                foldable = true;
                folded_index = referencing_declaration->value_index;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            ASSERT(an_is_notnone(expression->ref_decl), "referencing declaration must be present for dot expression");

            foldable = expression->foldable && expression->ref_decl->foldable;
            folded_index = foldable ? expression->ref_decl->value_index : value_index_nil();
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            foldable = true;
            folded_index = expression->value_index;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: {
            foldable = true;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            unless (state.mode & MODE_FOLDING_TIME) {
                break;
            }

            // TODO: Again find a better and faster way to do this
            // At this point, we are in folding time, and if a function cannot be compiled
            // we should probably let the user know and also not allow them to fold something
            // that has an invalid thing.
            if (type_is_function(ast->type_set.types, an_callee(expression)->value_type)) {
                ASSERT(false, "todo");
                // value_index_t value_index = expression->as.call.callee->value_index;
                // function_t* function = NULL;
                // memarr_get(&ast->constants, value_index.index, sizeof(function_t*), &function);
                // ast_node_t* function_definition = NULL;
                // for (size_t i = 0; i < ast->function_definition_pairs.count; ++i) {
                //     if (function == ast->function_definition_pairs.items[i].function) {
                //         function_definition = ast->function_definition_pairs.items[i].ast_defintion;
                //         break;
                //     }
                // }

                // ASSERT(function_definition, "this has to exist in the pair list");

                // unless (function_definition->as.function.compilable) {
                //     foldable = false;
                //     error_range(analyzer, function_definition->start, function_definition->end, ERROR_ANALYSIS_CANNOT_FOLD_ON_ERRORED_FUNCTION_DEFINITION);
                //     break;
                // }
            }

            foldable = an_callee(expression)->foldable;

            unless (foldable) {
                break;
            }

            for (size_t i = an_call_arg_start(expression); i < an_call_arg_end(expression); ++i) {
                foldable &= expression->children.items[i]->foldable;
                unless (foldable) {
                    break;
                }
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            unless (state.mode & MODE_FOLDING_TIME) {
                break;
            }

            foldable = is_block_compile_time_foldable(expression->children);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            unless (state.mode & MODE_FOLDING_TIME) {
                break;
            }

            foldable = true;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY:
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            foldable = true;
            folded_index = expression->value_index;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_JMP: {
            foldable = false;
            break;
        }

        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_NONE:
            UNREACHABLE(); break;
    }

    expression->foldable = foldable;
    expression->value_index = folded_index;
}

static void fold_constants_via_runtime(
        analyzer_t* analyzer,
        ast_t* ast,
        analysis_state_t state,
        ast_node_t* expression) {
    
    UNUSED(analyzer);

    resolve_foldable(analyzer, ast, state, expression);

    if (IS_FOLDED(expression)) {
        return;
    }

    // we can't fold it so nothing to do
    if (!expression->foldable) {
        return;
    }

    ASSERT(!TYPE_IS_TYPE(expression->value_type), "types must be created at compile time");


    value_index_t value_index = evaluate_expression(analyzer, ast, state.mode & MODE_FOLDING_TIME, expression);

    expression->value_index = value_index;
}

static void fold_function_signature(analyzer_t *analyzer, ast_t *ast, ast_node_t *expression) {
    ASSERT(expression->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");

    unless (IS_FOLDED(an_func_def_return(expression))) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, an_func_def_return(expression)));
        return;
    }

    bool hit_error = false;
    types_t parameter_types = {.allocator=&ast->allocator};

    for (size_t i = an_func_def_arg_start(expression); i < an_func_def_arg_end(expression); ++i) {
        ast_node_t *parameter = expression->children.items[i];
        unless (IS_FOLDED(parameter)) {
            hit_error = true;
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, parameter));
            break;
        }

        unless (TYPE_IS_TYPE(parameter->value_type)) {
            hit_error = true;
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_TYPE, parameter));
            break;
        }

        value_index_t index = parameter->value_index;

        type_t type = get_folded_type(ast, index);
        array_push(&parameter_types, type);
    }

    if (hit_error) {
        return;
    }

    ast_node_t *return_type_expression = an_func_def_return(expression);
    if (return_type_expression && !TYPE_IS_TYPE(return_type_expression->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_RETURN_TYPE, return_type_expression));
        return;
    }

    type_t return_type;
    {
        if (return_type_expression) {
            value_index_t index = return_type_expression->value_index;
            return_type = get_folded_type(ast, index);
        } else {
            return_type = typeid(TYPE_VOID);
        }
    }

    type_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);

    word_t function_type_slot = WORDU(function_type.i);
    value_index_t index = add_value_to_ast_constant_stack(ast, &function_type_slot, typeid(TYPE_TYPE));

    expression->foldable = true;
    expression->value_index = index;
}

// footnote(circular-dependencies)
static bool is_value_circular_dependency(analyzer_t* analyzer, ast_node_t* new_dependency) {
    for (size_t i_ = 0; i_ < analyzer->dependencies.count; ++i_) {
        size_t i = analyzer->dependencies.count-i_-1;
        analysis_dependency_t *dependency = &analyzer->dependencies.items[i];

        if (dependency->ast_node->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) {
            return false;
        }

        if (dependency->ast_node->node_type == AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION) {
            return false;
        }

        if (dependency->ast_node == new_dependency) {
            return true;
        }
    }

    return false;
}

static bool push_dependency(analyzer_t* analyzer, ast_node_t* node, int fold_level, IsCircularDependencyFunc is_circular_dependency) {
    if (is_circular_dependency(analyzer, node)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_FOLDING_LOOP, node));
        return false;
    }

    analysis_dependency_t dependency = {
        .fold_level = fold_level,
        .ast_node = node,
    };

    array_push(&analyzer->dependencies, dependency);

    return true;
}

static void pop_dependency(analyzer_t *analyzer) {
    --analyzer->dependencies.count;
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
                }
            } else {
                *found_scope = scope;
            }
            return true;
        }

        if (search_type != SCOPE_TYPE_FUNC_DEF && scope->type == SCOPE_TYPE_FUNC_DEF_BODY) {
            break;
        }

        if (scope->type == SCOPE_TYPE_CONDITION) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_JMP_IN_CONDITION, jmp_node));
            return false;
        }

        scope = scope->outer;
    }

    if (search_type == SCOPE_TYPE_FUNC_DEF) {
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
            break;
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
        i32 count);

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

static void resolve_func_def(
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

static void forward_scan_declaration_names(analyzer_t *analyzer, scope_t *scope, ast_nodes_t declarations, i32 count) {
    for (i32 i = 0; i < count; i++) {
        ast_node_t* declaration = declarations.items[i];
        if (declaration->node_type != AST_NODE_TYPE_DECLARATION_DEFINITION) {
            continue;
        }

        declare_definition(analyzer, scope, declaration);
    }
}

void resolve_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *expr) {
    
    ASSERT(ast_node_type_is_expression(expr->node_type), "should be only expressions");

    unless (TYPE_IS_UNRESOLVED(expr->value_type)) {
        return;
    }

    if (expr->fold) {
        ++state.fold_level;
        state.mode |= MODE_FOLDING_TIME;
    }

    switch (expr->node_type) {
        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            resolve_expression(analyzer, ast, state, an_operand(expr));
            expr->lvalue_node = an_operand(expr);
            expr->value_type = an_operand(expr)->value_type;
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

            unless (expr->as.initiailizer.type->value_index.exists) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, expr->as.initiailizer.type));
                INVALIDATE(expr);
                break;
            }

            type_t type = get_folded_type(ast, expr->as.initiailizer.type->value_index);
            type_info_t *type_info = get_type_info(&ast->type_set.types, type);

            if (type_info->kind == TYPE_STRUCT) {
                int arg_count = expr->as.initiailizer.arguments.count;
                if (type_info->data.struct_.field_count < arg_count) {
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

                    if (i < type_info->data.struct_.field_count) {
                        type_t field_type = type_info->data.struct_.fields[i].type;
                        type_t arg_type = arg->value_type;
                        unless (typeid_eq(field_type, arg_type)) {
                            if (can_cast_implicit(ast->type_set.types, arg_type, field_type)) {
                                ast_node_t *casted = implicit_cast(ast, arg, field_type);
                                fold_constants_via_runtime(analyzer, ast, state, casted);
                                expr->as.initiailizer.arguments.items[i] = casted;
                            } else {
                                printf("TODO: need to look up struct field and compare, first arg should be struct field ast node");
                                stan_error(analyzer, make_type_mismatch_error(arg, arg));
                                is_invalidated = true;
                                continue;
                            }
                        }
                    }
                }

                if (is_invalidated) {
                    INVALIDATE(expr);
                    break;
                }

                type_t type = get_folded_type(ast, expr->as.initiailizer.type->value_index);
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

            if (TYPE_IS_INVALID(left->value_type) || TYPE_IS_INVALID(right->value_type)) {
                INVALIDATE(expr);
                break;
            }

            // TODO: Remember to do different things depending on operation
            //   if it's arthimetic, or comparisons, then let them merge at the end
            //   if it's logical, then you need to do special things

            type_t cast_left = typeid(TYPE_INVALID);
            type_t cast_right = typeid(TYPE_INVALID);

            expr->value_type = typeid(TYPE_UNRESOLVED);

            bool is_logical_operator = false;

            switch (expr->operator.type) {
                case TOKEN_PERCENT_PERCENT:
                case TOKEN_PERCENT:
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                case TOKEN_STAR:
                case TOKEN_SLASH: {
                    type_t combined_type = typeid(TYPE_INVALID);
                    if (typeid_eq(left->value_type, right->value_type)) {
                        combined_type = left->value_type;
                    }
                    expr->value_type = combined_type;

                    cast_left = combined_type;
                    cast_right = combined_type;
                    break;
                }

                case TOKEN_BAR: {
                    INVALIDATE(expr);
                    break;
                }

                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL: {
                    if (typeid_eq(left->value_type, right->value_type)) {
                        cast_left = left->value_type;
                        cast_right = right->value_type;
                        expr->value_type = typeid(TYPE_BOOL);
                    } else {
                        expr->value_type = typeid(TYPE_INVALID);
                    }
                    break;
                }
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    if (typeid_eq(left->value_type, right->value_type)) {
                        cast_left = left->value_type;
                        cast_right = right->value_type;
                        expr->value_type = typeid(TYPE_BOOL);
                    } else {
                        expr->value_type = typeid(TYPE_INVALID);
                    }
                    break;
                }

                case TOKEN_AND:
                case TOKEN_OR: {
                    is_logical_operator = true;
                    cast_left = left->value_type;
                    cast_right = right->value_type;

                    expr->value_type = typeid(TYPE_BOOL);
                    break;
                }

                default: UNREACHABLE();
            }

            if (TYPE_IS_INVALID(cast_left) || TYPE_IS_INVALID(cast_right)) {
                INVALIDATE(expr);
                stan_error(analyzer, make_error_nodes(ERROR_ANALYSIS_INVALID_BINARY_OPERANDS, left, right));
                break;
            }

            if (!is_logical_operator) {
                if (!typeid_eq(cast_left, left->value_type)) {
                    an_lhs(expr) = implicit_cast(ast, left, cast_left);
                    fold_constants_via_runtime(analyzer, ast, state, an_lhs(expr));
                }


                if (!typeid_eq(cast_right, right->value_type)) {
                    an_rhs(expr) = implicit_cast(ast, right, cast_right);
                    fold_constants_via_runtime(analyzer, ast, state, an_rhs(expr));
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            resolve_expression(analyzer, ast, state, an_operand(expr));

            if (TYPE_IS_INVALID(an_operand(expr)->value_type)) {
                INVALIDATE(expr);
                break;
            }

            // todo: find a better way to check inside type context
            bool is_inside_type_context = false;
            if (is_inside_type_context && expr->operator.type == TOKEN_AMPERSAND) {
                if (!TYPE_IS_TYPE(an_operand(expr)->value_type)) {
                    INVALIDATE(expr);
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_UNARY_OPERAND, expr));
                    break;
                }

                unless (IS_FOLDED(an_operand(expr))) {
                    INVALIDATE(expr);
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CONSTANT, an_operand(expr)));
                    break;
                }
            } else {
                type_t new_type = resolve_unary_type(ast, expr->operator.type, an_operand(expr)->value_type);
                expr->value_type = new_type;
                
                if (TYPE_IS_INVALID(expr->value_type)) {
                    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_UNARY_OPERAND, expr));
                    break;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DEF_VALUE: {
            expr->lvalue_node = expr;

            scope_t *def_scope;
            bool is_inside_type_context = false;
            def_query_t query = {
                // todo: find a better way to keep track of if inside type context
                // if looking for type it should have a search type for type
                .search_type = is_inside_type_context ? typeid(TYPE_TYPE) : typeid(TYPE_INVALID),
                .skip_mutable = (state.mode & MODE_CONSTANT_TIME),
                .flags = is_inside_type_context ? QUERY_FLAG_MATCH_TYPE : QUERY_FLAG_MATCH_ANY,
            };

            ast_node_t *decl = get_resolved_def_by_identifier(analyzer, ast, state, expr->identifier, &query, &def_scope);

            if (decl == NULL) {
                INVALIDATE(expr);
                break;
            }

            // keep for constant folding so not redo definition look up and simplify work there
            expr->ref_decl = decl;

            expr->value_type = decl->value_type;

            if (!decl->is_mutable) {
                expr->foldable = true;
                expr->value_index = decl->value_index;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            ast_node_t *left = an_lhs(expr);
            ASSERT(left, "for now left must be present");

            if (an_is_notnone(expr->ref_decl)) {
                expr->value_type = expr->ref_decl->value_type;
                break;
            }

            resolve_expression(analyzer, ast, state, left);
            // expression->lvalue_node = expression;

            if (TYPE_IS_INVALID(left->value_type)) {
                INVALIDATE(expr);
                break;
            }

            ast_node_and_scope_t node_and_scope;
            bool skip_mutable = false;
            if (type_is_struct(ast->type_set.types, left->value_type)) {
                table_get(type2ns, ast->type_to_creation_node, left->value_type, &node_and_scope);
            } else if (TYPE_IS_TYPE(left->value_type) && type_is_struct(ast->type_set.types, get_folded_type(ast, left->value_index))) {
                type_t struct_type = get_folded_type(ast, left->value_index);
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

                def_query_t query = {
                    .flags = QUERY_FLAG_MATCH_ANY | QUERY_FLAG_MATCH_ONLY_IN_GIVEN_SCOPE,
                    .skip_mutable = skip_mutable,
                };

                scope_t* found_scope;
                ast_node_t *decl = get_resolved_def_by_identifier(analyzer, ast, search_state, expr->identifier, &query, &found_scope);

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

            if (lvalue_node->node_type == AST_NODE_TYPE_EXPRESSION_DEF_VALUE) {
                scope_t *def_scope;
                ast_node_t *def;
                token_t identifier = lvalue_node->identifier;
                def = get_resolved_def_by_identifier(analyzer, ast, state, identifier, NULL, &def_scope);

                
                if (def == NULL) {
                    INVALIDATE(expr);
                    break;
                }

                expr->value_type = def->value_type;

                type_t rhs_type = rhs->value_type;
                unless (typeid_eq(def->value_type, rhs_type)) {
                    stan_error(analyzer, make_type_mismatch_error(lhs, rhs));
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
                    stan_error(analyzer, make_type_mismatch_error(lhs, rhs));
                    break;
                }

                expr->value_type = lvalue_node->value_type;

            } else {
                UNREACHABLE();
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            i32 declarations_count = expr->children.count;

            scope_t block_scope;
            scope_init(&block_scope, &analyzer->allocator, SCOPE_TYPE_BLOCK, state.scope, expr);

            forward_scan_declaration_names(analyzer, &block_scope, expr->children, declarations_count);

            analysis_state_t block_state = state;
            block_state.scope = &block_scope;
            block_state.mode = MODE_RUNTIME | (state.mode & MODE_FOLDING_TIME);
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
                    return_state.mode = MODE_RUNTIME | (state.mode & MODE_FOLDING_TIME);
                    break;
                }

                case BRANCH_TYPE_IFTHEN: break;
            }

            resolve_expression(analyzer, ast, return_state, an_then(expr));

            resolve_expression(analyzer, ast, state, an_else(expr));

            type_t branch_type = resolve_block_return_types(expr);
            expr->value_type = branch_type;

            if (!TYPE_IS_UNREACHABLE(branch_type)) {
                if (TYPE_IS_VOID(an_then(expr)->value_type) && an_then(expr)->node_type == AST_NODE_TYPE_EXPRESSION_NIL) {
                    an_then(expr) = ast_nil(ast, branch_type, make_token_implicit(an_then(expr)->start));
                }

                if (TYPE_IS_VOID(an_else(expr)->value_type) && an_else(expr)->node_type == AST_NODE_TYPE_EXPRESSION_NIL) {
                    an_else(expr) = ast_nil(ast, branch_type, make_token_implicit(an_else(expr)->start));
                }
            }

            if (TYPE_IS_INVALID(branch_type)) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_BLOCK_RETURNS_MISMATCH, expr));
                INVALIDATE(expr);
            }

            if (TYPE_IS_INVALID(an_then(expr)->value_type) || TYPE_IS_INVALID(an_else(expr)->value_type)) {
                INVALIDATE(expr);
            }

            unless (typeid_eq(an_condition(expr)->value_type, typeid(TYPE_BOOL))) {
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
                    bool success = get_nearest_scope_in_func_or_error(analyzer, expr, state.scope, SCOPE_TYPE_FUNC_DEF, lit2sv(""), &func_def_scope);
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
                unless (TYPE_IS_INVALID(argument->value_type)) {
                    continue;
                }

                argument_invalid = true;
            }

            ast_node_t *callee = an_callee(expr);
            resolve_expression(analyzer, ast, state, callee);

            if (argument_invalid || TYPE_IS_INVALID(callee->value_type)) {
                INVALIDATE(expr);
                break;
            }

            type_t callee_type = callee->value_type;

            if ((!type_is_function(ast->type_set.types, callee_type) && !type_is_native_function(ast->type_set.types, callee_type)) || !can_call(ast->type_set.types, callee_type, expr->children, an_call_arg_start(expr), an_call_arg_end(expr))) {
                stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_CALLABLE, an_callee(expr)));
                break;
            }

            type_info_t *callee_type_info = ast->type_set.types.items[callee_type.i];
            expr->value_type = callee_type_info->data.function.return_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            resolve_func_def(analyzer, ast, state, expr);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            // since structs are boundaries for circular dependencies, they will never be the cause of one.
            push_dependency(analyzer, expr, state.fold_level, is_value_circular_dependency);
            resolve_struct_definition(analyzer, ast, state, expr);
            pop_dependency(analyzer);
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

            fold_function_signature(analyzer, ast, expr);

            expr->value_type = typeid(TYPE_TYPE);
            break;
        }

        case AST_NODE_TYPE_NONE:
        case AST_NODE_TYPE_DECLARATION_DEFINITION:
        case AST_NODE_TYPE_DECLARATION_STATEMENT:
        case AST_NODE_TYPE_MODULE:
        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: {
            UNREACHABLE();
            break;
        }
    }

    // we shouldn't fold expressions that are undefined (blocks or ifelses that return)
    if (TYPE_IS_UNDEFINED(expr->value_type) || TYPE_IS_INVALID(expr->value_type)) {
        return;
    }

    fold_constants_via_runtime(analyzer, ast, state, expr);
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
// due to the way structs are resolved, it is possible for the initial value expression to be swapped out with another
// this is simply a safer way of accessing the initial value expression

    type_t declaration_type = typeid(TYPE_UNRESOLVED);
    if (TYPE_IS_UNRESOLVED(declaration->value_type) && !an_is_none(an_decl_type(declaration))) {
        bool pushed = push_dependency(analyzer, an_decl_type(declaration), state.fold_level, is_value_circular_dependency);
        if (pushed) {
            analysis_state_t new_state = state;
            new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
            resolve_expression(analyzer, ast, new_state, an_decl_type(declaration));
            declaration_type = get_folded_type(ast, an_decl_type(declaration)->value_index);
            pop_dependency(analyzer);
        } else {
            declaration_type = typeid(TYPE_INVALID);
        }
    }

    if (!an_is_none(an_decl_expr(declaration))) {
        if (TYPE_IS_UNRESOLVED(an_decl_expr(declaration)->value_type)) {
            analysis_state_t new_state = state;
            switch (state.scope->type) {
                case SCOPE_TYPE_FUNC_DEF_BODY:
                case SCOPE_TYPE_BLOCK:
                case SCOPE_TYPE_CONDITION:
                case SCOPE_TYPE_JMPABLE:
                case SCOPE_TYPE_MODULE: {
                    folding_mode_t mode = declaration->is_mutable ? MODE_RUNTIME : MODE_CONSTANT_TIME;
                    mode = mode | (state.mode & MODE_FOLDING_TIME);
                    new_state.mode = mode;
                    break;
                }

                case SCOPE_TYPE_STRUCT:
                case SCOPE_TYPE_FUNC_DEF: {
                    new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
                    break;
                }

                case SCOPE_TYPE_NONE: UNREACHABLE(); break;
            }

            bool pushed = push_dependency(analyzer, an_decl_expr(declaration), new_state.fold_level, is_value_circular_dependency);
            if (pushed) {
                resolve_expression(analyzer, ast, new_state, an_decl_expr(declaration));
                pop_dependency(analyzer);
            } else {
                unless (TYPE_IS_UNRESOLVED(declaration_type)) {
                    declaration->value_type = declaration_type;
                }
                INVALIDATE(an_decl_expr(declaration));
            }
        } else {
            // if (is_sizing_circular_dependency(analyzer, initial_expression)) {
            //     unless (TYPE_IS_UNRESOLVED(declaration_type)) {
            //         declaration->value_type = declaration_type;
            //     }
            //     error_range(analyzer, declaration->start, declaration->end, "Circular dependency (definition declaration)");
            //     INVALIDATE(declaration->data.declaration.initial_value_expression);
            // }
        }
    }

    // we are resolved
    if (is_declaration_resolved(declaration)) {
        if (!TYPE_IS_TYPE(declaration->value_type)) {
            return;
        }

        // This must be available at compile time
        type_t struct_type = get_folded_type(ast, declaration->value_index);
        type_info_t *struct_type_info = ast->type_set.types.items[struct_type.i];
        unless (struct_type_is_incomplete(struct_type_info) && struct_type_info->data.struct_.name) {
            return;
        }

        ast_node_t *cast_node = an_decl_expr(declaration);
        ASSERT(cast_node->node_type == AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT, "must be implicit casting node");
        
        type_t completed_struct_type = an_operand(cast_node)->value_type;
        
        // this means that we found out later that the struct this was supposed to be was actually invalid, so we need to fix the ast
        if (TYPE_IS_INVALID(completed_struct_type)) {
            INVALIDATE(cast_node);
            INVALIDATE(declaration);
            return;
        }

        ASSERT(type_is_struct(ast->type_set.types, completed_struct_type), "casted expression must be a struct type");

        type_info_t *completed_struct_type_info = ast->type_set.types.items[completed_struct_type.i];
        if (struct_type_is_incomplete(completed_struct_type_info)) {
            return;
        }

        named_struct_copy_data_from_completed_struct_type(&ast->type_set, struct_type, completed_struct_type);
        return;
    }



    if (!an_is_none(an_decl_expr(declaration)) && (TYPE_IS_UNDEFINED(an_decl_expr(declaration)->value_type) || TYPE_IS_INVALID(an_decl_expr(declaration)->value_type))) {
        if (TYPE_IS_UNDEFINED(an_decl_expr(declaration)->value_type)) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_RESOLVED, an_decl_expr(declaration)));
        }
        
        INVALIDATE(declaration);
    }

    if (TYPE_IS_VOID(an_decl_expr(declaration)->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_STORE_VOID_EXPRESSIONS, declaration));
        INVALIDATE(declaration);
    }

    unless (declaration->is_mutable) {
        // if (!an_is_none(an_decl_expr(declaration)) && type_is_function(ast->type_set.types, an_decl_expr(declaration)->value_type)) {
        //     function_t *function = NULL;
        //     if (memarr_get(&ast->constants, an_decl_expr(declaration)->value_index.index, sizeof(function_t*), &function)) {
        //         ASSERT(false, "todo");
        //     }

        //     // todo bind name to a function?? 
        // }

        if (!an_is_none(an_decl_expr(declaration))
        && type_is_struct(ast->type_set.types, an_decl_expr(declaration)->value_type)
        && (TYPE_IS_UNRESOLVED(declaration_type) || TYPE_IS_TYPE(declaration_type))) {
            ast_node_t *to_struct_type = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT, an_decl_expr(declaration)->start);
            to_struct_type->value_type = typeid(TYPE_TYPE);
            an_operand(to_struct_type) = an_decl_expr(declaration);

            to_struct_type->fold = false;
            to_struct_type->foldable = true;
            type_info_t *initial_expression_type_info = get_type_info(&ast->type_set.types, an_decl_expr(declaration)->value_type);
            type_t named_struct_id = type_create_struct(&ast->type_set, declaration->start.view.data, declaration->start.view.length, initial_expression_type_info);
            word_t struct_type_slot = WORDU(named_struct_id.i);
            to_struct_type->value_index = add_value_to_ast_constant_stack(ast, &struct_type_slot, typeid(TYPE_TYPE));

            type_t initial_expression_type = an_decl_expr(declaration)->value_type;

            ast_node_and_scope_t node_and_scope;
            bool found = table_get(type2ns, ast->type_to_creation_node, initial_expression_type, &node_and_scope);
            ASSERT(found, "this shoudl always find something");

            an_decl_expr(declaration) = to_struct_type;

            table_put(type2ns, ast->type_to_creation_node, named_struct_id, node_and_scope);
            
            {
                value_index_t value_index = an_operand(to_struct_type)->value_index;
                ASSERT(value_index.exists, "must be the value of the anonymous type");
                table_put(ptr2sizet, ast->type_to_zero_index, named_struct_id, value_index.index);
            }
        }
    }

    // Could be resolved could be unresolved at this point.
    declaration->value_type = declaration_type;

    declaration->fold_level_resolved_at = state.fold_level;

    // TODO: Outer if should be if the expression is null or not
    if (TYPE_IS_UNRESOLVED(declaration->value_type)) {
        ASSERT(!an_is_none(an_decl_expr(declaration)), "this should be a parsing error.");

        type_t expression_type = an_decl_expr(declaration)->value_type;

        if (typeid_eq(expression_type, typeid(TYPE_BOOL)) || typeid_eq(expression_type, ast->type_set.i32_)) {
            declaration->value_type = expression_type;
        } else {
            declaration->value_type = expression_type;
        }
    } else {
        unless (an_is_none(an_decl_expr(declaration)) || typeid_eq(declaration->value_type, an_decl_expr(declaration)->value_type)) {
            unless (TYPE_IS_INVALID(an_decl_expr(declaration)->value_type)) {
                stan_error(analyzer, make_type_mismatch_error(an_decl_type(declaration), an_decl_expr(declaration)));
            }
        }
    }

    if (an_is_none(an_decl_expr(declaration))) {
        value_index_t value_index = value_index_nil();

        unless (TYPE_IS_INVALID(declaration->value_type)) {
            value_index = zero_value(ast, declaration->value_type);
        }

        declaration->value_index = value_index;
        declaration->foldable = value_index.exists;
    } else {
        if (IS_FOLDED(an_decl_expr(declaration))) {
            declaration->value_index = an_decl_expr(declaration)->value_index;
            declaration->foldable = an_decl_expr(declaration)->value_index.exists;
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
            value_slot = WORDU(type.i);
            value_type = typeid(TYPE_TYPE);
        }

        if (has_value) {
            value_index_t index = add_value_to_ast_constant_stack(ast, &value_slot, value_type);
            decl = add_builtin_definition(ast, identifier, value_type, index);
        } else {
            decl = NULL;
        }
    } else {
        decl = (ast_node_t*)def_slot.as.p;
    }

    allocator_return(tmp);
    return decl;
}

static ast_node_t *get_resolved_def_by_identifier(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        token_t identifier_token,
        def_query_t *query,
        scope_t **search_scope) { // TODO: consider removing search scope from params, check if it's actually used

    bool passed_local_mutable_access_barrier = false;

    // early return if looking at a built in type
    {
        ast_node_t *decl = get_builtin_decl(ast, identifier_token.view);
        if (decl) {
            search_scope = NULL;
            return decl;
        }
    }

    word_t def_slot;
    *search_scope = state.scope;

    while (*search_scope) {
        bool is_function_scope = (*search_scope)->type == SCOPE_TYPE_FUNC_DEF;

    #define NEXT_SCOPE() \
        if (is_function_scope) { passed_local_mutable_access_barrier = true; }\
        if (query && query->flags & QUERY_FLAG_MATCH_ONLY_IN_GIVEN_SCOPE) { break; }\
        *search_scope = (*search_scope)->outer

        {
            tmp_arena_t *tmp = allocator_borrow();
            string_t identifier_ = sv2string(identifier_token.view, tmp->allocator);

            unless (table_get(s2w, (*search_scope)->definitions, identifier_, &def_slot)) {
                NEXT_SCOPE();
                continue;
            }

            allocator_return(tmp);
        }


        ast_node_t *decl = (ast_node_t*)def_slot.as.p;

        if (query && query->skip_mutable && decl->is_mutable) {
            NEXT_SCOPE();
            continue;
        }

        if (passed_local_mutable_access_barrier && (*search_scope)->creator != NULL && decl->is_mutable) {
            NEXT_SCOPE();
            continue;
        }

        // this means that the declaration is *after* the definition we are trying to resolve
        if (decl->is_mutable && !is_declaration_resolved(decl)) {
            NEXT_SCOPE();
            continue;
        }

        // ensure that the value is resolved
        if (an_is_notnone(an_decl_expr(decl))) {
            analysis_state_t new_state = state;
            new_state.scope = *search_scope;
            resolve_declaration_definition(analyzer, ast, new_state, decl);
        }

        if (decl->is_mutable && decl->fold_level_resolved_at != state.fold_level) {
            stan_error(analyzer, make_error_node(ERROR_ANALYSIS_CANNOT_ACCESS_MUTABLE_ON_DIFFERENT_FOLD_LEVEL, decl));
            return NULL;
        }

        // footnote(folding-functions)
        // if (type_is_function(ast->type_set.types, decl->value_type)) {

            // UNREACHABLE();

            // function_t_ *function = arena_array_get_t(&ast->constants, definition->node->as.declaration.initial_value_expression->value_index, function_t_*);
            // for (size_t i = 0; i < analyzer->dependencies.count; ++i) {
            //     i32 i_ = analyzer->dependencies.count - 1 - i;
            //     analysis_dependency_t *dependency = &analyzer->dependencies.items[i_];
            //     ast_node_t *node = dependency->ast_node;
            //     if (node->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) {
            //         function_t_ *folded_function = arena_array_get_t(&ast->constants, node->value_index, function_t_*);
            //         if (folded_function == function && dependency->fold_level != state.fold_level) {
            //             // TODO: Use better system to figure out whether function definition can be compiled or not
            //             // In this case, it cannot because of the fold level circular dependency.
            //             node->as.function.compilable = false;

            //             error_range(analyzer, node->start, node->end, ERROR_ANALYSIS_FOLDING_LOOP);
            //             return NULL;
            //         }
            //     }
            // }
        // }

        if (!query) {
            return decl;
        }

        if (query->flags & QUERY_FLAG_MATCH_ANY) {
            return decl;
        }

        type_t type = decl->value_type;

        if (query->flags & QUERY_FLAG_MATCH_TYPE && typeid_eq(query->search_type, type)) {
            return decl;
        }

        if (query->flags & QUERY_FLAG_MACH_FUNCTION && type_is_function(ast->type_set.types, type)) {
            return decl;
        }

        NEXT_SCOPE();

#undef NEXT_SCOPE
    }

    printf("todo: cannot be nil node;\n");
    stan_error(analyzer, make_error_node(ERROR_ANALYSIS_EXPECTED_RESOLVED, &nil_node));

    return NULL;
}

static void resolve_func_def(
        analyzer_t *analyzer,
        ast_t *ast,
        analysis_state_t state,
        ast_node_t *func_def) {
    ASSERT(func_def->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, "must be function declaration at this point");

    if (IS_FOLDED(func_def)) {
        return;
    }

    scope_t function_parameter_scope;
    scope_init(&function_parameter_scope, &analyzer->allocator, SCOPE_TYPE_FUNC_DEF, state.scope, func_def);
    types_t parameter_types = {.allocator=&ast->allocator};

    {
        size_t parameter_count = func_def->children.count-2;
        tmp_arena_t *tmp = allocator_borrow();

        ast_nodes_t parameters = {.allocator=tmp->allocator};
        for (size_t i = an_func_def_arg_start(func_def); i < an_func_def_arg_end(func_def); ++i) {
            array_push(&parameters, func_def->children.items[i]);
        }

        forward_scan_declaration_names(analyzer, &function_parameter_scope, parameters, parameter_count);

        allocator_return(tmp);
    }

    bool parameter_invalid = false;

    // Resolves parameters for function type
    for (size_t i = an_func_def_arg_start(func_def); i < an_func_def_arg_end(func_def); ++i) {
        analysis_state_t new_state = state;
        // parameters look syntacticly like mutables, but their initial expressions should be constants
        new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
        new_state.scope = &function_parameter_scope;
        resolve_declaration_definition(analyzer, ast, new_state, func_def->children.items[i]);
        array_push(&parameter_types, func_def->children.items[i]->value_type);
        

        if (TYPE_IS_INVALID(func_def->children.items[i]->value_type)) {
            parameter_invalid = true;
        }
    }

    type_t return_type = typeid(TYPE_VOID);
    bool pushed = push_dependency(analyzer, an_func_def_return(func_def), state.fold_level, is_value_circular_dependency);
    if (pushed) {
        analysis_state_t new_state = state;
        new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
        resolve_expression(analyzer, ast, state, an_func_def_return(func_def));
        pop_dependency(analyzer);
    }

    ast_node_t *ret_expr = an_func_def_return(func_def);
    unless (TYPE_IS_TYPE(ret_expr->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_INVALID_RETURN_TYPE, an_func_def_return(func_def)));
    } else {
        return_type = get_folded_type(ast, an_func_def_return(func_def)->value_index);
    }

    if (parameter_invalid || TYPE_IS_INVALID(return_type)) {
        INVALIDATE(func_def);
        return;
    }

    type_t function_type = type_set_fetch_function(&ast->type_set, return_type, parameter_types);
    func_def->value_type = function_type;

    pushed = push_dependency(analyzer, func_def, state.fold_level, is_value_circular_dependency);
    if (pushed) {
        analysis_state_t new_state = state;
        new_state.mode = MODE_RUNTIME;

        scope_t function_scope;
        scope_init(&function_scope, &analyzer->allocator, SCOPE_TYPE_FUNC_DEF_BODY, &function_parameter_scope, func_def);
        new_state.scope = &function_scope;
        resolve_expression(analyzer, ast, new_state, an_func_def_block(func_def));
        pop_dependency(analyzer);
    }

    unless (TYPE_IS_UNREACHABLE(an_func_def_block(func_def)->value_type)) {
        stan_error(analyzer, make_error_node(ERROR_ANALYSIS_FUNCTION_MUST_RETURN_ON_ALL_BRANCHES, func_def));
    }

    for (size_t i = 0; i < func_def->jmp_nodes.count; ++i) {
        ast_node_t *jmp = func_def->jmp_nodes.items[i];
        
        ast_node_t *ret_expr = an_expression(jmp);

        type_t ret_expr_type = ret_expr->value_type;
        unless (typeid_eq(ret_expr_type, return_type)) {
            stan_error(analyzer, make_type_mismatch_error(an_func_def_return(func_def), ret_expr));
            INVALIDATE(func_def);
        }
    }
}

// footnote(struct-resolution)
static void resolve_struct_definition(analyzer_t *analyzer, ast_t *ast, analysis_state_t state, ast_node_t *struct_definition) {
    scope_t struct_scope;
    scope_init(&struct_scope, &analyzer->allocator, SCOPE_TYPE_STRUCT, state.scope, struct_definition);

    state.fold_level = MODE_CONSTANT_TIME;
    state.scope = &struct_scope;

    forward_scan_declaration_names(analyzer, state.scope, struct_definition->as.struct_.declarations, struct_definition->as.struct_.declarations.count);

    i32 declarations_count = struct_definition->as.struct_.declarations.count;

    type_t incomplete_struct_id = type_unique_incomplete_struct_type(&ast->type_set);
    struct_definition->value_type = incomplete_struct_id;

    struct_definition->foldable = true;
    struct_definition->value_index = value_index_(0);

    ast_node_and_scope_t node_and_scope = {
        .node = struct_definition,
        .scope = &struct_scope
    };
    table_put(type2ns, ast->type_to_creation_node, struct_definition->value_type, node_and_scope);

    i32 field_count = 0;

    bool invalid_struct = false;
    for (i32 i = 0; i < declarations_count; i++) {
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

    i32 constant_count = declarations_count - field_count;

    struct_field_t fields[field_count];
    ast_node_t* ast_fields[field_count];
    struct_constant_t constants[constant_count];

    {
        i32 field_counter = 0;
        i32 constant_counter = 0;

        for (i32 i = 0; i < declarations_count; ++i) {
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

    type_info_t *complete_struct_type_info;

    type_t complete_struct_type = type_set_fetch_anonymous_struct(&ast->type_set, field_count, fields, constant_count, constants);
    complete_struct_type_info = ast->type_set.types.items[complete_struct_type.i];

    i32 incomplete_index = -1;
    for (i32 i = 0; i < complete_struct_type_info->data.struct_.field_count; i++) {
        type_t field_type = complete_struct_type_info->data.struct_.fields[i].type;
        type_info_t *field_type_info = ast->type_set.types.items[field_type.i];
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

        for (i32 i = 0; i < field_count; i++) {
            type_t field_type = complete_struct_type_info->data.struct_.fields[i].type;
            u32 offset = complete_struct_type_info->data.struct_.fields[i].offset;
            ast_node_t *declaration = ast_fields[i];

            type_info_t *field_type_info = get_type_info(&ast->type_set.types, field_type);

            u32 bytes_to_copy = field_type_info->size;

            void *value_src = ast->constants.data + declaration->value_index.index;
            memcpy(struct_data + offset, value_src, bytes_to_copy);
        }

        struct_definition->value_index = add_value_to_ast_constant_stack(ast, struct_data, complete_struct_type);

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

void resolve_declarations(analyzer_t* analyzer, ast_t* ast, analysis_state_t state, ast_nodes_t declarations, i32 count) {
    for (i32 i = 0; i < count; i++) {
        ast_node_t* declaration = declarations.items[i];
        if (declaration->node_type == AST_NODE_TYPE_DECLARATION_DEFINITION && !declaration->is_mutable) {
            continue;
        }

        resolve_declaration(analyzer, ast, state, declarations.items[i]);
    }

    for (i32 i = 0; i < count; i++) {
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
        if (ast->root->children.items == NULL) {
            ast->resolved = false;
            // error(analyzer, 0, "No code to run. Akin to having no main function.");
            return false;
        }

        scope_t global_scope = {0};
        scope_init(&global_scope, &analyzer->allocator, SCOPE_TYPE_MODULE, NULL, NULL);

        analysis_state_t analysis_state = (analysis_state_t) {
            .mode = MODE_RUNTIME,
            .scope = &global_scope,
            .fold_level = 0,
        };

        i32 declaration_count = ast->root->children.count;
        forward_scan_declaration_names(analyzer, &global_scope, ast->root->children, declaration_count);
        resolve_declarations(analyzer, ast, analysis_state, ast->root->children, declaration_count);

        ast->resolved = !analyzer->had_error;

        return ast->resolved;
    } else {
        scope_t global_scope = {0};
        scope_init(&global_scope, &analyzer->allocator, SCOPE_TYPE_MODULE, NULL, NULL);

        analysis_state_t state = (analysis_state_t) {
            .mode = MODE_RUNTIME,
            .scope = &global_scope,
            .fold_level = 0,
        };

        resolve_expression(analyzer, ast, state, ast->root);

        ast->resolved = !analyzer->had_error;

        return ast->resolved;
    }
}

void analyzer_init(analyzer_t* analyzer, write_function_t write_fn, error_function_t error_fn) {
    analyzer->error_fn = error_fn;
    analyzer->had_error = false;

    analyzer->allocator = (arena_t){0};

    analyzer->dependencies = (analysis_dependencies_t){.allocator=&analyzer->allocator};

    // TODO: fix
    (void)write_fn;

    analyzer->symbols = table_new(s2w, &analyzer->allocator);
}

void analyzer_free(analyzer_t* analyzer) {
    analyzer->dependencies.count = 0;

    analyzer->ast = NULL;

    analyzer->error_fn = NULL;
    analyzer->had_error = false;
}
