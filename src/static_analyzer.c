#include "static_analyzer.h"

#include <stdio.h>

#include "interpreter.h"
#include "mathutils.h"
#include "type_set.h"
#include "error.h"

#include <time.h>

#define EXPRESSION_RESOLVED(expression) (!TYPE_IS_UNRESOLVED((expression)->value_type))

#define INVALIDATE(NODE) do {\
    ast_node_t* node = NODE;\
    node->value_type = typeid(TYPE_INVALID);\
    node->value_index = -1;\
} while(false)

typedef bool (*IsCircularDependencyFunc)(analyzer_t*, ast_node_t*);

static void clock_native(slot_t* arguments, slot_t* result) {
    (void)arguments;
    result[0] = SLOT_F((double)clock() / CLOCKS_PER_SEC);
}

typedef symbol_table_t SymbolTable;

typedef enum ExpressionFoldingMode {
    MODE_RUNTIME = 0x1,
    MODE_CONSTANT_TIME = 0x2,
    MODE_FOLDING_TIME = 0x4,
} ExpressionFoldingMode;

typedef struct AnalysisState {
    i32 fold_level;
    ExpressionFoldingMode mode;
    scope_t* scope;
} AnalysisState;

typedef struct Entity {
    // TODO: instead of using declarad type just use narrowed type and the type of the decalrtion node
    type_t declared_type;
    type_t narrowed_type;

    ast_node_t* node;
    i32 value_index;

} Entity;

static void scope_init(scope_t* scope, arena_t *allocator, scope_type_t type, scope_t* outer, ast_node_t* creator_expression) {
    scope->outer = outer;
    scope->creator = creator_expression;
    scope->type = type;
    symbol_table_init(&scope->named_entities, allocator);
}

static scope_t *scope_copy_new(scope_t *scope, arena_t *allocator) {
    if (scope == NULL) {
        return NULL;
    }

    scope_t *scope_copy = arena_alloc(allocator, sizeof(scope_t));
    scope_init(scope_copy, allocator, scope->type, NULL, scope->creator);
    symbol_table_add_all(&scope->named_entities, &scope_copy->named_entities);

    for (i32 i = 0; i < scope_copy->named_entities.capacity; i++) {
        symbol_table_entry_t* entry = &scope_copy->named_entities.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        Entity* entity_copy = (Entity*)arena_alloc(allocator, sizeof(Entity));
        *entity_copy = *((Entity*)entry->value.as.p);
        entry->value = SLOT_P(entity_copy);
    }

    scope_copy->outer = scope_copy_new(scope->outer, allocator);

    return scope_copy;
}

static void scope_merge(type_table_t* set, scope_t* scope, scope_t* a, scope_t* b) {
    if (!scope) {
        ASSERT(!a && !b, "if no more scopes then all should be no more");
        return;
    }

    for (i32 i = 0; i < scope->named_entities.capacity; i++) {
        symbol_table_entry_t* entry = &scope->named_entities.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        slot_t entity_a_slot;
        slot_t entity_b_slot;
        if (!symbol_table_get(&a->named_entities, entry->key, &entity_a_slot)
                || !symbol_table_get(&b->named_entities, entry->key, &entity_b_slot)) {
            continue;
        }

        Entity* entity_a = (Entity*)entity_a_slot.as.p;
        Entity* entity_b = (Entity*)entity_b_slot.as.p;

        type_t anded_type_id;
        type_t anded_narrowed_id;
        if (TYPE_IS_UNRESOLVED(entity_a->declared_type) || TYPE_IS_UNRESOLVED(entity_b->declared_type)) {
            if (typeid_eq(entity_a->declared_type, entity_b->declared_type)) {
                continue;
            }

            // since one of the entities was not narrowed, we should use the declared type
            // instead of the narrowed type. (since the other side wasn't narrowed to anything)
            if (!TYPE_IS_UNRESOLVED(entity_a->declared_type)) {
                anded_type_id = entity_a->declared_type;
                anded_narrowed_id = entity_a->declared_type;
            } else {
                anded_type_id = entity_b->declared_type;
                anded_narrowed_id = entity_b->declared_type;
            }
        } else {
            ASSERT(typeid_eq(entity_a->declared_type, entity_b->declared_type), "declared type should be stable. TODO: No need for declared type to be in here... should be in the declaration instead.");
            anded_type_id = entity_a->declared_type;
            anded_narrowed_id = type_merge(set, entity_a->narrowed_type, entity_b->narrowed_type);
        }

        Entity* scope_entity = (Entity*)entry->value.as.p;

        scope_entity->declared_type = anded_type_id;
        scope_entity->narrowed_type = anded_narrowed_id;
    }

    scope_merge(set, scope->outer, a->outer, b->outer);
}

static void add_entity(scope_t* scope, arena_t *allocator, symbol_t* identifier, ast_node_t* declaration_node) {
    Entity* entity = arena_alloc(allocator, sizeof(Entity));
    entity->declared_type = typeid(TYPE_UNRESOLVED);
    entity->narrowed_type = typeid(TYPE_UNRESOLVED);
    entity->node = declaration_node;
    entity->value_index = -1;

    symbol_table_set(&scope->named_entities, identifier, SLOT_P(entity));
}

static Entity* add_builtin_entity(ast_t *ast, symbol_t *identifier, type_t type_id, i32 value_index) {
    Entity* entity = arena_alloc(&ast->allocator, sizeof(Entity));
    entity->declared_type = type_id;
    entity->narrowed_type = type_id;
    entity->node = NULL;
    entity->value_index = value_index;

    symbol_table_set(&ast->builtins, identifier, SLOT_P(entity));

    return entity;
}

static void function_dependencies_cannot_be_compiled(analyzer_t *analyzer) {
    for (size_t i = 0; i < analyzer->dependencies.count; ++i) {
        analysis_dependency_t *dependency = &analyzer->dependencies.items[i];
        ast_node_t *node = dependency->ast_node;
        if (node->node_type != AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) {
            continue;
        }

        node->data.function.compilable = false;
    }
}

static void error_token(analyzer_t* analyzer, token_t token, const char* message) {
    analyzer->had_error = true;

    function_dependencies_cannot_be_compiled(analyzer);
    error_t error = {
        .type = ERROR_COMPILE,
        .region_type = ERROR_REGION_TYPE_TOKEN,
        .region.token = token,
        .message = (char*)message
    };

    analyzer->error_fn(error);
}

static void error_range(analyzer_t* analyzer, token_t start, token_t end, const char* message) {
    analyzer->had_error = true;

    function_dependencies_cannot_be_compiled(analyzer);
    error_t error = {
        .type = ERROR_COMPILE,
        .region_type = ERROR_REGION_TYPE_RANGE,
        .region.range.start = start,
        .region.range.end = end,
        .message = (char*)message,
    };

    analyzer->error_fn(error);
}

static void error_range2(analyzer_t* analyzer, token_t start1, token_t end1, token_t start2, token_t end2, const char* message) {
    analyzer->had_error = true;

    function_dependencies_cannot_be_compiled(analyzer);
    error_t error = {
        .type = ERROR_COMPILE,
        .region_type = ERROR_REGION_TYPE_TWO_RANGES,
        .region.range2.start1 = start1,
        .region.range2.end1 = end1,
        .region.range2.start2 = start2,
        .region.range2.end2 = end2,
        .message = (char*)message
    };
    
    analyzer->error_fn(error);
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
            if (!type_is_union(ast->type_set.types, operand_id)) {
                if (type_is_number(operand, false)) {
                    return operand_id;
                } else if (operand == &OrsoTypeBool) {
                    return typeid(TYPE_INT32);
                } else {
                    return typeid(TYPE_INVALID);
                }
            } else {
                return typeid(TYPE_INVALID);
            }
        }
        case TOKEN_NOT:
            return typeid(TYPE_BOOL);
        case TOKEN_AMPERSAND: {
            type_t type_id = type_set_fetch_pointer(&ast->type_set, operand_id);
            return type_id;
        }

        default: return typeid(TYPE_INVALID);
    }
}

static ast_node_t *implicit_cast(ast_t *ast, ast_node_t *operand, type_t value_type) {
    ast_node_t* implicit_cast = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT, operand->is_in_type_context, operand->start);
    implicit_cast->end = operand->end;

    implicit_cast->value_type = value_type;
    implicit_cast->data.expression = operand;
    implicit_cast->fold = false;
    implicit_cast->foldable = true;
    implicit_cast->value_index = -1;

    return implicit_cast;
}

#define IS_FOLDED(EXPRESSION_PTR) (EXPRESSION_PTR->value_index >= 0)

typedef enum {
    QUERY_FLAG_MATCH_ANY = 0x1,
    QUERY_FLAG_MATCH_ONLY_IN_GIVEN_SCOPE = 0x2,
    QUERY_FLAG_MATCH_TYPE = 0x4,
    QUERY_FLAG_MACH_FUNCTION = 0x8,
} QueryFlags;

typedef struct EntityQuery  {
    QueryFlags flags;
    type_t search_type;
    bool skip_mutable;
} EntityQuery;

static Entity* get_resolved_entity_by_identifier(
        analyzer_t* analyzer,
        ast_t* ast,
        AnalysisState state,
        token_t identifier_token,
        EntityQuery* query,
        scope_t** found_scope);

static bool is_builtin_type(symbol_t* identifier, type_t *type_id) {
#define RETURN_IF_TYPE(SYMBOL, TYPE_STRING, TYPE_ID) \
if (strlen(SYMBOL->text) == (sizeof(#TYPE_STRING) - 1) && \
    memcmp(SYMBOL->text, #TYPE_STRING, (sizeof(#TYPE_STRING) - 1)) == 0) { \
    type_id->i = TYPE_ID; \
    return true; \
}

    RETURN_IF_TYPE(identifier, void, TYPE_VOID)
    RETURN_IF_TYPE(identifier, bool, TYPE_BOOL)
    RETURN_IF_TYPE(identifier, i32, TYPE_INT32)
    RETURN_IF_TYPE(identifier, i64, TYPE_INT64)
    RETURN_IF_TYPE(identifier, f32, TYPE_FLOAT32)
    RETURN_IF_TYPE(identifier, f64, TYPE_FLOAT64)
    RETURN_IF_TYPE(identifier, string, TYPE_STRING)
    RETURN_IF_TYPE(identifier, symbol, TYPE_SYMBOL)
    RETURN_IF_TYPE(identifier, type, TYPE_TYPE)

    return false;

#undef RETURN_IF_TYPE
}

static bool is_builtin_function(ast_t *ast, symbol_t *identifier, native_function_t **function) {
    if (identifier->length == 5 && strncmp(identifier->text, "clock", 5) == 0) {
        type_t function_type_id = type_set_fetch_native_function(&ast->type_set, typeid(TYPE_INT64), (types_t){0});
        *function = orso_new_native_function(clock_native, function_type_id, &ast->allocator);
        return true;
    }

    return false;
}

static bool can_call(type_infos_t types, type_t type_id, ast_nodes_t arguments) {
    ASSERT(type_is_function(types, type_id), "must be a function type");

    type_info_t *type = types.items[type_id.i];

    if (type->data.function.argument_types.count != arguments.count) {
        return false;
    }

    for (size_t i = 0; i < type->data.function.argument_types.count; ++i) {
        type_t parameter_type_id = type->data.function.argument_types.items[i];
        type_t argument_type_id = arguments.items[i]->value_type;
        unless (typeid_eq(parameter_type_id, argument_type_id)) {
            return false;
        }
    }

    return true;
}

static i32 evaluate_expression(analyzer_t* analyzer, ast_t* ast, bool is_folding_time, ast_node_t* expression) {
    // vm_t *vm = &analyzer->vm;

    // compile_expression_to_vm(vm, expression);

    // vm_run(vm);

    // void *result = vm_get_global(vm, "__result");
    
    // i32 index = add_value_to_ast_constant_stack(ast, result, expression->value_type);

    // return index;

    vm_t vm;
    // TODO: Make sure this uses the same writing function as the vm that runs the code at the end.
    vm_init(&vm, NULL, 256);
    
    // TODO: try to set this in a more reobust place
    vm.type_set = &ast->type_set;

    code_builder_t builder;
    code_builder_init(&builder, &vm, ast);

    function_t* function = generate_expression_function(&builder, expression, is_folding_time, &analyzer->allocator);

    code_builder_free(&builder);

    slot_t* value = orso_call_function(&vm, function, analyzer->error_fn);

    i32 value_index = add_value_to_ast_constant_stack(ast, value, expression->value_type);

    return value_index;
}

static bool is_declaration_resolved(ast_node_t* entity) {
    ast_declaration_t* declaration = &entity->data.declaration;
    if (TYPE_IS_INVALID(entity->value_type)) {
        return true;
    }

    return (entity->value_index >= 0 || (declaration->initial_value_expression && !TYPE_IS_UNRESOLVED(declaration->initial_value_expression->value_type))) && !TYPE_IS_UNRESOLVED(entity->value_type);
}

static bool is_block_compile_time_foldable(ast_nodes_t block) {
    for (size_t i = 0; i < block.count; i++) {
        ast_node_t *declaration = block.items[i];
        switch (declaration->node_type) {
            case AST_NODE_TYPE_DECLARATION: {
                if (!is_declaration_resolved(declaration)) {
                    return false;
                }
                break;
            }
            
            case AST_NODE_TYPE_STATEMENT_EXPRESSION: {
                if (!EXPRESSION_RESOLVED(declaration->data.expression) || TYPE_IS_INVALID(declaration->data.expression->value_type)) {
                    return false;
                }
                break;
            }
            
            case AST_NODE_TYPE_STATEMENT_RETURN: return false;
            case AST_NODE_TYPE_UNDEFINED: return false;
            case AST_NODE_TYPE_EXPRESSION_CASE: UNREACHABLE();
        }
    }

    return true;
}

static void fold_constants_via_runtime(
        analyzer_t* analyzer,
        ast_t* ast,
        AnalysisState state,
        ast_node_t* expression);

static void resolve_foldable(
        analyzer_t* analyzer,
        ast_t* ast,
        AnalysisState state,
        ast_node_t* expression) {

    bool foldable = false;
    i32 folded_index = -1;

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            foldable = expression->data.expression->foldable;
            folded_index = expression->data.expression->value_index;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            if (expression->data.initiailizer.arguments.count == 0) {
                foldable = true;
                type_t type_id = get_folded_type(ast, expression->data.initiailizer.type->value_index);
                i32 value_index = zero_value(ast, type_id, ast->symbols);
                folded_index = value_index;
            } else {
                foldable = true;
                for (size_t i = 0; i < expression->data.initiailizer.arguments.count; ++i) {
                    ast_node_t *arg = expression->data.initiailizer.arguments.items[i];
                    unless (arg) continue;
                    foldable &= arg->foldable;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            ast_node_t *left = expression->data.binary.lhs;
            ast_node_t *right = expression->data.binary.rhs;
            foldable = left->foldable && right->foldable;

            if (expression->operator.type == TOKEN_BAR) {
                ASSERT(TYPE_IS_TYPE(left->value_type) && TYPE_IS_TYPE(right->value_type), "both left and right must be types");
                type_t lhs_folded_type_id = get_folded_type(ast, left->value_index);
                type_t rhs_folded_type_id = get_folded_type(ast, right->value_index);

                type_t merged_type_id = type_merge(&ast->type_set, lhs_folded_type_id, rhs_folded_type_id);
                slot_t merged_type_slot = SLOT_U(merged_type_id.i);

                folded_index = add_value_to_ast_constant_stack(ast, &merged_type_slot, typeid(TYPE_TYPE));
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            bool condition_is_foldable = expression->data.branch.condition->foldable;
            bool then_is_foldable = expression->data.branch.then_expression->foldable;
            bool else_is_foldable = expression->data.branch.else_expression ? expression->data.branch.else_expression->foldable : true;

            foldable = condition_is_foldable && then_is_foldable && else_is_foldable;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            foldable = expression->data.expression->foldable;

            if (expression->operator.type == TOKEN_AMPERSAND) {
                if (expression->is_in_type_context) {
                    type_t type = get_folded_type(ast, expression->data.expression->value_index);

                    type_t pointer_type_id = type_set_fetch_pointer(&ast->type_set, type);

                    expression->value_type = typeid(TYPE_TYPE);

                    slot_t type_slot = SLOT_U(pointer_type_id.i);
                    
                    folded_index = add_value_to_ast_constant_stack(ast, &type_slot, typeid(TYPE_TYPE));
                } else {
                    foldable = false;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ENTITY: {
            ast_node_t* referencing_declaration = expression->data.dot.referencing_declaration;

            // this happens when the referencing declaration cannot be found OR for a builtin type
            unless (referencing_declaration) {
                foldable = expression->foldable;
                folded_index = expression->value_index;
                break;
            }

            if (referencing_declaration->data.declaration.is_mutable) {
                foldable = false;
                break;
            }

            if (referencing_declaration->data.declaration.initial_value_expression) {
                foldable = referencing_declaration->data.declaration.initial_value_expression->foldable;
                folded_index = referencing_declaration->data.declaration.initial_value_expression->value_index;

                unless (TYPE_IS_INVALID(referencing_declaration->data.declaration.initial_value_expression->value_type)) {
                    ASSERT(folded_index >= 0, "since the entity is a constant, it should have a folded value already");
                }
            } else {
                foldable = true;
                folded_index = referencing_declaration->value_index;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            ASSERT(expression->data.dot.referencing_declaration, "referencing declaration must be present for dot expression");

            foldable = expression->foldable && expression->data.dot.referencing_declaration->foldable;
            folded_index = foldable ? expression->data.dot.referencing_declaration->value_index : -1;
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
            if (type_is_function(ast->type_set.types, expression->data.call.callee->value_type)) {
                function_t* function = (function_t*)ast->folded_constants.items[expression->data.call.callee->value_index].as.p;
                ast_node_t* function_definition = NULL;
                for (size_t i = 0; i < ast->function_definition_pairs.count; ++i) {
                    if (function == ast->function_definition_pairs.items[i].function) {
                        function_definition = ast->function_definition_pairs.items[i].ast_defintion;
                        break;
                    }
                }

                ASSERT(function_definition, "this has to exist in the pair list");

                unless (function_definition->data.function.compilable) {
                    foldable = false;
                    error_range(analyzer, function_definition->start, function_definition->end, "Cannot fold due to error within function definition.");
                    break;
                }
            }

            foldable = expression->data.call.callee->foldable;

            unless (foldable) {
                break;
            }

            for (size_t i = 0; i < expression->data.call.arguments.count; ++i) {
                foldable &= expression->data.call.arguments.items[i]->foldable;
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

            foldable = is_block_compile_time_foldable(expression->data.block);
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            unless (state.mode & MODE_FOLDING_TIME) {
                break;
            }

            foldable = true;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STATEMENT: {
            switch (expression->data.statement->node_type) {
                case AST_NODE_TYPE_STATEMENT_EXPRESSION: {
                    foldable = expression->data.statement->data.expression->foldable;
                    break;
                }

                case AST_NODE_TYPE_UNDEFINED:
                case AST_NODE_TYPE_STATEMENT_RETURN: foldable = false; break;

                case AST_NODE_TYPE_DECLARATION:
                case AST_NODE_TYPE_EXPRESSION_CASE: UNREACHABLE(); 
            }
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_PRINT:
        case AST_NODE_TYPE_EXPRESSION_PRINT_EXPR: {
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY:
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            foldable = true;
            folded_index = expression->value_index;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_DECLARATION:
        case AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case AST_NODE_TYPE_STATEMENT_RETURN:
        case AST_NODE_TYPE_UNDEFINED:
            UNREACHABLE(); break;
    }

    expression->foldable = foldable;
    expression->value_index = folded_index;
}

static void fold_constants_via_runtime(
        analyzer_t* analyzer,
        ast_t* ast,
        AnalysisState state,
        ast_node_t* expression) {

    // // TODO: we need to figure how a better way to figure out if an expression needs to be refolded
    // if (IS_FOLDED(expression)) {
    //     return;
    // }

    resolve_foldable(analyzer, ast, state, expression);

    if (IS_FOLDED(expression)) {
        return;
    }

    // we can't fold it so nothing to do
    if (!expression->foldable) {
        return;
    }

    ASSERT(!TYPE_IS_TYPE(expression->value_type), "types must be created at compile time");


    i32 value_index = evaluate_expression(analyzer, ast, state.mode & MODE_FOLDING_TIME, expression);

    /*
    * During constant time mode or folding mode, expressions types themselves will narrow down
    * since they will be either be a standalone statement, or put into a constant value. Either way
    * they should contain the the type of their actual folded value rather than the analyzed one.
    * 
    * I don't do this during runtime mode because I still want stuff like this to work:
    * 
    *           x := "hello world" or null;
    * I still want x's type to be string|null
    * 
    * 
    *           x :: "hello world" or null;
    * because x is a constant, I want x's type to be just string
    * 
    * 
    *           "hello world" or null;
    * in this case the type of expression doesn't matter. So if it's narrowed value is known an rune time or not
    * it doesn't matter.
    */
    if (type_is_union(ast->type_set.types, expression->value_type)) {

        type_t narrowed_type_id = get_folded_type(ast, value_index);

        if (state.mode & MODE_CONSTANT_TIME) {

            value_index++; // the type makes up the first slot value, and rest is the actual value

            expression->value_type = narrowed_type_id;
        }

        // TODO: might have an else here to do something for when this is RUNTIME mode
    }

    expression->value_index = value_index;
}

static void fold_function_signature(analyzer_t *analyzer, ast_t *ast, ast_node_t *expression) {
    ASSERT(expression->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE, "must be a function signature");

    unless (!expression->data.function.return_type_expression || IS_FOLDED(expression->data.function.return_type_expression)) {
        error_range(analyzer,
            expression->data.function.return_type_expression->start,
            expression->data.function.return_type_expression->end,
            "Return type must be resolved at compile time.");
        return;
    }

    bool hit_error = false;
    types_t parameter_types = {.allocator=&ast->allocator};

    for (size_t i = 0; i < expression->data.function.parameter_nodes.count; i++) {
        ast_node_t *parameter = expression->data.function.parameter_nodes.items[i];
        unless (IS_FOLDED(parameter)) {
            hit_error = true;
            error_range(analyzer, parameter->start, parameter->end, "Parameter type must be resolved at compile time.");
            break;
        }

        if (!TYPE_IS_TYPE(parameter->value_type)) {
            hit_error = true;
            error_range(analyzer, parameter->start, parameter->end, "Invalid valid type.");
            break;
        }

        i32 index = parameter->value_index + type_is_union(ast->type_set.types, parameter->value_type);

        type_t type_id = get_folded_type(ast, index);
        array_push(&parameter_types, type_id);
    }

    if (hit_error) {
        return;
    }

    ast_node_t *return_type_expression = expression->data.function.return_type_expression;
    if (return_type_expression && !TYPE_IS_TYPE(return_type_expression->value_type)) {
        error_range(analyzer, return_type_expression->start, return_type_expression->end, "Invalid return type.");
        return;
    }

    type_t return_type;
    {
        if (return_type_expression) {
            i32 index = return_type_expression->value_index + type_is_union(ast->type_set.types, return_type_expression->value_type);
            return_type = get_folded_type(ast, index);
        } else {
            return_type = typeid(TYPE_VOID);
        }
    }

    type_t function_type_id = type_set_fetch_function(&ast->type_set, return_type, parameter_types);

    slot_t function_type_slot = SLOT_U(function_type_id.i);
    i32 index = add_value_to_ast_constant_stack(ast, &function_type_slot, typeid(TYPE_TYPE));

    expression->foldable = true;
    expression->value_index = index;
}

/* 
 * We stop looking for circular dependencies at function boundaries. This took me a while to reason but this
 * is okay to do even with constant folding. We can think about compiling a progarm with a language like in two different ways.
 * Right now, the way the compiler works is through a principle I call "eager resolution". Which means that when a problem
 * comes up during static analysis (i.e. an unresolved expression) we immediately go to resolve it. For example, a problem
 * that may occur is that while trying to resolve the type of an entity, you need to resolve an expression, but to resolve
 * that expression, you might need to perform a folding operation. My compiler walks down the line of dependencies and resolves
 * all the problems that come along the way.
 * 
 * Circular dependencies are easily to avoid with non-functions but recursion makes things a little weird. Since a function
 * can dependent on itself. We can have a weird situation like this:
 *          
 *          foo();
 * 
 *          foo :: null or (n: i32) -> void {
 *              if n < 0 {
 *                  return 0;
 *              };
 *              
 *              return foo(n - 1);
 *          };
 * 
 * Technically... this should run just fine right? null or function literal, and foo being a constant, means foo 
 * should hold the address to the function expression on the right side of the OR. This is allowed and my language
 * and should be handled.
 * 
 * The way my dependency system works is through expressions. Expressions always, always, always rely on other expressions.
 * In the case above, when foo(n - 1) is about to be analyzed this is how the dependency chain looks like.
 * 
 *          NOTE: so far only the following things create dependencies.
 *              - The top level type of a declaration
 *              - The top level expression of a declaration
 *              - Function definitons
 *            this is why the dependency chain is not foo() -> foo, even though foo() technically depends on foo.
 *            I could make dependencies from each and all dependencies but that's way too much and I'm pretty sure
 *            is unnecessary since a lot of dependencies would be redundant. These are supposed to skip redundant
 *            dependencies.
 * 
 *          foo() => // this would not be in the chain, but it's here because it's what kicks off the chain
 *              (null or (n: i32) -> void { ... }) => // foo declaration expression
 *                  (n: i32) -> void { ... } => // function definition
 *                      foo(n - 1) // this would not be in the chain, but again, it kicks off the above since foo is not resolved.
 * 
 * At this point, foo does not have the function address yet during the anaysis! It's still in the process of being resolved...
 * BUT once its expression is resolved, it will be good go! At this point, we STILL don't really know what foo is!
 * 
 * 
 * If I were to naively check of dependencies the chain would contain to look like this (starting from foo(n - 1))
 * 
 *          foo(n - 1) => 
 *              (null or (n: i32) -> void { ... }) => // foo declaration expression
 * 
 * Then we've come back and hit an expression we haven't resolved yet (remember the or wasn't resolved
 * because we were in the middle of resolving (n: i32) -> void { ... } due to the principle of eager resolution)
 * and therefore have a circular dependency, right? However, I found a different to do things that could work.
 * 
 * Imagine if instead of resolving the function block right away, I actually waited until the declaration resolved
 * before trying to analysis the function body! Well, then I can use the address of the function, and as long as I
 * store the scope and body, I can put it in a queue to analysis later. After I am sure I can safely resolve the
 * function body, then I will. This wouldn't have to deal with circular dependencies.
 * 
 * This truth is that the above *should* work (although I haven't tested it) because of part of the proof reasoning below
 * but this is, in truth, a little annoying to implement. It means I need to sort of break the recursion scheme and
 * eager resolution princple. I have to allocate more memory on the heap. It's just not an ideal soluton and makes
 * things much messier...
 * 
 * However, the other reasoning of the proof below suggests that the order in which I resolve expressions shouldn't matter.
 * So I can't naively just walk up the dependency chain and check for duplicates, I need to be a tiny bit smarter. Since the
 * order in which I resolve things doesn't matter as long as I have the data there (the scope chains basically).
 * 
 * I noticed that if I analyse a function body's when I know it's safe, the circular dependencies *must* be either
 * be localized inside the function OR will hit a circular dependency of constants on the outer layer - but in either
 * case - these circular dependencies will be a result of unused entities trying to be resolved from *inside* the function.
 * 
 * Therefore instead of a simply looking for duplicate expression pointers in the entire dependency chain, I start
 * backwards and when I see a dependency to a function definition, I stop looking. This mimics the behavior of the
 * ideal solution but allows me not break any recursion.
 * 
 */
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
        error_range(analyzer, node->start, node->end, "Start of a circular dependency.");
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

static void resolve_declarations(
        analyzer_t *analyzer,
        ast_t *ast,
        AnalysisState state,
        ast_nodes_t declarations,
        i32 count);

static void resolve_declaration(
        analyzer_t* analyzer,
        ast_t* ast,
        AnalysisState state,
        ast_node_t* declaration_node);

static void resolve_statement(
    analyzer_t* analyzer,
    ast_t* ast,
    AnalysisState state,
    ast_node_t* statement);

static void resolve_function_expression(
        analyzer_t* analyzer,
        ast_t* ast,
        AnalysisState state,
        ast_node_t* function_definition_expression);

static void resolve_struct_definition(
    analyzer_t* analyzer,
    ast_t* ast,
    AnalysisState state,
    ast_node_t* struct_definition);

static void declare_entity(analyzer_t* analyzer, scope_t* scope, ast_node_t* entity);

static void forward_scan_declaration_names(analyzer_t* analyzer, scope_t* scope, ast_nodes_t declarations, i32 count) {
    for (i32 i = 0; i < count; i++) {
        ast_node_t* declaration = declarations.items[i];
        if (declaration->node_type != AST_NODE_TYPE_DECLARATION) {
            continue;
        }

        declare_entity(analyzer, scope, declaration);
    }
}

void resolve_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        AnalysisState state,
        ast_node_t *expression) {
    
    ASSERT(ast_node_type_is_expression(expression->node_type), "should be only expressions");

    if (!TYPE_IS_UNRESOLVED(expression->value_type)) {
        return;
    }

    if (expression->fold) {
        ++state.fold_level;
        state.mode |= MODE_FOLDING_TIME;
    }

    // expression->is_lvalue
    // this is a field that needs to be set if the expression can be an lvalue.
    // it is initially to false which means by default, an expression is not an lvalue
    // This needs to be set to true and transferred up the ast appropriately for expressions
    // that can ALSO be referred to as "location values". During an assignment, the left hand
    // side must be an lvalue.
    // lvalues can be as simple as this:
    //   foo = 10;
    // where foo is an lvalue
    //   (foo) = 10;
    // should also work fine
    //   foo + 1 = 10;
    // should NOT work (foo + 1 is not an lvalue) 
    //   foo.value = 10;
    // should work since foo refers to an lvalue, thus, foo.value refers to an lvalue.
    //   struct { value := 0; }.value = 10;
    // this should NOT work since the leftmost value is not an lvalue, therefore
    // .value cannot be an lvalue. Make note: this is totally fine syntax as a non-lvalue.
    // Rules for lvalues are found below in the code.
    // Also, for clarity: something being an lvalue is not mutally exclusive from it being
    // unassignable. i.e. constants, depending on how they are implemented, can be lvalues
    // since they inhabit a location in memory. However, that doesn't mean that they are assignable.
    // These are two independent checks that the analyzer makes.

    switch (expression->node_type) {
        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            resolve_expression(analyzer, ast, state, expression->data.expression);
            expression->lvalue_node = expression->data.expression->lvalue_node;

            expression->value_type = expression->data.expression->value_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            resolve_expression(analyzer, ast, state, expression->data.initiailizer.type);

            if (!TYPE_IS_TYPE(expression->data.initiailizer.type->value_type)) {
                error_range(analyzer, expression->data.initiailizer.type->start, expression->data.initiailizer.type->end, "Expression must be a type");
                INVALIDATE(expression);
                break;
            }

            if (expression->data.initiailizer.type->value_index < 0) {
                error_range(analyzer, expression->data.initiailizer.type->start, expression->data.initiailizer.type->end, "type must be a constant");
                INVALIDATE(expression);
                break;
            }

            type_t type_id = get_folded_type(ast, expression->data.initiailizer.type->value_index);
            type_info_t *type = get_type_info(&ast->type_set.types, type_id);

            if (type->kind == TYPE_STRUCT) {
                int arg_count = expression->data.initiailizer.arguments.count;
                if (type->data.struct_.field_count < arg_count) {
                    error_range(analyzer, expression->data.initiailizer.arguments.items[0]->start, expression->data.initiailizer.arguments.items[arg_count-1]->end, "Too many arguments for struct");
                    INVALIDATE(expression);
                    break;
                }

                bool is_invalidated = false;
                for (int i = 0; i < arg_count; ++i) {
                    ast_node_t *arg = expression->data.initiailizer.arguments.items[i];
                    unless (arg) continue;
                    resolve_expression(analyzer, ast, state, arg);
                    if (TYPE_IS_INVALID(arg->value_type)) {
                        is_invalidated = true;
                        continue;
                    }

                    if (i < type->data.struct_.field_count) {
                        type_t field_type_id = type->data.struct_.fields[i].type;
                        type_t arg_type_id = arg->value_type;
                        unless (typeid_eq(field_type_id, arg_type_id)) {
                            if (can_cast_implicit(ast->type_set.types, arg_type_id, field_type_id)) {
                                ast_node_t *casted = implicit_cast(ast, arg, field_type_id);
                                fold_constants_via_runtime(analyzer, ast, state, casted);
                                expression->data.initiailizer.arguments.items[i] = casted;
                            } else {
                                error_range(analyzer, arg->start, arg->end, "This argument is the wrong type");
                                is_invalidated = true;
                                continue;
                            }
                        }
                    }
                }

                if (is_invalidated) {
                    INVALIDATE(expression);
                    break;
                }

                type_t type_id = get_folded_type(ast, expression->data.initiailizer.type->value_index);
                expression->value_type = type_id;
                break;
            } else {
                error_range(analyzer, expression->start, expression->end, "initializer list is only for structs at the moment");
                INVALIDATE(expression);
                break;
            }
        }

        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            INVALIDATE(expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            ast_node_t *left = expression->data.binary.lhs;
            resolve_expression(analyzer, ast, state, left);

            ast_node_t *right = expression->data.binary.rhs;
            resolve_expression(analyzer, ast, state, right);

            if (TYPE_IS_INVALID(left->value_type) || TYPE_IS_INVALID(right->value_type)) {
                INVALIDATE(expression);
                break;
            }

            // TODO: Remember to do different things depending on operation
            //   if it's arthimetic, or comparisons, then let them merge at the end
            //   if it's logical, then you need to do special things

            type_t cast_left = typeid(TYPE_INVALID);
            type_t cast_right = typeid(TYPE_INVALID);

            expression->value_type = typeid(TYPE_UNRESOLVED);

            bool is_logical_operator = false;

            switch (expression->operator.type) {
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                case TOKEN_STAR:
                case TOKEN_SLASH: {
                    type_t combined_type = typeid(TYPE_INVALID);
                    if (typeid_eq(left->value_type, right->value_type)) {
                        combined_type = left->value_type;
                    }
                    expression->value_type = combined_type;

                    ASSERT(!type_is_union(ast->type_set.types, expression->value_type), "arthimetic must narrow down to a single type");

                    cast_left = combined_type;
                    cast_right = combined_type;
                    break;
                }

                case TOKEN_BAR: {
                    if (typeid_eq(left->value_type, right->value_type) && typeid_eq(left->value_type, typeid(TYPE_TYPE))) {
                        cast_left = typeid(TYPE_TYPE);
                        cast_right = typeid(TYPE_TYPE);

                        expression->value_type = typeid(TYPE_TYPE);
                    } else {
                        cast_left = typeid(TYPE_INVALID);
                        cast_right = typeid(TYPE_INVALID);

                        INVALIDATE(expression);
                    }
                    break;
                }

                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL: {
                    if (typeid_eq(left->value_type, right->value_type)) {
                        cast_left = left->value_type;
                        cast_right = right->value_type;
                        expression->value_type = typeid(TYPE_BOOL);
                    } else {
                        expression->value_type = typeid(TYPE_INVALID);
                    }
                    break;
                }
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    if (typeid_eq(left->value_type, right->value_type)) {
                        cast_left = left->value_type;
                        cast_right = right->value_type;
                        expression->value_type = typeid(TYPE_BOOL);
                    } else {
                        expression->value_type = typeid(TYPE_INVALID);
                    }
                    break;
                }

                case TOKEN_AND:
                case TOKEN_OR: {
                    is_logical_operator = true;
                    cast_left = left->value_type;
                    cast_right = right->value_type;

                    type_t merged_type = type_merge(&ast->type_set, left->value_type, right->value_type);
                    expression->value_type = merged_type;
                    break;
                }

                default: UNREACHABLE();
            }

            if (TYPE_IS_INVALID(cast_left) || TYPE_IS_INVALID(cast_right)) {
                INVALIDATE(expression);
                error_range2(analyzer, left->start, left->end, right->start, right->end, "Incompatible types for binary operation.");
                break;
            }

            if (!is_logical_operator) {
                if (!typeid_eq(cast_left, left->value_type)) {
                    expression->data.binary.lhs = implicit_cast(ast, left, cast_left);
                    fold_constants_via_runtime(analyzer, ast, state, expression->data.binary.lhs);
                }


                if (!typeid_eq(cast_right, right->value_type)) {
                    expression->data.binary.rhs = implicit_cast(ast, right, cast_right);
                    fold_constants_via_runtime(analyzer, ast, state, expression->data.binary.rhs);
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            resolve_expression(analyzer, ast, state, expression->data.expression);

            if (TYPE_IS_INVALID(expression->data.expression->value_type)) {
                INVALIDATE(expression);
                break;
            }

            if (expression->is_in_type_context && expression->operator.type == TOKEN_AMPERSAND) {
                if (!TYPE_IS_TYPE(expression->data.expression->value_type)) {
                    INVALIDATE(expression);
                    error_range(analyzer, expression->start, expression->end, "Ampersand is only allowed on types.");
                    break;
                }

                unless (IS_FOLDED(expression->data.expression)) {
                    INVALIDATE(expression);
                    error_range(analyzer, expression->data.expression->start, expression->data.expression->end, "Value of expression must be resolved at compile time.");
                    break;
                }
            } else {
                type_t new_type_id = resolve_unary_type(ast, expression->operator.type, expression->data.expression->value_type);
                expression->value_type = new_type_id;
                
                // TODO: Must negate the new type implications if the unary operation is NOT

                if (TYPE_IS_INVALID(expression->value_type)) {
                    error_range(analyzer, expression->start, expression->end, "Incompatible type for unary operation");
                    break;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ENTITY: {
            expression->lvalue_node = expression;

            scope_t *entity_scope;
            EntityQuery query = {
                .search_type = expression->is_in_type_context ? typeid(TYPE_TYPE) : typeid(TYPE_INVALID),
                .skip_mutable = (state.mode & MODE_CONSTANT_TIME),
                .flags = expression->is_in_type_context ? QUERY_FLAG_MATCH_TYPE : QUERY_FLAG_MATCH_ANY,
            };

            Entity *entity = get_resolved_entity_by_identifier(analyzer, ast, state, expression->data.dot.identifier, &query, &entity_scope);

            if (entity == NULL) {
                INVALIDATE(expression);
                break;
            }

            // keep for constant folding so not redo entity look up and simplify work there
            expression->data.dot.referencing_declaration = entity->node;

            expression->value_type = entity->declared_type;

            if (entity->node == NULL) {
                expression->foldable = true;
                expression->value_index = entity->value_index;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            ast_node_t *left = expression->data.dot.lhs;
            ASSERT(left, "for now left must be present");

            if (expression->data.dot.referencing_declaration) {
                expression->value_type = expression->data.dot.referencing_declaration->value_type;
                break;
            }

            resolve_expression(analyzer, ast, state, left);
            if (left->lvalue_node != NULL) {
                expression->lvalue_node = left->lvalue_node;
            }


            if (type_is_union(ast->type_set.types, left->value_type)) {
                error_range(analyzer, left->start, left->end, "Member accessor can only be used on concrete non-union types.");
                INVALIDATE(expression);
                break;
            }

            if (TYPE_IS_INVALID(left->value_type)) {
                INVALIDATE(expression);
                break;
            }

            ast_node_and_scope_t node_and_scope;
            bool skip_mutable = false;
            if (type_is_struct(ast->type_set.types, left->value_type)) {
                table_get(type2ns, ast->type_to_creation_node, left->value_type, &node_and_scope);
            } else if (TYPE_IS_TYPE(left->value_type) && type_is_struct(ast->type_set.types, get_folded_type(ast, left->value_index))) {
                type_t struct_type_id = get_folded_type(ast, left->value_index);
                table_get(type2ns, ast->type_to_creation_node, struct_type_id, &node_and_scope);
                skip_mutable = true;
            } else {
                error_range(analyzer, left->start, left->end, "Member accessor can only be used on structs or struct types");
                INVALIDATE(expression);
                break;
            }

            ast_node_t *referencing_declaration = NULL;
            for (size_t i = 0; i < node_and_scope.node->data.struct_.declarations.count; ++i) {
                ast_node_t *declaration = node_and_scope.node->data.struct_.declarations.items[i];
                if (skip_mutable && declaration->data.declaration.is_mutable) {
                    continue;
                }

                token_t declaration_name = declaration->data.declaration.identifier;
                if (declaration_name.length == expression->data.dot.identifier.length &&
                    strncmp(declaration_name.start, expression->data.dot.identifier.start, declaration_name.length) == 0) {
                    referencing_declaration = declaration;
                    break;
                }
            }

            if (referencing_declaration == NULL) {
                error_token(analyzer, expression->data.dot.identifier, "Field is not present in type.");
                INVALIDATE(expression);
                break;
            }

            // keep until constant folding so no need to redo the entity look up
            expression->data.dot.referencing_declaration = referencing_declaration;

            unless (is_declaration_resolved(referencing_declaration)) {
                AnalysisState search_state = state;
                search_state.scope = node_and_scope.scope;

                EntityQuery query = {
                    .flags = QUERY_FLAG_MATCH_ANY | QUERY_FLAG_MATCH_ONLY_IN_GIVEN_SCOPE,
                    .skip_mutable = skip_mutable,
                };

                scope_t* found_scope;
                Entity* entity = get_resolved_entity_by_identifier(analyzer, ast, search_state, expression->data.dot.identifier, &query, &found_scope);

                unless (entity) {
                    INVALIDATE(expression);
                    break;
                }

                expression->value_type = entity->node->value_type;
                break;
            }

            expression->value_type = referencing_declaration->value_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            resolve_expression(analyzer, ast, state, expression->data.binary.rhs);

            if (TYPE_IS_INVALID(expression->data.binary.rhs->value_type)) {
                INVALIDATE(expression);
                break;
            }

            resolve_expression(analyzer, ast, state, expression->data.binary.lhs);
            ast_node_t* lvalue_node = expression->data.binary.lhs->lvalue_node;
            if (lvalue_node == NULL) {
                error_range(analyzer, expression->data.binary.lhs->start, expression->data.binary.lhs->end, "Expression does not resolve to an lvalue.");
                INVALIDATE(expression);
                break;
            } 

            lvalue_node = expression->data.binary.lhs;
            expression->lvalue_node = lvalue_node;

            // // TODO: Find a way to merge the fitting logic
            if (lvalue_node->node_type == AST_NODE_TYPE_EXPRESSION_ENTITY) {
                scope_t* entity_scope;
                Entity* entity;
                token_t identifier = lvalue_node->data.dot.identifier;
                entity = get_resolved_entity_by_identifier(analyzer, ast, state, identifier, NULL, &entity_scope);

                
                if (entity == NULL) {
                    INVALIDATE(expression);
                    break;
                }

                expression->value_type = entity->declared_type;

                type_t right_side_narrowed_type_id = expression->data.binary.rhs->value_type;
                unless (typeid_eq(entity->declared_type, right_side_narrowed_type_id)) {
                    error_range(analyzer, expression->start, expression->end, "Explicit cast required to assign to variable.");
                    break;
                }

                expression->value_type = right_side_narrowed_type_id;
                ASSERT(!type_is_union(ast->type_set.types, expression->value_type) ||
                        union_type_contains_type(ast->type_set.types, expression->value_type, right_side_narrowed_type_id),
                        "this will fail in situations where the right side is not converted to the type of the left side. As of now, this is only known to happen with small numbers going into bigger numbers");

                if (type_is_union(ast->type_set.types, entity->declared_type)) {
                    entity->narrowed_type = expression->value_type;
                }

            } else if (lvalue_node->node_type == AST_NODE_TYPE_EXPRESSION_DOT) {
                ast_node_t* dot_node = lvalue_node;
                resolve_expression(analyzer, ast, state, dot_node);

                if (TYPE_IS_INVALID(dot_node->value_type)) {
                    INVALIDATE(expression);
                    break;
                }

                unless (typeid_eq(dot_node->value_type, expression->data.binary.rhs->value_type)) {
                    error_range(analyzer, expression->start, expression->end, "Explicit cast required to cast to member.");
                    break;
                }

                expression->value_type = dot_node->value_type;

            } else {
                UNREACHABLE();
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            scope_t block_scope;
            scope_init(&block_scope, &analyzer->allocator, SCOPE_TYPE_BLOCK, state.scope, expression);

            i32 declarations_count = expression->data.block.count;
            forward_scan_declaration_names(analyzer, &block_scope, expression->data.block, declarations_count);

            AnalysisState block_state = state;
            block_state.scope = &block_scope;
            block_state.mode = MODE_RUNTIME | (state.mode & MODE_FOLDING_TIME);
            resolve_declarations(analyzer, ast, block_state, expression->data.block, declarations_count);

            return_guarentee_t block_return_guarentee = RETURN_GUARENTEE_NONE;
            for (i32 i = 0; i < declarations_count; i++) {
                ast_node_t* declaration = expression->data.block.items[i];
                if (declaration->node_type == AST_NODE_TYPE_STATEMENT_RETURN) {
                    block_return_guarentee = RETURN_GUARENTEE_YES;
                } else if (declaration->node_type == AST_NODE_TYPE_STATEMENT_EXPRESSION) {
                    ast_node_t* statement = declaration->data.expression;
                    if (statement->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK && statement->return_guarentee != RETURN_GUARENTEE_NONE) {
                        block_return_guarentee = statement->return_guarentee;
                    } else if (statement->node_type == AST_NODE_TYPE_EXPRESSION_BRANCHING && statement->return_guarentee != RETURN_GUARENTEE_NONE) {
                        block_return_guarentee = statement->return_guarentee;
                    }
                }

                if (block_return_guarentee == RETURN_GUARENTEE_YES) {
                    break;
                }
            }

            if (block_return_guarentee != RETURN_GUARENTEE_NONE) {
                expression->return_guarentee = block_return_guarentee;
                expression->value_type = typeid(TYPE_UNDEFINED);
            } else {
                ast_node_t* last_expression_statement = NULL;
                if (declarations_count > 0) {
                    ast_node_t* last_declaration = expression->data.block.items[declarations_count - 1];
                    if (last_declaration->node_type == AST_NODE_TYPE_STATEMENT_EXPRESSION) {
                        last_expression_statement = last_declaration;
                    }
                }

                if (last_expression_statement == NULL) {
                    expression->value_type = typeid(TYPE_VOID);
                } else {
                    expression->value_type = last_expression_statement->data.expression->value_type;
                }
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STATEMENT: {
            resolve_statement(analyzer, ast, state, expression->data.statement);

            if (TYPE_IS_INVALID(expression->data.statement->data.expression->value_type)) {
                INVALIDATE(expression);
                break;
            }

            expression->return_guarentee = expression->data.statement->data.expression ? 
                    expression->data.statement->data.expression->return_guarentee : RETURN_GUARENTEE_NONE;

            if (expression->data.statement->node_type == AST_NODE_TYPE_STATEMENT_RETURN) {
                if (expression->return_guarentee != RETURN_GUARENTEE_NONE) {
                    error_range(analyzer, expression->start, expression->end, "Cannot return within a return expression.");
                } else {
                    expression->return_guarentee = RETURN_GUARENTEE_YES;
                }
            }

            if (expression->return_guarentee != RETURN_GUARENTEE_NONE) {
                expression->value_type = typeid(TYPE_UNDEFINED);
            } else {
                expression->value_type = expression->data.statement->data.expression->value_type;
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            resolve_expression(analyzer, ast, state, expression->data.branch.condition);

            scope_t* then_scope = scope_copy_new(state.scope, &analyzer->allocator);
            AnalysisState then_state = state;
            then_state.scope = then_scope;
            resolve_expression(analyzer, ast, then_state, expression->data.branch.then_expression);

            scope_t* else_scope = scope_copy_new(state.scope, &analyzer->allocator);
            AnalysisState else_state = state;
            else_state.scope = else_scope;
            if (expression->data.branch.else_expression) {
                resolve_expression(analyzer, ast, else_state, expression->data.branch.else_expression);
            }

            return_guarentee_t branch_return_guarentee;
            // resolve return guarentee first
            {
                ASSERT(expression->data.branch.then_expression->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK || expression->data.branch.then_expression->node_type == AST_NODE_TYPE_EXPRESSION_STATEMENT, "must be expression statement or block, not other choices");
                return_guarentee_t then_return_guarentee = expression->data.branch.then_expression->return_guarentee;

                return_guarentee_t else_return_guarentee = RETURN_GUARENTEE_NONE;
                if (expression->data.branch.else_expression) {
                    if (expression->data.branch.else_expression->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK
                    || expression->data.branch.else_expression->node_type == AST_NODE_TYPE_EXPRESSION_BRANCHING
                    || expression->data.branch.else_expression->node_type == AST_NODE_TYPE_EXPRESSION_STATEMENT) {
                        else_return_guarentee = expression->data.branch.else_expression->return_guarentee;
                    } else {
                        ASSERT(false, "only blocks and ifs allowed during elses for now");
                    }
                }

                if (then_return_guarentee == RETURN_GUARENTEE_YES && else_return_guarentee == RETURN_GUARENTEE_YES) {
                    branch_return_guarentee = RETURN_GUARENTEE_YES;
                } else if (then_return_guarentee == RETURN_GUARENTEE_NONE && else_return_guarentee == RETURN_GUARENTEE_NONE) {
                    branch_return_guarentee = RETURN_GUARENTEE_NONE;
                } else {
                    branch_return_guarentee = RETURN_GUARENTEE_MAYBE;
                }
            }

            scope_merge(&ast->type_set, state.scope, then_state.scope, else_state.scope);

            while (then_scope) {
                scope_t *outer_scope = scope_copy_new(then_scope->outer, &analyzer->allocator);
                then_scope = outer_scope;
            }

            while (else_scope) {
                scope_t* outer_scope = scope_copy_new(else_scope->outer, &analyzer->allocator);
                else_scope = outer_scope;
            }

            if (TYPE_IS_INVALID(expression->data.branch.then_expression->value_type) ||
                (expression->data.branch.else_expression && TYPE_IS_INVALID(expression->data.branch.else_expression->value_type))) {
                INVALIDATE(expression);
                break;
            }

            if (branch_return_guarentee == RETURN_GUARENTEE_NONE) {
                type_t else_block_type = typeid(TYPE_VOID);
                if (expression->data.branch.else_expression) {
                    else_block_type = expression->data.branch.else_expression->value_type;
                }

                expression->value_type = type_merge(&ast->type_set,
                    expression->data.branch.then_expression->value_type, else_block_type
                );
            } else {
                expression->return_guarentee = branch_return_guarentee;
                expression->value_type = typeid(TYPE_UNDEFINED);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CALL: {
            bool argument_invalid = false;
            for (size_t i = 0; i < expression->data.call.arguments.count; ++i) {
                ast_node_t *argument = expression->data.call.arguments.items[i];
                resolve_expression(analyzer, ast, state, argument);
                unless (TYPE_IS_INVALID(argument->value_type)) {
                    continue;
                }

                argument_invalid = true;
            }

            resolve_expression(analyzer, ast, state, expression->data.call.callee);

            if (argument_invalid || TYPE_IS_INVALID(expression->data.call.callee->value_type)) {
                INVALIDATE(expression);
                break;
            }

            type_t narrowed_callee_type_id = expression->data.call.callee->value_type;
            if ((!type_is_function(ast->type_set.types, narrowed_callee_type_id) && !type_is_native_function(ast->type_set.types, narrowed_callee_type_id)) || !can_call(ast->type_set.types, narrowed_callee_type_id, expression->data.call.arguments)) {
                error_range(analyzer, expression->data.call.callee->start, expression->data.call.callee->end, "Cannot call this value.");
                break;
            }

            type_info_t *narrowed_callee_type = ast->type_set.types.items[narrowed_callee_type_id.i];
            expression->value_type = narrowed_callee_type->data.function.return_type;
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            resolve_function_expression(analyzer, ast, state, expression);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            // since structs are boundaries for circular dependencies, they will never be the cause of one.
            push_dependency(analyzer, expression, state.fold_level, is_value_circular_dependency);
            resolve_struct_definition(analyzer, ast, state, expression);
            pop_dependency(analyzer);
            break;
        }
        
         case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: {
            bool invalid_parameter = false;
            for (size_t i = 0; i < expression->data.function.parameter_nodes.count; i++) {
                ast_node_t *parameter = expression->data.function.parameter_nodes.items[i];
                resolve_expression(analyzer, ast, state, parameter);

                if (TYPE_IS_INVALID(parameter->value_type)) {
                    invalid_parameter = true;
                }
            }

            ASSERT(expression->data.function.return_type_expression, "during static analysis, function signatures should not have a null return type, that's only valid for function definitions after parsing");
            resolve_expression(analyzer, ast, state, expression->data.function.return_type_expression);

            if (invalid_parameter || TYPE_IS_INVALID(expression->data.function.return_type_expression->value_type)) {
                INVALIDATE(expression);
                break;
            }

            fold_function_signature(analyzer, ast, expression);

            expression->value_type = typeid(TYPE_TYPE);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRINT:
        case AST_NODE_TYPE_EXPRESSION_PRINT_EXPR: {
            resolve_expression(analyzer, ast, state, expression->data.expression);
            expression->value_type = typeid(TYPE_VOID);
            break;
        }

        case AST_NODE_TYPE_UNDEFINED:
        case AST_NODE_TYPE_DECLARATION:
        case AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case AST_NODE_TYPE_STATEMENT_RETURN: {
            UNREACHABLE();
            break;
        }
    }

    // we shouldn't fold expressions that are undefined (blocks or ifelses that return)
    if (TYPE_IS_UNDEFINED(expression->value_type) || TYPE_IS_INVALID(expression->value_type)) {
        return;
    }

    fold_constants_via_runtime(analyzer, ast, state, expression);
}

static void declare_entity(analyzer_t* analyzer, scope_t* scope, ast_node_t* entity) {
    symbol_t *identifier = orso_unmanaged_symbol_from_cstrn(entity->start.start, entity->start.length, &analyzer->symbols, &analyzer->allocator);
    slot_t slot_type_pair;
    if (symbol_table_get(&scope->named_entities, identifier, &slot_type_pair)) {
        const char message[126];
        snprintf((char*)message, 126, "Duplicate entity definition of '%.*s'.", entity->start.length, entity->start.start);
        error_token(analyzer, entity->start, "Cannot overload this entity definition");
        return;
    }

    add_entity(scope, &analyzer->allocator, identifier, entity);
}

static void resolve_entity_declaration(analyzer_t* analyzer, ast_t* ast, AnalysisState state, ast_node_t* entity_declaration) {
// due to the way structs are resolved, it is possible for the initial value expression to be swapped out with another
// this is simply a safer way of accessing the initial value expression
#define INITIAL_EXPRESSION (entity_declaration->data.declaration.initial_value_expression)

    type_t declaration_type_id = typeid(TYPE_UNRESOLVED);
    if (TYPE_IS_UNRESOLVED(entity_declaration->value_type) && entity_declaration->data.declaration.type_expression) {
        bool pushed = push_dependency(analyzer, entity_declaration->data.declaration.type_expression, state.fold_level, is_value_circular_dependency);
        if (pushed) {
            AnalysisState new_state = state;
            new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
            resolve_expression(analyzer, ast, new_state, entity_declaration->data.declaration.type_expression);
            declaration_type_id = get_folded_type(ast, entity_declaration->data.declaration.type_expression->value_index);
            pop_dependency(analyzer);
        } else {
            declaration_type_id = typeid(TYPE_INVALID);
        }
    }

    if (INITIAL_EXPRESSION != NULL) {
        if (TYPE_IS_UNRESOLVED(INITIAL_EXPRESSION->value_type)) {
            AnalysisState new_state = state;
            switch (state.scope->type) {
                case SCOPE_TYPE_FUNCTION_BODY:
                case SCOPE_TYPE_BLOCK:
                case SCOPE_TYPE_MODULE: {
                    ExpressionFoldingMode mode = entity_declaration->data.declaration.is_mutable ? MODE_RUNTIME : MODE_CONSTANT_TIME;
                    mode = mode | (state.mode & MODE_FOLDING_TIME);
                    new_state.mode = mode;
                    break;
                }

                case SCOPE_TYPE_STRUCT:
                case SCOPE_TYPE_FUNCTION_PARAMETERS: {
                    new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
                    break;
                }
            }

            bool pushed = push_dependency(analyzer, entity_declaration->data.declaration.initial_value_expression, new_state.fold_level, is_value_circular_dependency);
            if (pushed) {
                resolve_expression(analyzer, ast, new_state, entity_declaration->data.declaration.initial_value_expression);
                pop_dependency(analyzer);
            } else {
                unless (TYPE_IS_UNRESOLVED(declaration_type_id)) {
                    entity_declaration->value_type = declaration_type_id;
                }
                INVALIDATE(entity_declaration->data.declaration.initial_value_expression);
            }
        } else {
            // if (is_sizing_circular_dependency(analyzer, initial_expression)) {
            //     unless (TYPE_IS_UNRESOLVED(declaration_type)) {
            //         entity_declaration->value_type = declaration_type;
            //     }
            //     error_range(analyzer, entity_declaration->start, entity_declaration->end, "Circular dependency (entity declaration)");
            //     INVALIDATE(entity_declaration->data.declaration.initial_value_expression);
            // }
        }
    }

    slot_t entity_slot;
    symbol_t *name = orso_unmanaged_symbol_from_cstrn(entity_declaration->start.start, entity_declaration->start.length, &analyzer->symbols, &ast->allocator);

    ASSERT(symbol_table_get(&state.scope->named_entities, name, &entity_slot), "should be forward_declared already");

    symbol_table_get(&state.scope->named_entities, name, &entity_slot);

    Entity* entity = (Entity*)entity_slot.as.p;

    // we are resolved
    if (is_declaration_resolved(entity_declaration)) {
        // this is possible if we are in a branch (i.e. function is defined at the top level, copy scopes are created for branching, it's resolved in one branch but not the other)
        if (TYPE_IS_UNRESOLVED(entity->declared_type)) {
            ASSERT(TYPE_IS_UNRESOLVED(entity->narrowed_type), "both should always be set at the same time");

            entity->declared_type = entity_declaration->value_type;
            entity->narrowed_type = entity_declaration->data.declaration.initial_value_expression->value_type;
        }

        if (!TYPE_IS_TYPE(entity_declaration->value_type)) {
            return;
        }

        // This must be available at compile time
        type_t struct_type_id = get_folded_type(ast, entity_declaration->value_index);
        type_info_t *struct_type = ast->type_set.types.items[struct_type_id.i];
        unless (struct_type_is_incomplete(struct_type) && struct_type->data.struct_.name) {
            return;
        }

        ast_node_t *cast_node = entity_declaration->data.declaration.initial_value_expression;
        ASSERT(cast_node->node_type == AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT, "must be implicit casting node");
        
        type_t completed_struct_type_id = cast_node->data.expression->value_type;
        
        // this means that we found out later that the struct this was supposed to be was actually invalid, so we need to fix the ast
        if (TYPE_IS_INVALID(completed_struct_type_id)) {
            INVALIDATE(cast_node);
            INVALIDATE(entity_declaration);
            return;
        }

        ASSERT(type_is_struct(ast->type_set.types, completed_struct_type_id), "casted expression must be a struct type");

        type_info_t *completed_struct_type = ast->type_set.types.items[completed_struct_type_id.i];
        if (struct_type_is_incomplete(completed_struct_type)) {
            return;
        }

        named_struct_copy_data_from_completed_struct_type(&ast->type_set, struct_type_id, completed_struct_type_id);
        return;
    }



    if (INITIAL_EXPRESSION && (TYPE_IS_UNDEFINED(INITIAL_EXPRESSION->value_type) || TYPE_IS_INVALID(INITIAL_EXPRESSION->value_type))) {
        if (TYPE_IS_UNDEFINED(INITIAL_EXPRESSION->value_type)) {
            error_range(analyzer,
                INITIAL_EXPRESSION->start,
                INITIAL_EXPRESSION->end,
                "Initial declaration expression has an undefined type.");
        }
        
        INVALIDATE(entity_declaration);
    }

    unless (entity_declaration->data.declaration.is_mutable) {
        if (INITIAL_EXPRESSION != NULL && type_is_function(ast->type_set.types, INITIAL_EXPRESSION->value_type)) {
            function_t* function = (function_t*)ast->folded_constants.items[INITIAL_EXPRESSION->value_index].as.p;
            if (function->binded_name == NULL) {
                function->binded_name = name;
            }
        }

        if (INITIAL_EXPRESSION != NULL
        && type_is_struct(ast->type_set.types, INITIAL_EXPRESSION->value_type)
        && (TYPE_IS_UNRESOLVED(declaration_type_id) || TYPE_IS_TYPE(declaration_type_id))) {
            ast_node_t *to_struct_type = ast_node_new(ast, AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT, INITIAL_EXPRESSION->is_in_type_context, INITIAL_EXPRESSION->start);
            to_struct_type->value_type = typeid(TYPE_TYPE);
            to_struct_type->data.expression = INITIAL_EXPRESSION;

            to_struct_type->fold = false;
            to_struct_type->foldable = true;
            type_info_t *initial_expression_type = get_type_info(&ast->type_set.types, INITIAL_EXPRESSION->value_type);
            type_t named_struct_id = type_create_struct(&ast->type_set, entity_declaration->start.start, entity_declaration->start.length, initial_expression_type);
            slot_t struct_type_slot = SLOT_U(named_struct_id.i);
            to_struct_type->value_index = add_value_to_ast_constant_stack(ast, &struct_type_slot, typeid(TYPE_TYPE));

            type_t initial_expression_type_id = INITIAL_EXPRESSION->value_type;

            ast_node_and_scope_t node_and_scope;
            bool found = table_get(type2ns, ast->type_to_creation_node, initial_expression_type_id, &node_and_scope);
            ASSERT(found, "this shoudl always find something");

            INITIAL_EXPRESSION = to_struct_type;

            table_put(type2ns, ast->type_to_creation_node, named_struct_id, node_and_scope);
            
            {
                i32 value_index = to_struct_type->data.expression->value_index;
                ASSERT(value_index >= 0, "must be the value of the anonymous type");
                table_put(ptr2i32, ast->type_to_zero_index, named_struct_id, value_index);
            }
        }
    }

    // Could be resolved could be unresolved at this point.
    entity_declaration->value_type = declaration_type_id;

    entity_declaration->data.declaration.fold_level_resolved_at = state.fold_level;

    // TODO: Outer if should be if the expression is null or not
    unless (type_is_union(ast->type_set.types, entity_declaration->value_type)) {
        if (TYPE_IS_UNRESOLVED(entity_declaration->value_type)) {
            ASSERT(INITIAL_EXPRESSION != NULL, "this should be a parsing error.");

            type_t expression_type_id = INITIAL_EXPRESSION->value_type;

            if (expression_type_id.i == TYPE_BOOL || expression_type_id.i == TYPE_INT32) {
                entity_declaration->value_type = expression_type_id;
            } else {
                entity_declaration->value_type = expression_type_id;
            }
        } else {
            unless (INITIAL_EXPRESSION == NULL || typeid_eq(entity_declaration->value_type, INITIAL_EXPRESSION->value_type)) {
                unless (TYPE_IS_INVALID(INITIAL_EXPRESSION->value_type)) {
                    error_range(analyzer, INITIAL_EXPRESSION->start, INITIAL_EXPRESSION->end, "Explicit cast required to set define entity.");
                }
            }
        }
    } else {
        if (INITIAL_EXPRESSION != NULL && !typeid_eq(entity_declaration->value_type, INITIAL_EXPRESSION->value_type)) {
            error_range2(analyzer,
                    entity_declaration->data.declaration.type_expression->start, entity_declaration->data.declaration.type_expression->end,
                    INITIAL_EXPRESSION->start, INITIAL_EXPRESSION->end, "Type mismatch between expression type and declaration type.");
        }
    }

    entity->declared_type = entity_declaration->value_type;
    entity->narrowed_type = entity_declaration->value_type;

    if (INITIAL_EXPRESSION == NULL) {
        if (type_is_union(ast->type_set.types, entity_declaration->value_type)) {
            entity->narrowed_type = typeid(TYPE_VOID);
        } 

        i32 value_index = -1;

        if (type_is_union(ast->type_set.types, entity_declaration->value_type) && !typeid_eq(entity_declaration->value_type, typeid(TYPE_VOID))) {
            error_range(analyzer, entity_declaration->end, entity_declaration->end, "Non-void union types must have a default value.");
        } else unless (TYPE_IS_INVALID(entity->node->value_type)) {
            value_index = zero_value(ast, entity->node->value_type, &analyzer->symbols);
        }

        entity->node->value_index = value_index;
        entity->node->foldable = value_index >= 0;
    } else {
        entity->narrowed_type = INITIAL_EXPRESSION->value_type;
        if (IS_FOLDED(INITIAL_EXPRESSION)) {
            if (type_is_union(ast->type_set.types, INITIAL_EXPRESSION->value_type)) {
                type_t folded_value_type_id = get_folded_type(ast, INITIAL_EXPRESSION->value_index);
                entity->narrowed_type = folded_value_type_id;
            }

            entity->node->value_index = INITIAL_EXPRESSION->value_index;
            entity->node->foldable = INITIAL_EXPRESSION->value_index >= 0;
        }
    }

#undef INITIAL_EXPRESSION
}

static Entity *get_builtin_entity(ast_t *ast, symbol_t *identifier) {
    slot_t entity_slot;
    if (!symbol_table_get(&ast->builtins, identifier, &entity_slot)) {
        type_t type_id;
        native_function_t *function;
        bool has_value = false;
        type_t value_type;
        slot_t value_slot;
        if (is_builtin_type(identifier, &type_id)) {
            has_value = true;
            value_slot = SLOT_U(type_id.i);
            value_type = typeid(TYPE_TYPE);
        } else if (is_builtin_function(ast, identifier, &function)) {
            has_value = true;
            value_slot = SLOT_P(function);
            value_type = function->signature;
        }

        if (has_value) {
            i32 index = add_value_to_ast_constant_stack(ast, &value_slot, value_type);
            Entity *entity = add_builtin_entity(ast, identifier, value_type, index);
            return entity;
        }

        return NULL;
    }

    Entity *entity = (Entity*)entity_slot.as.p;
    return entity;
}

/*
 * This is the big boy function. The meat of compile time expression evaluation. This language has a couple of rules it must
 * follow for this to work correctly.
 *     1. There are only "declarations" and "expressions" in this language
 *         Declarations: Declarations store values in memory in a region called the scope
 *             - There are two types (1) named declarations and (2) anonymous declarations
 *             - Named declarations are accessible through their identifiers.
 *             - Anonymous declarations are not accessible since they have no identifier and thus should cause
 *                 side-effects to be useful
 *             - Named declarations can be made constant, which requires their expression to be made up of literals recursively
 *                 (symbols, strings, numbers, types, void, arrays, maps, functions, structs, other constants)
 *         Expressions: Expressions do computations and "return" a value
 * 
 *     2. Expressions can only access named declarations from their own scope or an outer one
 * 
 *     3. Expressions can only create inner scopes
 *     
 *     4. Declarations are NOT expressions
 * 
 *     5. Expression operations can only be composed of other expressions on the same scope (therefore, expressions cannot make declarations in the same scope)
 * 
 *     6. Expressions for a mutable declaration can only access name declarations within
 *         (1) their function definition and (2) the file scope
 * 
 * The way compile time expression evaluation works is by recursively figuring out what to solve next.
 * Jon Blow uses some sort of "dependency system" for this. From what I understand he does not scan forward
 * in the scope to gather declaration names, and instead uses some sort of queue system. The reason he told me he
 * doesn't scan the scope first is because sometimes the name of a variable is not known anyways due to it being 
 * in a far off module or something. I disagree and I think scanning the scope names is the solution to doing this
 * without a ton of overhead.
 * After scanning the scope for the names, now the current scope and all inner scopes will know what entities
 * exist in the program. When an expression is being resolved and requests the type or value of a named entity
 * this is the function that is called. An expression can be dependant on resursively on other named entities.
 * 
 *       {
 *           B :: A;
 *           A :: C * 2;
 *       };
 *       C :: 60;
 * 
 * In the case above when resolving what B is, expression A needs to be resolved, but resolving A requires
 * resolving C. Using forward scanning allows me to resolve the expressions in the right order without having
 * to use a queue. All identifiers exist and therefore if a name cannot be found it does not exist.
 * 
 * However, things can get tricker with structs and modules... but not that tricky.
 * 
 *       
 *       {
 *           B :: A.x * 2;
 *           A :: C { x = 60 };
 *       };
 *       C :: struct { x: i32; y: string; }; 
 * 
 * In this case to resolve B -> A.x -> A -> C -> x. When the expression evaluator is ran on A.x * 2, a partially imcomplete
 * struct will be used since y is not accessed
 * 
 * This function below is what does this recursive dependency thing.
*/
static Entity *get_resolved_entity_by_identifier(
        analyzer_t *analyzer,
        ast_t *ast,
        AnalysisState state,
        token_t identifier_token,
        EntityQuery *query,
        scope_t **search_scope) { // TODO: consider removing search scope from params, check if it's actually used

    bool passed_local_mutable_access_barrier = false;

    symbol_t *identifier = orso_unmanaged_symbol_from_cstrn(identifier_token.start, identifier_token.length, &analyzer->symbols, &ast->allocator);
    
    // early return if looking at a built in type
    {
        Entity *entity = get_builtin_entity(ast, identifier);
        if (entity) {
            search_scope = NULL;
            return entity;
        }
    }

    slot_t entity_slot;
    *search_scope = state.scope;

    while (*search_scope) {
        bool is_function_scope = (*search_scope)->type == SCOPE_TYPE_FUNCTION_PARAMETERS;

    #define NEXT_SCOPE() \
        if (is_function_scope) { passed_local_mutable_access_barrier = true; }\
        if (query->flags & QUERY_FLAG_MATCH_ONLY_IN_GIVEN_SCOPE) { break; }\
        *search_scope = (*search_scope)->outer

        unless (symbol_table_get(&(*search_scope)->named_entities, identifier, &entity_slot)) {
            NEXT_SCOPE();
            continue;
        }

        Entity *entity = (Entity*)entity_slot.as.p;

        if (query && query->skip_mutable && entity->node->data.declaration.is_mutable) {
            NEXT_SCOPE();
            continue;
        }

        if (passed_local_mutable_access_barrier && (*search_scope)->creator != NULL && entity->node->data.declaration.is_mutable) {
            NEXT_SCOPE();
            continue;
        }

        // this means that the declaration is *after* the entity we are trying to resolve
        if (entity->node->data.declaration.is_mutable && !is_declaration_resolved(entity->node)) {
            NEXT_SCOPE();
            continue;
        }

        // ensure that the value is resolved
        if (entity->node->data.declaration.initial_value_expression) {
            AnalysisState new_state = state;
            new_state.scope = *search_scope;
            resolve_entity_declaration(analyzer, ast, new_state, entity->node);
        }

        if (entity->node->data.declaration.is_mutable && entity->node->data.declaration.fold_level_resolved_at != state.fold_level) {
            error_range2(analyzer,
                    entity->node->start, entity->node->start,
                    identifier_token, identifier_token,
                    "Cannot access mutable entity declared on a different fold level.");
            return NULL;
        }

        if (type_is_function(ast->type_set.types, entity->node->value_type)) {
            /*
            * Okay... Time to do some explaining. This is a complicated one. Let's begin with the context.
            *
            * In a regular compiler that does not care about compile time evaluation of arbitrary expressions,
            * they need not worry about *when* a function's body has been analyzed[1], only that it needs to be
            * analyzed *eventually*. This is because in a regular compile, the assumption is that all functions
            * run *after* everything already has been analyzed *and* compiled. This means that once we know a
            * function's address in memory, every time we need to reference the function we can safely use its address.
            * This is true even if we haven't fully compiled a function's body too. This is useful for recursion because
            * while analyzing (mainly constant folding) a function's body, the body might call a function that references
            * the current function's body that you're analyzing. You can simply place a function's address where ever it's
            * references, and continue analyzing. This is an example of not having to care *when* a function's body gets analyzed
            * since it doesn't interfere with the rest of the compilation. 
            * 
            * When a language cares about compile time expression evaluation (CTEE) things get a little more complicated... Despite 
            * only needing to solve *when* it is possible to do CTEE this one problem is very complex to solve.
            * 
            * While solving recursion was straight forward with a regular compiler, it's not as clear with a CTEE compiler. Any
            * expression composed of only constant values can be CTEEed. This is different from constant folding which reduces
            * but doesn't run functions (even if its parameters are constants). This means functions can be ran while certain
            * portions of the program are still compiling. Again, this is in contrast while a regular compiler where *all* analysis
            * is done before *any* function is called ever.
            * 
            * So *when* can we perform CTEE? First, all entities of the expression being folded must
            *      1. be constant
            *      2. a mutable variable that was declared in the same fold level as the expression
            * 
            * The first requirement is trivial. You cannot fold an expression if the entities do not hold a concrete value.
            * The second is less obvious. In Orso, entities can be defined inside blocks, and blocks are expressions. This means
            * that the declarations in the block get run in order, inlined. Unlike a function that can be called at any time.
            * This means blocks undergoing constant folding can still access variables that were created during the fold level
            * since everything in the same fold level must be able to run on its own (assuming all constants are resolved)
            * 
            * The constant rule (1) only applies outside function boundaries. When resolving the inside of a function
            * all declarations are localized to that function, and the function cannot access local variables at higher
            * scope. So while the (2) rule must still apply, functions already restrict access to mutable variables outside
            * the their body. That said, the second rule still comes into play when a resolving a function under any folding level
            * and trying to access the global state. The global state starts at fold level 0, and so if a function is being folded it
            * will never be able to access the global state (i.e. must be a pure function). 
            * 
            * Rule A: Remember that when folding happens, the expression *must* be able to run on its own aside from constants.
            * i.e. the expression can be put into a compiler with the given folded constants and compile perfect.
            * 
            * Another issues arises when you can nest folds. i.e. in the following case
            * 
            *      T :: #fold foo();
            *       
            *      foo :: () -> i32 {
            *          return T;
            *      };
            * 
            * Even though T is a constant, because T requires the body of foo to fold, this creates a circular dependency.
            * However, how do you know when there is a circualr dependency like this or we are just recursioning and only
            * need the function address? If we are currently resolving the body of the function and we increase the fold level
            * this implies that whatever is being folded must be able to run on its own (as per Rule A). If we need to call
            * a function we need to check if its currently being resolved at a different fold level - because if it is, then
            * we cannot call the function during compile time.
            * 
            * Anyways, that's what the we are doing below. If the function definition was processed during
            * the same fold level that we are currenty on, then we are good. We are doing a regular function call.
            * 
            * However, if we are on a different fold level, we need to check if the definition is in the process of
            * being resolved because it needs to be compiled before running, and since it's in the middle of being resolved
            * it cannot be compiled won't ever be due to the circular dependency.
            * 
            *     [1] by analyzed I mean type checked, type flowed, constants folded, etc. i.e. getting the ast ready for codegen.
            */

            function_t *function = (function_t*)ast->folded_constants.items[entity->node->data.declaration.initial_value_expression->value_index].as.p;
            for (size_t i = 0; i < analyzer->dependencies.count; ++i) {
                i32 i_ = analyzer->dependencies.count - 1 - i;
                analysis_dependency_t *dependency = &analyzer->dependencies.items[i_];
                ast_node_t *node = dependency->ast_node;
                if (node->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION) {
                    function_t *folded_function = (function_t*)ast->folded_constants.items[node->value_index].as.p;
                    if (folded_function == function && dependency->fold_level != state.fold_level) {
                        // TODO: Use better system to figure out whether function definition can be compiled or not
                        // In this case, it cannot because of the fold level circular dependency.
                        node->data.function.compilable = false;

                        error_range(analyzer, node->start, node->end, "Function definition circular dependency.");
                        return NULL;
                    }
                }
            }
        }

        if (!query) {
            return entity;
        }

        if (query->flags & QUERY_FLAG_MATCH_ANY) {
            return entity;
        }

        type_t type = entity->node->value_type;

        if (query->flags & QUERY_FLAG_MATCH_TYPE && typeid_eq(query->search_type, type)) {
            return entity;
        }

        if (query->flags & QUERY_FLAG_MACH_FUNCTION && type_is_function(ast->type_set.types, type)) {
            return entity;
        }

        NEXT_SCOPE();

#undef NEXT_SCOPE
    }

    error_token(analyzer, identifier_token, "Undefined entity.");

    return NULL;
}

static void resolve_function_expression(
        analyzer_t *analyzer,
        ast_t *ast,
        AnalysisState state,
        ast_node_t *function_definition_expression) {
    ASSERT(function_definition_expression->node_type == AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION, "must be function declaration at this point");

    if (IS_FOLDED(function_definition_expression)) {
        return;
    }

    ast_function_t *definition = &function_definition_expression->data.function;

    i32 parameter_count = definition->parameter_nodes.count;
    // TODO: use a code gen error instead for parameter limit
    // // TODO: instead of hardcoding the number of parameters, instead use the numbers of bytes the params take up
    // if (parameter_count > MAX_PARAMETERS - 1) {
    //     error(analyzer, definition->parameter_nodes[0]->start.line, "Orso only allows a maximum of 100 parameters");
    // }

    scope_t function_parameter_scope;
    scope_init(&function_parameter_scope, &analyzer->allocator, SCOPE_TYPE_FUNCTION_PARAMETERS, state.scope, function_definition_expression);
    types_t parameter_types = {.allocator=&ast->allocator};

    forward_scan_declaration_names(analyzer, &function_parameter_scope, definition->parameter_nodes, parameter_count);

    bool parameter_invalid = false;

    // Resolves parameters for function type
    for (i32 i = 0; i < parameter_count; ++i) {
        AnalysisState new_state = state;
        // parameters look syntacticly like mutables, but their initial expressions should be constants
        new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
        new_state.scope = &function_parameter_scope;
        resolve_entity_declaration(analyzer, ast, new_state, definition->parameter_nodes.items[i]);
        array_push(&parameter_types, definition->parameter_nodes.items[i]->value_type);
        

        if (TYPE_IS_INVALID(definition->parameter_nodes.items[i]->value_type)) {
            parameter_invalid = true;
        }
    }

    type_t return_type = typeid(TYPE_VOID);
    if (definition->return_type_expression) {
        bool pushed = push_dependency(analyzer, definition->return_type_expression, state.fold_level, is_value_circular_dependency);
        if (pushed) {
            AnalysisState new_state = state;
            new_state.mode = MODE_CONSTANT_TIME | (state.mode & MODE_FOLDING_TIME);
            resolve_expression(analyzer, ast, state, definition->return_type_expression);
            pop_dependency(analyzer);
        }

        if (!TYPE_IS_TYPE(definition->return_type_expression->value_type)) {
            error_range(analyzer, definition->return_type_expression->start, definition->return_type_expression->end, "Invalid return type.");
        } else {
            return_type = get_folded_type(ast, definition->return_type_expression->value_index);
        }
    } else {
        if (!TYPE_IS_UNRESOLVED(function_definition_expression->value_type)) {
            type_info_t *signature = ast->type_set.types.items[function_definition_expression->value_type.i];
            return_type = signature->data.function.return_type;
        }
    }

    if (parameter_invalid || TYPE_IS_INVALID(return_type)) {
        INVALIDATE(function_definition_expression);
        return;
    }

    type_t function_type_id = type_set_fetch_function(&ast->type_set, return_type, parameter_types);
    function_t *function = orso_new_function(&analyzer->allocator);
    function->signature = function_type_id;

    array_push(&ast->function_definition_pairs, ((function_definition_pair_t){
        .ast_defintion = function_definition_expression,
        .function = function
    }));

    
    function_definition_expression->foldable = true;
    slot_t function_slot_value = SLOT_P(function);
    i32 function_constant_index = add_value_to_ast_constant_stack(ast, &function_slot_value, function_type_id);
    function_definition_expression->value_index = function_constant_index;

    // TODO: Maybe use a marco defined for this file for setting both the value and type, maybe an inlined function
    function_definition_expression->value_type = function_type_id;

    ASSERT(definition->block->node_type == AST_NODE_TYPE_EXPRESSION_BLOCK, "must be block expression");

    bool pushed = push_dependency(analyzer, function_definition_expression, state.fold_level, is_value_circular_dependency);
    if (pushed) {
        AnalysisState new_state = state;
        new_state.mode = MODE_RUNTIME;

        scope_t function_scope;
        scope_init(&function_scope, &analyzer->allocator, SCOPE_TYPE_FUNCTION_BODY, &function_parameter_scope, function_definition_expression);
        new_state.scope = &function_scope;
        resolve_expression(analyzer, ast, new_state, definition->block);
        pop_dependency(analyzer);
    }

    if (!TYPE_IS_VOID(return_type) && definition->block->return_guarentee != RETURN_GUARENTEE_YES) {
        error_range(analyzer, function_definition_expression->start, function_definition_expression->end, "Function definition does not return on all branches.");
    }
}

/*
* At this point in the analysis, we know the type for this struct definition is unknown. Otherwise
* we wouldn't be here trying to resolve it (expression calls this only if the ast is unresolved)
* It's possible for a struct to be defined like this:
* 
*          Foo :: struct {
*              i: i32;
*              foo: Foo;
*          };
* 
* Despite this being an invalid type (since this creates an infinitely sized struct) I can only find out
* after I figure out *what* Foo is. The first time Foo's assigned value is being analyzed is also the first
* time the analyzer enters the struct and encounters Foo. At this point, it is not obvious what Foo is because
* we are actively in the process of figuring that out! Also, this is a simple example, but since this language
* allows for this type of syntax (where ors and ands can resolve to any value) we cannot just consider the rhs
* to be a single ast node, like a function definition or structure.
* 
*          Foo :: null or struct {
*              i: i32;
*              foo: Foo;
*          };
* 
* This looks dumb, and it is, but it's also valid. And it means we cannot know what Foo is until *after* we have
* analyzed the `or` expression. And for that, we need to loop around and figure out the type for Foo.
* 
*          Foo :: #fold generate_foo();
* 
*          generate_foo :: (n: i32) -> struct { x: i32; Foo: foo; } { ... }
*          // this fails because of this dependency chain
*          // #fold generate_foo() => (n: i32) -> struct { x: i32, foo: Foo; } => struct { x: i32, foo: Foo; }
*          //      => Foo -> #fold generate_foo() <== this generates a cycle error
* 
* Here's another weird example
* 
*          Foo :: #fold generate_foo();
* 
*          generate_foo :: (n: i32) -> struct { x: i32; } {
*              return struct {
*                  x := Foo.x;
*              };
*          };
* 
*          This creates a circular dependency like this:
*          #fold generate_foo() [fold level: 1] => (n: i32) -> struct { x: i32; } { ... }
*              => struct { x := Foo.x } => Foo.x => Foo => #fold generate_foo() [fold level: 2]
*                  => (n: i32) -> struct { x: i32 } { ... } <=== ...
*                      // circular dependency since generate_foo's body is in the dependency list still
*                      // because constructed but on a different fold level
* 
* Important to note that if the dependency chain started with generate_foo instead of Foo, the circular
* dependency would be resolved a little earlier. 
*                               
* Here's another *working* example however
* 
*          Foo :: #fold generate_foo(10);
* 
*          generate_foo() :: (n: i32) -> struct { x: i32; } {
*              return struct {
*                  x := n * 10;
*              };
*          };
* 
* In the above example, Foo = *should* be a struct like this: struct { x := 100; }, I think that
* makes sense.
* 
* Later on, it will be possible to do this based on compile type parameters
* 
*          Vector2i :: #fold generate_vector2(i32, 0);
*          Vector2f :: #fold generate_vector2(f32, 0.0);
*          
*          // for now the `#!` means the parameter in the function call must be a compile time constnat
*          generate_foo(#! t: type, default_value: t) -> struct { x: t; y: t; } {
*              return struct {
*                  x := default_value;
*                  y := default_value;
*              };
*          };
* 
* This is an interesting one because this means that generic structs already exist in a similar way they do
* in C++ with templates or C with macros. Howver, there will be actual generic structs.
* 
* Here's another complicated one I guess
* 
*          Foo :: struct {
*              MyValue :: 10;
*              value := #fold generate_value();
*          };
* 
*          generate_value :: () -> i32 {
*              return Foo.MyValue;
*          };
* 
* This *should* work I guess?
* struct { MyValue:: 10; value := #fold generate_value(); }; 
*      => #fold generate_value()
*          => () -> i32 { return Foo.MyValue; }
*              => Foo.MyValue => Foo // At this point, get_resolved_entity_by_identifier will find
*                                    // Foo points to the "resolved" struct. But at this point, its still
*                                    // potentially incomplete (in this case it is because while we are figuring out
*                                    // what `value` is we are figuring out what is `Foo.MyValue`).
*                                    // However, this should still work because MyValue is a constant that does
*                                    // does not have a ciruclar dependency.
* 
* This gets more complicated when accessing a non-constant value:
* 
*          Foo :: struct {
*              value1 := 10;
*              value2 := #fold generate_value();
*          };
* 
*          (1)
*          generate_value :: () -> i32 {
*              foo := Foo();
*              return foo.value1;
*          };
* 
*          (2)
*          generate_value :: () -> i32 {
*              foo := Foo();
*              return foo.value2;
*          };
*
* For (1) this can be "designed" to work since its only accessing resolved fields. In the interpreter, 
* we can easily create an "incomplete" type just for the vm run. And since there were no causes for 
* circular dependencies, the codegen should be fine and the resulting runtime should be as well. This is 
* because the analyzer is so pathological. We know if we resolve a branch of the ast, all the dependencies
* under it are ready.
* 
* For (2) this needs to cause a circular dependency. When `foo` is found to be `Foo` type, then we try to figure
* out what `value2` is, and when we try to resolve that, we should be able to detect a circular depedency as we
* would normally. If we start the dependency chain at the function, when we resolve `foo.value2`,
* `generate_value` is being resolved at fold level 0. When we try to resolve foo.value2, we find that it
* folds to a function, but at that point we at fold level 1, meaning we have a circular dependency while folding.
* The same thing would happen if we start the dependency chain at the struct, except we'd find the circular
* dependency at fold level 2 (since we'd start at fold level 1 instead).
* 
* 
*/
static void resolve_struct_definition(analyzer_t *analyzer, ast_t *ast, AnalysisState state, ast_node_t *struct_definition) {
    scope_t struct_scope;
    scope_init(&struct_scope, &analyzer->allocator, SCOPE_TYPE_STRUCT, state.scope, struct_definition);

    state.fold_level = MODE_CONSTANT_TIME;
    state.scope = &struct_scope;

    forward_scan_declaration_names(analyzer, state.scope, struct_definition->data.struct_.declarations, struct_definition->data.struct_.declarations.count);

    i32 declarations_count = struct_definition->data.struct_.declarations.count;

    type_t incomplete_struct_id = type_unique_incomplete_struct_type(&ast->type_set);
    struct_definition->value_type = incomplete_struct_id;

    struct_definition->foldable = true;
    struct_definition->value_index = ast->true_index;

    ast_node_and_scope_t node_and_scope = {
        .node = struct_definition,
        .scope = &struct_scope
    };
    table_put(type2ns, ast->type_to_creation_node, struct_definition->value_type, node_and_scope);

    i32 field_count = 0;

    bool invalid_struct = false;
    for (i32 i = 0; i < declarations_count; i++) {
        ast_node_t* declaration = struct_definition->data.struct_.declarations.items[i];
        field_count += (declaration->data.declaration.is_mutable);

        resolve_entity_declaration(analyzer, ast, state, declaration);

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
            token_t identifier = struct_definition->data.struct_.declarations.items[i]->data.declaration.identifier;
            char* name = arena_alloc(&analyzer->allocator, sizeof(char)*(identifier.length + 1));

            memcpy(name, identifier.start, identifier.length);
            
            name[identifier.length] = '\0';

            if (struct_definition->data.struct_.declarations.items[i]->data.declaration.is_mutable) {
                fields[field_counter].type = struct_definition->data.struct_.declarations.items[i]->value_type;
                fields[field_counter].name = name;

                ast_fields[field_counter] = struct_definition->data.struct_.declarations.items[i];

                ++field_counter;
            } else {
                constants[constant_counter].type = struct_definition->data.struct_.declarations.items[i]->value_type;
                constants[constant_counter].name = name;
                ++constant_counter;
            }
        }
    }

    type_info_t *complete_struct_type;

    type_t complete_struct_type_id = type_set_fetch_anonymous_struct(&ast->type_set, field_count, fields, constant_count, constants);
    complete_struct_type = ast->type_set.types.items[complete_struct_type_id.i];

    i32 incomplete_index = -1;
    for (i32 i = 0; i < complete_struct_type->data.struct_.field_count; i++) {
        type_t field_type_id = complete_struct_type->data.struct_.fields[i].type;
        type_info_t *field_type = ast->type_set.types.items[field_type_id.i];
        if (struct_type_is_incomplete(field_type)) {
            incomplete_index = i;
            break;
        }
    }

    if (incomplete_index < 0) {
        struct_definition->value_type = complete_struct_type_id;

        ASSERT(complete_struct_type->data.struct_.field_count == field_count, "completed struct must have the same number as fields as ast field declaration nodes");

        u64 size_in_slots = bytes_to_slots(complete_struct_type->data.struct_.total_bytes);
        byte struct_data[size_in_slots * sizeof(slot_t)];
        for (u64 i = 0; i < size_in_slots * sizeof(slot_t); i++) {
            struct_data[i] = 0;
        }

        for (i32 i = 0; i < field_count; i++) {
            type_t field_type_id = complete_struct_type->data.struct_.fields[i].type;
            u32 offset = complete_struct_type->data.struct_.fields[i].offset;
            ast_node_t *declaration = ast_fields[i];

            type_info_t *field_type = ast->type_set.types.items[field_type_id.i];

            u32 bytes_to_copy = type_size_bytes(field_type);

            if (type_is_union(ast->type_set.types, field_type_id)) {
                type_t value_expression_type_id = declaration->data.declaration.initial_value_expression == NULL ?
                    typeid(TYPE_TYPE) : declaration->data.declaration.initial_value_expression->value_type;
                ASSERT(!type_is_union(ast->type_set.types, value_expression_type_id), "initial expression cannot be a union.");

                memcpy(struct_data + offset, (byte*)(&value_expression_type_id), sizeof(slot_t));
                offset += sizeof(slot_t);
                bytes_to_copy -= sizeof(slot_t);
            }

            byte* value_src = (byte*)&ast->folded_constants.items[declaration->value_index];
            memcpy(struct_data + offset, value_src, bytes_to_copy);
        }

        struct_definition->value_index = add_value_to_ast_constant_stack(ast, (slot_t*)struct_data, complete_struct_type_id);

        node_and_scope.scope = NULL;
        table_put(type2ns, ast->type_to_creation_node, complete_struct_type_id, node_and_scope);
    } else {
        INVALIDATE(struct_definition);

        ast_node_t *incomplete_field = struct_definition->data.struct_.declarations.items[incomplete_index];
        error_range(analyzer, incomplete_field->start, incomplete_field->end, "Incomplete field");
    }
}

static scope_t *get_closest_outer_function_scope(type_infos_t *types, scope_t *scope) {
    while (scope && scope->creator && !type_is_function(*types, scope->creator->value_type)) {
        scope = scope->outer;
    }

    return scope;
}

static void resolve_statement(
        analyzer_t *analyzer,
        ast_t *ast,
        AnalysisState state,
        ast_node_t *statement) {
    switch (statement->node_type) {
        case AST_NODE_TYPE_STATEMENT_EXPRESSION:
            resolve_expression(analyzer, ast, state, statement->data.expression);
            statement->value_type = statement->data.expression->value_type;
            break;

        case AST_NODE_TYPE_STATEMENT_RETURN: {
            type_t return_expression_type = typeid(TYPE_VOID);

            if (statement->data.expression) {
                resolve_expression(analyzer, ast, state, statement->data.expression);
                return_expression_type = statement->data.expression->value_type;
            }

            scope_t *function_scope = get_closest_outer_function_scope(&ast->type_set.types, state.scope);
            ASSERT(function_scope, "right now all scopes should be under a function scope");

            type_t function_type_id = function_scope->creator->value_type;
            type_info_t *function_type = ast->type_set.types.items[function_type_id.i];

            type_t function_return_type_id = function_scope ? function_type->data.function.return_type : typeid(TYPE_VOID);
            unless (TYPE_IS_INVALID(return_expression_type) || typeid_eq(function_return_type_id, return_expression_type)) {
                error_range(analyzer, statement->data.expression->start, statement->data.expression->end, "Requires explict cast to return.");
            }
            break;
        }

        case AST_NODE_TYPE_UNDEFINED: break;
        case AST_NODE_TYPE_DECLARATION:
        case AST_NODE_TYPE_EXPRESSION_CASE: UNREACHABLE();
    }
}

static void resolve_declaration(
        analyzer_t* analyzer,
        ast_t* ast,
        AnalysisState state,
        ast_node_t* declaration_node) {

    // TODO: eventually this will be resolved, everything will be a declaration entity
    // there will only be two types: anonymous declarations and named declarations
    // print_expr, print and statement expressions will be all me anonymous declarations
    if (declaration_node->node_type == AST_NODE_TYPE_DECLARATION) {
        resolve_entity_declaration(analyzer, ast, state, declaration_node);
    } else {
        resolve_statement(analyzer,ast, state, declaration_node);

    }
}

void resolve_declarations(analyzer_t* analyzer, ast_t* ast, AnalysisState state, ast_nodes_t declarations, i32 count) {
    for (i32 i = 0; i < count; i++) {
        ast_node_t* declaration = declarations.items[i];
        // Constants are meant to be solved at compile time, which means they do not run during a particular state
        // of the program. Constants are only useful during runtime when things need to happen and they are accessed.
        // If they are never accessed they don't even need to be compiled into the running program.
        // By skipping them, I have a really convenient way telling which constants are being used or not.
        // That being said, I still might want to compile them if I'm making a library of some sort.
        // I can always go through them after... Like for example, right after this loop, I can just check
        // which constants in this scope are not being used and make a warning (for local constants), and
        // I can simply compile them if its a constant in the global scope (so they can be accessed).
        //
        // This is also to remove the possibility of false circular dependencies. For example, a function like this:
        //
        //         foo :: () -> i32 {
        //             FOO :: #fold foo();
        //             return 42;
        //         };
        //
        // should still be able to compile since nothing in the function scope or below it is accessing `FOO`. While this may
        // seem useless to check for since I shouldn't allow constant expressions to fold a function call that is inside
        // the body of its own function regardless, it's something that I need to check for anyways to make sure they are no
        // circular dependencies.
        // Anyways, that's why constants are skipped and resolved only when they are accessed
        if (declaration->node_type == AST_NODE_TYPE_DECLARATION && !declaration->data.declaration.is_mutable) {
            continue;
        }

        resolve_declaration(analyzer, ast, state, declarations.items[i]);
    }

    for (i32 i = 0; i < count; i++) {
        ast_node_t *declaration = declarations.items[i];
        if (declaration->node_type != AST_NODE_TYPE_DECLARATION) {
            continue;
        }

        if (declaration->data.declaration.is_mutable) {
            continue;
        }

        if (is_declaration_resolved(declaration)) {
            continue;
        }

        resolve_declaration(analyzer, ast, state, declaration);
    }
}

bool resolve_ast(analyzer_t* analyzer, ast_t* ast) {
    if (ast->root->data.block.items == NULL) {
        ast->resolved = false;
        // error(analyzer, 0, "No code to run. Akin to having no main function.");
        return false;
    }

    scope_t global_scope;
    scope_init(&global_scope, &analyzer->allocator, SCOPE_TYPE_MODULE, NULL, NULL);

    AnalysisState analysis_state = (AnalysisState) {
        .mode = MODE_RUNTIME,
        .scope = &global_scope,
        .fold_level = 0,
    };

    i32 declaration_count = ast->root->data.block.count;
    forward_scan_declaration_names(analyzer, &global_scope, ast->root->data.block, declaration_count);
    resolve_declarations(analyzer, ast, analysis_state, ast->root->data.block, declaration_count);

    ast->resolved = !analyzer->had_error;

    return ast->resolved;
}

void analyzer_init(analyzer_t* analyzer, write_function_t write_fn, error_function_t error_fn) {
    analyzer->error_fn = error_fn;
    analyzer->had_error = false;

    analyzer->allocator = (arena_t){0};

    analyzer->dependencies = (analysis_dependencies_t){.allocator=&analyzer->allocator};

    // TODO: fix
    (void)write_fn;

    symbol_table_init(&analyzer->symbols, &analyzer->allocator);
}

void analyzer_free(analyzer_t* analyzer) {
    for (i32 i = 0; i < analyzer->symbols.capacity; i++) {
        symbol_table_entry_t* entry = &analyzer->symbols.entries[i];
        if (entry->key == NULL) {
            continue;
        }
    }

    analyzer->dependencies.count = 0;

    analyzer->ast = NULL;

    analyzer->error_fn = NULL;
    analyzer->had_error = false;
}
