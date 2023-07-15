#include "static_analyzer.h"

#include <stdio.h>

#include "interpreter.h"
#include "sb.h"
#include "mathutils.h"
#include "type_set.h"

#include <time.h>

#define EXPRESSION_RESOLVED(expression) (expression->value_type != &OrsoTypeUnresolved)

static void clock_native(OrsoSlot* arguments, OrsoSlot* result) {
    (void)arguments;
    result[0] = ORSO_SLOT_F((double)clock() / CLOCKS_PER_SEC, &OrsoTypeFloat64);
}

typedef OrsoSymbolTable SymbolTable;

typedef enum ExpressionFoldingMode {
    MODE_RUNTIME,
    MODE_CONSTANT_TIME,
    MODE_FOLDING_TIME,
} ExpressionFoldingMode;

typedef struct Entity {
    // TODO: instead of using declarad type just use narrowed type and the type of the decalrtion node
    OrsoType* declared_type;
    OrsoType* narrowed_type;

    OrsoEntityDeclarationNode* declaration_node;
} Entity;

static void scope_init(OrsoScope* scope, OrsoScope* outer, OrsoExpressionNode* creator_expression) {
    scope->outer = outer;
    scope->creator = creator_expression;
    orso_symbol_table_init(&scope->named_entities);
}

static void scope_free(OrsoScope* scope) {
    scope->outer = NULL;
    for (i32 i = 0; i < scope->named_entities.capacity; i++) {
        OrsoSymbolTableEntry* entry = &scope->named_entities.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        Entity* pair = (Entity*)entry->value.as.p;
        free(pair);
    }
    orso_symbol_table_free(&scope->named_entities);
}

static OrsoScope* scope_copy_new(OrsoScope* scope) {
    if (scope == NULL) {
        return NULL;
    }

    OrsoScope* scope_copy = ORSO_ALLOCATE(OrsoScope);
    scope_init(scope_copy, NULL, scope->creator);
    orso_symbol_table_add_all(&scope->named_entities, &scope_copy->named_entities);

    for (i32 i = 0; i < scope_copy->named_entities.capacity; i++) {
        OrsoSymbolTableEntry* entry = &scope_copy->named_entities.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        Entity* entity_copy = ORSO_ALLOCATE(Entity);
        *entity_copy = *((Entity*)entry->value.as.p);
        entry->value = ORSO_SLOT_P(entity_copy, &OrsoTypeType);
    }

    scope_copy->outer = scope_copy_new(scope->outer);

    return scope_copy;
}

static void scope_merge(OrsoTypeSet* set, OrsoScope* scope, OrsoScope* a, OrsoScope* b) {
    if (!scope) {
        ASSERT(!a && !b, "if no more scopes then all should be no more");
        return;
    }

    for (i32 i = 0; i < scope->named_entities.capacity; i++) {
        OrsoSymbolTableEntry* entry = &scope->named_entities.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        OrsoSlot entity_a_slot;
        OrsoSlot entity_b_slot;
        if (!orso_symbol_table_get(&a->named_entities, entry->key, &entity_a_slot)
                || !orso_symbol_table_get(&b->named_entities, entry->key, &entity_b_slot)) {
            continue;
        }

        Entity* entity_a = (Entity*)entity_a_slot.as.p;
        Entity* entity_b = (Entity*)entity_b_slot.as.p;

        OrsoType* anded_type;
        OrsoType* anded_narrowed;
        if (entity_a->declared_type == &OrsoTypeUnresolved || entity_b->declared_type == &OrsoTypeUnresolved) {
            if (entity_a->declared_type == entity_b->declared_type) {
                continue;
            }

            // since one of the entities was not narrowed, we should use the declared type
            // instead of the narrowed type. (since the other side wasn't narrowed to anything)
            if (entity_a->declared_type != &OrsoTypeUnresolved) {
                anded_type = entity_a->declared_type;
                anded_narrowed = entity_a->declared_type;
            } else {
                anded_type = entity_b->declared_type;
                anded_narrowed = entity_b->declared_type;
            }
        } else {
            ASSERT(entity_a->declared_type == entity_b->declared_type, "declared type should be stable. TODO: No need for declared type to be in here... should be in the declaration instead.");
            anded_type = entity_a->declared_type;
            anded_narrowed = orso_type_merge(set, entity_a->narrowed_type, entity_b->narrowed_type);
        }

        Entity* scope_entity = (Entity*)entry->value.as.p;

        scope_entity->declared_type = anded_type;
        scope_entity->narrowed_type = anded_narrowed;
    }

    scope_merge(set, scope->outer, a->outer, b->outer);
}

static void add_entity(OrsoScope* scope, OrsoSymbol* identifier, OrsoEntityDeclarationNode* declaration_node) {
    Entity* entity = ORSO_ALLOCATE(Entity);
    entity->declared_type = &OrsoTypeUnresolved; //declared;
    entity->narrowed_type = &OrsoTypeUnresolved; //narrowed;
    entity->declaration_node = declaration_node;

    orso_symbol_table_set(&scope->named_entities, identifier, ORSO_SLOT_P(entity, &OrsoTypeVoid));
}

static void error(OrsoStaticAnalyzer* analyzer, i32 line, const char* message) {
    if (analyzer->panic_mode) {
        return;
    }

    analyzer->panic_mode = true;
    analyzer->had_error = true;

    if (!analyzer->error_fn) {
        return;
    }

    analyzer->error_fn(ORSO_ERROR_COMPILE, line, message);
}

static void error_incompatible_binary_types(OrsoStaticAnalyzer* analyzer, Token operation, OrsoType* left, OrsoType* right, i32 line) {

    const char message[256];
    char* msg = (char*)message;

    const char left_type_str[128];
    const char right_type_str[128];
    orso_type_to_cstrn(left, (char*)left_type_str, 128);
    orso_type_to_cstrn(right, (char*)right_type_str, 128);

    msg += sprintf(msg, "Incompatible Types: '%s' %.*s '%s'",
        left_type_str, operation.length, operation.start, right_type_str);

    error(analyzer, line, message);
}

static void error_incompatible_unary_type(OrsoStaticAnalyzer* analyzer, Token operation, OrsoType* operand, i32 line) {
    const char message[256];
    char* msg = (char*)message;

    const char operand_type_str[128];
    orso_type_to_cstrn(operand, (char*)operand_type_str, 128);

    msg += sprintf(msg, "Incompatible Type: unary(%.*s) and type '%s'",
        operation.length, operation.start, operand_type_str);

    error(analyzer, line, message);
}

static OrsoType* resolve_unary_type(TokenType operator, OrsoType* operand) {
    if (operand == &OrsoTypeInvalid) {
        return &OrsoTypeInvalid;
    }

    if (operand == &OrsoTypeUnresolved) {
        return &OrsoTypeUnresolved;
    }

    switch (operator) {
        case TOKEN_MINUS: {
            if (!ORSO_TYPE_IS_UNION(operand)) {
                if (orso_type_is_number(operand, false)) {
                    return operand;
                } else if (operand == &OrsoTypeBool) {
                    return &OrsoTypeInteger32;
                } else {
                    return &OrsoTypeInvalid;
                }
            } else {
                return &OrsoTypeInvalid;
            }
        }
        case TOKEN_NOT:
            return &OrsoTypeBool;
        default: return &OrsoTypeInvalid;
    }
}

static OrsoExpressionNode* implicit_cast(OrsoExpressionNode* operand, OrsoType* value_type) {
    OrsoExpressionNode* implicit_cast = ORSO_ALLOCATE(OrsoExpressionNode);

    implicit_cast->start = operand->start;
    implicit_cast->end = operand->end;
    implicit_cast->type = EXPRESSION_IMPLICIT_CAST;
    implicit_cast->value_type = value_type;
    implicit_cast->narrowed_value_type = value_type;
    implicit_cast->expr.cast.operand = operand;
    implicit_cast->fold = false;
    implicit_cast->foldable = true;
    implicit_cast->folded_value_index = -1;

    return implicit_cast;
}

#define IS_FOLDED(EXPRESSION_PTR) (EXPRESSION_PTR->folded_value_index >= 0)

static OrsoType* get_folded_type(OrsoAST* ast, i32 index) {
    ASSERT(index >= 0, "must be foldable and folded");

    OrsoSlot* type_slot = &ast->folded_constants[index];
    OrsoType* type = (OrsoType*)type_slot->as.p;
    return type;
}

typedef enum {
    ENTITY_QUERY_ANY,
    ENTITY_QUERY_TYPE,
    ENTITY_QUERY_FUNCTION,
} QueryType;

typedef struct EntityQuery  {
    QueryType type;
    OrsoType* search_type;
    bool skip_mutable;
} EntityQuery;

static Entity* get_resolved_entity_by_identifier(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        Token identifier_token,
        EntityQuery* query,
        OrsoScope** found_scope);

static bool is_builtin_type(Token identifier, OrsoType** type) {
#define RETURN_IF_TYPE(TOKEN, TYPE_STRING, TYPE_ID) \
if (memcmp(identifier.start, #TYPE_STRING, (sizeof(#TYPE_STRING) - 1)/sizeof(char)) == 0) { \
    *type = TYPE_ID; \
    return true; \
}

    RETURN_IF_TYPE(identifier, void, &OrsoTypeVoid)
    RETURN_IF_TYPE(identifier, bool, &OrsoTypeBool)
    RETURN_IF_TYPE(identifier, i32, &OrsoTypeInteger32)
    RETURN_IF_TYPE(identifier, i64, &OrsoTypeInteger64)
    RETURN_IF_TYPE(identifier, f32, &OrsoTypeFloat32)
    RETURN_IF_TYPE(identifier, f64, &OrsoTypeFloat64)
    RETURN_IF_TYPE(identifier, string, &OrsoTypeString)
    RETURN_IF_TYPE(identifier, symbol, &OrsoTypeSymbol)
    RETURN_IF_TYPE(identifier, type, &OrsoTypeType)

    *type = NULL;
    return false;

#undef RETURN_IF_TYPE
}

static OrsoType* resolve_identifier_type(OrsoStaticAnalyzer* analyzer, OrsoAST* ast, OrsoScope* scope, i32 fold_level, Token identifier_type) {
    OrsoType* type;
    if (is_builtin_type(identifier_type, &type)) {
        return type;
    }

    EntityQuery query = (EntityQuery) { .type = ENTITY_QUERY_TYPE, .skip_mutable = true };
    OrsoScope* entity_scope;
    Entity* value = get_resolved_entity_by_identifier(analyzer, ast, scope, fold_level, identifier_type, &query, &entity_scope);

    if (!value) {
        char message[512];
        sprintf(message, "Type %.*s has not been declared.", identifier_type.length, identifier_type.start);
        error(analyzer, identifier_type.line, message);
        return &OrsoTypeInvalid;
    }

    // if (value->declaration_node->is_mutable) {
    //     char message[512];
    //     sprintf(message, "Declaration for %.*s is mutable and cannot be resolved at compile time.", identifier_type.length, identifier_type.start);
    //     error(analyzer, identifier_type.line, message);
    //     return &OrsoTypeInvalid;
    // }

    type = (OrsoType*)get_folded_type(ast, value->declaration_node->expression->folded_value_index);
    return type;
}

static bool can_call(OrsoFunctionType* type, OrsoExpressionNode** arguments) {
    if (type->argument_count != sb_count(arguments)) {
        return false;
    }

    for (i32 i = 0; i < type->argument_count; i++) {
        OrsoType* parameter_type = type->argument_types[i];
        OrsoType* argument_type = arguments[i]->narrowed_value_type;
        if (!orso_type_fits(parameter_type, argument_type)) {
            return false;
        }
    }

    return true;
}

static i32 add_value_to_ast_constant_stack(OrsoAST* ast, OrsoSlot* value, OrsoType* type) {
    i32 slot_count = orso_type_slot_count(type);
    for (i32 i = 0; i < slot_count; i ++) {
        sb_push(ast->folded_constants, value[i]);
    }
    
    return sb_count(ast->folded_constants) - slot_count;
}

static i32 evaluate_expression(OrsoStaticAnalyzer* analyzer, OrsoAST* ast, OrsoScope* scope, OrsoExpressionNode* expression) {
    (void)scope; // TODO: remove this if unnecessary

    OrsoVM vm;
    // TODO: Make sure this uses the same writing function as the vm that runs the code at the end.
    orso_vm_init(&vm, NULL);

    OrsoSlot stack[512];
    vm.stack = stack;
    vm.stack_top = stack;

    OrsoCodeBuilder builder;
    orso_code_builder_init(&builder, &vm, ast);

    OrsoFunction* function = orso_generate_expression_function(&builder, expression);

    orso_code_builder_free(&builder);

    OrsoSlot* value = orso_call_function(&vm, function, analyzer->error_fn);

    i32 value_index = add_value_to_ast_constant_stack(ast, value, expression->value_type);

    return value_index;
}

static bool get_native_function(OrsoStaticAnalyzer* analyzer, OrsoAST* ast, Token name, i32* index) {
    if (strncmp(name.start, "clock", MIN(5, name.length)) == 0) {
        OrsoSymbol* function_name = orso_new_symbol_from_cstrn(name.start, name.length, &analyzer->symbols);
        OrsoSlot index_slot;
        if (orso_symbol_table_get(&ast->builtins, function_name, &index_slot)) {
            *index = index_slot.as.i;
            return true;
        }

        OrsoType* function_type = orso_type_set_fetch_native_function(&ast->type_set, &OrsoTypeFloat64, NULL, 0);
        OrsoNativeFunction* native_function = orso_new_native_function(clock_native, function_type);
        OrsoSlot native_function_slot = ORSO_SLOT_P(native_function, function_type);
        *index = add_value_to_ast_constant_stack(ast, &native_function_slot, function_type);

        index_slot = ORSO_SLOT_I(*index, &OrsoTypeInteger32);
        orso_symbol_table_set(&ast->builtins, function_name, index_slot);

        return true;
    }

    return false;
}

static void fold_constants(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoExpressionNode* expression,
        ExpressionFoldingMode mode);

static OrsoType* resolve_type(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoTypeNode* type_node);

static void resolve_foldable(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast, OrsoScope* scope,
        i32 fold_level,
        OrsoExpressionNode* expression,
        ExpressionFoldingMode mode) {

    bool foldable = false;
    i32 folded_index = -1;

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            foldable = expression->expr.grouping.expression->foldable;
            folded_index = expression->expr.grouping.expression->folded_value_index;
            break;
        }

        case EXPRESSION_BINARY: {
            foldable = expression->expr.binary.left->foldable && expression->expr.binary.right->foldable;
            break;
        }

        case EXPRESSION_IFELSE: {
            bool condition_is_foldable = expression->expr.ifelse.condition->foldable;
            bool then_is_foldable = expression->expr.ifelse.then->foldable;
            bool else_is_foldable = expression->expr.ifelse.else_ ? expression->expr.ifelse.else_->foldable : true;

            foldable = condition_is_foldable && then_is_foldable && else_is_foldable;
            break;
        }

        case EXPRESSION_UNARY: {
            foldable = expression->expr.unary.operand->foldable;
            break;
        }

        case EXPRESSION_ENTITY: {
            OrsoScope* entity_scope;
            Token name = expression->expr.entity.name;


            i32 native_function_index;
            if (get_native_function(analyzer, ast, name, &native_function_index)) {
                foldable = true;
                folded_index = native_function_index;
                break;
            }

            EntityQuery query = (EntityQuery){ .skip_mutable = (mode == MODE_CONSTANT_TIME) || (mode == MODE_FOLDING_TIME) };
            Entity* entity = get_resolved_entity_by_identifier(analyzer, ast, scope, fold_level, name, &query, &entity_scope);

            if (!entity) {
                char message[512];
                sprintf(message, "Entity %.*s undefined.", expression->expr.entity.name.length, expression->expr.entity.name.start);
                error(analyzer, expression->expr.entity.name.line, message);
                break;
            }

            if (entity->declaration_node->is_mutable) {
                foldable = false;
                break;
            }

            if (entity->declaration_node->expression) {
                foldable = entity->declaration_node->expression->foldable;
                folded_index = entity->declaration_node->expression->folded_value_index;

                ASSERT(folded_index >= 0, "since the entity is a constant, it should have a folded value already");
            } else {
                foldable = true;
                folded_index = entity->declaration_node->implicit_default_value_index;
            }
            break;
        }

        case EXPRESSION_FUNCTION_DEFINITION: {
            foldable = true;
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            foldable = true;
            break;
        }

        case EXPRESSION_CALL: {
            if (mode != MODE_FOLDING_TIME) {
                break;
            }

            foldable = expression->expr.call.callee->foldable;
            if (!foldable) {
                break;
            }

            for (i32 i = 0; i < sb_count(expression->expr.call.arguments); i++) {
                foldable &= expression->expr.call.arguments[i]->foldable;
                if (!foldable) {
                    break;
                }
            }
            break;
        }
        case EXPRESSION_BLOCK: {
            if (mode != MODE_FOLDING_TIME) {
                break;
            }

            foldable = true;
            break;
        }
        case EXPRESSION_ASSIGNMENT: {
            break;
        }

        case EXPRESSION_PRIMARY: {
            UNREACHABLE();
            break;
        }

        case EXPRESSION_NONE:
        case EXPRESSION_FOR: {
            UNREACHABLE();
        }
    }

    expression->foldable = foldable;
    expression->folded_value_index = folded_index;
}

static void fold_constants(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoExpressionNode* expression,
        ExpressionFoldingMode mode) {

    // already folded nothing to do
    if (IS_FOLDED(expression)) {
        return;
    }

    resolve_foldable(analyzer, ast, scope, fold_level, expression, mode);

    if (IS_FOLDED(expression)) {
        return;
    }

    // we can't fold it so nothing to do
    if (!expression->foldable) {
        return;
    }

    i32 value_index = evaluate_expression(analyzer, ast, scope, expression);

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
    if ((mode == MODE_CONSTANT_TIME || mode == MODE_FOLDING_TIME) && ORSO_TYPE_IS_UNION(expression->value_type)) {
        OrsoType* narrowed_type = (OrsoType*)ast->folded_constants[value_index].as.p;

        value_index++; // the type makes up the first slot value, and rest is the actual value

        expression->value_type = narrowed_type;
        expression->narrowed_value_type = narrowed_type;

        // TODO: Maybe do this in a more robust way? Like maybe during compilation somehow?
        #ifdef DEBUG_TRACE_EXECUTION
        ast->folded_constants[value_index].type = narrowed_type;
        #endif
    }

    expression->folded_value_index = value_index;
}

static void resolve_declarations(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoDeclarationNode** declarations,
        i32 count);

static void resolve_declaration(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoDeclarationNode* declaration_node);

static void resolve_function_expression(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoExpressionNode* function_definition_expression,
        ExpressionFoldingMode mode);

static void declare_entity(OrsoStaticAnalyzer* analyzer, OrsoScope* scope, OrsoEntityDeclarationNode* entity);

static void forward_scan_declaration_names(OrsoStaticAnalyzer* analyzer, OrsoScope* scope, OrsoDeclarationNode** declarations, i32 count) {
    for (i32 i = 0; i < count; i++) {
        OrsoDeclarationNode* declaration = declarations[i];
        if (declaration->type != ORSO_DECLARATION_ENTITY) {
            continue;
        }

        OrsoEntityDeclarationNode* entity = declaration->decl.entity;

        declare_entity(analyzer, scope, entity);
    }
}

void orso_resolve_expression(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoExpressionNode* expression,
        ExpressionFoldingMode mode) {

    if (expression->value_type != &OrsoTypeUnresolved) {
        return;
    }

    if (expression->fold) {
        fold_level++;
        mode = MODE_FOLDING_TIME;
    }

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            orso_resolve_expression(analyzer, ast, scope, fold_level, expression->expr.grouping.expression, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }

            expression->value_type = expression->expr.grouping.expression->narrowed_value_type;
            expression->narrowed_value_type = expression->expr.grouping.expression->narrowed_value_type;
            break;
        }
        case EXPRESSION_PRIMARY: {
            expression->value_type = &OrsoTypeInvalid;
            break;
        }
        case EXPRESSION_BINARY: {
            OrsoExpressionNode* left = expression->expr.binary.left;
            OrsoExpressionNode* right = expression->expr.binary.right;
            orso_resolve_expression(analyzer, ast, scope, fold_level, left, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }

            orso_resolve_expression(analyzer, ast, scope, fold_level, right, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }

            // TODO: Remember to do different things depending on operation
            //   if it's arthimetic, or comparisons, then let them merge at the end
            //   if it's logical, then you need to do special things

            OrsoType* cast_left = &OrsoTypeInvalid;
            OrsoType* cast_right = &OrsoTypeInvalid;

            expression->narrowed_value_type = &OrsoTypeUnresolved;

            bool is_logical_operator = false;

            switch (expression->expr.binary.operator.type) {
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                case TOKEN_STAR:
                case TOKEN_SLASH: {
                    OrsoType* combined_type = orso_binary_arithmetic_cast(left->narrowed_value_type, right->narrowed_value_type, expression->expr.binary.operator.type);
                    expression->value_type = combined_type;

                    ASSERT(!ORSO_TYPE_IS_UNION(expression->value_type), "arthimetic must narrow down to a single type");

                    cast_left = combined_type;
                    cast_right = combined_type;
                    break;
                }

                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL: {
                    orso_binary_comparison_casts(left->narrowed_value_type, right->narrowed_value_type, &cast_left, &cast_right);
                    expression->value_type = &OrsoTypeBool;
                    break;
                }
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    orso_binary_equality_casts(left->narrowed_value_type, right->narrowed_value_type, &cast_left, &cast_right);
                    expression->value_type = &OrsoTypeBool;
                    break;
                }

                case TOKEN_AND:
                case TOKEN_OR: {
                    is_logical_operator = true;
                    cast_left = left->value_type;
                    cast_right = right->value_type;

                    OrsoType* merged_type = orso_type_merge(&ast->type_set, left->value_type, right->value_type);
                    if (merged_type == &OrsoTypeInvalid) {
                        error(analyzer, expression->expr.binary.operator.line, "too many types in union for logical operations.");
                    } else {
                        expression->narrowed_value_type = orso_type_merge(&ast->type_set, left->narrowed_value_type, right->narrowed_value_type);
                    }

                    expression->value_type = merged_type;
                    break;
                }
                default: UNREACHABLE();
            }

            if (expression->narrowed_value_type == &OrsoTypeUnresolved) {
                expression->narrowed_value_type = expression->value_type;
            }

            if (cast_left == &OrsoTypeInvalid || cast_right == &OrsoTypeInvalid) {
                expression->value_type = &OrsoTypeInvalid;
                error_incompatible_binary_types(analyzer, expression->expr.binary.operator, left->narrowed_value_type, right->narrowed_value_type, expression->expr.binary.operator.line);
                return;
            }

            if (!is_logical_operator) {
                if (cast_left != left->narrowed_value_type) {
                    expression->expr.binary.left = implicit_cast(left, cast_left);
                    fold_constants(analyzer, ast, scope, fold_level, expression->expr.binary.left, mode);
                }


                if (cast_right != right->narrowed_value_type) {
                    expression->expr.binary.right = implicit_cast(right, cast_right);
                    fold_constants(analyzer, ast, scope, fold_level, expression->expr.binary.right, mode);
                }
            }
            break;
        }
        case EXPRESSION_UNARY: {
            OrsoUnaryOp* unary_op = &expression->expr.unary;
            orso_resolve_expression(analyzer, ast, scope, fold_level, unary_op->operand, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }

            OrsoType* new_type = resolve_unary_type(unary_op->operator.type, unary_op->operand->narrowed_value_type);
            expression->value_type = new_type;
            expression->narrowed_value_type = new_type;
            
            // TODO: Must negate the new type implications if the unary operation is NOT

            if (expression->narrowed_value_type == &OrsoTypeInvalid) {
                error_incompatible_unary_type(analyzer, unary_op->operator, unary_op->operand->narrowed_value_type, unary_op->operator.line);
                return;
            }
            break;
        }
        case EXPRESSION_ENTITY: {
            i32 native_call_index;
            if (get_native_function(analyzer, ast, expression->expr.entity.name, &native_call_index)) {
                OrsoNativeFunction* native_function_obj = (OrsoNativeFunction*)ast->folded_constants[native_call_index].as.p;
                expression->value_type = (OrsoType*)native_function_obj->type;
                expression->narrowed_value_type = (OrsoType*)native_function_obj->type;
            } else {
                OrsoScope* entity_scope;
                Entity* entity = get_resolved_entity_by_identifier(analyzer, ast, scope, fold_level, expression->expr.entity.name, NULL, &entity_scope);

                if (entity == NULL) {
                    error(analyzer, expression->expr.entity.name.line, "Entity does not exist.");
                    return;
                }

                expression->value_type = entity->declared_type;
                expression->narrowed_value_type = entity->narrowed_type;
            }
            break;
        }

        case EXPRESSION_ASSIGNMENT: {
            OrsoScope* entity_scope;
            Entity* entity = get_resolved_entity_by_identifier(analyzer, ast, scope, fold_level, expression->expr.assignment.name, NULL, &entity_scope);

            if (entity == NULL) {
                error(analyzer, expression->start.line, "Entity does not exist.");
                return;
            }

            orso_resolve_expression(analyzer, ast, scope, fold_level, expression->expr.assignment.right_side, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }
            if (analyzer->panic_mode) {
                return;
            }

            expression->value_type = expression->expr.assignment.right_side->value_type;
            expression->narrowed_value_type = expression->expr.assignment.right_side->narrowed_value_type;

            // no-commit: this needs to be used during the flow typing phase
            OrsoType* right_side_narrowed_type = expression->expr.assignment.right_side->narrowed_value_type;
            expression->value_type = entity->declared_type;
            
            if (!orso_type_fits(entity->declared_type, right_side_narrowed_type)) {
                error(analyzer, expression->start.line, "Expression needs explicit cast to store in variable.");
                return;
            }

            expression->narrowed_value_type = right_side_narrowed_type;

            if (ORSO_TYPE_IS_UNION(entity->declared_type)) {
                entity->narrowed_type = expression->narrowed_value_type;
            }
            break;
        }

        case EXPRESSION_BLOCK: {
            OrsoScope block_scope;
            scope_init(&block_scope, scope, expression);

            i32 declarations_count = sb_count(expression->expr.block.declarations);
            forward_scan_declaration_names(analyzer, &block_scope, expression->expr.block.declarations, declarations_count);

            resolve_declarations(analyzer, ast, &block_scope, fold_level, expression->expr.block.declarations, declarations_count);

            if (analyzer->panic_mode) {
                return;
            }

            // for (i32 i = 0; i < declarations_count; i++) {
            //     OrsoDeclarationNode* declaration = expression->expr.block.declarations[i];
            //     // What am I lookinig for here? type, name or subexpression
            //     ___resolve_declaration(analyzer, ast, &block_scope, declaration);
            // }

            OrsoDeclarationNode* last_expression_statement = NULL;
            if (declarations_count > 0) {
                OrsoDeclarationNode* last_declaration = expression->expr.block.declarations[declarations_count - 1];
                if (last_declaration->type == ORSO_DECLARATION_STATEMENT && last_declaration->decl.statement->type == ORSO_STATEMENT_EXPRESSION) {
                    last_expression_statement = last_declaration;
                }
            }

            if (last_expression_statement == NULL) {
                expression->value_type = &OrsoTypeVoid;
                expression->narrowed_value_type = &OrsoTypeVoid;
            } else {
                expression->expr.block.final_expression_statement = last_expression_statement;
                expression->value_type = last_expression_statement->decl.statement->stmt.expression->value_type;
                expression->narrowed_value_type = last_expression_statement->decl.statement->stmt.expression->narrowed_value_type;
            }

            scope_free(&block_scope);
            break;
        }

        case EXPRESSION_IFELSE: {
            orso_resolve_expression(analyzer, ast, scope, fold_level, expression->expr.ifelse.condition, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }

            OrsoScope* then_state = scope_copy_new(scope);

            orso_resolve_expression(analyzer, ast, then_state, fold_level, expression->expr.ifelse.then, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }

            OrsoScope* else_state = scope_copy_new(scope);
            if (expression->expr.ifelse.else_) {
                orso_resolve_expression(analyzer, ast, else_state, fold_level, expression->expr.ifelse.else_, mode);
                if (EXPRESSION_RESOLVED(expression)) {
                    return;
                }
            }

            scope_merge(&ast->type_set, scope, then_state, else_state);

            while (then_state) {
                OrsoScope* outer_state = then_state->outer;
                scope_free(then_state);
                free(then_state);

                then_state = outer_state;
            }

            while (else_state) {
                OrsoScope* outer_state = else_state->outer;
                scope_free(else_state);
                free(else_state);

                else_state = outer_state;
            }

            OrsoType* else_block_type = &OrsoTypeVoid;
            OrsoType* else_block_narrowed_type = else_block_type;
            if (expression->expr.ifelse.else_) {
                else_block_type = expression->expr.ifelse.else_->value_type;
                else_block_narrowed_type = expression->expr.ifelse.else_->narrowed_value_type;
            }

            expression->value_type = orso_type_merge(&ast->type_set,
                expression->expr.ifelse.then->value_type, else_block_type
            );

            if (expression->value_type == &OrsoTypeInvalid) {
                error(analyzer, expression->end.line, "if expression union type is too large.");
                return;
            }

            expression->narrowed_value_type = orso_type_merge(&ast->type_set,
                expression->expr.ifelse.then->narrowed_value_type, else_block_narrowed_type
            );
            break;
        }
        case EXPRESSION_CALL: {
            for (i32 i = 0; i < sb_count(expression->expr.call.arguments); i++) {
                OrsoExpressionNode* argument = expression->expr.call.arguments[i];
                orso_resolve_expression(analyzer, ast, scope, fold_level, argument, mode);
                if (EXPRESSION_RESOLVED(expression)) {
                    return;
                }

                if (analyzer->panic_mode) {
                    return;
                }
            }

            orso_resolve_expression(analyzer, ast, scope, fold_level, expression->expr.call.callee, mode);
            if (EXPRESSION_RESOLVED(expression)) {
                return;
            }

            if (analyzer->panic_mode) {
                return;
            }


            OrsoType* narrowed_callee_type = expression->expr.call.callee->narrowed_value_type;
            if ((narrowed_callee_type->kind != ORSO_TYPE_FUNCTION && narrowed_callee_type->kind != ORSO_TYPE_NATIVE_FUNCTION) || !can_call((OrsoFunctionType*)narrowed_callee_type, expression->expr.call.arguments)) {
                error(analyzer, expression->expr.call.callee->start.line, "Function does not exist.");
                return;
            }

            OrsoFunctionType* function_type = (OrsoFunctionType*)narrowed_callee_type;

            expression->value_type = function_type->return_type;
            expression->narrowed_value_type = function_type->return_type;
            break;
        }
        case EXPRESSION_FUNCTION_DEFINITION: {
            resolve_function_expression(analyzer, ast, scope, fold_level, expression, mode);
            break;
        }
        case EXPRESSION_FOR:
        case EXPRESSION_IMPLICIT_CAST:
        case EXPRESSION_NONE: UNREACHABLE();
    }

    fold_constants(analyzer, ast, scope, fold_level, expression, mode);
}

static OrsoType* resolve_type(OrsoStaticAnalyzer* analyzer, OrsoAST* ast, OrsoScope* scope, i32 fold_level, OrsoTypeNode* type_node) {
    ASSERT(type_node, "cannot be null");

    switch (type_node->type) {
        case ORSO_TYPE_NODE_TYPE_IDENTIFIER: {
            OrsoType* type = resolve_identifier_type(analyzer, ast, scope, fold_level, type_node->items.primitive);
            return type;
        }

        case ORSO_TYPE_NODE_TYPE_UNION: {
            if (sb_count(type_node->items.union_) > ORSO_UNION_NUM_MAX) {
                error(analyzer, type_node->start.line, "Orso only allows for a maximum of 4 types in a union.");
                return &OrsoTypeInvalid;
            }

            i32 type_count = 0;
            OrsoType* types[ORSO_UNION_NUM_MAX];
            for (i32 i = 0; i < sb_count(type_node->items.union_); i++) {
                OrsoType* single_type = resolve_type(analyzer, ast, scope, fold_level, type_node->items.union_[i]);
                if (single_type == &OrsoTypeInvalid) {
                    return &OrsoTypeInvalid;
                }

                types[type_count++] = single_type;
            }

            OrsoType* type = orso_type_set_fetch_union(&ast->type_set, types, type_count);
            return type;
        }

        case ORSO_TYPE_NODE_TYPE_FUNCTION: {
            OrsoType* return_type = resolve_type(analyzer, ast, scope, fold_level, type_node->items.function.return_type);
            if (return_type == &OrsoTypeInvalid) {
                error(analyzer, type_node->items.function.return_type->start.line, "Return type does not exist.");
                return &OrsoTypeInvalid;
            }

            i32 argument_count = sb_count(type_node->items.function.argument_types);
            OrsoType* arguments[argument_count];

            for (i32 i = 0; i < argument_count; i++) {
                OrsoType* argument = resolve_type(analyzer, ast, scope, fold_level, type_node->items.function.argument_types[i]);
                if (argument == &OrsoTypeInvalid) {
                    error(analyzer, type_node->items.function.argument_types[i]->start.line, "TODO: Make better error for this, argument type errors are handled in the resolve_type function");
                    return &OrsoTypeInvalid;
                }
            }

            OrsoType* type = orso_type_set_fetch_function(&ast->type_set, return_type, arguments, argument_count);
            return type;
        }
    }

    return &OrsoTypeInvalid;
}

static void declare_entity(OrsoStaticAnalyzer* analyzer, OrsoScope* scope, OrsoEntityDeclarationNode* entity) {
    OrsoSymbol* identifier = orso_unmanaged_symbol_from_cstrn(entity->name.start, entity->name.length, &analyzer->symbols);
    OrsoSlot slot_type_pair;
    if (orso_symbol_table_get(&scope->named_entities, identifier, &slot_type_pair)) {
        const char message[126];
        sprintf((char*)message, "Duplicate entity definition of '%.*s'.", entity->name.length, entity->name.start);
        error(analyzer, entity->name.line, (char*)message);
        return;
    }

    add_entity(scope, identifier, entity);
}

static bool is_declaration_resolved(OrsoEntityDeclarationNode* entity) {
    return (entity->implicit_default_value_index >= 0 || (entity->expression && entity->expression->value_type != &OrsoTypeUnresolved)) && entity->type != &OrsoTypeUnresolved;
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
 * unideal solution but allows me not break any recursion.
 * 
 */
static bool is_circular_dependency(OrsoStaticAnalyzer* analyzer, void* new_dependency) {
    for (i32 i = analyzer->dependencies.count - 1; i >= 0; i--) {
        OrsoAnalysisDependency* dependency = &analyzer->dependencies.chain[i];

        if (!dependency->is_type && ((OrsoExpressionNode*)dependency->ptr)->type == EXPRESSION_FUNCTION_DEFINITION) {
            return false;
        }

        if (dependency->ptr == new_dependency) {
            return true;
        }
    }

    return false;
}

static void push_dependency(OrsoStaticAnalyzer* analyzer, void* thing, bool is_type, int fold_level) {
    if (is_circular_dependency(analyzer, thing)) {
        i32 line;
        if (is_type) {
            line = ((OrsoTypeNode*)thing)->start.line;
        } else {
            line = ((OrsoExpressionNode*)thing)->start.line;
        }

        error(analyzer, line, "Circular dependency");
        return;
    }

    OrsoAnalysisDependency dependency = {
        .fold_level = fold_level,
        .is_type = is_type,
        .ptr = thing,
    };

    if (sb_count(analyzer->dependencies.chain) <= analyzer->dependencies.count) {
        sb_push(analyzer->dependencies.chain, dependency);
        analyzer->dependencies.count = sb_count(analyzer->dependencies.chain);
    } else {
        analyzer->dependencies.chain[analyzer->dependencies.count++] = dependency;
    }
}

static void pop_dependency(OrsoStaticAnalyzer* analyzer) {
    analyzer->dependencies.count--;
}

static void resolve_entity_declaration(OrsoStaticAnalyzer* analyzer, OrsoAST* ast, OrsoScope* scope, i32 fold_level, OrsoEntityDeclarationNode* entity_declaration) {
    OrsoType* declaration_type = &OrsoTypeUnresolved;
    if (entity_declaration->type == &OrsoTypeUnresolved && entity_declaration->type_node) {
        push_dependency(analyzer, entity_declaration->type_node, true, fold_level);
        declaration_type = resolve_type(analyzer, ast, scope, fold_level, entity_declaration->type_node);
        pop_dependency(analyzer);

        if (analyzer->panic_mode) {
            return;
        }
    }

    if (entity_declaration->expression != NULL && entity_declaration->expression->value_type == &OrsoTypeUnresolved) {
        ExpressionFoldingMode mode = entity_declaration->is_mutable ? MODE_RUNTIME : MODE_CONSTANT_TIME;

        push_dependency(analyzer, entity_declaration->expression, false, fold_level);
        orso_resolve_expression(analyzer, ast, scope, fold_level, entity_declaration->expression, mode);
        pop_dependency(analyzer);
    }

    // we are resolved
    if (is_declaration_resolved(entity_declaration)) {
        return;
    }

    // Could be resolved could be unresolved at this point.
    entity_declaration->type = declaration_type;

    entity_declaration->fold_level_resolved_at = fold_level;

    // TODO: Outer if should be if the expression is null or not
    if (!ORSO_TYPE_IS_UNION(entity_declaration->type)) {
        if (entity_declaration->type == &OrsoTypeUnresolved) {
            ASSERT(entity_declaration->expression != NULL, "this should be a parsing error.");

            OrsoType* expression_type = entity_declaration->is_mutable ?
                    entity_declaration->expression->value_type :
                    entity_declaration->expression->narrowed_value_type;

            if (entity_declaration->expression->value_type != &OrsoTypeBool
                    && orso_type_fits(&OrsoTypeInteger32, expression_type)) {
                entity_declaration->type = &OrsoTypeInteger32;
            } else {
                entity_declaration->type = expression_type;
            }
        } else {
            if (entity_declaration->expression != NULL && !orso_type_fits(entity_declaration->type, entity_declaration->expression->narrowed_value_type)) {
                error(analyzer, entity_declaration->start.line, "Must cast expression explicitly to match var type.");
            }
        }
    } else {
        if (entity_declaration->expression == NULL) {
            if (!orso_type_fits(entity_declaration->type, &OrsoTypeVoid)) {
                error(analyzer, entity_declaration->start.line, "Non-void union types must have a default value.");
            } 
        } else if (!orso_type_fits(entity_declaration->type, entity_declaration->expression->value_type)) {
            error(analyzer, entity_declaration->start.line, "Type mismatch between expression and declaration.");
        }
    }

    if (analyzer->panic_mode) {
        return;
    }

    OrsoSymbol* name = orso_unmanaged_symbol_from_cstrn(entity_declaration->name.start, entity_declaration->name.length, &analyzer->symbols);
    OrsoSlot entity_slot;

    ASSERT(orso_symbol_table_get(&scope->named_entities, name, &entity_slot), "should be forward_declared already");

    orso_symbol_table_get(&scope->named_entities, name, &entity_slot);

    Entity* entity = (Entity*)entity_slot.as.p;

    entity->declared_type = entity_declaration->type;
    entity->narrowed_type = entity_declaration->type;

    if (entity_declaration->expression == NULL) {
        if (ORSO_TYPE_IS_UNION(entity_declaration->type)) {
            entity->narrowed_type = &OrsoTypeVoid;
        } 

        OrsoSlot value = orso_zero_value(entity->declaration_node->type, &analyzer->symbols);

        // TODO: Optimize default values since they are always the same. Probably can use the same index for them. somehjow
        i32 value_index = add_value_to_ast_constant_stack(ast, &value, entity->declaration_node->type);
        entity->declaration_node->implicit_default_value_index = value_index;
    } else {
        entity->narrowed_type = entity_declaration->expression->narrowed_value_type;
    }
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

static Entity* get_resolved_entity_by_identifier(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        Token identifier_token,
        EntityQuery* query,
        OrsoScope** search_scope) {

    bool passed_local_mutable_access_barrier = false;

    OrsoSymbol* identifier = orso_unmanaged_symbol_from_cstrn(identifier_token.start, identifier_token.length, &analyzer->symbols);

    *search_scope = scope;
    OrsoSlot entity_slot;;

    while (*search_scope) {
        // TODO: search_scope->creator should never be null but right now it represents the global scope
        bool is_function_scope = (*search_scope)->creator && (*search_scope)->creator->type == EXPRESSION_FUNCTION_DEFINITION;

    #define NEXT_SCOPE() if (is_function_scope) { passed_local_mutable_access_barrier = true; } *search_scope = (*search_scope)->outer

        if (!orso_symbol_table_get(&(*search_scope)->named_entities, identifier, &entity_slot)) {
            NEXT_SCOPE();
            continue;
        }

        Entity* entity = (Entity*)entity_slot.as.p;

        if (query && query->skip_mutable && entity->declaration_node->is_mutable) {
            NEXT_SCOPE();
            continue;
        }

        if (passed_local_mutable_access_barrier && (*search_scope)->creator != NULL && entity->declaration_node->is_mutable) {
            NEXT_SCOPE();
            continue;
        }

        if (entity->declaration_node->is_mutable && !is_declaration_resolved(entity->declaration_node)) {
            ASSERT(entity->declaration_node->name.start <= identifier_token.start, "cannot think of a case when the declaration wont happen PRIOR to using it with mutables");
            NEXT_SCOPE();
            continue;
        }

        if (entity->declaration_node->expression) {
            resolve_entity_declaration(analyzer, ast, *search_scope, fold_level, entity->declaration_node);
        }

        if (entity->declaration_node->is_mutable && entity->declaration_node->fold_level_resolved_at != fold_level) {
            NEXT_SCOPE();
            continue;
        }

        if (entity->declaration_node->type->kind == ORSO_TYPE_FUNCTION) {
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

            OrsoFunction* function = (OrsoFunction*)ast->folded_constants[entity->declaration_node->expression->folded_value_index].as.p;
            for (i32 i = 0; i < analyzer->dependencies.count; i++) {
                i32 i_ = analyzer->dependencies.count - 1 - i;
                OrsoAnalysisDependency* dependency = &analyzer->dependencies.chain[i_];
                if (dependency->is_type) {
                    continue;
                }

                OrsoExpressionNode* expression = (OrsoExpressionNode*)dependency->ptr;
                if (expression->type == EXPRESSION_FUNCTION_DEFINITION) {
                    OrsoFunction* folded_function = (OrsoFunction*)ast->folded_constants[expression->folded_value_index].as.p;
                    if (folded_function == function && dependency->fold_level != fold_level) {
                        error(analyzer, expression->start.line, "Fold level circular dependency. TODO: show dependency chain.");
                        return NULL;
                    }
                }
            }
        }

        if (analyzer->panic_mode) {
            return NULL;
        }

        if (!query) {
            return entity;
        }

        if (query->type == ENTITY_QUERY_ANY) {
            return entity;
        }

        OrsoType* type = entity->declaration_node->type;

        if (query->type == ENTITY_QUERY_TYPE && query->search_type == type) {
            return entity;
        }

        if (query->type == ENTITY_QUERY_FUNCTION && type->kind == ORSO_TYPE_FUNCTION) {
            return entity;
        }

        NEXT_SCOPE();

#undef NEXT_SCOPE
    }

    return NULL;
}

static void resolve_function_expression(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoExpressionNode* function_definition_expression,
        ExpressionFoldingMode mode) {
    ASSERT(function_definition_expression->type == EXPRESSION_FUNCTION_DEFINITION, "must be function declaration at this point");

    mode = MODE_RUNTIME;

    OrsoFunctionDefinition* definition = &function_definition_expression->expr.function_definition;

    if (IS_FOLDED(function_definition_expression)) {
        return;
    }

    i32 parameter_count = sb_count(definition->parameters);
    // TODO: instead of hardcoding the number of parameters, instead use the numbers of bytes the params take up
    if (parameter_count > MAX_PARAMETERS - 1) {
        error(analyzer, definition->parameters[0]->name.line, "Orso only allows a maximum of 100 parameters");
    }

    OrsoScope function_scope;
    scope_init(&function_scope, scope, function_definition_expression);
    OrsoType* parameter_types[parameter_count];

    // forward declare parameters
    for (i32 i = 0; i < sb_count(definition->parameters); i++) {
        declare_entity(analyzer, &function_scope, definition->parameters[i]);
    }

    // Resolves parameters for function type
    for (i32 i = 0; i < parameter_count; i++) {
        resolve_entity_declaration(analyzer, ast, &function_scope, fold_level, definition->parameters[i]);
        parameter_types[i] = definition->parameters[i]->type;
    }

    OrsoType* return_type = &OrsoTypeVoid;
    if (/*function_definition_expression->value_type == &OrsoTypeUnresolved && */definition->return_type) {
        push_dependency(analyzer, definition->return_type, true, fold_level);
        return_type = resolve_type(analyzer, ast, scope, fold_level, definition->return_type);
        pop_dependency(analyzer);
    } else {
        if (function_definition_expression->value_type != &OrsoTypeUnresolved) {
            return_type = ((OrsoFunctionType*)function_definition_expression->value_type)->return_type;
        }
    }

    if (analyzer->panic_mode) {
        return;
    }

    OrsoFunctionType* function_type = (OrsoFunctionType*)orso_type_set_fetch_function(&ast->type_set, return_type, parameter_types, parameter_count);

    OrsoFunction* function = orso_new_function();
    function->type = function_type;

    sb_push(ast->function_definition_pairs, ((FunctionDefinitionPair){
        .ast_defintion = function_definition_expression,
        .function = function
    }));

    
    function_definition_expression->foldable = true;
    OrsoSlot function_slot_value = ORSO_SLOT_P(function, (OrsoType*)function_type);
    i32 function_constant_index = add_value_to_ast_constant_stack(ast, &function_slot_value, (OrsoType*)function_type);
    function_definition_expression->folded_value_index = function_constant_index;

    // TODO: Maybe use a marco defined for this file for setting both the value and type, maybe an inlined function
    function_definition_expression->value_type = (OrsoType*)function_type;
    function_definition_expression->narrowed_value_type = (OrsoType*)function_type;

    ASSERT(definition->block_expression->type == EXPRESSION_BLOCK, "must be block expression");

    push_dependency(analyzer, function_definition_expression, false, fold_level);
    orso_resolve_expression(analyzer, ast, &function_scope, fold_level, definition->block_expression, mode);
    pop_dependency(analyzer);

    scope_free(&function_scope);
}

static OrsoScope* get_closest_outer_function_scope(OrsoScope* scope) {
    while (scope && scope->creator && scope->creator->value_type->kind != ORSO_TYPE_FUNCTION) {
        scope = scope->outer;
    }

    return scope;
}

static void resolve_declaration(
        OrsoStaticAnalyzer* analyzer,
        OrsoAST* ast,
        OrsoScope* scope,
        i32 fold_level,
        OrsoDeclarationNode* declaration_node) {
    analyzer->panic_mode = false;

    // TODO: eventually this will be resolved, everything will be a declaration entity
    // there will only be two types: anonymous declarations and named declarations
    // print_expr, print and statement expressions will be all me anonymous declarations
    if (declaration_node->type != ORSO_DECLARATION_ENTITY) {
        switch (declaration_node->type) {
            case ORSO_DECLARATION_STATEMENT: {
                OrsoStatementNode* statement_node = declaration_node->decl.statement;
                switch (statement_node->type) {
                    case ORSO_STATEMENT_PRINT_EXPR:
                    case ORSO_STATEMENT_PRINT:
                    case ORSO_STATEMENT_EXPRESSION: {
                        orso_resolve_expression(analyzer, ast, scope, fold_level, declaration_node->decl.statement->stmt.expression, false);
                        break;
                    }
                    case ORSO_STATEMENT_RETURN: {
                        OrsoType* return_expression_type = &OrsoTypeVoid;

                        if (declaration_node->decl.statement->stmt.expression) {
                            orso_resolve_expression(analyzer, ast, scope, fold_level, declaration_node->decl.statement->stmt.expression, false);
                            return_expression_type = declaration_node->decl.statement->stmt.expression->value_type;
                        }

                        OrsoScope* function_scope = get_closest_outer_function_scope(scope);
                        ASSERT(function_scope, "right now all scopes should be under a function scope");

                        OrsoFunctionType* function_type = (OrsoFunctionType*)function_scope->creator->value_type;
                        OrsoType* function_return_type = function_scope ? function_type->return_type : &OrsoTypeVoid;
                        if (!orso_type_fits(function_return_type, return_expression_type)) {
                            error(analyzer, declaration_node->start.line, "Return expression must be compatible with function return type.");
                        }
                        break;
                    }
                    case ORSO_STATEMENT_NONE: UNREACHABLE();
                }

                analyzer->panic_mode = false;
                break;
            }

            case ORSO_DECLARATION_ENTITY:
            case ORSO_DECLARATION_NONE: UNREACHABLE();
        }
        return;
    }

    OrsoEntityDeclarationNode* entity_declaration = declaration_node->decl.entity;

    resolve_entity_declaration(analyzer, ast, scope, fold_level, entity_declaration);
}

void resolve_declarations(OrsoStaticAnalyzer* analyzer, OrsoAST* ast, OrsoScope* scope, i32 fold_level, OrsoDeclarationNode** declarations, i32 count) {
    for (i32 i = 0; i < count; i++) {
        OrsoDeclarationNode* declaration = declarations[i];
        // Constants are meant to be solved at compile time, which means they do not run during a particular state
        // of the program. Constants are only useful during runtime when things need to happen and they are accessed.
        // If they are never accessed they don't even need to be compiled into the running program.
        // By skipping them, I have a really convenient way telling which constants are being used or not.
        // That being said, I still might want to compile them if I'm making a library of some sort.
        // I can always go through them after... Like for example, right after this loop, I can just check
        // which constants in this scope are not being used and make a warning (for local constants), and
        // I can simple compile them if its a constant in the global scope (so they can be accessed).
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
        // Anyways, that's why constants are skipped and resolved on the fly. 
        if (declaration->type == ORSO_DECLARATION_ENTITY && !declaration->decl.entity->is_mutable) {
            continue;
        }

        resolve_declaration(analyzer, ast, scope, fold_level, declarations[i]);
    }
}

bool orso_resolve_ast(OrsoStaticAnalyzer* analyzer, OrsoAST* ast) {
    if (ast->declarations == NULL) {
        return true;
    }

    OrsoScope global_scope;
    scope_init(&global_scope, NULL, NULL);

    i32 declaration_count = sb_count(ast->declarations);
    forward_scan_declaration_names(analyzer, &global_scope, ast->declarations, declaration_count);
    resolve_declarations(analyzer, ast, &global_scope, 0, ast->declarations, declaration_count);

    scope_free(&global_scope);

    ast->resolved = !analyzer->had_error;

    return ast->resolved;
}

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoWriteFunction write_fn, OrsoErrorFunction error_fn) {
    analyzer->error_fn = error_fn;
    analyzer->had_error = false;
    analyzer->panic_mode = false;

    analyzer->dependencies.count = 0;
    analyzer->dependencies.chain = NULL;

    // TODO: fix
    (void)write_fn;

    orso_symbol_table_init(&analyzer->symbols);
}

void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer) {
    for (i32 i = 0; i < analyzer->symbols.capacity; i++) {
        OrsoSymbolTableEntry* entry = &analyzer->symbols.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        orso_unmanaged_symbol_free(entry->key);
    }

    orso_symbol_table_free(&analyzer->symbols);

    sb_free(analyzer->dependencies.chain);
    analyzer->dependencies.chain = NULL;
    analyzer->dependencies.count = 0;

    analyzer->error_fn = NULL;
    analyzer->had_error = false;
    analyzer->panic_mode = false;
}

OrsoSlot orso_zero_value(OrsoType* type, OrsoSymbolTable* symbol_table) {
    OrsoSlot slot;
    switch (type->kind) {
        case ORSO_TYPE_VOID:
        case ORSO_TYPE_BOOL:
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64:
            slot = ORSO_SLOT_I(0, type);
            break;
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64:
            slot = ORSO_SLOT_F(0.0, type);
            break;
        case ORSO_TYPE_STRING:
            slot = ORSO_SLOT_P(orso_new_string_from_cstrn("", 0), type);
            break;
        case ORSO_TYPE_SYMBOL:
            slot = ORSO_SLOT_P(orso_new_symbol_from_cstrn("", 0, symbol_table), type);
            break;
        case ORSO_TYPE_UNION: {
            ASSERT(orso_union_type_has_type((OrsoUnionType*)type, &OrsoTypeVoid), "must include void type if looking for zero value");
            slot = ORSO_SLOT_I(0, type);
            break;
        }
        case ORSO_TYPE_FUNCTION:
        case ORSO_TYPE_TYPE:
        default:
            UNREACHABLE();
            slot = ORSO_SLOT_I(0, &OrsoTypeInvalid);
            break;
    }

    return slot;
}
