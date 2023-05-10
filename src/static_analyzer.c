#include "static_analyzer.h"

#include <stdio.h>

#include "sb.h"
#include "mathutils.h"
#include "type_set.h"

typedef OrsoSymbolTable SymbolTable;

typedef struct OrsoTypePair {
    OrsoType* declared;
    OrsoType* narrowed;
} OrsoTypePair;

static void declared_state_init(OrsoDeclaredState* state) {
    state->outer = NULL;
    orso_symbol_table_init(&state->scope);
}

static void declared_state_free(OrsoDeclaredState* state) {
    state->outer = NULL;
    for (i32 i = 0; i < state->scope.capacity; i++) {
        OrsoSymbolTableEntry* entry = &state->scope.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        OrsoTypePair* pair = (OrsoTypePair*)entry->value.as.p;
        free(pair);
    }
    orso_symbol_table_free(&state->scope);
}

static OrsoDeclaredState* declared_state_copy_new(OrsoDeclaredState* state) {
    if (state == NULL) {
        return NULL;
    }

    OrsoDeclaredState* state_copy = ORSO_ALLOCATE(OrsoDeclaredState);
    declared_state_init(state_copy);
    orso_symbol_table_add_all(&state->scope, &state_copy->scope);

    for (i32 i = 0; i < state->scope.capacity; i++) {
        OrsoSymbolTableEntry* entry = &state->scope.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        OrsoTypePair* pair_copy = ORSO_ALLOCATE(OrsoTypePair);
        *pair_copy = *((OrsoTypePair*)entry->value.as.p);
        entry->value = ORSO_SLOT_P(pair_copy, &OrsoTypeType);
    }

    state_copy->outer = declared_state_copy_new(state->outer);

    return state_copy;
}

static void declared_state_merge(OrsoTypeSet* set, OrsoDeclaredState* state, OrsoDeclaredState* a, OrsoDeclaredState* b) {
    if (state == NULL) {
        ASSERT(a == NULL && b == NULL, "the alternative states must also be null");
        return;
    }

    for (i32 i = 0; i < state->scope.capacity; i++) {
        OrsoSymbolTableEntry* entry = &state->scope.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        OrsoSlot type_pair_a_slot;
        OrsoSlot type_pair_b_slot;
        if (!orso_symbol_table_get(&a->scope, entry->key, &type_pair_a_slot)
                || !orso_symbol_table_get(&b->scope, entry->key, &type_pair_b_slot)) {
            continue;
        }

        OrsoTypePair* type_pair_a = (OrsoTypePair*)type_pair_a_slot.as.p;
        OrsoTypePair* type_pair_b = (OrsoTypePair*)type_pair_b_slot.as.p;

        OrsoType* anded_narrowed = orso_type_merge(set, type_pair_a->narrowed, type_pair_b->narrowed);

        OrsoTypePair* state_type_pair = (OrsoTypePair*)entry->value.as.p;

        state_type_pair->narrowed = anded_narrowed;
    }

    declared_state_merge(set, state->outer, a->outer, b->outer);
}

static void add_type_pair(OrsoDeclaredState* state, OrsoSymbol* identifier, OrsoType* declared, OrsoType* narrowed) {
    OrsoTypePair* type_pair = ORSO_ALLOCATE(OrsoTypePair);
    type_pair->declared = declared;
    type_pair->narrowed = narrowed;

    orso_symbol_table_set(&state->scope, identifier, ORSO_SLOT_P(type_pair, &OrsoTypeVoid));
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

static OrsoType* orso_resolve_unary(TokenType operator, OrsoType* operand) {
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
    implicit_cast->narrowed_value_type = implicit_cast->value_type;
    implicit_cast->expr.cast.operand = operand;

    return implicit_cast;
}

static OrsoType* resolve_primitive_type(OrsoStaticAnalyzer* analyzer, Token identifier_type) {
    u32 hash = orso_hash_cstrn(identifier_type.start, identifier_type.length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(&analyzer->symbol_to_type, identifier_type.start, identifier_type.length, hash);
    if (symbol == NULL) {
        char tmp_buffer[512];
        sprintf(tmp_buffer, "Type %.*s does not exist.", identifier_type.length, identifier_type.start);
        error(analyzer, identifier_type.line, tmp_buffer);
        return &OrsoTypeInvalid;
    }

    OrsoSlot slot;
    orso_symbol_table_get(&analyzer->symbol_to_type, symbol, &slot);
    return (OrsoType*)slot.as.p;
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

static void resolve_declaration(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoDeclarationNode* declaration_node);
static void forward_declare_functions(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoDeclarationNode** declarations);

void orso_resolve_expression(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoExpressionNode* expression) {
    if (expression->value_type != &OrsoTypeUnresolved) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            orso_resolve_expression(analyzer, state, expression->expr.grouping.expression);
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
            orso_resolve_expression(analyzer, state, left);
            orso_resolve_expression(analyzer, state, right);

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

                    OrsoType* merged_type = orso_type_merge(&analyzer->type_set, left->value_type, right->value_type);
                    if (merged_type == &OrsoTypeInvalid) {
                        error(analyzer, expression->expr.binary.operator.line, "too many types in union for logical operations.");
                    } else {
                        expression->narrowed_value_type = orso_type_merge(&analyzer->type_set, left->narrowed_value_type, right->narrowed_value_type);
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
            } else {
                if (!is_logical_operator) {
                    if (cast_left != left->narrowed_value_type) {
                        expression->expr.binary.left = implicit_cast(left, cast_left);
                    }

                    if (cast_right != right->narrowed_value_type) {
                        expression->expr.binary.right = implicit_cast(right, cast_right);
                    }
                }
            }
            break;
        }
        case EXPRESSION_UNARY: {
            OrsoUnaryOp* unary_op = &expression->expr.unary;
            orso_resolve_expression(analyzer, state, unary_op->operand);

            OrsoType* new_type = orso_resolve_unary(unary_op->operator.type, unary_op->operand->narrowed_value_type);
            expression->value_type = new_type;
            expression->narrowed_value_type = new_type;
            
            // TODO: Must negate the new type implications if the unary operation is NOT

            if (expression->narrowed_value_type == &OrsoTypeInvalid) {
                error_incompatible_unary_type(analyzer, unary_op->operator, unary_op->operand->narrowed_value_type, unary_op->operator.line);
            }
            break;
        }
        case EXPRESSION_VARIABLE: {
            bool exists = false;
            OrsoSymbol* variable_name = orso_unmanaged_symbol_from_cstrn(expression->expr.variable.name.start, expression->expr.variable.name.length, &analyzer->symbols);
            OrsoSlot type_pair_slot;
            {
                OrsoDeclaredState* current_scope = state;
                while (current_scope) {
                    if ((exists = orso_symbol_table_get(&current_scope->scope, variable_name, &type_pair_slot))) {
                        break;
                    }

                    current_scope = current_scope->outer;
                }
            }

            if (!exists) {
                error(analyzer, expression->expr.variable.name.line, "Variable does not exist.");
            } else {
                OrsoTypePair* type_pair = (OrsoTypePair*)type_pair_slot.as.p;
                expression->value_type = type_pair->declared;
                expression->narrowed_value_type = type_pair->narrowed;
            }
            break;
        }

        case EXPRESSION_ASSIGNMENT: {
            bool exists = false;
            OrsoSymbol* variable_name = orso_unmanaged_symbol_from_cstrn(expression->expr.assignment.name.start, expression->expr.assignment.name.length, &analyzer->symbols);
            OrsoSlot type_pair_slot;
            {
                OrsoDeclaredState* current_scope = state;
                while (current_scope) {
                    if ((exists = orso_symbol_table_get(&current_scope->scope, variable_name, &type_pair_slot))) {
                        break;
                    }

                    current_scope = current_scope->outer;
                }
            }

            if (!exists) {
                error(analyzer, expression->start.line, "Variable does not exist.");
            } else {
                OrsoTypePair* type_pair = (OrsoTypePair*)type_pair_slot.as.p;

                orso_resolve_expression(analyzer, state, expression->expr.assignment.right_side);
                OrsoType* right_side_narrowed_type = expression->expr.assignment.right_side->narrowed_value_type;
                expression->value_type = type_pair->declared;
                expression->narrowed_value_type = type_pair->declared;
                
                if (!orso_type_fits(type_pair->declared, right_side_narrowed_type)) {
                    error(analyzer, expression->start.line, "Expression needs explicit cast to store in variable.");
                } else {
                    expression->narrowed_value_type = right_side_narrowed_type;
                }

                if (ORSO_TYPE_IS_UNION(type_pair->declared)) {
                    type_pair->narrowed = expression->narrowed_value_type;
                }
            }
            break;
        }

        case EXPRESSION_BLOCK: {
            OrsoDeclaredState block_state;
            declared_state_init(&block_state);

            block_state.outer = state;

            forward_declare_functions(analyzer, &block_state, expression->expr.block.declarations);

            i32 declarations_count = sb_count(expression->expr.block.declarations);
            for (i32 i = 0; i < declarations_count; i++) {
                OrsoDeclarationNode* declaration = expression->expr.block.declarations[i];
                resolve_declaration(analyzer, &block_state, declaration);
            }

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

            declared_state_free(&block_state);
            break;
        }

        case EXPRESSION_IFELSE: {
            orso_resolve_expression(analyzer, state, expression->expr.ifelse.condition);

            OrsoDeclaredState* then_state = declared_state_copy_new(state);

            orso_resolve_expression(analyzer, then_state, expression->expr.ifelse.then);

            OrsoDeclaredState* else_state = declared_state_copy_new(state);
            if (expression->expr.ifelse.else_) {
                orso_resolve_expression(analyzer, else_state, expression->expr.ifelse.else_);
            }

            declared_state_merge(&analyzer->type_set, state, then_state, else_state);

            while (then_state) {
                OrsoDeclaredState* outer_state = then_state->outer;
                declared_state_free(then_state);
                free(then_state);

                then_state = outer_state;
            }

            while (else_state) {
                OrsoDeclaredState* outer_state = else_state->outer;
                declared_state_free(else_state);
                free(else_state);

                else_state = outer_state;
            }

            OrsoType* else_block_type = &OrsoTypeVoid;
            OrsoType* else_block_narrowed_type = else_block_type;
            if (expression->expr.ifelse.else_) {
                else_block_type = expression->expr.ifelse.else_->value_type;
                else_block_narrowed_type = expression->expr.ifelse.else_->narrowed_value_type;
            }

            expression->value_type = orso_type_merge(&analyzer->type_set,
                expression->expr.ifelse.then->value_type, else_block_type
            );

            if (expression->value_type == &OrsoTypeInvalid) {
                error(analyzer, expression->end.line, "if expression union type is too large.");
            }

            expression->narrowed_value_type = orso_type_merge(&analyzer->type_set,
                expression->expr.ifelse.then->narrowed_value_type, else_block_narrowed_type
            );
            break;
        }
        case EXPRESSION_CALL: {
            Token* callee = &expression->expr.call.callee;
            OrsoSymbol* function_name = orso_unmanaged_symbol_from_cstrn(callee->start, callee->length, &analyzer->symbols);
            OrsoType* callee_type = NULL;
            OrsoFunctionType* function_type = NULL;

            for (i32 i = 0; i < sb_count(expression->expr.call.arguments); i++) {
                OrsoExpressionNode* argument = expression->expr.call.arguments[i];
                orso_resolve_expression(analyzer, state, argument);
            }

            OrsoDeclaredState* current_scope = state;
            while (current_scope) {
                OrsoSlot slot;
                if (orso_symbol_table_get(&current_scope->scope, function_name, &slot)) {
                    OrsoTypePair* type_pair = (OrsoTypePair*)slot.as.p;
                    if (ORSO_TYPE_IS_UNION(type_pair->narrowed)) {
                        error(analyzer, expression->expr.call.callee.line, "Cannot call unnarrowed union type.");
                        return;
                    }

                    if (type_pair->narrowed->kind != ORSO_TYPE_FUNCTION) {
                        error(analyzer, expression->expr.call.callee.line, "Cannot call non-function type.");
                        return;
                    }

                    if (can_call((OrsoFunctionType*)type_pair->narrowed, expression->expr.call.arguments)) {
                        callee_type = type_pair->declared;
                        function_type = (OrsoFunctionType*)type_pair->narrowed;
                        break;
                    }
                }

                current_scope = current_scope->outer;
            }

            if (!callee_type) {
                error(analyzer, expression->expr.call.callee.line, "Function does not exist.");
                return;
            }

            expression->value_type = function_type->return_type;
            expression->narrowed_value_type = expression->value_type;
            expression->expr.call.callee_type = callee_type;
            expression->expr.call.callee_function_type = function_type;
            break;
        }
        case EXPRESSION_FOR:
        case EXPRESSION_IMPLICIT_CAST:
        case EXPRESSION_NONE: UNREACHABLE();
    }
}

static OrsoType* resolve_type(OrsoStaticAnalyzer* analyzer, OrsoTypeNode* type_node) {
    if (!type_node) {
        return &OrsoTypeUnresolved;
    }

    switch (type_node->type) {
        case ORSO_TYPE_NODE_TYPE_PRIMITIVE: {
            OrsoType* type = resolve_primitive_type(analyzer, type_node->items.primitive);
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
                OrsoType* single_type = resolve_type(analyzer, type_node->items.union_[i]);
                if (single_type == &OrsoTypeInvalid) {
                    return &OrsoTypeInvalid;
                }

                types[type_count++] = single_type;
            }

            OrsoType* type = orso_type_set_fetch_union(&analyzer->type_set, types, type_count);
            return type;
        }

        case ORSO_TYPE_NODE_TYPE_FUNCTION: {
            OrsoType* return_type = resolve_type(analyzer, type_node->items.function.return_type);
            if (return_type == &OrsoTypeInvalid) {
                error(analyzer, type_node->items.function.return_type->start.line, "Return type does not exist.");
                return &OrsoTypeInvalid;
            }

            i32 argument_count = sb_count(type_node->items.function.argument_types);
            OrsoType* arguments[argument_count];

            for (i32 i = 0; i < argument_count; i++) {
                OrsoType* argument = resolve_type(analyzer, type_node->items.function.argument_types[i]);
                if (argument == &OrsoTypeInvalid) {
                    error(analyzer, type_node->items.function.argument_types[i]->start.line, "Argument type does not exist.");
                    return &OrsoTypeInvalid;
                }
            }

            OrsoType* type = orso_type_set_fetch_function(&analyzer->type_set, return_type, arguments, argument_count);
            return type;
        }
    }

    return &OrsoTypeInvalid;
}

static void declare_variable(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoVariableDeclarationNode* variable) {
    ASSERT(variable->type != &OrsoTypeInvalid, "Variable being declared must have valid type.");
    ASSERT(variable->type != &OrsoTypeUnresolved, "Variable being declared must have resolved type.");

    OrsoType* narrowed_type = variable->type;
    if (ORSO_TYPE_IS_UNION(variable->type)) {
        if (variable->expression) {
            narrowed_type = variable->expression->narrowed_value_type;
        } else {
            ASSERT(orso_type_fits(variable->type, &OrsoTypeVoid), "must contain void type here.");
            narrowed_type = &OrsoTypeVoid;
        }
    }

    OrsoSymbol* identifier = orso_unmanaged_symbol_from_cstrn(variable->name.start, variable->name.length, &analyzer->symbols);
    OrsoSlot slot_type_pair;
    if (orso_symbol_table_get(&state->scope, identifier, &slot_type_pair)) {
        const char message[126];
        sprintf((char*)message, "Duplicate variable definition of '%.*s'.", variable->name.length, variable->name.start);
        error(analyzer, variable->name.line, (char*)message);
        return;
    }

    add_type_pair(state, identifier, variable->type, narrowed_type);
    //orso_symbol_table_set(&state->scope, identifier, ORSO_SLOT_P(variable->type, &OrsoTypeType));
}

static void declare_function(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, Token* name, OrsoFunctionDeclarationNode* function) {
    OrsoSymbol* identifier = orso_unmanaged_symbol_from_cstrn(name->start, name->length, &analyzer->symbols);
    OrsoSlot slot;

    if (orso_symbol_table_get(&state->scope, identifier, &slot)) {
        error(analyzer, function->start.line, "Cannot create function overloads yet.");
        return;
    }

    add_type_pair(state, identifier, (OrsoType*)function->type, (OrsoType*)function->type);
    //orso_symbol_table_set(&state->scope, identifier, ORSO_SLOT_P(function->type, &OrsoTypeType));
}

static void resolve_variable_declaration_type(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoVariableDeclarationNode* variable_declaration) {
    OrsoType* type = resolve_type(analyzer, variable_declaration->type_node);
    if (analyzer->had_error) {
        return;
    }

    variable_declaration->type = type;

    if (variable_declaration->expression != NULL) {
        orso_resolve_expression(analyzer, state, variable_declaration->expression);
    }

    if (!ORSO_TYPE_IS_UNION(variable_declaration->type)) {
        if (variable_declaration->type == &OrsoTypeUnresolved) {
            ASSERT(variable_declaration->expression != NULL, "this should be a parsing error.");
            if (variable_declaration->expression->value_type != &OrsoTypeBool
                    && orso_type_fits(&OrsoTypeInteger32, variable_declaration->expression->value_type)) {
                variable_declaration->type = &OrsoTypeInteger32;
            } else {
                variable_declaration->type = variable_declaration->expression->value_type;
            }
        } else {
            if (variable_declaration->expression != NULL && !orso_type_fits(variable_declaration->type, variable_declaration->expression->narrowed_value_type)) {
                error(analyzer, variable_declaration->start.line, "Must cast expression explicitly to match var type.");
            }
        }
    } else {
        if (variable_declaration->expression == NULL) {
            if (!orso_type_fits(variable_declaration->type, &OrsoTypeVoid)) {
                error(analyzer, variable_declaration->start.line, "Non-void union types must have a default value.");
            } 
        } else if (!orso_type_fits(variable_declaration->type, variable_declaration->expression->value_type)) {
            error(analyzer, variable_declaration->start.line, "Type mismatch between expression and declaration.");
        }
    }
}

void resolve_function_declaration(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoFunctionDeclarationNode* function) {
    i32 parameter_count = sb_count(function->parameters);
    OrsoType* parameter_types[parameter_count];

    for (i32 i = 0; i < parameter_count; i++) {
        resolve_variable_declaration_type(analyzer, state, function->parameters[i]);
        parameter_types[i] = function->parameters[i]->type;
    }

    OrsoType* return_type = resolve_type(analyzer, function->return_type);

    if (analyzer->had_error) {
        return;
    }

    if (return_type == &OrsoTypeUnresolved) {
        return_type = &OrsoTypeVoid;
    }

    function->type = (OrsoFunctionType*)orso_type_set_fetch_function(&analyzer->type_set, return_type, parameter_types, parameter_count);

    declare_function(analyzer, state, &function->name, function);
}

void forward_declare_functions(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoDeclarationNode** declarations) {
    for (i32 i = 0; i < sb_count(declarations); i++) {
        OrsoDeclarationNode* declaration = declarations[i];
        if (declaration->type != ORSO_DECLARATION_FUNCTION) {
            continue;
        }

        OrsoFunctionDeclarationNode* function = declaration->decl.function;
        resolve_function_declaration(analyzer, state, function);
    }
}

static void resolve_function_definition(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoFunctionDeclarationNode* function_declaration) {

    if (sb_count(function_declaration->parameters) > MAX_PARAMETERS - 1) {
        error(analyzer, function_declaration->parameters[0]->name.line, "Orso only allows a maximum of 100 parameters");
        return;
    }

    // I basically pop off all the defined variables above the outermost scope
    // and same with the inferences. Then I just pass those into the resolve functions
    {
        OrsoFunctionDeclarationNode* outer_function = analyzer->function;
        analyzer->function = function_declaration;

        OrsoDeclaredState* global_state = state;
        while(global_state->outer) {
            global_state = global_state->outer;
        }

        OrsoDeclaredState function_state;
        declared_state_init(&function_state);
        function_state.outer = global_state;

        declare_function(analyzer, &function_state, &function_declaration->name, function_declaration);

        for (i32 i = 0; i < sb_count(function_declaration->parameters); i++) {
            declare_variable(analyzer, &function_state, function_declaration->parameters[i]);
        }

        forward_declare_functions(analyzer, &function_state, function_declaration->block.declarations);

        for (i32 i = 0; i < sb_count(function_declaration->block.declarations); i++) {
            resolve_declaration(analyzer, &function_state, function_declaration->block.declarations[i]);
        }

        declared_state_free(&function_state);

        analyzer->function = outer_function;

        if (analyzer->had_error) {
            return;
        }
    }
}

static void resolve_declaration(OrsoStaticAnalyzer* analyzer, OrsoDeclaredState* state, OrsoDeclarationNode* declaration_node) {
    analyzer->panic_mode = false;
    switch (declaration_node->type) {
        case ORSO_DECLARATION_STATEMENT: {
            OrsoStatementNode* statement_node = declaration_node->decl.statement;
            switch (statement_node->type) {
                case ORSO_STATEMENT_PRINT_EXPR:
                case ORSO_STATEMENT_PRINT:
                case ORSO_STATEMENT_EXPRESSION: {
                    OrsoSymbolTable type_implications;
                    orso_symbol_table_init(&type_implications);

                    orso_resolve_expression(analyzer, state, declaration_node->decl.statement->stmt.expression);

                    orso_symbol_table_free(&type_implications);
                    break;
                }
                case ORSO_STATEMENT_RETURN: {
                    OrsoSymbolTable type_implications;
                    OrsoType* return_expression_type = &OrsoTypeVoid;

                    if (declaration_node->decl.statement->stmt.expression) {
                        orso_symbol_table_init(&type_implications);
                        orso_resolve_expression(analyzer, state, declaration_node->decl.statement->stmt.expression);
                        return_expression_type = declaration_node->decl.statement->stmt.expression->value_type;
                        orso_symbol_table_free(&type_implications);
                    }

                    OrsoType* function_return_type = analyzer->function ?
                            analyzer->function->type->return_type : &OrsoTypeVoid;
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
        case ORSO_DECLARATION_VAR: {
            resolve_variable_declaration_type(analyzer, state, declaration_node->decl.variable);
            if (analyzer->had_error) {
                break;
            }

            declare_variable(analyzer, state, declaration_node->decl.variable);
            break;
        }
        case ORSO_DECLARATION_FUNCTION: {
            resolve_function_definition(analyzer, state, declaration_node->decl.function);
            break;
        }
        case ORSO_DECLARATION_NONE: UNREACHABLE();
    }
}

bool orso_resolve_ast_types(OrsoStaticAnalyzer* analyzer, OrsoAST* ast) {
    if (ast->declarations == NULL) {
        return true;
    }

    OrsoDeclaredState global_declared_state;
    declared_state_init(&global_declared_state);

    forward_declare_functions(analyzer, &global_declared_state, ast->declarations);

    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        resolve_declaration(analyzer, &global_declared_state, ast->declarations[i]);
    }

    declared_state_free(&global_declared_state);

    return !analyzer->had_error;
}

static void add_builtin_types(OrsoStaticAnalyzer* analyzer) {
#define ADD_TYPE(CTYPE, N, ORSO_TYPE) do { \
    OrsoSymbol* symbol_##CTYPE = orso_unmanaged_symbol_from_cstrn(#CTYPE, N, &analyzer->symbols); \
    OrsoSlot slot = ORSO_SLOT_P(ORSO_TYPE, &OrsoTypeType); \
    orso_symbol_table_set(&analyzer->symbol_to_type, symbol_##CTYPE, slot); \
    } while (false)

    ADD_TYPE(void, 4, &OrsoTypeVoid);
    ADD_TYPE(bool, 4, &OrsoTypeBool);
    ADD_TYPE(i32, 3, &OrsoTypeInteger32);
    ADD_TYPE(i64, 3, &OrsoTypeInteger64);
    ADD_TYPE(f32, 3, &OrsoTypeFloat32);
    ADD_TYPE(f64, 3, &OrsoTypeFloat64);
    ADD_TYPE(string, 6, &OrsoTypeString);
    ADD_TYPE(symbol, 6, &OrsoTypeSymbol);

#undef ADD_TYPE
}

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoErrorFunction error_fn) {
    analyzer->function = NULL;
    analyzer->error_fn = error_fn;
    analyzer->had_error = false;
    analyzer->panic_mode = false;

    orso_symbol_table_init(&analyzer->symbols);

    orso_symbol_table_init(&analyzer->symbol_to_type);
    add_builtin_types(analyzer);

    orso_type_set_init(&analyzer->type_set);
}

void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer) {
    for (i32 i = 0; i < analyzer->symbols.capacity; i++) {
        OrsoSymbolTableEntry* entry = &analyzer->symbols.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        orso_unmanaged_symbol_free(entry->key);
    }

    orso_symbol_table_free(&analyzer->symbol_to_type);
    orso_symbol_table_free(&analyzer->symbols);

    orso_type_set_free(&analyzer->type_set);

    analyzer->error_fn = NULL;
    analyzer->had_error = false;
    analyzer->panic_mode = false;
}
