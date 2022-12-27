#include "static_analyzer.h"

#include <stdio.h>

#include "sb.h"
#include "mathutils.h"

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

static void error_incompatible_binary_types(OrsoStaticAnalyzer* analyzer, Token operation, OrsoType left, OrsoType right, i32 line) {

    const char message[256];
    char* msg = message;

    const char left_type_str[128];
    const char right_type_str[128];
    orso_type_to_cstr(left, left_type_str);
    orso_type_to_cstr(right, right_type_str);

    msg += sprintf(msg, "Incompatible Types: '%s' %.*s '%s'",
        left_type_str, operation.length, operation.start, right_type_str);

    error(analyzer, line, message);
}

static void error_incompatible_unary_type(OrsoStaticAnalyzer* analyzer, Token operation, OrsoType operand, i32 line) {
    const char message[256];
    char* msg = message;

    const char operand_type_str[128];
    orso_type_to_cstr(operand, operand_type_str);

    msg += sprintf(msg, "Incompatible Type: unary(%.*s) and type '%s'",
        operation.length, operation.start, operand_type_str);

    error(analyzer, line, message);
}

static OrsoType* duplicate_types(OrsoType* types) {
    OrsoType* duplicate = NULL;
    for (i32 i = 0; i < sb_count(types); i++) {
        sb_push(duplicate, types[i]);
    }

    return duplicate;
}

static OrsoType orso_resolve_unary(TokenType operator, OrsoType operand) {
    if (ORSO_TYPE_IS_UNION(operand)) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    if (operand.one == ORSO_TYPE_INVALID) {
        return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }

    if (operand.one == ORSO_TYPE_UNRESOLVED) {
        return ORSO_TYPE_ONE(ORSO_TYPE_UNRESOLVED);
    }

    switch (operator) {
        case TOKEN_MINUS: {
            if (orso_is_number_type_kind(operand.one, false)) {
                return operand;
            } else if (operand.one == ORSO_TYPE_BOOL) {
                return ORSO_TYPE_ONE(ORSO_TYPE_INT32);
            } else {
                return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
            }
        }
        case TOKEN_NOT:
            return ORSO_TYPE_ONE(ORSO_TYPE_BOOL);
        default: return ORSO_TYPE_ONE(ORSO_TYPE_INVALID);
    }
}

static OrsoExpressionNode* implicit_cast(OrsoExpressionNode* parent, OrsoExpressionNode* operand, OrsoType value_type) {
    OrsoExpressionNode* implicit_cast = ORSO_ALLOCATE(OrsoExpressionNode);

    implicit_cast->start = operand->start;
    implicit_cast->end = operand->end;
    implicit_cast->type = EXPRESSION_IMPLICIT_CAST;
    implicit_cast->value_type = value_type;
    implicit_cast->cast.operand = operand;

    return implicit_cast;
}

static OrsoTypeKind resolve_type_kind_identifier(OrsoStaticAnalyzer* analyzer, Token identifier_type) {
    u32 hash = orso_hash_cstrn(identifier_type.start, identifier_type.length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(&analyzer->symbol_to_type, identifier_type.start, identifier_type.length, hash);
    if (symbol == NULL) {
        return ORSO_TYPE_INVALID;
    }

    OrsoSlot slot;
    orso_symbol_table_get(&analyzer->symbol_to_type, symbol, &slot);
    return slot.u;
}

void orso_resolve_expression(OrsoStaticAnalyzer* analyzer, OrsoExpressionNode* expression) {
    if (expression->value_type.one != ORSO_TYPE_UNRESOLVED) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            orso_resolve_expression(analyzer, expression->grouping.expression);
            expression->value_type = expression->grouping.expression->value_type;
            break;
        }
        case EXPRESSION_PRIMARY: {
            expression->value_type.one = ORSO_TYPE_INVALID;
            break;
        }
        case EXPRESSION_BINARY: {
            OrsoExpressionNode* left = expression->binary.left;
            OrsoExpressionNode* right = expression->binary.right;
            orso_resolve_expression(analyzer, left);
            orso_resolve_expression(analyzer, right);

            OrsoType cast_left;
            OrsoType cast_right;

            switch (expression->binary.operator.type) {
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                case TOKEN_STAR:
                case TOKEN_SLASH: {
                    OrsoType combined_type = orso_binary_arithmetic_cast(left->value_type, right->value_type, expression->binary.operator.type);
                    expression->value_type = combined_type;

                    cast_left = combined_type;
                    cast_right = combined_type;
                    break;
                }

                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL: {
                    orso_binary_comparison_casts(left->value_type, right->value_type, &cast_left, &cast_right);
                    expression->value_type.one = ORSO_TYPE_BOOL;
                    break;
                }
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    orso_binary_equality_casts(left->value_type, right->value_type, &cast_left, &cast_right);
                    expression->value_type.one = ORSO_TYPE_BOOL;
                    break;
                }
            }

            if (cast_left.one == ORSO_TYPE_INVALID || cast_right.one == ORSO_TYPE_INVALID) {
                expression->value_type.one = ORSO_TYPE_INVALID;
                error_incompatible_binary_types(analyzer, expression->binary.operator, left->value_type, right->value_type, expression->binary.operator.line);
            } else {
                if (cast_left.one != left->value_type.one) {
                    expression->binary.left = implicit_cast(expression, left, cast_left);
                }

                if (cast_right.one != right->value_type.one) {
                    expression->binary.right = implicit_cast(expression, right, cast_right);
                }
            }
            break;
        }
        case EXPRESSION_UNARY: {
            OrsoUnaryOp* unary_op = &expression->unary;
            orso_resolve_expression(analyzer, unary_op->operand);
            expression->value_type = orso_resolve_unary(unary_op->operator.type, unary_op->operand->value_type);
            if (expression->value_type.one == ORSO_TYPE_INVALID) {
                error_incompatible_unary_type(analyzer, unary_op->operator, unary_op->operand->value_type, unary_op->operator.line);
            }
            break;
        }
        case EXPRESSION_VARIABLE: {
            OrsoSlot slot;
            OrsoSymbol* variable_name = orso_unmanaged_symbol_from_cstrn(expression->variable.name.start, expression->variable.name.length, &analyzer->symbols);
            if (!orso_symbol_table_get(&analyzer->defined_variables, variable_name, &slot)) {
                error(analyzer, expression->variable.name.line, "Variable does not exist.");
            } else {
                expression->value_type.one = slot.u;
            }
            break;
        }

        case EXPRESSION_ASSIGNMENT: {
            OrsoSlot type_slot;
            OrsoSymbol* variable_name = orso_unmanaged_symbol_from_cstrn(expression->assignment.variable_name.start, expression->assignment.variable_name.length, &analyzer->symbols);
            bool exist = orso_symbol_table_get(&analyzer->defined_variables, variable_name, &type_slot);
            if (!exist) {
                error(analyzer, expression->start.line, "Variable does not exist.");
            } else {
                OrsoType type = ORSO_TYPE_ONE(type_slot.u);

                orso_resolve_expression(analyzer, expression->assignment.right_side);
                expression->value_type = expression->assignment.right_side->value_type;
                
                if (!orso_type_fits(type, expression->value_type)) {
                    error(analyzer, expression->start.line, "Expression needs explicit cast to store in variable");
                }
            }
            break;
        }

        default: break; // unreachable
    }
}

static void resolve_var_declaration(OrsoStaticAnalyzer* analyzer, OrsoVarDeclarationNode* var_declaration) {
    if (sb_count(var_declaration->type_identifiers) >= ORSO_UNION_NUM_MAX) {
        error(analyzer, var_declaration->type_identifiers[0].line, "Orso only allows for a maximum of 4 types in a union.");
        return;
    }

    bool invalid_type_kind_exists = false;
    Token invalid_type_kind_identifier;
    OrsoType type = ORSO_TYPE_ONE(sb_count(var_declaration->type_identifiers) == 0 ? ORSO_TYPE_UNRESOLVED : ORSO_TYPE_INVALID);

    for (i32 i = 0; i < sb_count(var_declaration->type_identifiers); i++) {
        OrsoTypeKind type_kind = resolve_type_kind_identifier(analyzer, var_declaration->type_identifiers[i]);

        type.union_[i] = type_kind;

        if (type_kind != ORSO_TYPE_INVALID) {
            continue;
        }

        invalid_type_kind_exists = true;
        invalid_type_kind_identifier = var_declaration->type_identifiers[i];
        break;
    }

    if (invalid_type_kind_exists) {
        char error_message[256];
        sprintf(error_message, "Type %.*s does not exist.", invalid_type_kind_identifier.length, invalid_type_kind_identifier.start);
        error(analyzer, invalid_type_kind_identifier.line, error_message);
        return;
    }

    var_declaration->var_type = type;

    if (var_declaration->expression != NULL) {
        orso_resolve_expression(analyzer, var_declaration->expression);
    }

    if (ORSO_TYPE_IS_SINGLE(var_declaration->var_type)) {
        switch (var_declaration->var_type.one) {
            case ORSO_TYPE_INVALID: {
                error(analyzer, var_declaration->start.length, "Type does not exist.");
                break;
            }
            case ORSO_TYPE_UNRESOLVED: {
                // Should not be possible when expression is null because otherwise that's a
                // a parsing error ASSERT
                if (orso_integer_fit(ORSO_TYPE_ONE(ORSO_TYPE_INT32), var_declaration->expression->value_type, false)) {
                    var_declaration->var_type.one = ORSO_TYPE_INT32;
                } else {
                    var_declaration->var_type = var_declaration->expression->value_type;
                }
                break;
            }
            default: {
                if (var_declaration->expression != NULL && !orso_type_fits(var_declaration->var_type, var_declaration->expression->value_type)) {
                    error(analyzer, var_declaration->start.line, "Must cast expression explicitly to match var type.");
                }
                break;
            }
        }
    } else {
        if (var_declaration->expression == NULL) {
            error(analyzer, var_declaration->start.line, "Union types must have a default value.");
        } else if (!orso_type_fits(var_declaration->var_type, var_declaration->expression->value_type)) {
            error(analyzer, var_declaration->start.line, "Type mismatch between expression and declaration.");
        }
    }

    if (analyzer->had_error) {
        return;
    }

    OrsoSymbol* identifier = orso_unmanaged_symbol_from_cstrn(var_declaration->variable_name.start, var_declaration->variable_name.length, &analyzer->symbols);
    OrsoSlot slot_type;
    if (orso_symbol_table_get(&analyzer->defined_variables, identifier, &slot_type)) {
        const char message[100];
        sprintf(message, "Duplicate variable definition of '%.*s'.", var_declaration->variable_name.length, var_declaration->variable_name.start);
        error(analyzer, var_declaration->start.line, message);
        return;
    }

    orso_symbol_table_set(&analyzer->defined_variables, identifier, ORSO_SLOT_U(var_declaration->var_type.one, ORSO_TYPE_INVALID));
}

static void resolve_declaration(OrsoStaticAnalyzer* analyzer, OrsoDeclarationNode* declaration_node) {
    analyzer->panic_mode = false;
    switch (declaration_node->type) {
        case ORSO_DECLARATION_NONE: break;
        case ORSO_DECLARATION_STATEMENT: {
            OrsoStatementNode* statement_node = declaration_node->statement;
            switch (statement_node->type) {
                case ORSO_STATEMENT_NONE: break;
                case ORSO_STATEMENT_PRINT_EXPR:
                case ORSO_STATEMENT_EXPRESSION: {
                    orso_resolve_expression(analyzer, declaration_node->statement->expression);
                    break;
                }
            }

            analyzer->panic_mode = false;
            break;
        }
        case ORSO_DECLARATION_VAR: {
            resolve_var_declaration(analyzer, declaration_node->var);
            break;
        }
    }
}

bool orso_resolve_ast_types(OrsoStaticAnalyzer* analyzer, OrsoAST* ast) {
    if (ast->declarations == NULL) {
        return true;
    }

    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        resolve_declaration(analyzer, ast->declarations[i]);
    }

    return !analyzer->had_error;
}

static add_builtin_types(OrsoStaticAnalyzer* analyzer) {
#define ADD_TYPE(CTYPE, N, ORSO_TYPE) do { \
    OrsoSymbol* symbol_##CTYPE = orso_unmanaged_symbol_from_cstrn(#CTYPE, N, &analyzer->symbols); \
    OrsoSlot slot = { .i = ORSO_TYPE }; \
    orso_symbol_table_set(&analyzer->symbol_to_type, symbol_##CTYPE, slot); \
    } while (false)

    ADD_TYPE(void, 4, ORSO_TYPE_NULL);
    ADD_TYPE(bool, 4, ORSO_TYPE_BOOL);
    ADD_TYPE(i32, 3, ORSO_TYPE_INT32);
    ADD_TYPE(i64, 3, ORSO_TYPE_INT64);
    ADD_TYPE(f32, 3, ORSO_TYPE_FLOAT32);
    ADD_TYPE(f64, 3, ORSO_TYPE_FLOAT64);
    ADD_TYPE(string, 6, ORSO_TYPE_STRING);
    ADD_TYPE(symbol, 6, ORSO_TYPE_SYMBOL);

#undef ADD_TYPE
}

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoGarbageCollector* gc, OrsoSymbolTable* vm_symbol_table, OrsoErrorFunction error_fn) {
    analyzer->gc = gc;
    analyzer->error_fn = error_fn;
    analyzer->had_error = false;
    analyzer->panic_mode = false;

    orso_symbol_table_init(&analyzer->symbols);

    orso_symbol_table_init(&analyzer->symbol_to_type);
    add_builtin_types(analyzer);

    orso_symbol_table_init(&analyzer->defined_variables);

}

void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer) {
    orso_symbol_table_free(&analyzer->symbol_to_type);
    orso_symbol_table_free(&analyzer->defined_variables);

    for (i32 i = 0; i < analyzer->symbols.capacity; i++) {
        OrsoSymbolTableEntry* entry = &analyzer->symbols.entries[i];
        if (entry->key == NULL) {
            continue;
        }

        orso_unmanaged_symbol_free(entry->key);
    }
    orso_symbol_table_free(&analyzer->symbols);

    analyzer->error_fn = NULL;
    analyzer->had_error = false;
    analyzer->panic_mode = false;
}