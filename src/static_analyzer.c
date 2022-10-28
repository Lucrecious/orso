#include "static_analyzer.h"

#include <stdio.h>

#include "sb.h"

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

    const char message[100];
    char* msg = message;
    msg += sprintf(msg, "Incompatible Types: '%s' %.*s '%s'",
        orso_type_to_cstr(left), operation.length, operation.start, orso_type_to_cstr(right));

    error(analyzer, line, message);
}

static void error_incompatible_unary_type(OrsoStaticAnalyzer* analyzer, Token operation, OrsoType operand, i32 line) {
    const char message[100];
    char* msg = message;
    msg += sprintf(msg, "Incompatible Type: unary(%.*s) and type '%s'",
        operation.length, operation.start, orso_type_to_cstr(operand));

    error(analyzer, line, message);
}

static OrsoType orso_resolve_unary(TokenType operator, OrsoType operand) {
    if (operand == ORSO_TYPE_INVALID) {
        return ORSO_TYPE_INVALID;
    }

    if (operand == ORSO_TYPE_UNRESOLVED) {
        return ORSO_TYPE_UNRESOLVED;
    }

    switch (operator) {
        case TOKEN_MINUS: {
            if (orso_is_number_type(operand, false)) {
                return operand;
            } else {
                return ORSO_TYPE_INVALID;
            }
        }
        case TOKEN_NOT:
            return ORSO_TYPE_BOOL;
        default: return ORSO_TYPE_INVALID;
    }
}

static OrsoExpressionNode* implicit_cast(OrsoExpressionNode* parent, OrsoExpressionNode* operand, OrsoType value_type) {
    OrsoExpressionNode* implicit_cast = ALLOCATE(OrsoExpressionNode);
    implicit_cast->start = operand->start;
    implicit_cast->end = operand->end;
    implicit_cast->type = EXPRESSION_IMPLICIT_CAST;
    implicit_cast->value_type = value_type;
    implicit_cast->cast.operand = operand;

    return implicit_cast;
}

static OrsoType resolve_type_identifier(OrsoStaticAnalyzer* analyzer, Token identifier_type) {
    u32 hash = orso_hash_cstrn(identifier_type.start, identifier_type.length);
    OrsoSymbol* symbol = orso_symbol_table_find_cstrn(&analyzer->symbol_to_type, identifier_type.start, identifier_type.length, hash);
    if (symbol == NULL) {
        return ORSO_TYPE_INVALID;
    }

    OrsoSlot slot;
    orso_symbol_table_get(&analyzer->symbol_to_type, symbol, &slot);
    return slot.i;
}

void orso_resolve_expression(OrsoStaticAnalyzer* analyzer, OrsoExpressionNode* expression) {
    if (expression->value_type != ORSO_TYPE_UNRESOLVED) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            orso_resolve_expression(analyzer, expression->grouping.expression);
            expression->value_type = expression->grouping.expression->value_type;
            break;
        }
        case EXPRESSION_PRIMARY: {
            expression->value_type = ORSO_TYPE_INT64;
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
                    expression->value_type = ORSO_TYPE_BOOL;
                    break;
                }
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    orso_binary_equality_casts(left->value_type, right->value_type, &cast_left, &cast_right);
                    expression->value_type = ORSO_TYPE_BOOL;
                    break;
                }
            }

            if (cast_left == ORSO_TYPE_INVALID || cast_right == ORSO_TYPE_INVALID) {
                expression->value_type = ORSO_TYPE_INVALID;
                error_incompatible_binary_types(analyzer, expression->binary.operator, left->value_type, right->value_type, expression->binary.operator.line);
            } else {
                if (cast_left != left->value_type) {
                    expression->binary.left = implicit_cast(expression, left, cast_left);
                }

                if (cast_right != right->value_type) {
                    expression->binary.right = implicit_cast(expression, right, cast_right);
                }
            }
            break;
        }
        case EXPRESSION_UNARY: {
            OrsoUnaryOp* unary_op = &expression->unary;
            orso_resolve_expression(analyzer, unary_op->operand);
            expression->value_type = orso_resolve_unary(unary_op->operator.type, unary_op->operand->value_type);
            if (expression->value_type == ORSO_TYPE_INVALID) {
                error_incompatible_unary_type(analyzer, unary_op->operator, unary_op->operand->value_type, unary_op->operator.line);
            }
            break;
        }

        default: break; // unreachable
    }
}

static OrsoSlot zero_value(OrsoType type, OrsoSymbolTable* symbol_table) {
    OrsoSlot slot;
#ifdef DEBUG_TRACE_EXECUTION
    slot.type = type;
#endif
    switch (type) {
        case ORSO_TYPE_NULL:
        case ORSO_TYPE_BOOL:
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64:
            slot.i = 0;
            break;
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64:
            slot.f = 0.0;
            break;
        case ORSO_TYPE_STRING:
            slot.p = orso_new_string_from_cstrn("", 0);
            break;
        case ORSO_TYPE_SYMBOL:
            slot.p = orso_new_symbol_from_cstrn("", 0, symbol_table);
            break;
        default:
            slot.i = 0;
            break;
    }

    return slot;
}

static void resolve_var_declaration(OrsoStaticAnalyzer* analyzer, OrsoVarDeclarationNode* var_declaration) {
    bool is_new = orso_symbol_table_set(&analyzer->defined_variables, var_declaration->identifier, (OrsoSlot){
        .i = 0,
#ifdef DEBUG_TRACE_EXECUTION
        .type = ORSO_TYPE_NULL,
#endif
    });

    if (!is_new) {
        const char message[100];
        sprintf(message, "Duplicate variable definition of '%s'.", var_declaration->identifier->text);
        error(analyzer, var_declaration->start.line, message);
        return;
    }

    if (var_declaration->type_identifier.length != 0) {
        var_declaration->var_type = resolve_type_identifier(analyzer, var_declaration->type_identifier);
    }

    if (var_declaration->expression != NULL) {
        orso_resolve_expression(analyzer, var_declaration->expression);
    } else {
        OrsoSlot slot = zero_value(var_declaration->var_type, analyzer->vm_symbol_table);
        OrsoExpressionNode* zero = ALLOCATE(OrsoExpressionNode);
        zero->type = EXPRESSION_PRIMARY;
        zero->value_type = var_declaration->var_type;
        zero->primary.constant = slot;
        
        Token token = { .length = 0, .line = var_declaration->start.line };
        zero->start = token;
        zero->end = token;
        zero->primary.token = token;

        var_declaration->expression = zero;
    }

    switch (var_declaration->var_type) {
        case ORSO_TYPE_INVALID: {
            error(analyzer, var_declaration->start.length, "Type does not exist.");
            break;
        }
        case ORSO_TYPE_UNRESOLVED: {
            var_declaration->var_type = var_declaration->expression->value_type;
            break;
        }
        default: {
            if (var_declaration->var_type != var_declaration->expression->value_type) {
                error(analyzer, var_declaration->start.line, "Must cast expression explicitly to match var type.");
            }
            break;
        }
    }
}

static void resolve_declaration(OrsoStaticAnalyzer* analyzer, OrsoDeclarationNode* declaration_node) {
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
    OrsoSymbol* symbol_##CTYPE = orso_new_symbol_from_cstrn(#CTYPE, N, &analyzer->symbol_to_type); \
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
    ADD_TYPE(symbol, 6, ORSO_TYPE_STRING);

#undef ADD_TYPE
}

void orso_static_analyzer_init(OrsoStaticAnalyzer* analyzer, OrsoSymbolTable* vm_symbol_table, OrsoErrorFunction error_fn) {
    orso_symbol_table_init(&analyzer->symbol_to_type);
    add_builtin_types(analyzer);

    orso_symbol_table_init(&analyzer->defined_variables);

    analyzer->vm_symbol_table = vm_symbol_table;
    analyzer->error_fn = error_fn;
    analyzer->had_error = false;
    analyzer->panic_mode = false;
}

void orso_static_analyzer_free(OrsoStaticAnalyzer* analyzer) {
    orso_symbol_table_free(&analyzer->symbol_to_type);
    orso_symbol_table_free(&analyzer->defined_variables);

    analyzer->vm_symbol_table = NULL;
    analyzer->error_fn = NULL;
    analyzer->had_error = false;
    analyzer->panic_mode = false;
}