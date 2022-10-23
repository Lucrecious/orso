#include "static_analyzer.h"

static OrsoType orso_resolve_unary(TokenType operator, OrsoType operand) {
    if (operand == ORSO_TYPE_INVALID) {
        return ORSO_TYPE_INVALID;
    }

    if (operand == ORSO_TYPE_UNRESOLVED) {
        return ORSO_TYPE_UNRESOLVED;
    }

    switch (operator) {
        case TOKEN_MINUS:
            return operand;
        case TOKEN_NOT:
            return ORSO_TYPE_BOOL;
        default: return ORSO_TYPE_INVALID;
    }
}

static OrsoExpressionNode* implicit_cast(OrsoExpressionNode* parent, OrsoExpressionNode* operand, OrsoType value_type) {
    OrsoExpressionNode* implicit_cast = ALLOCATE(OrsoExpressionNode);
    implicit_cast->type = EXPRESSION_IMPLICIT_CAST;
    implicit_cast->value_type = value_type;
    implicit_cast->cast.operand = operand;

    return implicit_cast;
}

void orso_resolve_expression(OrsoExpressionNode* expression) {
    if (expression->value_type != ORSO_TYPE_UNRESOLVED) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            orso_resolve_expression(expression->grouping.expression);
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
            orso_resolve_expression(left);
            orso_resolve_expression(right);

            OrsoType cast_left;
            OrsoType cast_right;

            switch (expression->binary.operator.type) {
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                case TOKEN_STAR:
                case TOKEN_SLASH: {
                    OrsoType combined_type = orso_binary_arithmetic_cast(left->value_type, right->value_type);
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
            orso_resolve_expression(expression->unary.operand);
            expression->value_type = orso_resolve_unary(
                    expression->unary.operator.type, expression->unary.operand->value_type);
            break;
        }

        default: break; // unreachable
    }
}

void orso_resolve_ast_types(OrsoAST* ast) {
    if (ast->expression == NULL) {
        return;
    }

    orso_resolve_expression(ast->expression);
}