#include "static_analyzer.h"

static SavineType savine_resolve_unary(TokenType operator, SavineType operand) {
    if (operand == SAVINE_TYPE_INVALID) {
        return SAVINE_TYPE_INVALID;
    }

    if (operand == SAVINE_TYPE_UNRESOLVED) {
        return SAVINE_TYPE_UNRESOLVED;
    }

    switch (operator) {
        case TOKEN_MINUS:
            return operand;
        case TOKEN_NOT:
            return SAVINE_TYPE_BOOL;
        default: return SAVINE_TYPE_INVALID;
    }
}

static SavineExpressionNode* implicit_cast(SavineExpressionNode* parent, SavineExpressionNode* operand, SavineType value_type) {
    SavineExpressionNode* implicit_cast = ALLOCATE(SavineExpressionNode);
    implicit_cast->type = EXPRESSION_IMPLICIT_CAST;
    implicit_cast->value_type = value_type;
    implicit_cast->cast.operand = operand;

    return implicit_cast;
}

void savine_resolve_expression(SavineExpressionNode* expression) {
    if (expression->value_type != SAVINE_TYPE_UNRESOLVED) {
        return;
    }

    switch (expression->type) {
        case EXPRESSION_GROUPING: {
            savine_resolve_expression(expression->grouping.expression);
            expression->value_type = expression->grouping.expression->value_type;
            break;
        }
        case EXPRESSION_PRIMARY: {
            expression->value_type = SAVINE_TYPE_INT64;
            break;
        }
        case EXPRESSION_BINARY: {
            SavineExpressionNode* left = expression->binary.left;
            SavineExpressionNode* right = expression->binary.right;
            savine_resolve_expression(left);
            savine_resolve_expression(right);

            SavineType cast_left;
            SavineType cast_right;

            switch (expression->binary.operator.type) {
                case TOKEN_PLUS:
                case TOKEN_MINUS:
                case TOKEN_STAR:
                case TOKEN_SLASH: {
                    SavineType combined_type = savine_binary_arithmetic_cast(left->value_type, right->value_type);
                    expression->value_type = combined_type;

                    cast_left = combined_type;
                    cast_right = combined_type;
                    break;
                }

                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL: {
                    savine_binary_comparison_casts(left->value_type, right->value_type, &cast_left, &cast_right);
                    expression->value_type = SAVINE_TYPE_BOOL;
                    break;
                }
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    savine_binary_equality_casts(left->value_type, right->value_type, &cast_left, &cast_right);
                    expression->value_type = SAVINE_TYPE_BOOL;
                    break;
                }
            }

            if (cast_left == SAVINE_TYPE_INVALID || cast_right == SAVINE_TYPE_INVALID) {
                expression->value_type = SAVINE_TYPE_INVALID;
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
            savine_resolve_expression(expression->unary.operand);
            expression->value_type = savine_resolve_unary(
                    expression->unary.operator.type, expression->unary.operand->value_type);
            break;
        }

        default: break; // unreachable
    }
}

void savine_resolve_ast_types(SavineAST* ast) {
    if (ast->expression == NULL) {
        return;
    }

    savine_resolve_expression(ast->expression);
}