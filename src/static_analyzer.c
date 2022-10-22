#include "static_analyzer.h"

static i32 FORCE_INLINE savine_number_type_bit_count(SavineType number_type) {
    switch (number_type) {
        case SAVINE_TYPE_INT64:
        case SAVINE_TYPE_FLOAT64: return 64;

        case SAVINE_TYPE_FLOAT32:
        case SAVINE_TYPE_INT32: return 32;

        case SAVINE_TYPE_BOOL: return 1;
        default: return 0;
    }
}

static SavineType savine_resolve_binary(TokenType operator, SavineType left, SavineType right) {
    if (left == SAVINE_TYPE_INVALID || right == SAVINE_TYPE_INVALID) {
        return SAVINE_TYPE_INVALID;
    }

    if (left == SAVINE_TYPE_UNRESOLVED || right == SAVINE_TYPE_UNRESOLVED) {
        return SAVINE_TYPE_UNRESOLVED;
    }

    switch (operator) {
        case TOKEN_PLUS:
        case TOKEN_MINUS:
        case TOKEN_STAR:
        case TOKEN_SLASH: {
            i32 left_count = savine_number_type_bit_count(left);
            if (left_count == 0) {
                return SAVINE_TYPE_INVALID;
            }

            i32 right_count = savine_number_type_bit_count(right);
            if (right_count == 0) {
                return SAVINE_TYPE_INVALID;
            }
            
            SavineType merged_type;
            if ((savine_is_float_type(left) && savine_is_float_type(right))
            || (!savine_is_float_type(left) && !savine_is_float_type(right))) {
                merged_type = left_count > right_count ? left : right;
            } else {
                merged_type = (left_count > 32 || right_count > 32) ? SAVINE_TYPE_FLOAT64 : SAVINE_TYPE_FLOAT32;
            }

            return merged_type;
        }
        default: return SAVINE_TYPE_INVALID;
    }
}

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
        default: return SAVINE_TYPE_INVALID;
    }
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
            expression->value_type = SAVINE_TYPE_INT32;
            break;
        }
        case EXPRESSION_BINARY: {
            SavineExpressionNode* left = expression->binary.left;
            SavineExpressionNode* right = expression->binary.right;
            savine_resolve_expression(left);
            savine_resolve_expression(right);
            expression->value_type = savine_resolve_binary(
                    expression->binary.operator.type, left->value_type, right->value_type);
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