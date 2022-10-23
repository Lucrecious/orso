#include "codegen.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

static void emit_byte(byte byte, Chunk* chunk, i32 line) {
    chunk_write(chunk, byte, line);
}

static void emit_bytes(byte byte1, byte byte2, Chunk* chunk, i32 line) {
    emit_byte(byte1, chunk, line);
    emit_byte(byte2, chunk, line);
}

static void emit_return(Chunk* chunk, i32 line) {
    emit_byte(OP_RETURN, chunk, line);
}

static void emit_constant(Chunk* chunk, OrsoValue value, i32 line) {
    chunk_write_constant(chunk, value, line);
}

static void emit_type_convert(OrsoType from_type, OrsoType to_type, Chunk* chunk, i32 line) {
    bool include_bool = true;
    if (orso_is_float_type(from_type) && (orso_is_integer_type(to_type, include_bool))) {
        emit_byte(OP_DOUBLE_TO_INT, chunk, line);
    } else if (orso_is_integer_type(from_type, include_bool) && orso_is_float_type(to_type)) {
        emit_byte(OP_INT_TO_DOUBLE, chunk, line);
    } else {
        // Unreachable
    }
}

static void end_code_generation(Chunk* chunk, i32 line) {
    emit_return(chunk, line);
}

static void expression(OrsoExpressionNode* expression_node, Chunk* chunk) {
#define EMIT_OP(SUFFIX) \
    do { \
        switch(operator.type) { \
            case TOKEN_MINUS: emit_byte(OP_SUBTRACT_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_PLUS: emit_byte(OP_ADD_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_STAR: emit_byte(OP_MULTIPLY_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_SLASH: emit_byte(OP_DIVIDE_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_EQUAL_EQUAL: emit_byte(OP_EQUAL_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_BANG_EQUAL: emit_bytes(OP_EQUAL_ ## SUFFIX, OP_NOT, chunk, operator.line); break; \
            case TOKEN_LESS: emit_byte(OP_LESS_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_GREATER: emit_byte(OP_GREATER_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_LESS_EQUAL: emit_bytes(OP_GREATER_ ## SUFFIX, OP_NOT, chunk, operator.line); break; \
            case TOKEN_GREATER_EQUAL: emit_bytes(OP_LESS_ ## SUFFIX, OP_NOT, chunk, operator.line); break; \
            default: break; /* unreachable */ \
        } \
    } while (false)

    switch(expression_node->type) {
        case EXPRESSION_BINARY: {
            Token operator = expression_node->binary.operator;
            OrsoExpressionNode* left = expression_node->binary.left;
            OrsoExpressionNode* right = expression_node->binary.right;

            switch (operator.type) {
                case TOKEN_MINUS:
                case TOKEN_PLUS:
                case TOKEN_STAR:
                case TOKEN_SLASH:
                case TOKEN_LESS:
                case TOKEN_GREATER:
                case TOKEN_LESS_EQUAL:
                case TOKEN_GREATER_EQUAL:
                case TOKEN_BANG_EQUAL:
                case TOKEN_EQUAL_EQUAL: {
                    expression(left, chunk);
                    expression(right, chunk);
                    if (orso_is_integer_type(left->value_type, true)) {
                        EMIT_OP(INT);
                    } else if (orso_is_float_type(left->value_type)) {
                        EMIT_OP(DOUBLE);
                    } else {
                        // Unreacheable
                    }
                }
            }

            break;
        }

        case EXPRESSION_UNARY: {
            expression(expression_node->unary.operand, chunk);

            OrsoExpressionNode* unary = expression_node->unary.operand;
            Token operator = expression_node->unary.operator;

            if (orso_is_integer_type(unary->value_type, true)) {
                switch (operator.type) {
                    case TOKEN_MINUS: emit_byte(OP_NEGATE_INT, chunk, operator.line); break;
                    case TOKEN_NOT: emit_byte(OP_NOT, chunk, operator.line); break;
                    default: break; // unreachable
                }
            } else if (orso_is_float_type(unary->value_type)) {
                switch (operator.type) {
                    case TOKEN_MINUS: emit_byte(OP_NEGATE_DOUBLE, chunk, operator.line); break;
                    case TOKEN_NOT: emit_byte(OP_NOT, chunk, operator.line);
                    default: break; // unreachable
                }
            }
            break;
        }

        case EXPRESSION_GROUPING: {
            expression(expression_node->grouping.expression, chunk);
            break;
        }
        
        case EXPRESSION_PRIMARY: {
            if (expression_node->primary.constant.as_int == 0) {
                emit_byte(OP_ZERO, chunk, expression_node->primary.token.line);
            } else if (expression_node->primary.constant.as_int == 1) {
                emit_byte(OP_ONE, chunk, expression_node->primary.token.line);
            } else {
                emit_constant(chunk, expression_node->primary.constant, expression_node->primary.token.line);
            }
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            expression(expression_node->cast.operand, chunk);
            emit_type_convert(expression_node->cast.operand->value_type, expression_node->value_type, chunk, -1);
            break;
        }
    }

#undef EMIT_OP
}

bool orso_generate_code(OrsoAST* ast, Chunk* chunk) {
    if (!ast->expression) {
        return true;
    }

    expression(ast->expression, chunk);

    emit_return(chunk, -1);

#ifdef DEBUG_PRINT_CODE
    chunk_disassemble(chunk, "code");
#endif

    return true;
}