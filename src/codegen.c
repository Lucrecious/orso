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

static void emit_constant(Chunk* chunk, SavineValue value, i32 line) {
    chunk_write_constant(chunk, value, line);
}

static void emit_type_convert(SavineType from_type, SavineType to_type, Chunk* chunk, i32 line) {
    if (savine_is_float_type(from_type) && savine_is_integer_type(to_type)) {
        emit_byte(OP_DOUBLE_TO_INT, chunk, line);
    } else if (savine_is_integer_type(from_type) && savine_is_float_type(to_type)) {
        emit_byte(OP_INT_TO_DOUBLE, chunk, line);
    } else {
        // Unreachable
    }
}

static void end_code_generation(Chunk* chunk, i32 line) {
    emit_return(chunk, line);
}

static void expression(SavineExpressionNode* expression_node, Chunk* chunk, SavineType convert_type) {
#define EMIT_OP(SUFFIX) \
    do { \
        switch(operator.type) { \
            case TOKEN_MINUS: emit_byte(OP_SUBTRACT_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_PLUS: emit_byte(OP_ADD_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_STAR: emit_byte(OP_MULTIPLY_ ## SUFFIX, chunk, operator.line); break; \
            case TOKEN_SLASH: emit_byte(OP_DIVIDE_ ## SUFFIX, chunk, operator.line); break; \
            default: break; /* unreachable */ \
        } \
    } while (false)

    switch(expression_node->type) {
        case EXPRESSION_BINARY: {
            expression(expression_node->binary.left, chunk, expression_node->value_type);
            expression(expression_node->binary.right, chunk, expression_node->value_type);

            Token operator = expression_node->binary.operator;

            if (savine_is_integer_type(expression_node->value_type)) {
                EMIT_OP(INT);
            } else if (savine_is_float_type(expression_node->value_type)) {
                EMIT_OP(DOUBLE);
            } else {
                // Unreacheable
            }
            break;
        }

        case EXPRESSION_UNARY: {
            expression(expression_node->unary.operand, chunk, expression_node->value_type);

            Token operator = expression_node->unary.operator;

            if (savine_is_integer_type(expression_node->value_type)) {
                switch (operator.type) {
                    case TOKEN_MINUS: emit_byte(OP_NEGATE_INT, chunk, operator.line); break;
                    default: break; // unreachable
                }
            } else if (savine_is_float_type(expression_node->value_type)) {
                switch (operator.type) {
                    case TOKEN_MINUS: emit_byte(OP_NEGATE_DOUBLE, chunk, operator.line); break;
                    default: break; // unreachable
                }
            }
            break;
        }

        case EXPRESSION_GROUPING: {
            expression(expression_node->grouping.expression, chunk, expression_node->value_type);
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
    }

    if (convert_type != expression_node->value_type) {
        emit_type_convert(expression_node->value_type, convert_type, chunk, -1);
    }
#undef EMIT_OP
}

bool savine_generate_code(SavineAST* ast, Chunk* chunk) {
    if (!ast->expression) {
        return true;
    }

    expression(ast->expression, chunk, ast->expression->value_type);

    emit_return(chunk, -1);

#ifdef DEBUG_PRINT_CODE
    chunk_disassemble(chunk, "code");
#endif

    return true;
}