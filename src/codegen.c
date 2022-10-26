#include "codegen.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

static void emit_instruction(const OrsoInstruction* instruction, Chunk* chunk, i32 line) {
    chunk_write(chunk, instruction, line);
}

static void emit_return(Chunk* chunk, i32 line, OrsoType type) {
    const OrsoInstruction instruction = { .op_code = ORSO_OP_RETURN, .value = type };
    chunk_write(chunk, &instruction, line);
}

static void emit_constant(Chunk* chunk, OrsoSlot slot, i32 line) {
    chunk_write_constant(chunk, slot, line);
}

static void emit_type_convert(OrsoType from_type, OrsoType to_type, Chunk* chunk, i32 line) {
    bool include_bool = true;
    if (orso_is_float_type(from_type) && (orso_is_integer_type(to_type, include_bool))) {
        const OrsoInstruction instruction = { .op_code = ORSO_OP_F64_TO_I64 };
        emit_instruction(&instruction, chunk, line);
    } else if (orso_is_integer_type(from_type, include_bool) && orso_is_float_type(to_type)) {
        const OrsoInstruction instruction = { .op_code = ORSO_OP_I64_TO_F64 };
        emit_instruction(&instruction, chunk, line);
    } else {
        // Unreachable
    }
}

static void expression(OrsoExpressionNode* expression_node, Chunk* chunk) {
#define EMIT_BINARY_OP(OP, TYPE) do { \
    const OrsoInstruction instruction = { .op_code = ORSO_OP_##OP##_##TYPE }; \
    emit_instruction(&instruction, chunk, operator.line); \
     } while(false)

#define EMIT_BINARY_OP_I64(OP) EMIT_BINARY_OP(OP, I64)
#define EMIT_BINARY_OP_F64(OP) EMIT_BINARY_OP(OP, F64)

#define EMIT_NOT() do { \
    const OrsoInstruction instruction = { .op_code = ORSO_OP_LOGICAL_NOT }; \
    emit_instruction(&instruction, chunk, operator.line); \
} while(false)

#define EMIT_NEGATE(TYPE) do { \
    const OrsoInstruction instruction = { .op_code = ORSO_OP_NEGATE_##TYPE }; \
    emit_instruction(&instruction, chunk, operator.line); \
} while (false)

    switch(expression_node->type) {
        case EXPRESSION_BINARY: {
            Token operator = expression_node->binary.operator;
            OrsoExpressionNode* left = expression_node->binary.left;
            OrsoExpressionNode* right = expression_node->binary.right;
            expression(left, chunk);
            expression(right, chunk);

            if (orso_is_integer_type(left->value_type, true)) {
                switch (operator.type) {
                    case TOKEN_PLUS: EMIT_BINARY_OP_I64(ADD); break;
                    case TOKEN_MINUS: EMIT_BINARY_OP_I64(SUBTRACT); break;
                    case TOKEN_STAR: EMIT_BINARY_OP_I64(MULTIPLY); break;
                    case TOKEN_SLASH: EMIT_BINARY_OP_I64(DIVIDE); break;
                    case TOKEN_LESS: EMIT_BINARY_OP_I64(LESS); break;
                    case TOKEN_GREATER: EMIT_BINARY_OP_I64(GREATER); break;
                    case TOKEN_LESS_EQUAL: EMIT_BINARY_OP_I64(GREATER); EMIT_NOT(); break;
                    case TOKEN_GREATER_EQUAL: EMIT_BINARY_OP_I64(LESS); EMIT_NOT(); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP_I64(EQUAL); EMIT_NOT(); break;
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP_I64(EQUAL); break;
                    default: break; // Unreachable
                }
            } else if (orso_is_float_type(left->value_type)) {
                switch (operator.type) {
                    case TOKEN_PLUS: EMIT_BINARY_OP_F64(ADD); break;
                    case TOKEN_MINUS: EMIT_BINARY_OP_F64(SUBTRACT); break;
                    case TOKEN_STAR: EMIT_BINARY_OP_F64(MULTIPLY); break;
                    case TOKEN_SLASH: EMIT_BINARY_OP_F64(DIVIDE); break;
                    case TOKEN_LESS: EMIT_BINARY_OP_F64(LESS); break;
                    case TOKEN_GREATER: EMIT_BINARY_OP_F64(GREATER); break;
                    case TOKEN_LESS_EQUAL: EMIT_BINARY_OP_F64(GREATER); EMIT_NOT(); break;
                    case TOKEN_GREATER_EQUAL: EMIT_BINARY_OP_F64(LESS); EMIT_NOT(); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP_F64(EQUAL); EMIT_NOT(); break;
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP_F64(EQUAL); break;
                    default: break; // Unreachable
                }
            } else if (left->value_type == ORSO_TYPE_STRING) {
                switch (operator.type) {
                    case TOKEN_PLUS: EMIT_BINARY_OP(CONCAT, STRING); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); EMIT_NOT(); break;
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); break;
                    default: break; // Unreachable
                }
            } else if (left->value_type == ORSO_TYPE_SYMBOL) {
                switch (operator.type) {
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP_I64(EQUAL); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP_I64(EQUAL); EMIT_NOT(); break;
                    default: break; // Unreachable
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
                    case TOKEN_MINUS: EMIT_NEGATE(I64); break;
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: break; // unreachable
                }
            } else if (orso_is_float_type(unary->value_type)) {
                switch (operator.type) {
                    case TOKEN_MINUS: EMIT_NEGATE(I64); break;
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: break; // unreachable
                }
            } else if (unary->value_type == ORSO_TYPE_NULL) {
                switch (operator.type) {
                    case TOKEN_NOT: EMIT_NOT(); break;
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
            if (expression_node->primary.constant.i == 0) {
                const OrsoInstruction instruction = { .op_code = ORSO_OP_PUSH_0 };
                emit_instruction(&instruction, chunk, expression_node->primary.token.line);
            } else if (expression_node->primary.constant.i == 1) {
                const OrsoInstruction instruction = { .op_code = ORSO_OP_PUSH_1 };
                emit_instruction(&instruction, chunk, expression_node->primary.token.line);
            } else {
                OrsoSlot slot = expression_node->primary.constant;
                emit_constant(chunk, slot, expression_node->primary.token.line);
            }
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            expression(expression_node->cast.operand, chunk);
            emit_type_convert(expression_node->cast.operand->value_type, expression_node->value_type, chunk, -1);
            break;
        }
    }

#undef EMIT_NEGATE
#undef EMIT_NOT
#undef EMIT_BINARY_OP_F64
#undef EMIT_BINARY_OP_I64
#undef EMIT_BINARY_OP
}

bool orso_generate_code(OrsoAST* ast, Chunk* chunk) {
    if (!ast->expression) {
        return true;
    }

    expression(ast->expression, chunk);

    emit_return(chunk, -1, ast->expression->value_type);

#ifdef DEBUG_PRINT_CODE
    chunk_disassemble(chunk, "code");
#endif

    return true;
}