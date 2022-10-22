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

static void emit_constant(Chunk* chunk, Value value, i32 line) {
    chunk_write_constant(chunk, value, line);
}

static void end_code_generation(Chunk* chunk, i32 line) {
    emit_return(chunk, line);
}

static void expression(SavineExpressionNode* expression_node, Chunk* chunk) {
    switch(expression_node->type) {
        case EXPRESSION_BINARY: {
            expression(expression_node->binary.left, chunk);
            expression(expression_node->binary.right, chunk);

            Token operator = expression_node->binary.operator;
            switch (operator.type) {
                case TOKEN_MINUS: emit_byte(OP_SUBTRACT, chunk, operator.line); break;
                case TOKEN_PLUS: emit_byte(OP_ADD, chunk, operator.line); break;
                case TOKEN_STAR: emit_byte(OP_MULTIPLY, chunk, operator.line); break;
                case TOKEN_SLASH: emit_byte(OP_DIVIDE, chunk, operator.line); break;
                default: break; // unreachable
            }
            break;
        }

        case EXPRESSION_UNARY: {
            expression(expression_node->unary.operand, chunk);

            Token operator = expression_node->unary.operator;
            switch (operator.type) {
                case TOKEN_MINUS: emit_byte(OP_NEGATE, chunk, operator.line);
                default: break; // unreachable
            }
            break;
        }

        case EXPRESSION_GROUPING: {
            expression(expression_node->grouping.expression, chunk);
            break;
        }
        
        case EXPRESSION_PRIMARY: {
            emit_constant(chunk, expression_node->primary.value, expression_node->primary.token.line);
            break;
        }
    }
}

bool savine_generate_code(SavineAST* ast, Chunk* chunk) {
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