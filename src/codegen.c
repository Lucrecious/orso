#include "codegen.h"

#ifdef DEBUG_PRINT
#include "debug.h"
#endif

#include "sb.h"

static void emit_instruction(const OrsoInstruction* instruction, Chunk* chunk, i32 line) {
    chunk_write(chunk, instruction, line);
}

static void emit_constant(Chunk* chunk, OrsoSlot slot, i32 line) {
    i32 index = chunk_add_constant(chunk, slot);
    const OrsoInstruction instruction = { .op_code = ORSO_OP_CONSTANT, .constant.index = index };
    chunk_write(chunk, &instruction, line);
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

static i32 identifier_constant(OrsoSymbol* identifier, Chunk* chunk) {
    OrsoSlot slot = {
        .p = identifier,
#ifdef DEBUG_TRACE_EXECUTION
        .type = ORSO_TYPE_SYMBOL,
#endif
    };
    i32 index = chunk_add_constant(chunk, slot);

    return index;
}

static OrsoSlot zero_value(OrsoType type, OrsoGarbageCollector* gc, OrsoSymbolTable* symbol_table) {
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
            slot.p = orso_new_string_from_cstrn(gc, "", 0);
            break;
        case ORSO_TYPE_SYMBOL:
            slot.p = orso_new_symbol_from_cstrn(gc, "", 0, symbol_table);
            break;
        default:
            slot.i = 0;
            break;
    }

    return slot;
}

static void expression(OrsoVM* vm, OrsoExpressionNode* expression_node, Chunk* chunk) {
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
            expression(vm, left, chunk);
            expression(vm, right, chunk);

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
            expression(vm, expression_node->unary.operand, chunk);

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
                    case TOKEN_MINUS: EMIT_NEGATE(F64); break;
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
            expression(vm, expression_node->grouping.expression, chunk);
            break;
        }
        
        case EXPRESSION_PRIMARY: {
            switch (expression_node->value_type) {
                case ORSO_TYPE_BOOL:
                case ORSO_TYPE_INT32:
                case ORSO_TYPE_INT64:
                case ORSO_TYPE_FLOAT32:
                case ORSO_TYPE_FLOAT64:
                case ORSO_TYPE_NULL: {
                    if (expression_node->primary.constant.i == 0) {
                        const OrsoInstruction instruction = {
                            .op_code = ORSO_OP_PUSH_0,
#ifdef DEBUG_TRACE_EXECUTION
                            .constant.type = expression_node->value_type,
#endif
                        };
                        emit_instruction(&instruction, chunk, expression_node->primary.token.line);
                    } else if (expression_node->primary.constant.i == 1) {
                        const OrsoInstruction instruction = {
                            .op_code = ORSO_OP_PUSH_1,
#ifdef DEBUG_TRACE_EXECUTION
                            .constant.type = expression_node->value_type,
#endif
                        };
                        emit_instruction(&instruction, chunk, expression_node->primary.token.line);
                    } else {
                        OrsoSlot slot = expression_node->primary.constant;
#ifdef DEBUG_TRACE_EXECUTION
                        slot.type = expression_node->value_type;
#endif
                        emit_constant(chunk, slot, expression_node->primary.token.line);
                    }
                    break;
                }
                case ORSO_TYPE_STRING: {
                    OrsoString* string = orso_new_string_from_cstrn(&vm->gc, expression_node->primary.token.start + 1, expression_node->primary.token.length - 2);
                    OrsoSlot slot = {
                        .p = string,
#ifdef DEBUG_TRACE_EXECUTION
                        slot.type = expression_node->value_type;
#endif
                    };

                    emit_constant(chunk, slot, expression_node->primary.token.line);
                    break;
                }
                case ORSO_TYPE_SYMBOL: {
                    OrsoSymbol* symbol = orso_new_symbol_from_cstrn(&vm->gc, expression_node->primary.token.start + 1, expression_node->primary.token.length - 2, &vm->symbols);
                    OrsoSlot slot = {
                        .p = symbol,
#ifdef DEBUG_TRACE_EXECUTION
                        slot.type = expression_node->value_type;
#endif
                    };

                    emit_constant(chunk, slot, expression_node->primary.token.line);
                    break;
                }
                default: break; // Unreachable
            }
            break;
        }

        case EXPRESSION_VARIABLE: {
            Token identifier_token = expression_node->variable.name;
            OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc, identifier_token.start, identifier_token.length, &vm->symbols);
            i32 index = identifier_constant(identifier, chunk);

            const OrsoInstruction instruction = { 
                .op_code = ORSO_OP_GET_GLOBAL,
                .constant.index = index,
#ifdef DEBUG_TRACE_EXECUTION
                .constant.type = expression_node->value_type,
#endif
            };

            emit_instruction(&instruction, chunk, expression_node->start.line);
            break;
        }

        case EXPRESSION_ASSIGNMENT: {
            Token identifier_token = expression_node->assignment.variable_name;
            OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc,
                identifier_token.start, identifier_token.length, &vm->symbols);
            i32 index = identifier_constant(identifier, chunk);
            
            const OrsoInstruction instruction = {
                .op_code = ORSO_OP_SET_GLOBAL,
                .constant.index = index,
#ifdef DEBUG_TRACE_EXECUTION
                .constant.type = expression_node->value_type,
#endif
            };

            expression(vm, expression_node->assignment.right_side, chunk);

            emit_instruction(&instruction, chunk, expression_node->start.line);
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            OrsoExpressionNode* operand = expression_node->cast.operand;
            expression(vm, operand, chunk);
            emit_type_convert(operand->value_type, expression_node->value_type, chunk, operand->start.line);
            break;
        }
    }

#undef EMIT_NEGATE
#undef EMIT_NOT
#undef EMIT_BINARY_OP_F64
#undef EMIT_BINARY_OP_I64
#undef EMIT_BINARY_OP
}

static void statement(OrsoVM* vm, OrsoStatementNode* statement, Chunk* chunk) {
    switch (statement->type) {
        case ORSO_STATEMENT_EXPRESSION: {
            OrsoExpressionNode* expression_ = statement->expression;
            expression(vm, expression_, chunk);

            const OrsoInstruction instruction = {
                .op_code = ORSO_OP_POP,
            };
            emit_instruction(&instruction, chunk, statement->start.line);
            break;
        }
        case ORSO_STATEMENT_PRINT_EXPR: {
                expression(vm, statement->expression, chunk);

                Token start = statement->expression->start;
                Token end = statement->expression->end;

                OrsoString* expression_string = orso_new_string_from_cstrn(&vm->gc, start.start, (end.start + end.length) - start.start);

                OrsoSlot slot = {
                    .p = expression_string,
#ifdef DEBUG_TRACE_EXECUTION
                    .type = ORSO_TYPE_STRING,
#endif
                    };

                emit_constant(chunk, slot, start.line);

                const OrsoInstruction instruction = {
                    .op_code = ORSO_OP_PRINT_EXPR,
                    .print_expr.type = statement->expression->value_type,
                };
                emit_instruction(&instruction, chunk, start.line);
            break;
        }
        case ORSO_STATEMENT_NONE: break; // Unreachable
    }
}

static void var_declaration(OrsoVM* vm, OrsoVarDeclarationNode* var_declaration, Chunk* chunk) {
    if (var_declaration->expression != NULL) {
        expression(vm, var_declaration->expression, chunk);
    } else {
        OrsoSlot slot = zero_value(var_declaration->var_type, &vm->gc, &vm->symbols);
        emit_constant(chunk, slot, var_declaration->start.line);
    }

    Token identifier_token = var_declaration->variable_name;
    OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc, identifier_token.start, identifier_token.length, &vm->symbols);

    const OrsoInstruction instruction = {
        .op_code = ORSO_OP_DEFINE_GLOBAL,
        .constant.index = identifier_constant(identifier, chunk),
    };

    emit_instruction(&instruction, chunk, var_declaration->start.line);
}

static void declaration(OrsoVM* vm, OrsoDeclarationNode* declaration, Chunk* chunk) {
    switch (declaration->type) {
        case ORSO_DECLARATION_STATEMENT: statement(vm, declaration->statement, chunk); break;
        case ORSO_DECLARATION_VAR: var_declaration(vm, declaration->var, chunk); break;
        case ORSO_DECLARATION_NONE: break; // Unreachable
    }
}

bool orso_generate_code(OrsoVM* vm, OrsoAST* ast, Chunk* chunk) {
    const OrsoInstruction instruction = { .op_code = ORSO_OP_RETURN };

    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        declaration(vm, ast->declarations[i], chunk);
    }

    emit_instruction(&instruction, chunk, -1);

#ifdef DEBUG_PRINT
    chunk_disassemble(chunk, "code");
#endif

    return true;
}