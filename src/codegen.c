#include "codegen.h"

#ifdef DEBUG_PRINT
#include "debug.h"
#endif

#include "sb.h"

static void emit_instruction(const OrsoOPCode op_code, Chunk* chunk, i32 line) {
    chunk_write(chunk, op_code, line);
}

static void emit_constant(Chunk* chunk, OrsoSlot slot, i32 line, bool is_ptr) {
    u32 index = chunk_add_constant(chunk, slot, is_ptr);
    chunk_write(chunk, ORSO_OP_CONSTANT, line);

    // ASSERT index < largest 24 bit number
    byte b1, b2, b3;
    ORSO_u24_to_u8s(index, b1, b2, b3);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
    chunk_write(chunk, b3, line);

    if (is_ptr) {
        emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, chunk, line);
    }
}

static void emit_global(OrsoOPCode op_code, u32 index, Chunk* chunk, i32 line) {
    // ASSERT op_code is one of the global instructions (set, get, define for either union or single)
    emit_instruction(op_code, chunk, line);

    // ASSERT index < largest 24 bit number
    byte b1, b2, b3;
    ORSO_u24_to_u8s(index, b1, b2, b3);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
    chunk_write(chunk, b3, line);
}

static void emit_put_in_union(OrsoTypeKind type_kind, Chunk* chunk, i32 line) {
    emit_instruction(ORSO_OP_PUT_IN_UNION, chunk, line);

    // ASSERT type_kind < largest u16
    byte b1, b2;
    ORSO_TypeKind_to_u8s(type_kind, b1, b2);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
}

static void emit_update_global_union_gc_type(i32 gc_index, Chunk* chunk, i32 line) {
    emit_instruction(ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE, chunk, line);

    // ASSERT gc_index < largest u24
    byte b1, b2, b3;
    ORSO_u24_to_u8s(gc_index, b1, b2, b3);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
    chunk_write(chunk, b3, line);
}

static void emit_print_expr(OrsoType type, Chunk* chunk, i32 line) {
    emit_instruction(ORSO_OP_PRINT_EXPR, chunk, line);

    u64 utype = type.one;
    byte a, b, c, d, e, f, g, h;
    ORSO_u64_to_u8s(utype, a, b, c, d, e, f, g, h);
    chunk_write(chunk, a, line);
    chunk_write(chunk, b, line);
    chunk_write(chunk, c, line);
    chunk_write(chunk, d, line);
    chunk_write(chunk, e, line);
    chunk_write(chunk, f, line);
    chunk_write(chunk, g, line);
    chunk_write(chunk, h, line);
}

static void emit_type_convert(OrsoTypeKind from_type_kind, OrsoTypeKind to_type_kind, Chunk* chunk, i32 line) {
    bool include_bool = true;
    if (orso_is_float_type_kind(from_type_kind) && (orso_is_integer_type_kind(to_type_kind, include_bool))) {
        emit_instruction(ORSO_OP_F64_TO_I64, chunk, line);
    } else if (orso_is_integer_type_kind(from_type_kind, include_bool) && orso_is_float_type_kind(to_type_kind)) {
        emit_instruction(ORSO_OP_I64_TO_F64, chunk, line);
    } else {
        // Unreachable
    }
}

static i32 identifier_constant(OrsoVM* vm, OrsoSymbol* identifier, i32 slot_count) {
    OrsoSlot index_slot;
    if (!orso_symbol_table_get(&vm->globals.name_to_index, identifier, &index_slot)) {
        index_slot = ORSO_SLOT_I(sb_count(vm->globals.values), ORSO_TYPE_INT32);
        orso_symbol_table_set(&vm->globals.name_to_index, identifier, index_slot);

        for (i32 i = 0; i < slot_count; i++) {
            sb_push(vm->globals.values, ORSO_SLOT_I(0, ORSO_TYPE_INVALID));
        }
    }

    i32 index = index_slot.as.i;

    return index;
}

static OrsoSlot zero_value(OrsoTypeKind type_kind, OrsoGarbageCollector* gc, OrsoSymbolTable* symbol_table) {
    OrsoSlot slot;
    switch (type_kind) {
        case ORSO_TYPE_NULL:
        case ORSO_TYPE_BOOL:
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64:
            slot = ORSO_SLOT_I(0, ORSO_TYPE_ONE(type_kind));
            break;
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64:
            slot = ORSO_SLOT_F(0.0, ORSO_TYPE_ONE(type_kind));
            break;
        case ORSO_TYPE_STRING:
            slot = ORSO_SLOT_P(orso_new_string_from_cstrn(gc, "", 0), ORSO_TYPE_ONE(type_kind));
            break;
        case ORSO_TYPE_SYMBOL:
            slot = ORSO_SLOT_P(orso_new_symbol_from_cstrn(gc, "", 0, symbol_table), ORSO_TYPE_ONE(type_kind));
            break;
        case ORSO_TYPE_TYPE:
        default:
            // Unreachable
            slot = ORSO_SLOT_I(0, ORSO_TYPE_INVALID);
            break;
    }

    return slot;
}

static i32 find_global_gc_index(OrsoVM* vm, i32 global_index) {
    for (i32 i = 0; i < sb_count(vm->globals.gc_values_indices); i++) {
        i32 index = vm->globals.gc_values_indices[i].index;
        if (index != global_index) {
            continue;
        }

        return i;
    }

    return -1;
}

static void expression(OrsoVM* vm, OrsoExpressionNode* expression_node, Chunk* chunk) {
#define EMIT_BINARY_OP(OP, TYPE) do { \
    emit_instruction(ORSO_OP_##OP##_##TYPE, chunk, operator.line); \
     } while(false)

#define EMIT_BINARY_OP_I64(OP) EMIT_BINARY_OP(OP, I64)
#define EMIT_BINARY_OP_F64(OP) EMIT_BINARY_OP(OP, F64)

#define EMIT_NOT() do { \
    emit_instruction(ORSO_OP_LOGICAL_NOT, chunk, operator.line); \
} while(false)

#define EMIT_NEGATE(TYPE) do { \
    emit_instruction(ORSO_OP_NEGATE_##TYPE, chunk, operator.line); \
} while (false)

    switch(expression_node->type) {
        case EXPRESSION_BINARY: {
            Token operator = expression_node->expr.binary.operator;
            OrsoExpressionNode* left = expression_node->expr.binary.left;
            OrsoExpressionNode* right = expression_node->expr.binary.right;
            expression(vm, left, chunk);
            expression(vm, right, chunk);

            if (orso_is_integer_type_kind(left->value_type.one, true)) {
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
            } else if (orso_is_float_type_kind(left->value_type.one)) {
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
            } else if (left->value_type.one == ORSO_TYPE_STRING) {
                switch (operator.type) {
                    case TOKEN_PLUS: EMIT_BINARY_OP(CONCAT, STRING); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); EMIT_NOT(); break;
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); break;
                    default: break; // Unreachable
                }
            } else if (left->value_type.one == ORSO_TYPE_SYMBOL) {
                switch (operator.type) {
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); EMIT_NOT(); break;
                    default: break; // Unreachable
                }
            }
            break;
        }

        case EXPRESSION_UNARY: {
            expression(vm, expression_node->expr.unary.operand, chunk);

            OrsoExpressionNode* unary = expression_node->expr.unary.operand;
            Token operator = expression_node->expr.unary.operator;

            if (orso_is_integer_type_kind(unary->value_type.one, true)) {
                switch (operator.type) {
                    case TOKEN_MINUS: EMIT_NEGATE(I64); break;
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: break; // unreachable
                }
            } else if (orso_is_float_type_kind(unary->value_type.one)) {
                switch (operator.type) {
                    case TOKEN_MINUS: EMIT_NEGATE(F64); break;
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: break; // unreachable
                }
            } else if (unary->value_type.one == ORSO_TYPE_NULL) {
                switch (operator.type) {
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: break; // unreachable
                }
            }
            break;
        }

        case EXPRESSION_GROUPING: {
            expression(vm, expression_node->expr.grouping.expression, chunk);
            break;
        }
        
        case EXPRESSION_PRIMARY: {
            switch (expression_node->value_type.one) {
                case ORSO_TYPE_BOOL:
                case ORSO_TYPE_INT32:
                case ORSO_TYPE_INT64:
                case ORSO_TYPE_FLOAT32:
                case ORSO_TYPE_FLOAT64:
                case ORSO_TYPE_NULL: {
                    if (expression_node->expr.primary.constant.as.i == 0) {
                        emit_instruction(ORSO_OP_PUSH_0, chunk, expression_node->expr.primary.token.line);
                    } else if (expression_node->expr.primary.constant.as.i == 1) {
                        emit_instruction(ORSO_OP_PUSH_1, chunk, expression_node->expr.primary.token.line);
                    } else {
                        OrsoSlot slot = expression_node->expr.primary.constant;
#ifdef DEBUG_TRACE_EXECUTION
                        slot.type = expression_node->value_type;
#endif
                        emit_constant(chunk, slot, expression_node->expr.primary.token.line, orso_is_gc_type(expression_node->value_type));
                    }
                    break;
                }
                case ORSO_TYPE_STRING: {
                    OrsoString* string = orso_new_string_from_cstrn(&vm->gc, expression_node->expr.primary.token.start + 1, expression_node->expr.primary.token.length - 2);
                    OrsoSlot slot = ORSO_SLOT_P(string, expression_node->value_type);

                    emit_constant(chunk, slot, expression_node->expr.primary.token.line, true);
                    break;
                }
                case ORSO_TYPE_SYMBOL: {
                    OrsoSymbol* symbol = orso_new_symbol_from_cstrn(&vm->gc, expression_node->expr.primary.token.start + 1, expression_node->expr.primary.token.length - 2, &vm->symbols);
                    OrsoSlot slot = ORSO_SLOT_P(symbol, expression_node->value_type);

                    emit_constant(chunk, slot, expression_node->expr.primary.token.line, true);
                    break;
                }
                default: break; // Unreachable
            }
            break;
        }

        case EXPRESSION_VARIABLE: {
            Token identifier_token = expression_node->expr.variable.name;
            OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc, identifier_token.start, identifier_token.length, &vm->symbols);

            if (ORSO_TYPE_IS_SINGLE(expression_node->value_type)) {
                i32 index = identifier_constant(vm, identifier, 1);

                emit_global(ORSO_OP_GET_GLOBAL, index, chunk, expression_node->start.line);

                if (orso_is_gc_type(expression_node->value_type)) {
                    emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, chunk, expression_node->start.line);
                }
            } else {
                // ASSERT somehow that the identifier is already defined
                i32 index = identifier_constant(vm, identifier, 2);
                emit_global(ORSO_OP_GET_GLOBAL_UNION, index, chunk, expression_node->start.line);
            }
            break;
        }

        case EXPRESSION_ASSIGNMENT: {
            Token identifier_token = expression_node->expr.assignment.variable_name;
            OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc,
                identifier_token.start, identifier_token.length, &vm->symbols);
            
            expression(vm, expression_node->expr.assignment.right_side, chunk);

            if (ORSO_TYPE_IS_SINGLE(expression_node->value_type)) {
                i32 index = identifier_constant(vm, identifier, 1);
                emit_global(ORSO_OP_SET_GLOBAL, index, chunk, expression_node->start.line);
            } else {
                if (ORSO_TYPE_IS_SINGLE(expression_node->expr.assignment.right_side->value_type)) {
                    const OrsoTypeKind right_side_type_kind = expression_node->expr.assignment.right_side->value_type.one;
                    emit_put_in_union(right_side_type_kind, chunk, expression_node->start.line);
                }

                i32 index = identifier_constant(vm, identifier, 2);

                if (orso_is_gc_type(expression_node->value_type)) {
                    const i32 gc_index = find_global_gc_index(vm, index + 1);
                    // ASSERT that the .constant.index is not negative
                    emit_update_global_union_gc_type(gc_index, chunk, expression_node->start.line);
                }

                emit_global(ORSO_OP_SET_GLOBAL_UNION, index, chunk, expression_node->start.line);
            }
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            OrsoExpressionNode* operand = expression_node->expr.cast.operand;
            expression(vm, operand, chunk);
            emit_type_convert(operand->value_type.one, expression_node->value_type.one, chunk, operand->start.line);
            break;
        }

        default:
            // ASSERT false
            break;
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
            OrsoExpressionNode* expression_ = statement->stmt.expression;
            expression(vm, expression_, chunk);

            if (orso_is_gc_type(expression_->value_type)) {
                emit_instruction(ORSO_OP_POP_TOP_OBJECT, chunk, statement->start.line);
            }

            emit_instruction(ORSO_OP_POP, chunk, statement->start.line);

            if (ORSO_TYPE_IS_UNION(expression_->value_type)) {
                emit_instruction(ORSO_OP_POP, chunk, statement->start.line);
            }

            break;
        }
        case ORSO_STATEMENT_PRINT_EXPR: {
                expression(vm, statement->stmt.expression, chunk);

                Token start = statement->stmt.expression->start;
                Token end = statement->stmt.expression->end;

                OrsoString* expression_string = orso_new_string_from_cstrn(&vm->gc, start.start, (end.start + end.length) - start.start);

                OrsoSlot slot = ORSO_SLOT_P(expression_string, ORSO_TYPE_STRING);

                emit_constant(chunk, slot, start.line, true);

                emit_print_expr(statement->stmt.expression->value_type, chunk, start.line);
            break;
        }
        case ORSO_STATEMENT_NONE: break; // Unreachable
    }
}

static void var_declaration(OrsoVM* vm, OrsoVarDeclarationNode* var_declaration, Chunk* chunk) {
    if (var_declaration->expression != NULL) {
        expression(vm, var_declaration->expression, chunk);
    } else {
        if (ORSO_TYPE_IS_UNION(var_declaration->var_type)) {
            // ASSERT that the var_type's union has void in it
            emit_instruction(ORSO_OP_PUSH_NULL_UNION, chunk, var_declaration->start.line);
        } else {
            OrsoSlot slot = zero_value(var_declaration->var_type.one, &vm->gc, &vm->symbols);
            emit_constant(chunk, slot, var_declaration->start.line, orso_is_gc_type(var_declaration->var_type));
        }
    }

    Token identifier_token = var_declaration->variable_name;
    OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc, identifier_token.start, identifier_token.length, &vm->symbols);

    if (ORSO_TYPE_IS_SINGLE(var_declaration->var_type)) {
        i32 index = identifier_constant(vm, identifier, 1);

        if (orso_is_gc_type(var_declaration->var_type)) {
            sb_push(vm->globals.gc_values_indices, ((OrsoGCValueIndex){
                .is_object = true,
                .index = index,
            }));

            emit_instruction(ORSO_OP_POP_TOP_OBJECT, chunk, var_declaration->start.line);
        }

        emit_global(ORSO_OP_DEFINE_GLOBAL, index, chunk, var_declaration->start.line);
    } else {
        // ASSERT var_declaration->expression cannot be NULL if the declaration type is a union type.
        i32 index = identifier_constant(vm, identifier, 2);

        if (var_declaration->expression && ORSO_TYPE_IS_SINGLE(var_declaration->expression->value_type)) {
            emit_put_in_union(var_declaration->expression->value_type.one, chunk, var_declaration->start.line);
        }

        if (orso_is_gc_type(var_declaration->var_type)) {
            sb_push(vm->globals.gc_values_indices, ((OrsoGCValueIndex) {
                .is_object = false,
                .index = index + 1,
            }));

            const u32 gc_values_index = sb_count(vm->globals.gc_values_indices) - 1;

            emit_update_global_union_gc_type(gc_values_index, chunk, var_declaration->start.line);
        }

        emit_global(ORSO_OP_DEFINE_GLOBAL_UNION, index, chunk, var_declaration->start.line);
    }
}

static void declaration(OrsoVM* vm, OrsoDeclarationNode* declaration, Chunk* chunk) {
    switch (declaration->type) {
        case ORSO_DECLARATION_STATEMENT: statement(vm, declaration->decl.statement, chunk); break;
        case ORSO_DECLARATION_VAR: var_declaration(vm, declaration->decl.var, chunk); break;
        case ORSO_DECLARATION_NONE: break; // Unreachable
    }
}

bool orso_generate_code(OrsoVM* vm, OrsoAST* ast, Chunk* chunk) {
    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        declaration(vm, ast->declarations[i], chunk);
    }

    emit_instruction(ORSO_OP_RETURN, chunk, -1);

#ifdef DEBUG_PRINT
    chunk_disassemble(chunk, "code");
#endif

    return true;
}
