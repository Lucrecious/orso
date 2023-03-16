#include "codegen.h"

#ifdef DEBUG_PRINT
#include "debug.h"
#endif

#include "sb.h"

typedef struct {
    Token name;
    i32 depth;
    byte slot_count;
    i32 stack_position;
    bool is_gc_type;
    i32 object_stack_position;
} Local;

typedef struct {
    Local* locals;
    i32 count;
    i32 scope_depth;
    i32 current_stack_size;
    i32 current_object_stack_size;
    i32 max_stack_size;
} Compiler;

static i32 OP_STACK_EFFECT[] = {
    [ORSO_OP_POP] = -1,
    [ORSO_OP_POP_TOP_OBJECT] = 0,

    [ORSO_OP_PUSH_TOP_OBJECT] = 0,
    [ORSO_OP_PUSH_TOP_OBJECT_NULL] = 0,

    [ORSO_OP_I64_TO_F64] = 0,
    [ORSO_OP_F64_TO_I64] = 0,

    [ORSO_OP_ADD_I64] = -1,
    [ORSO_OP_SUBTRACT_I64] = -1,
    [ORSO_OP_MULTIPLY_I64] = -1,
    [ORSO_OP_DIVIDE_I64] = -1,

    [ORSO_OP_ADD_F64] = -1,
    [ORSO_OP_SUBTRACT_F64] = -1,
    [ORSO_OP_MULTIPLY_F64] = -1,
    [ORSO_OP_DIVIDE_F64] = -1,

    [ORSO_OP_NEGATE_I64] = 0,
    [ORSO_OP_NEGATE_F64] = 0,

    [ORSO_OP_EQUAL_I64] = -1,
    [ORSO_OP_LESS_I64] = -1,
    [ORSO_OP_GREATER_I64] = -1,

    [ORSO_OP_EQUAL_F64] = -1,
    [ORSO_OP_LESS_F64] = -1,
    [ORSO_OP_GREATER_F64] = -1,

    [ORSO_OP_EQUAL_SYMBOL] = -1,

    [ORSO_OP_EQUAL_STRING] = -1,
    [ORSO_OP_CONCAT_STRING] = -1,

    [ORSO_OP_LOGICAL_NOT] = 0,

    [ORSO_OP_PUSH_1] = 1,
    [ORSO_OP_PUSH_0] = 1,
    [ORSO_OP_PUSH_NULL_UNION] = 2,

    [ORSO_OP_CONSTANT] = 1,

    [ORSO_OP_SET_LOCAL] = 0,
    [ORSO_OP_GET_LOCAL] = 1,

    [ORSO_OP_SET_LOCAL_UNION] = 0,
    [ORSO_OP_GET_LOCAL_UNION] = 2,
    
    [ORSO_OP_DEFINE_GLOBAL] = -1,
    [ORSO_OP_SET_GLOBAL_GC_TYPE] = 0,
    [ORSO_OP_GET_GLOBAL] = 1,
    [ORSO_OP_SET_GLOBAL] = 0,

    [ORSO_OP_DEFINE_GLOBAL_UNION] = -2,
    [ORSO_OP_GET_GLOBAL_UNION] = 2,
    [ORSO_OP_SET_GLOBAL_UNION] = 0,

    [ORSO_OP_PUT_IN_UNION] = 1,
    [ORSO_OP_NARROW_UNION] = -1,
    [ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE] = 0,
    [ORSO_OP_UPDATE_STACK_GC_TYPE] = 0,

    [ORSO_OP_JUMP_IF_FALSE] = 0,
    [ORSO_OP_JUMP_IF_TRUE] = 0,
    [ORSO_OP_JUMP] = 0,

    [ORSO_OP_PRINT_EXPR] = -4,

    [ORSO_OP_RETURN] = 0,
};

static i32 OP_OBJECT_STACK_EFFECT[] = {
    [ORSO_OP_POP] = 0,
    [ORSO_OP_POP_TOP_OBJECT] = -1,

    [ORSO_OP_PUSH_TOP_OBJECT] = 1,
    [ORSO_OP_PUSH_TOP_OBJECT_NULL] = 1,

    [ORSO_OP_I64_TO_F64] = 0,
    [ORSO_OP_F64_TO_I64] = 0,

    [ORSO_OP_ADD_I64] = 0,
    [ORSO_OP_SUBTRACT_I64] = 0,
    [ORSO_OP_MULTIPLY_I64] = 0,
    [ORSO_OP_DIVIDE_I64] = 0,

    [ORSO_OP_ADD_F64] = 0,
    [ORSO_OP_SUBTRACT_F64] = 0,
    [ORSO_OP_MULTIPLY_F64] = 0,
    [ORSO_OP_DIVIDE_F64] = 0,

    [ORSO_OP_NEGATE_I64] = 0,
    [ORSO_OP_NEGATE_F64] = 0,

    [ORSO_OP_EQUAL_I64] = 0,
    [ORSO_OP_LESS_I64] = 0,
    [ORSO_OP_GREATER_I64] = 0,

    [ORSO_OP_EQUAL_F64] = 0,
    [ORSO_OP_LESS_F64] = 0,
    [ORSO_OP_GREATER_F64] = 0,

    [ORSO_OP_EQUAL_SYMBOL] = -2,

    [ORSO_OP_EQUAL_STRING] = -2,
    [ORSO_OP_CONCAT_STRING] = -1,

    [ORSO_OP_LOGICAL_NOT] = 0,

    [ORSO_OP_PUSH_1] = 0,
    [ORSO_OP_PUSH_0] = 0,
    [ORSO_OP_PUSH_NULL_UNION] = 0,

    [ORSO_OP_CONSTANT] = 0,

    [ORSO_OP_SET_LOCAL] = 0,
    [ORSO_OP_GET_LOCAL] = 0,

    [ORSO_OP_SET_LOCAL_UNION] = 0,
    [ORSO_OP_GET_LOCAL_UNION] = 0,
    
    [ORSO_OP_DEFINE_GLOBAL] = 0,
    [ORSO_OP_SET_GLOBAL_GC_TYPE] = 0,
    [ORSO_OP_GET_GLOBAL] = 0,
    [ORSO_OP_SET_GLOBAL] = 0,

    [ORSO_OP_DEFINE_GLOBAL_UNION] = 0,
    [ORSO_OP_GET_GLOBAL_UNION] = 0,
    [ORSO_OP_SET_GLOBAL_UNION] = 0,

    [ORSO_OP_PUT_IN_UNION] = 0,
    [ORSO_OP_NARROW_UNION] = 0,
    [ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE] = 0,
    [ORSO_OP_UPDATE_STACK_GC_TYPE] = 0,

    [ORSO_OP_JUMP_IF_FALSE] = 0,
    [ORSO_OP_JUMP_IF_TRUE] = 0,
    [ORSO_OP_JUMP] = 0,

    [ORSO_OP_PRINT_EXPR] = -2,

    [ORSO_OP_RETURN] = 0,
};

static void compiler_init(Compiler* compiler) {
    compiler->locals = NULL;
    compiler->scope_depth = 0;
    compiler->count = 0;

    compiler->current_stack_size = 0;
    compiler->current_object_stack_size = 0;
    compiler->max_stack_size = 0;
}

static void compiler_free(Compiler* compiler) {
    sb_free(compiler->locals);
    compiler->locals = NULL;
    compiler->count = 0;

    compiler->current_stack_size = 0;
    compiler->current_object_stack_size = 0;
    compiler->max_stack_size = 0;
}

static void emit_instruction(const OrsoOPCode op_code, Compiler* compiler, Chunk* chunk, i32 line) {
    chunk_write(chunk, op_code, line);

    compiler->current_stack_size += OP_STACK_EFFECT[op_code];
    compiler->current_object_stack_size += OP_OBJECT_STACK_EFFECT[op_code];
    compiler->max_stack_size = compiler->current_stack_size > compiler->max_stack_size 
        ? compiler->current_stack_size : compiler->max_stack_size;
}

static void emit_constant(Compiler* compiler, Chunk* chunk, OrsoSlot slot, i32 line, bool is_ptr) {
    u32 index = chunk_add_constant(chunk, slot, is_ptr);
    emit_instruction(ORSO_OP_CONSTANT, compiler, chunk, line);

    ASSERT(index < 0xFFFFFF, "index must be less than the largest 24 bit unsigned int.");
    byte b1, b2, b3;
    ORSO_u24_to_u8s(index, b1, b2, b3);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
    chunk_write(chunk, b3, line);

    if (is_ptr) {
        emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, compiler, chunk, line);
    }
}

static i32 emit_jump(OrsoOPCode op_code, Compiler* compiler, Chunk* chunk, i32 line) {
    emit_instruction(op_code, compiler, chunk, line);
    chunk_write(chunk, 0xFF, line);
    chunk_write(chunk, 0xFF, line);
    return sb_count(chunk->code) - 2;
}

static void patch_jump(Chunk* chunk, i32 offset) {
    i32 jump = sb_count(chunk->code) - offset - 2;

    if (jump > UINT16_MAX) {
        ASSERT(false, "TODO");
    }

    byte b1, b2;
    ORSO_u16_to_u8s(jump, b1, b2);
    chunk->code[offset] = b1;
    chunk->code[offset + 1] = b2;
}

static void emit_instruction3(OrsoOPCode op_code, Compiler* compiler, u32 index, Chunk* chunk, i32 line) {
    emit_instruction(op_code, compiler, chunk, line);

    ASSERT(index < 0xFFFFFF, "index must be less than the largest 24 bit unsigned int.");

    byte b1, b2, b3;
    ORSO_u24_to_u8s(index, b1, b2, b3);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
    chunk_write(chunk, b3, line);
}

static void emit_put_in_union(Compiler* compiler, OrsoTypeKind type_kind, Chunk* chunk, i32 line) {
    emit_instruction(ORSO_OP_PUT_IN_UNION, compiler, chunk, line);

    ASSERT(type_kind < ORSO_TYPE_MAX, "type kind must be smaller than max (around 65000)");
    byte b1, b2;
    ORSO_TypeKind_to_u8s(type_kind, b1, b2);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
}

static void emit_type_kind_convert(Compiler* compiler, OrsoTypeKind from_type_kind, OrsoTypeKind to_type_kind, Chunk* chunk, i32 line) {
    bool include_bool = true;
    if (orso_is_float_type_kind(from_type_kind) && (orso_is_integer_type_kind(to_type_kind, include_bool))) {
        emit_instruction(ORSO_OP_F64_TO_I64, compiler, chunk, line);
    } else if (orso_is_integer_type_kind(from_type_kind, include_bool) && orso_is_float_type_kind(to_type_kind)) {
        emit_instruction(ORSO_OP_I64_TO_F64, compiler, chunk, line);
    } else {
        // Unreachable
    }
}

static void emit_storage_type_convert(Compiler* compiler, Chunk* chunk,  OrsoType source, OrsoType destination, i32 line) {
    if (ORSO_TYPE_IS_UNION(source) && ORSO_TYPE_IS_SINGLE(destination)) {
        emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, line);
    } else if (ORSO_TYPE_IS_SINGLE(source) && ORSO_TYPE_IS_UNION(destination)) {
        emit_put_in_union(compiler, (OrsoTypeKind)source.one, chunk, line);
    }

    if (!orso_is_gc_type(source) && orso_is_gc_type(destination)) {
        emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, line);
    }

    ASSERT(!orso_is_gc_type(source) || orso_is_gc_type(destination), "cannot convert from gc type to non-gc type");
}

static void emit_pop(Compiler* compiler, Chunk* chunk, OrsoType type, i32 line) {
    if (orso_is_gc_type(type)) {
        emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, line);
    }

    emit_instruction(ORSO_OP_POP, compiler, chunk, line);

    if (ORSO_TYPE_IS_UNION(type)) {
        emit_instruction(ORSO_OP_POP, compiler, chunk, line);
    }
}

static i32 add_global(OrsoVM* vm, Token* name, i32 slot_count, bool is_gc_type, bool is_union) {
    OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc, name->start, name->length, &vm->symbols);

    OrsoSlot index_slot;
    ASSERT(!orso_symbol_table_get(&vm->globals.name_to_index, identifier, &index_slot), "double global definition");

    index_slot = ORSO_SLOT_I(sb_count(vm->globals.values), ORSO_TYPE_ONE(ORSO_TYPE_INT32));
    orso_symbol_table_set(&vm->globals.name_to_index, identifier, index_slot);

    for (i32 i = 0; i < slot_count; i++) {
        sb_push(vm->globals.values, ORSO_SLOT_I(0, ORSO_TYPE_ONE(ORSO_TYPE_INVALID)));
    }

    i32 index = index_slot.as.i;

    if (is_gc_type) {
        sb_push(vm->globals.gc_values_indices, ((OrsoGCValueIndex){
            .is_object = false,
            .index = is_union ? index + 1 : index,
        }));
    }

    return index;
}

static i32 add_local(Compiler* compiler, Token name, i32 slot_count, bool is_gc_type) {
    ASSERT(slot_count > 0, "local must consume stack space");

    while (sb_count(compiler->locals) <= compiler->count) {
        sb_push(compiler->locals, (Local){ .depth = 0 });
    }

    Local* local = &compiler->locals[compiler->count++];
    local->name = name;
    local->depth = compiler->scope_depth;
    local->slot_count = slot_count;
    local->is_gc_type = is_gc_type;

    local->stack_position = compiler->current_stack_size - (slot_count);
    if (is_gc_type) {
        local->object_stack_position = compiler->current_object_stack_size - 1;
    } else {
        local->object_stack_position = -1;
    }

    return compiler->count - 1;
}

static FORCE_INLINE bool is_global_scope(Compiler* compiler) {
    return compiler->scope_depth == 0;
}

static i32 declare_global_variable(OrsoVM* vm, Token* name, i32 slot_count, bool is_gc_type, bool is_union) {
    return add_global(vm, name, slot_count, is_gc_type, is_union);
}

static i32 declare_local_variable(Compiler* compiler, Token* name, i32 slot_count, bool is_gc_type) {
    return add_local(compiler, *name, slot_count, is_gc_type);
}

static i32 retrieve_global_variable(OrsoVM* vm, Token* name) {
    OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc, name->start, name->length, &vm->symbols);
    OrsoSlot value;
    if (!orso_symbol_table_get(&vm->globals.name_to_index, identifier, &value)) {
        return -1;
    }

    return value.as.i;
}

static bool identifiers_equal(Token* a, Token* b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->start, b->start, a->length) == 0;
}

static i32 retrieve_local_variable(Compiler* compiler, Token* name) {
    for (i32 i = compiler->count - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiers_equal(name, &local->name)) {
            return i;
        }
    }

    return -1;
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
            slot = ORSO_SLOT_I(0, ORSO_TYPE_ONE(ORSO_TYPE_INVALID));
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

static void begin_scope(Compiler* compiler) {
    compiler->scope_depth++;
}

static void end_scope(Compiler* compiler, Chunk* chunk, i32 line) {
#define IS_RETURN_VARIABLE(INDEX) (compiler->locals[INDEX].name.length == 0)

    compiler->scope_depth--;
    while (compiler->count > 0) {
        compiler->count--;
        Local* local = &compiler->locals[compiler->count];

        if (IS_RETURN_VARIABLE(compiler->count)) {
            break;
        }

        for (i32 i = 0; i < local->slot_count; i++) {
            emit_instruction(ORSO_OP_POP, compiler, chunk, line);
        }

        if (local->is_gc_type) {
            emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, line);
        }
    }

#undef IS_RETURN_VARIABLE
}

static void declaration(OrsoVM* vm, Compiler* compiler, OrsoDeclarationNode* declaration, Chunk* chunk);

static void expression(OrsoVM* vm, Compiler* compiler, OrsoExpressionNode* expression_node, Chunk* chunk) {
#define EMIT_BINARY_OP(OP, TYPE) do { \
    emit_instruction(ORSO_OP_##OP##_##TYPE, compiler, chunk, operator.line); \
     } while(false)

#define EMIT_BINARY_OP_I64(OP) EMIT_BINARY_OP(OP, I64)
#define EMIT_BINARY_OP_F64(OP) EMIT_BINARY_OP(OP, F64)

#define EMIT_NOT() do { \
    emit_instruction(ORSO_OP_LOGICAL_NOT, compiler, chunk, operator.line); \
} while(false)

#define EMIT_NEGATE(TYPE) do { \
    emit_instruction(ORSO_OP_NEGATE_##TYPE, compiler, chunk, operator.line); \
} while (false)

    switch(expression_node->type) {
        case EXPRESSION_BINARY: {
            Token operator = expression_node->expr.binary.operator;
            OrsoExpressionNode* left = expression_node->expr.binary.left;
            OrsoExpressionNode* right = expression_node->expr.binary.right;
            expression(vm, compiler, left, chunk);

            if (ORSO_TYPE_IS_UNION(left->value_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, left->start.line);
            }

            expression(vm, compiler, right, chunk);

            if (ORSO_TYPE_IS_UNION(right->value_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, right->start.line);
            }

            if (orso_is_integer_type_kind(left->narrowed_value_type.one, true)) {
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

            } else if (orso_is_float_type_kind(left->narrowed_value_type.one)) {
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

            } else if (left->narrowed_value_type.one == ORSO_TYPE_STRING) {
                switch (operator.type) {
                    case TOKEN_PLUS: EMIT_BINARY_OP(CONCAT, STRING); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); EMIT_NOT(); break;
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); break;
                    default: break; // Unreachable
                }

            } else if (left->narrowed_value_type.one == ORSO_TYPE_SYMBOL) {
                switch (operator.type) {
                    case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); break;
                    case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); EMIT_NOT(); break;
                    default: break; // Unreachable
                }

            }

            break;
        }

        case EXPRESSION_UNARY: {
            expression(vm, compiler, expression_node->expr.unary.operand, chunk);
            if (ORSO_TYPE_IS_SINGLE(expression_node->value_type)
            && ORSO_TYPE_IS_UNION(expression_node->expr.unary.operand->value_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, expression_node->start.line);
            }

            OrsoExpressionNode* unary = expression_node->expr.unary.operand;
            Token operator = expression_node->expr.unary.operator;

            if (orso_is_integer_type_kind(unary->narrowed_value_type.one, true)) {
                switch (operator.type) {
                    case TOKEN_MINUS: EMIT_NEGATE(I64); break;
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: UNREACHABLE();
                }
            } else if (orso_is_float_type_kind(unary->narrowed_value_type.one)) {
                switch (operator.type) {
                    case TOKEN_MINUS: EMIT_NEGATE(F64); break;
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: UNREACHABLE();
                }
            } else if (unary->value_type.one == ORSO_TYPE_NULL) {
                switch (operator.type) {
                    case TOKEN_NOT: EMIT_NOT(); break;
                    default: UNREACHABLE();
                }
            }
            break;
        }

        case EXPRESSION_GROUPING: {
            expression(vm, compiler, expression_node->expr.grouping.expression, chunk);
            if (ORSO_TYPE_IS_UNION(expression_node->expr.grouping.expression->value_type)
                && ORSO_TYPE_IS_SINGLE(expression_node->value_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, expression_node->end.line);
            }
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
                        emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, expression_node->expr.primary.token.line);
                    } else if (expression_node->expr.primary.constant.as.i == 1) {
                        emit_instruction(ORSO_OP_PUSH_1, compiler, chunk, expression_node->expr.primary.token.line);
                    } else {
                        OrsoSlot slot = expression_node->expr.primary.constant;
#ifdef DEBUG_TRACE_EXECUTION
                        slot.type = expression_node->value_type;
#endif
                        emit_constant(compiler, chunk, slot, expression_node->expr.primary.token.line, orso_is_gc_type(expression_node->value_type));
                    }
                    break;
                }
                case ORSO_TYPE_STRING: {
                    OrsoString* string = orso_new_string_from_cstrn(&vm->gc, expression_node->expr.primary.token.start + 1, expression_node->expr.primary.token.length - 2);
                    OrsoSlot slot = ORSO_SLOT_P(string, expression_node->value_type);

                    emit_constant(compiler, chunk, slot, expression_node->expr.primary.token.line, true);
                    break;
                }
                case ORSO_TYPE_SYMBOL: {
                    OrsoSymbol* symbol = orso_new_symbol_from_cstrn(&vm->gc, expression_node->expr.primary.token.start + 1, expression_node->expr.primary.token.length - 2, &vm->symbols);
                    OrsoSlot slot = ORSO_SLOT_P(symbol, expression_node->value_type);

                    emit_constant(compiler, chunk, slot, expression_node->expr.primary.token.line, true);
                    break;
                }
                default: break; // Unreachable
            }
            break;
        }

        case EXPRESSION_VARIABLE: {
            Token identifier_token = expression_node->expr.variable.name;
            i32 index = retrieve_local_variable(compiler, &identifier_token);

            OrsoOPCode get_op;
            OrsoOPCode get_union_op;

            if (index < 0) {
                index = retrieve_global_variable(vm, &identifier_token);
                ASSERT(index >= 0, "global must be defined.");

                get_op = ORSO_OP_GET_GLOBAL;
                get_union_op = ORSO_OP_GET_GLOBAL_UNION;
            } else {
                index = compiler->locals[index].stack_position;
                get_op = ORSO_OP_GET_LOCAL;
                get_union_op = ORSO_OP_GET_LOCAL_UNION;
            }

            if (ORSO_TYPE_IS_SINGLE(expression_node->value_type)) {
                emit_instruction3(get_op, compiler, index, chunk, expression_node->start.line);
            } else {
                emit_instruction3(get_union_op, compiler, index, chunk, expression_node->start.line);
            }

            if (orso_is_gc_type(expression_node->value_type)) {
                emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, compiler, chunk, expression_node->start.line);

                if (ORSO_TYPE_IS_UNION(expression_node->value_type)) {
                    emit_instruction3(ORSO_OP_UPDATE_STACK_GC_TYPE, compiler, compiler->current_object_stack_size - 1, chunk, expression_node->end.line);
                }
            }
            break;
        }

        case EXPRESSION_ASSIGNMENT: {
            Token identifier_token = expression_node->expr.assignment.variable_name;
            
            expression(vm, compiler, expression_node->expr.assignment.right_side, chunk);
            OrsoType right_side_type = expression_node->expr.assignment.right_side->value_type;
            if (ORSO_TYPE_IS_UNION(expression_node->value_type) && ORSO_TYPE_IS_SINGLE(right_side_type)) {
                emit_put_in_union(compiler, (OrsoTypeKind)right_side_type.one, chunk, expression_node->start.line);
            } else if (ORSO_TYPE_IS_SINGLE(expression_node->value_type) && ORSO_TYPE_IS_UNION(right_side_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, expression_node->start.line);
            }

            if (orso_is_gc_type(expression_node->value_type) && !orso_is_gc_type(right_side_type)) {
                emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, expression_node->start.line);
            }

            OrsoOPCode set_op;
            OrsoOPCode set_union_op;

            i32 index = retrieve_local_variable(compiler, &identifier_token);
            i32 local_index;
            if (index < 0) {
                index = retrieve_global_variable(vm, &identifier_token);
                ASSERT(index >= 0, "global must be defined.");

                set_op = ORSO_OP_SET_GLOBAL;
                set_union_op = ORSO_OP_SET_GLOBAL_UNION;
            } else {
                local_index = index;
                index = compiler->locals[index].stack_position;
                set_op = ORSO_OP_SET_LOCAL;
                set_union_op = ORSO_OP_SET_LOCAL_UNION;
            }

            if (ORSO_TYPE_IS_SINGLE(expression_node->value_type)) {
                emit_instruction3(set_op, compiler, index, chunk, expression_node->start.line);
            } else {
                if (orso_is_gc_type(expression_node->value_type)) {
                    if (set_union_op == ORSO_OP_SET_GLOBAL_UNION) {
                        const i32 gc_index = find_global_gc_index(vm, index + 1);
                        ASSERT(gc_index >= 0, "gc_index must exist.");
                        emit_instruction3(ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE, compiler, gc_index, chunk, expression_node->start.line);
                    } else {
                        const i32 object_stack_position = compiler->locals[local_index].object_stack_position;
                        ASSERT(object_stack_position >= 0, "must be on object stack.");
                        emit_instruction3(ORSO_OP_UPDATE_STACK_GC_TYPE, compiler, object_stack_position, chunk, expression_node->start.line);
                    }
                }

                emit_instruction3(set_union_op, compiler, index, chunk, expression_node->start.line);
            }
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            OrsoExpressionNode* operand = expression_node->expr.cast.operand;
            expression(vm, compiler, operand, chunk);
            emit_type_kind_convert(compiler, operand->value_type.one, expression_node->value_type.one, chunk, operand->start.line);
            break;
        }

        case EXPRESSION_BLOCK: {
            begin_scope(compiler);
            OrsoDeclarationNode* final_expression_statement = expression_node->expr.block.final_expression_statement;

            // create local variable at the beginning of the stack to hold block return value
            OrsoType return_variable_type = final_expression_statement == NULL ?
                    ORSO_TYPE_ONE(ORSO_TYPE_NULL) : final_expression_statement->decl.statement->stmt.expression->value_type;
            Token return_identifier = { .length = 0, .line = expression_node->start.line, .start = "", .type = TOKEN_IDENTIFIER };
            i32 return_value_slot_count = ORSO_TYPE_IS_UNION(return_variable_type) ? 2 : 1;

            if (ORSO_TYPE_IS_UNION(return_variable_type)) {
                emit_instruction(ORSO_OP_PUSH_NULL_UNION, compiler, chunk, expression_node->start.line);
            } else {
                emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, expression_node->start.line);
            }

            if (orso_is_gc_type(return_variable_type)) {
                emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, compiler, chunk, expression_node->start.line);
            }

            i32 return_variable_index = declare_local_variable(compiler, &return_identifier, return_value_slot_count, orso_is_gc_type(return_variable_type));

            // regular declaration stuff
            for (i32 i = 0; i < sb_count(expression_node->expr.block.declarations) - (final_expression_statement != NULL); i++) {
                declaration(vm, compiler, expression_node->expr.block.declarations[i], chunk);
            }

            // store final expression in first auto-generated local variable
            {
                if (final_expression_statement != NULL) {
                    expression(vm, compiler, final_expression_statement->decl.statement->stmt.expression, chunk);
                    i32 stack_position = compiler->locals[return_variable_index].stack_position;

                    if (ORSO_TYPE_IS_UNION(return_variable_type)) {
                        if (compiler->locals[return_variable_index].is_gc_type) {
                            const u32 object_stack_position = compiler->locals[return_variable_index].object_stack_position;
                            emit_instruction3(ORSO_OP_UPDATE_STACK_GC_TYPE, compiler, object_stack_position, chunk, expression_node->end.line);
                        }

                        emit_instruction3(ORSO_OP_SET_LOCAL_UNION, compiler, stack_position, chunk, expression_node->end.line);

                        emit_instruction(ORSO_OP_POP, compiler, chunk, expression_node->end.line);
                        emit_instruction(ORSO_OP_POP, compiler, chunk, expression_node->end.line);

                        if (orso_is_gc_type(return_variable_type)) {
                            emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, expression_node->end.line);
                        }
                    } else {
                        emit_instruction3(ORSO_OP_SET_LOCAL, compiler, stack_position, chunk, expression_node->end.line);
                        emit_instruction(ORSO_OP_POP, compiler, chunk, expression_node->end.line);
                        if (orso_is_gc_type(return_variable_type)) {
                            emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, expression_node->end.line);
                        }
                    }
                } else {
                    emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, expression_node->end.line);
                    emit_instruction3(ORSO_OP_SET_LOCAL, compiler, return_variable_index, chunk, expression_node->end.line);
                    emit_instruction(ORSO_OP_POP, compiler, chunk, expression_node->end.line);
                }
            }

            end_scope(compiler, chunk, expression_node->end.line);
            break;
        }
        
        case EXPRESSION_IFELSE: {
            OrsoExpressionNode* condition = expression_node->expr.ifelse.condition;

            expression(vm, compiler, condition, chunk);

            i32 then_jump = emit_jump(ORSO_OP_JUMP_IF_FALSE, compiler, chunk, condition->end.line);

            emit_pop(compiler, chunk, condition->value_type, condition->end.line);

            expression(vm, compiler, expression_node->expr.ifelse.then, chunk);

            emit_storage_type_convert(compiler, chunk,
                    expression_node->expr.ifelse.then->value_type, expression_node->value_type,
                    expression_node->expr.ifelse.then->end.line);

            i32 else_jump = emit_jump(ORSO_OP_JUMP, compiler, chunk, expression_node->expr.ifelse.then->end.line);

            emit_pop(compiler, chunk, condition->value_type, condition->end.line);


            patch_jump(chunk, then_jump);

            if (expression_node->expr.ifelse.else_) {
                expression(vm, compiler, expression_node->expr.ifelse.else_, chunk);

                emit_storage_type_convert(compiler, chunk,
                        expression_node->expr.ifelse.else_->value_type, expression_node->value_type,
                        expression_node->expr.ifelse.else_->end.line);
            } else {
                emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, expression_node->end.line);

                emit_storage_type_convert(compiler, chunk,
                        ORSO_TYPE_ONE(ORSO_TYPE_NULL), expression_node->value_type,
                        expression_node->end.line);
            }

            patch_jump(chunk, else_jump);

            break;
        }

        default:
            UNREACHABLE();
    }

#undef EMIT_NEGATE
#undef EMIT_NOT
#undef EMIT_BINARY_OP_F64
#undef EMIT_BINARY_OP_I64
#undef EMIT_BINARY_OP
}

static void statement(OrsoVM* vm, Compiler* compiler, OrsoStatementNode* statement, Chunk* chunk) {
    switch (statement->type) {
        case ORSO_STATEMENT_EXPRESSION: {
            OrsoExpressionNode* expression_ = statement->stmt.expression;
            expression(vm, compiler, expression_, chunk);

            emit_instruction(ORSO_OP_POP, compiler, chunk, statement->start.line);

            if (orso_is_gc_type(expression_->value_type)) {
                emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, statement->start.line);
            }

            if (ORSO_TYPE_IS_UNION(expression_->value_type)) {
                emit_instruction(ORSO_OP_POP, compiler, chunk, statement->start.line);
            }
            break;
        }
        
        case ORSO_STATEMENT_PRINT:
        case ORSO_STATEMENT_PRINT_EXPR: {
                expression(vm, compiler, statement->stmt.expression, chunk);

                if (ORSO_TYPE_IS_SINGLE(statement->stmt.expression->value_type)) {
                    emit_put_in_union(compiler, (OrsoTypeKind)statement->stmt.expression->value_type.one, chunk, statement->start.line);
                }

                if (!orso_is_gc_type(statement->stmt.expression->value_type)) {
                    emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, statement->start.line);
                }

                Token start = statement->stmt.expression->start;
                Token end = statement->stmt.expression->end;

                OrsoString* expression_string = orso_new_string_from_cstrn(&vm->gc, start.start, (end.start + end.length) - start.start);

                OrsoSlot slot = ORSO_SLOT_P(expression_string, ORSO_TYPE_ONE(ORSO_TYPE_STRING));
                emit_constant(compiler, chunk, slot, start.line, true);

                OrsoSlot value_type = ORSO_SLOT_U(statement->stmt.expression->value_type.one, ORSO_TYPE_ONE(ORSO_TYPE_TYPE));
                emit_constant(compiler, chunk, value_type, start.line, false);

                if (statement->type == ORSO_STATEMENT_PRINT_EXPR) {
                    emit_instruction(ORSO_OP_PRINT_EXPR, compiler, chunk, start.line);
                } else {
                    emit_instruction(ORSO_OP_PRINT, compiler, chunk, start.line);
                }
            break;
        }
        case ORSO_STATEMENT_NONE: break; // Unreachable
    }
}

static void var_declaration(OrsoVM* vm, Compiler* compiler, OrsoVarDeclarationNode* var_declaration, Chunk* chunk) {
    bool is_gc_type = orso_is_gc_type(var_declaration->var_type);

    if (var_declaration->expression != NULL) {
        expression(vm, compiler, var_declaration->expression, chunk);

        if (ORSO_TYPE_IS_UNION(var_declaration->var_type) && ORSO_TYPE_IS_SINGLE(var_declaration->expression->value_type)) {
            emit_put_in_union(compiler, (OrsoTypeKind)var_declaration->expression->value_type.one, chunk, var_declaration->start.line);
        } else if (ORSO_TYPE_IS_SINGLE(var_declaration->var_type) && ORSO_TYPE_IS_UNION(var_declaration->expression->value_type)) {
            emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, var_declaration->start.line);
        }

        // This is for local unions since they stay on the stack and have their value tracked
        if (is_gc_type && !orso_is_gc_type(var_declaration->expression->value_type)) {
            emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, var_declaration->start.line);
        }

    } else {
        if (ORSO_TYPE_IS_UNION(var_declaration->var_type)) {
            ASSERT(orso_type_has_kind(var_declaration->var_type, ORSO_TYPE_NULL), "default type only allowed for void type unions.");
            emit_instruction(ORSO_OP_PUSH_NULL_UNION, compiler, chunk, var_declaration->start.line);

            if (is_gc_type) {
                emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, var_declaration->start.line);
            }
        } else {
            OrsoSlot slot = zero_value(var_declaration->var_type.one, &vm->gc, &vm->symbols);
            emit_constant(compiler, chunk, slot, var_declaration->start.line, is_gc_type);
        }
    }

    Token identifier_token = var_declaration->variable_name;

    if (ORSO_TYPE_IS_SINGLE(var_declaration->var_type)) {
        if (is_global_scope(compiler)) {
            i32 index = declare_global_variable(vm, &identifier_token, 1, is_gc_type, false);

            if (is_gc_type) {
                u32 gc_index = find_global_gc_index(vm, index);
                emit_instruction3(ORSO_OP_SET_GLOBAL_GC_TYPE, compiler, gc_index, chunk, var_declaration->start.line);
                emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, var_declaration->start.line);
            }

            emit_instruction3(ORSO_OP_DEFINE_GLOBAL, compiler, index, chunk, var_declaration->start.line);

        } else {
            declare_local_variable(compiler, &identifier_token, 1, is_gc_type);
        }

    } else {
        if (is_global_scope(compiler)) {
            i32 index = declare_global_variable(vm, &identifier_token, 2, is_gc_type, true);

            if (is_gc_type) {
                const u32 gc_values_index = find_global_gc_index(vm, index + 1);
                emit_instruction3(ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE, compiler, gc_values_index, chunk, var_declaration->start.line);
                emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, var_declaration->start.line);
            }

            emit_instruction3(ORSO_OP_DEFINE_GLOBAL_UNION, compiler, index, chunk, var_declaration->start.line);

        } else {
            i32 index = declare_local_variable(compiler, &identifier_token, 2, is_gc_type);

            if (compiler->locals[index].is_gc_type) {
                const u32 object_stack_index = compiler->locals[index].object_stack_position;
                emit_instruction3(ORSO_OP_UPDATE_STACK_GC_TYPE, compiler, object_stack_index, chunk, var_declaration->start.line);
            }
        }
    }
}

static void declaration(OrsoVM* vm, Compiler* compiler, OrsoDeclarationNode* declaration, Chunk* chunk) {
    switch (declaration->type) {
        case ORSO_DECLARATION_STATEMENT: statement(vm, compiler, declaration->decl.statement, chunk); break;
        case ORSO_DECLARATION_VAR: var_declaration(vm, compiler, declaration->decl.var, chunk); break;
        case ORSO_DECLARATION_NONE: break; // Unreachable
    }
}

bool orso_generate_code(OrsoVM* vm, OrsoAST* ast, Chunk* chunk) {
    Compiler compiler;
    compiler_init(&compiler);

    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        declaration(vm, &compiler, ast->declarations[i], chunk);
    }

    compiler_free(&compiler);

    emit_instruction(ORSO_OP_RETURN, &compiler, chunk, ast->declarations[sb_count(ast->declarations) - 1]->end.line);

#ifdef DEBUG_PRINT
    chunk_disassemble(chunk, "code");
#endif

    return true;
}
