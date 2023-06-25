#include "codegen.h"

#ifdef DEBUG_PRINT
#include "debug.h"
#endif

#include "opcodes.h"
#include "instructions.h"
#include "sb.h"
#include "type_set.h"

typedef struct {
    Token name;
    i32 depth;
    byte slot_count;
    i32 stack_position;
    bool is_gc_type;
    i32 object_stack_position;
} Local;

typedef enum {
    ORSO_FUNCTION_TYPE_FUNCTION,
    ORSO_FUNCTION_TYPE_SCRIPT,
} OrsoCompilerFunctionType;

typedef struct Compiler {
    bool literals_only;
    OrsoFunction* function;
    OrsoCompilerFunctionType function_type;

    Local* locals;
    i32 locals_count;
    i32 scope_depth;
    i32 current_stack_size;
    i32 current_object_stack_size;
    i32 max_stack_size;
} Compiler;

static i32 get_stack_effect(OrsoOPCode op_code) {
    switch (op_code) {
        case ORSO_OP_POP: return -1;
        case ORSO_OP_POP_SCOPE: return 0;
        case ORSO_OP_POP_TOP_OBJECT: return 0;

        case ORSO_OP_PUSH_TOP_OBJECT: return 0;
        case ORSO_OP_PUSH_TOP_OBJECT_NULL: return 0;

        case ORSO_OP_I64_TO_F64: return 0;
        case ORSO_OP_F64_TO_I64: return 0;

        case ORSO_OP_ADD_I64: return -1;
        case ORSO_OP_SUBTRACT_I64: return -1;
        case ORSO_OP_MULTIPLY_I64: return -1;
        case ORSO_OP_DIVIDE_I64: return -1;

        case ORSO_OP_ADD_F64: return -1;
        case ORSO_OP_SUBTRACT_F64: return -1;
        case ORSO_OP_MULTIPLY_F64: return -1;
        case ORSO_OP_DIVIDE_F64: return -1;

        case ORSO_OP_NEGATE_I64: return 0;
        case ORSO_OP_NEGATE_F64: return 0;

        case ORSO_OP_EQUAL_I64: return -1;
        case ORSO_OP_LESS_I64: return -1;
        case ORSO_OP_GREATER_I64: return -1;

        case ORSO_OP_EQUAL_F64: return -1;
        case ORSO_OP_LESS_F64: return -1;
        case ORSO_OP_GREATER_F64: return -1;

        case ORSO_OP_EQUAL_SYMBOL: return -1;

        case ORSO_OP_EQUAL_STRING: return -1;
        case ORSO_OP_CONCAT_STRING: return -1;

        case ORSO_OP_LOGICAL_NOT: return 0;

        case ORSO_OP_PUSH_1: return 1;
        case ORSO_OP_PUSH_0: return 1;
        case ORSO_OP_PUSH_NULL_UNION: return 2;

        case ORSO_OP_CONSTANT: return 1;

        case ORSO_OP_SET_LOCAL: return 0;
        case ORSO_OP_GET_LOCAL: return 1;

        case ORSO_OP_SET_LOCAL_UNION: return 0;
        case ORSO_OP_GET_LOCAL_UNION: return 2;
        
        case ORSO_OP_DEFINE_GLOBAL: return -1;
        case ORSO_OP_SET_GLOBAL_GC_TYPE: return 0;
        case ORSO_OP_GET_GLOBAL: return 1;
        case ORSO_OP_SET_GLOBAL: return 0;

        case ORSO_OP_DEFINE_GLOBAL_UNION: return -2;
        case ORSO_OP_GET_GLOBAL_UNION: return 2;
        case ORSO_OP_SET_GLOBAL_UNION: return 0;

        case ORSO_OP_PUT_IN_UNION: return 0;
        case ORSO_OP_NARROW_UNION: return -1;
        case ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE: return 0;
        case ORSO_OP_UPDATE_STACK_GC_TYPE: return 0;

        case ORSO_OP_JUMP_IF_FALSE: return 0;
        case ORSO_OP_JUMP_IF_TRUE: return 0;
        case ORSO_OP_JUMP: return 0;
        case ORSO_OP_LOOP: return 0;

        case ORSO_OP_CALL: return 0;

        case ORSO_OP_PRINT_EXPR: return -4;
        case ORSO_OP_PRINT: return -4;

        case ORSO_OP_RETURN: return 0;
    }
}

static i32 get_object_stack_effect(OrsoOPCode op_code) {
    switch (op_code) {
        case ORSO_OP_POP: return 0;
        case ORSO_OP_POP_SCOPE: return 0;
        case ORSO_OP_POP_TOP_OBJECT: return -1;

        case ORSO_OP_PUSH_TOP_OBJECT: return 1;
        case ORSO_OP_PUSH_TOP_OBJECT_NULL: return 1;

        case ORSO_OP_I64_TO_F64: return 0;
        case ORSO_OP_F64_TO_I64: return 0;

        case ORSO_OP_ADD_I64: return 0;
        case ORSO_OP_SUBTRACT_I64: return 0;
        case ORSO_OP_MULTIPLY_I64: return 0;
        case ORSO_OP_DIVIDE_I64: return 0;

        case ORSO_OP_ADD_F64: return 0;
        case ORSO_OP_SUBTRACT_F64: return 0;
        case ORSO_OP_MULTIPLY_F64: return 0;
        case ORSO_OP_DIVIDE_F64: return 0;

        case ORSO_OP_NEGATE_I64: return 0;
        case ORSO_OP_NEGATE_F64: return 0;

        case ORSO_OP_EQUAL_I64: return 0;
        case ORSO_OP_LESS_I64: return 0;
        case ORSO_OP_GREATER_I64: return 0;

        case ORSO_OP_EQUAL_F64: return 0;
        case ORSO_OP_LESS_F64: return 0;
        case ORSO_OP_GREATER_F64: return 0;

        case ORSO_OP_EQUAL_SYMBOL: return -2;

        case ORSO_OP_EQUAL_STRING: return -2;
        case ORSO_OP_CONCAT_STRING: return -1;

        case ORSO_OP_LOGICAL_NOT: return 0;

        case ORSO_OP_PUSH_1: return 0;
        case ORSO_OP_PUSH_0: return 0;
        case ORSO_OP_PUSH_NULL_UNION: return 0;

        case ORSO_OP_CONSTANT: return 0;

        case ORSO_OP_SET_LOCAL: return 0;
        case ORSO_OP_GET_LOCAL: return 0;

        case ORSO_OP_SET_LOCAL_UNION: return 0;
        case ORSO_OP_GET_LOCAL_UNION: return 0;
        
        case ORSO_OP_DEFINE_GLOBAL: return 0;
        case ORSO_OP_SET_GLOBAL_GC_TYPE: return 0;
        case ORSO_OP_GET_GLOBAL: return 0;
        case ORSO_OP_SET_GLOBAL: return 0;

        case ORSO_OP_DEFINE_GLOBAL_UNION: return 0;
        case ORSO_OP_GET_GLOBAL_UNION: return 0;
        case ORSO_OP_SET_GLOBAL_UNION: return 0;

        case ORSO_OP_PUT_IN_UNION: return 0;
        case ORSO_OP_NARROW_UNION: return 0;
        case ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE: return 0;
        case ORSO_OP_UPDATE_STACK_GC_TYPE: return 0;

        case ORSO_OP_JUMP_IF_FALSE: return 0;
        case ORSO_OP_JUMP_IF_TRUE: return 0;
        case ORSO_OP_JUMP: return 0;
        case ORSO_OP_LOOP: return 0;

        case ORSO_OP_CALL: return 0;

        case ORSO_OP_PRINT_EXPR: return -2;
        case ORSO_OP_PRINT: return -2;

        case ORSO_OP_RETURN: return 0;
    }
}

static i32 add_local(Compiler* compiler, Token name, i32 slot_count, bool is_gc_type);

// A Token* is passed in instead of OrsoSymbol* because Token* cant be garbage collected and OrsoSymbol* can and would be.
static void compiler_init(Compiler* compiler, OrsoCompilerFunctionType function_type, OrsoVM* vm, OrsoType* creator_type) {
    compiler->literals_only = false;
    compiler->function = NULL;
    compiler->function_type = function_type;

    compiler->locals = NULL;
    compiler->scope_depth = 0;
    compiler->locals_count = 0;

    compiler->current_stack_size = 0;
    compiler->current_object_stack_size = 0;
    compiler->max_stack_size = 0;

    compiler->function = orso_new_function(&vm->gc);
    compiler->function->type = (OrsoFunctionType*)creator_type;
}

static void compiler_free(Compiler* compiler) {
    sb_free(compiler->locals);
    compiler->locals = NULL;
    compiler->locals_count = 0;

    compiler->current_stack_size = 0;
    compiler->current_object_stack_size = 0;
    compiler->max_stack_size = 0;
}

static void _apply_stack_effects(Compiler* compiler, i32 stack_effects, i32 object_stack_effects) {
    compiler->current_stack_size += stack_effects;
    compiler->current_object_stack_size += object_stack_effects;
    compiler->max_stack_size = compiler->current_stack_size > compiler->max_stack_size
        ? compiler->current_stack_size : compiler->max_stack_size;
}

static void emit_instruction(const OrsoOPCode op_code, Compiler* compiler, Chunk* chunk, i32 line) {
    chunk_write(chunk, op_code, line);
    _apply_stack_effects(compiler, get_stack_effect(op_code), get_object_stack_effect(op_code));
}

// TODO: Remove compiler from the parameters... instead try to bubble up the stack effect somehow
static void emit_constant(Compiler* compiler, Chunk* chunk, OrsoSlot slot, i32 line, bool is_gc_type) {
    u32 index = chunk_add_constant(chunk, slot, is_gc_type);
    emit_instruction(ORSO_OP_CONSTANT, compiler, chunk, line);

    ASSERT(index < 0xFFFFFF, "index must be less than the largest 24 bit unsigned int.");
    byte b1, b2, b3;
    ORSO_u24_to_u8s(index, b1, b2, b3);
    chunk_write(chunk, b1, line);
    chunk_write(chunk, b2, line);
    chunk_write(chunk, b3, line);

    if (is_gc_type) {
        emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, compiler, chunk, line);
    }
}

static i32 emit_jump(OrsoOPCode op_code, Compiler* compiler, Chunk* chunk, i32 line) {
    emit_instruction(op_code, compiler, chunk, line);
    chunk_write(chunk, 0xFF, line);
    chunk_write(chunk, 0xFF, line);
    return sb_count(chunk->code) - 2;
}

static void emit_loop(Compiler* compiler, Chunk* chunk, i32 line, u32 loop_start) {
    emit_instruction(ORSO_OP_LOOP, compiler, chunk, line);

    u32 offset = sb_count(chunk->code) - loop_start + 2;
    ASSERT(offset <= UINT16_MAX, "loop cant go back more than 2^16");
    byte a;
    byte b;
    ORSO_u16_to_u8s(offset, a, b);
    chunk_write(chunk, a, line);
    chunk_write(chunk, b, line);
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

static void emit_put_in_union(Compiler* compiler, OrsoType* type, Chunk* chunk, i32 line) {
    emit_constant(compiler, chunk, ORSO_SLOT_P(type, &OrsoTypeType), line, false);
    emit_instruction(ORSO_OP_PUT_IN_UNION, compiler, chunk, line);
}

static void emit_type_convert(Compiler* compiler, OrsoType* from_type, OrsoType* to_type, Chunk* chunk, i32 line) {
    bool include_bool = true;
    ASSERT(orso_type_is_number(from_type, include_bool) && orso_type_is_number(to_type, include_bool), "Implicit type conversion only works for number types right now.");
    if (orso_type_is_float(from_type) && orso_type_is_integer(to_type, include_bool)) {
        emit_instruction(ORSO_OP_F64_TO_I64, compiler, chunk, line);
    } else if (orso_type_is_integer(from_type, include_bool) && orso_type_is_float(to_type)) {
        emit_instruction(ORSO_OP_I64_TO_F64, compiler, chunk, line);
    } 
}

static void emit_storage_type_convert(Compiler* compiler, Chunk* chunk,  OrsoType* source, OrsoType* destination, i32 line) {
    if (ORSO_TYPE_IS_UNION(source) && !ORSO_TYPE_IS_UNION(destination)) {
        emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, line);
    } else if (!ORSO_TYPE_IS_UNION(source) && ORSO_TYPE_IS_UNION(destination)) {
        emit_put_in_union(compiler, source, chunk, line);
    }

    if (!orso_is_gc_type(source) && orso_is_gc_type(destination)) {
        emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, line);
    }

    ASSERT(!orso_is_gc_type(source) || orso_is_gc_type(destination), "cannot convert from gc type to non-gc type");
}

static void emit_pop(Compiler* compiler, Chunk* chunk, OrsoType* type, i32 line) {
    if (orso_is_gc_type(type)) {
        emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, line);
    }

    emit_instruction(ORSO_OP_POP, compiler, chunk, line);

    if (ORSO_TYPE_IS_UNION(type)) {
        emit_instruction(ORSO_OP_POP, compiler, chunk, line);
    }
}

static void emit_pop_scope(Compiler* compiler, Chunk* chunk, byte scope_size_slots, byte block_value_slots, bool block_value_is_gc_type, i32 line) {
    emit_instruction(ORSO_OP_POP_SCOPE, compiler, chunk, line);
    chunk_write(chunk, scope_size_slots, line);
    chunk_write(chunk, block_value_slots, line);

    _apply_stack_effects(compiler, -scope_size_slots, 0);

    if (block_value_is_gc_type) {
        emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, compiler, chunk, line);
    }
}

static void emit_call(Compiler* compiler, Chunk* chunk, OrsoFunctionType const * function_type, i32 line) {
    emit_instruction(ORSO_OP_CALL, compiler, chunk, line);

    i32 argument_slots = 0;
    i32 object_count = 0;
    for (i32 i = 0; i < function_type->argument_count; i++) {
        argument_slots += orso_type_slot_count(function_type->argument_types[i]);
        object_count += orso_is_gc_type(function_type->argument_types[i]);
    }
    byte a;
    byte b;
    ORSO_u16_to_u8s(argument_slots, a, b);
    chunk_write(chunk, a, line);
    chunk_write(chunk, b, line);
    chunk_write(chunk, object_count, line);

    i32 call_stack_effect = -argument_slots - 1 + orso_type_slot_count(function_type->return_type);
    i32 call_object_stack_effect = -object_count - 1 + (orso_is_gc_type(function_type->return_type) ? 1 : 0);
    _apply_stack_effects(compiler, call_stack_effect, call_object_stack_effect);
}

static void emit_return(Compiler* compiler, Chunk* chunk, OrsoType* type, i32 line) {
    emit_instruction(ORSO_OP_RETURN, compiler, chunk, line);

    // TODO: Figure out more robust way of doing this instead of casting to byte
    chunk_write(chunk, (byte)orso_type_slot_count(type), line);
}

static OrsoFunction* compiler_end(Compiler* compiler, Chunk* chunk, i32 line) {
    emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, line);
    emit_return(compiler, chunk, &OrsoTypeVoid, line);

#ifdef DEBUG_PRINT
    chunk_disassemble(chunk, "<TODO: put function name here somehow>");
#endif

    return compiler->function;
}

static i32 add_global(OrsoVM* vm, Token* name, i32 slot_count, bool is_gc_type, bool is_union) {
    OrsoSymbol* identifier = orso_new_symbol_from_cstrn(&vm->gc, name->start, name->length, &vm->symbols);

    OrsoSlot index_slot;
    ASSERT(!orso_symbol_table_get(&vm->globals.name_to_index, identifier, &index_slot), "double global definition");

    index_slot = ORSO_SLOT_I(sb_count(vm->globals.values), &OrsoTypeInteger32);
    orso_symbol_table_set(&vm->globals.name_to_index, identifier, index_slot);

    for (i32 i = 0; i < slot_count; i++) {
        sb_push(vm->globals.values, ORSO_SLOT_I(0, &OrsoTypeInvalid));
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

    while (sb_count(compiler->locals) <= compiler->locals_count) {
        sb_push(compiler->locals, (Local){ .depth = 0 });
    }

    Local* previous_local = compiler->locals_count > 0 ?
            &compiler->locals[compiler->locals_count - 1] : NULL;
    Local* local = &compiler->locals[compiler->locals_count++];
    local->name = name;
    local->depth = compiler->scope_depth;
    local->slot_count = slot_count;
    local->is_gc_type = is_gc_type;

    if (previous_local) {
        local->stack_position = previous_local->stack_position + previous_local->slot_count;
    } else {
        local->stack_position = 0;
    }

    ASSERT(local->stack_position == compiler->current_stack_size - slot_count,
            "The calculated stack position for the local must properly related to the tracked compiler stack");
    if (is_gc_type) {
        local->object_stack_position = local->stack_position + local->slot_count - 1;
    } else {
        local->object_stack_position = -1;
    }

    return compiler->locals_count - 1;
}

static FORCE_INLINE bool is_global_scope(Compiler* compiler) {
    return compiler->scope_depth == 0;
}

static i32 declare_global_entity(OrsoVM* vm, Token* name, i32 slot_count, bool is_gc_type, bool is_union) {
    return add_global(vm, name, slot_count, is_gc_type, is_union);
}

static i32 declare_local_entity(Compiler* compiler, Token* name, i32 slot_count, bool is_gc_type) {
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
    for (i32 i = compiler->locals_count - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiers_equal(name, &local->name)) {
            return i;
        }
    }

    return -1;
}

static i32 retrieve_variable(OrsoVM* vm, Compiler* compiler, Token* name, bool* is_local) {
    i32 index = retrieve_local_variable(compiler, name);
    i32 local_index;
    if (index < 0) {
        index = retrieve_global_variable(vm, name);
        ASSERT(index >= 0, "global must be defined.");

        *is_local = false;
        return index;
    } else {
        local_index = index;
        index = compiler->locals[index].stack_position;

        *is_local = true;
        return local_index;
    }
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

static void end_scope(Compiler* compiler, Chunk* chunk, OrsoType* block_value_type, i32 line) {
    compiler->scope_depth--;

    // pop left-over expression object marker
    bool block_value_is_gc_type = orso_is_gc_type(block_value_type);
    if (block_value_is_gc_type) {
        emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, line);
    }

    // pop all local object markers
    i32 total_local_slot_count = 0;
    while (compiler->locals_count > 0) {
        Local* local = &compiler->locals[compiler->locals_count - 1];
        if (local->depth <= compiler->scope_depth) {
            break;
        }

        total_local_slot_count += local->slot_count;
        compiler->locals_count--;

        if (local->is_gc_type) {
            emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, line);
        }
    }

    i32 block_value_slots = orso_type_slot_count(block_value_type);

    emit_pop_scope(compiler, chunk, total_local_slot_count, block_value_slots, block_value_is_gc_type, line);
}

static void declaration(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoDeclarationNode* declaration, Chunk* chunk);
static void expression(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoExpressionNode* expression_node, Chunk* chunk);

static OrsoType* gen_block(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, Chunk* chunk, OrsoBlock* block, bool is_function_body, i32 end_line) {
    OrsoDeclarationNode* final_expression_statement = block->final_expression_statement;

    if (!is_function_body) {
        for (i32 i = 0; i < sb_count(block->declarations) - (final_expression_statement != NULL); i++) {
            declaration(vm, compiler, ast, block->declarations[i], chunk);
        }

        OrsoType* return_value_type = &OrsoTypeInvalid;
        if (final_expression_statement) {
            OrsoExpressionNode* final_expression = final_expression_statement->decl.statement->stmt.expression;
            return_value_type = final_expression->value_type;
            expression(vm, compiler, ast, final_expression, chunk);
        } else {
            return_value_type = &OrsoTypeVoid;
            emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, end_line);
        }
    
        return return_value_type;
    } else {
        for (i32 i = 0; i < sb_count(block->declarations); i++) {
            declaration(vm, compiler, ast, block->declarations[i], chunk);
        }

        return &OrsoTypeVoid;
    }
}

static void define_entity(OrsoVM* vm, Compiler* compiler, Chunk* chunk, OrsoType* type, Token* name, i32 line) {
    bool is_gc_type = orso_is_gc_type(type);

    if (!ORSO_TYPE_IS_UNION(type)) {
        if (is_global_scope(compiler)) {
            i32 index = declare_global_entity(vm, name, 1, is_gc_type, false);

            if (is_gc_type) {
                u32 gc_index = find_global_gc_index(vm, index);
                emit_instruction3(ORSO_OP_SET_GLOBAL_GC_TYPE, compiler, gc_index, chunk, line);
                emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, line);
            }

            emit_instruction3(ORSO_OP_DEFINE_GLOBAL, compiler, index, chunk, line);

        } else {
            declare_local_entity(compiler, name, 1, is_gc_type);
        }

    } else {
        if (is_global_scope(compiler)) {
            i32 index = declare_global_entity(vm, name, 2, is_gc_type, true);

            if (is_gc_type) {
                const u32 gc_values_index = find_global_gc_index(vm, index + 1);
                emit_instruction3(ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE, compiler, gc_values_index, chunk, line);
                emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, line);
            }

            emit_instruction3(ORSO_OP_DEFINE_GLOBAL_UNION, compiler, index, chunk, line);

        } else {
            i32 index = declare_local_entity(compiler, name, 2, is_gc_type);

            if (compiler->locals[index].is_gc_type) {
                const u32 object_stack_index = compiler->locals[index].object_stack_position;
                emit_instruction3(ORSO_OP_UPDATE_STACK_GC_TYPE, compiler, object_stack_index, chunk, line);
            }
        }
    }
}

static void declare_local_function_definition(Compiler* compiler, OrsoFunction* function){ 
    // used in assert
    (void)function;
    // TODO: fix this assert, why does function->type need to be converted to type? Will it never be unresolved?
    ASSERT((OrsoType*)function->type != &OrsoTypeUnresolved, "Must have resolved type");

    Token empty = { .start = "", .length = 0 };
    declare_local_entity(compiler, &empty, 1, true);
}

static void function_expression(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoExpressionNode* function_defintion_expression, Chunk* chunk) {
    ASSERT(function_defintion_expression->value_type->kind == ORSO_TYPE_FUNCTION, "must be function if calling this");

    Compiler function_compiler;
    compiler_init(&function_compiler, ORSO_FUNCTION_TYPE_FUNCTION, vm, function_defintion_expression->value_type);

    declare_local_function_definition(&function_compiler, function_compiler.function);
    
    //Token empty = { .start = "", .length = 0 };
    //declare_local_entity(&function_compiler, &empty, 1, true);

    // if (is_global_scope(compiler)) {
    //     i32 global_index = declare_global_variable(vm, &function_declaration->name, 1, true, false);
    //     u32 global_gc_index = find_global_gc_index(vm, global_index);
    //     emit_instruction3(ORSO_OP_SET_GLOBAL_GC_TYPE, compiler, global_gc_index, chunk, function_declaration->start.line);

    //     emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, function_declaration->start.line);
    //     emit_instruction3(ORSO_OP_DEFINE_GLOBAL, compiler, global_index, chunk, function_declaration->start.line);
    // } else {
    //     declare_local_variable(compiler, &function_declaration->name, 1, true);
    // }


    begin_scope(&function_compiler);

    function_compiler.function->type = (OrsoFunctionType*)function_defintion_expression->value_type; //function_declaration->type;
    Chunk* function_chunk = &function_compiler.function->chunk;

    OrsoFunctionDefinition* function_definition = &function_defintion_expression->expr.function_definition;

    for (i32 i = 0; i < sb_count(function_definition->parameters); i++) {
        OrsoEntityDeclarationNode* parameter = function_definition->parameters[i];
        _apply_stack_effects(&function_compiler, orso_type_slot_count(parameter->type), orso_is_gc_type(parameter->type) ? 1 : 0);
        define_entity(vm, &function_compiler, function_chunk,
                parameter->type, &parameter->name, parameter->start.line);
    }

    gen_block(vm, &function_compiler, ast, function_chunk, &function_definition->block, true, function_defintion_expression->end.line);

    compiler_end(&function_compiler, function_chunk, function_defintion_expression->end.line);

    emit_constant(compiler, chunk, ORSO_SLOT_P(function_compiler.function, (OrsoType*)function_compiler.function->type), function_defintion_expression->start.line, true);
}

static void gen_primary(Compiler* compiler, Chunk* chunk, OrsoAST* ast, OrsoType* value_type, i32 value_index, i32 line) {
    ASSERT(value_index >= 0, "must be pointing to a contant value...");

    OrsoSlot value = ast->folded_constants[value_index];
    switch (value_type->kind) {
        case ORSO_TYPE_BOOL:
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64:
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64:
        case ORSO_TYPE_VOID: {
            if (value.as.i == 0) {
                emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, line);
            } else if (value.as.i == 1) {
                emit_instruction(ORSO_OP_PUSH_1, compiler, chunk, line);
            } else {
#ifdef DEBUG_TRACE_EXECUTION
                value.type = value_type;
#endif
                emit_constant(compiler, chunk, value, line, orso_is_gc_type(value_type));
            }
            break;
        }
        case ORSO_TYPE_STRING: {
        case ORSO_TYPE_SYMBOL:
            emit_constant(compiler, chunk, value, line, true);
            break;
        }
        default: UNREACHABLE();
    }
}

static void expression(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoExpressionNode* expression_node, Chunk* chunk) {
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

#ifdef DEBUG
    bool requires_entity = expression_node->type == EXPRESSION_ASSIGNMENT || expression_node->type == EXPRESSION_ENTITY;
    ASSERT(!compiler->literals_only || (expression_node->folded_value_index >= 0 || !requires_entity), "compiler is assuming folded values only for ALL expressions");
#endif

    if (compiler->literals_only && expression_node->folded_value_index >= 0) {
        gen_primary(compiler, chunk, ast,
                expression_node->value_type,
                expression_node->folded_value_index, expression_node->start.line);
        return;
    }

    switch(expression_node->type) {
        case EXPRESSION_BINARY: {
            Token operator = expression_node->expr.binary.operator;
            OrsoExpressionNode* left = expression_node->expr.binary.left;
            OrsoExpressionNode* right = expression_node->expr.binary.right;

            switch (operator.type) {
                case TOKEN_OR:
                case TOKEN_AND: {
                    expression(vm, compiler, ast, left, chunk);

                    emit_storage_type_convert(compiler, chunk, left->value_type, expression_node->value_type, left->end.line);

                    OrsoOPCode jump_instruction = operator.type == TOKEN_AND ? ORSO_OP_JUMP_IF_FALSE : ORSO_OP_JUMP_IF_TRUE;
                    i32 jump_rest = emit_jump(jump_instruction, compiler, chunk, operator.line);

                    i32 previous_stack_count = compiler->current_stack_size;
                    i32 previous_object_stack_count = compiler->current_object_stack_size;
                    (void)previous_stack_count;
                    (void)previous_object_stack_count;

                    emit_pop(compiler, chunk, expression_node->value_type, right->start.line);

                    expression(vm, compiler, ast, right, chunk);

                    emit_storage_type_convert(compiler, chunk, right->value_type, expression_node->value_type, right->end.line);

                    patch_jump(chunk, jump_rest);

                    ASSERT(previous_stack_count == compiler->current_stack_size, "must be the as before jump.");
                    ASSERT(previous_object_stack_count == compiler->current_object_stack_size, "must be the same as before jump.");
                    break;
                }

                default: {
                    expression(vm, compiler, ast, left, chunk);

                    if (ORSO_TYPE_IS_UNION(left->value_type)) {
                        emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, left->start.line);
                    }

                    expression(vm, compiler, ast, right, chunk);

                    if (ORSO_TYPE_IS_UNION(right->value_type)) {
                        emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, right->start.line);
                    }

                    if (orso_type_is_integer(left->narrowed_value_type, true)) {
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
                            default: UNREACHABLE();
                        }

                    } else if (orso_type_is_float(left->narrowed_value_type)) {
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
                            default: UNREACHABLE();
                        }

                    } else if (left->narrowed_value_type == &OrsoTypeString) {
                        switch (operator.type) {
                            case TOKEN_PLUS: EMIT_BINARY_OP(CONCAT, STRING); break;
                            case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); EMIT_NOT(); break;
                            case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); break;
                            default: UNREACHABLE();
                        }

                    } else if (left->narrowed_value_type == &OrsoTypeSymbol) {
                        switch (operator.type) {
                            case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); break;
                            case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); EMIT_NOT(); break;
                            default: UNREACHABLE();
                        }
                    }
                    break;
                }
            }
            break;
        }

        case EXPRESSION_UNARY: {
            expression(vm, compiler, ast, expression_node->expr.unary.operand, chunk);
            if (!ORSO_TYPE_IS_UNION(expression_node->value_type)
                && ORSO_TYPE_IS_UNION(expression_node->expr.unary.operand->value_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, expression_node->start.line);
            }

            OrsoExpressionNode* unary = expression_node->expr.unary.operand;
            Token operator = expression_node->expr.unary.operator;

            switch (operator.type) {
                case TOKEN_MINUS: {
                    if (orso_type_is_or_has_integer(unary->value_type, true)) {
                        EMIT_NEGATE(I64);
                    } else if (orso_type_is_or_has_float(unary->value_type)) {
                        EMIT_NEGATE(F64);
                    } else {
                        UNREACHABLE();
                    }
                    break;
                }

                case TOKEN_NOT: {
                    EMIT_NOT();

                    if (orso_is_gc_type(unary->value_type)) {
                        emit_instruction(ORSO_OP_POP_TOP_OBJECT, compiler, chunk, expression_node->end.line);
                    }
                    break;
                }

                default: UNREACHABLE();
            }
            break;
        }

        case EXPRESSION_GROUPING: {
            expression(vm, compiler, ast, expression_node->expr.grouping.expression, chunk);
            if (ORSO_TYPE_IS_UNION(expression_node->expr.grouping.expression->value_type)
                && !ORSO_TYPE_IS_UNION(expression_node->value_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, expression_node->end.line);
            }
            break;
        }
        
        case EXPRESSION_PRIMARY: {
            gen_primary(compiler, chunk, ast,
                    expression_node->value_type,
                    expression_node->expr.primary.value_index, expression_node->expr.primary.token.line);
            break;
        }

        case EXPRESSION_ENTITY: {
            Token identifier_token = expression_node->expr.entity.name;
            bool is_local;
            i32 index = retrieve_variable(vm, compiler, &identifier_token, &is_local);

            OrsoOPCode get_op;
            OrsoOPCode get_union_op;
            if (is_local) {
                get_op = ORSO_OP_GET_LOCAL;
                get_union_op = ORSO_OP_GET_LOCAL_UNION;
            } else {
                get_op = ORSO_OP_GET_GLOBAL;
                get_union_op = ORSO_OP_GET_GLOBAL_UNION;
            }

            if (!ORSO_TYPE_IS_UNION(expression_node->value_type)) {
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
            Token identifier_token = expression_node->expr.assignment.name;
            
            expression(vm, compiler, ast, expression_node->expr.assignment.right_side, chunk);
            OrsoType* right_side_type = expression_node->expr.assignment.right_side->value_type;
            if (ORSO_TYPE_IS_UNION(expression_node->value_type) && !ORSO_TYPE_IS_UNION(right_side_type)) {
                emit_put_in_union(compiler, right_side_type, chunk, expression_node->start.line);
            } else if (!ORSO_TYPE_IS_UNION(expression_node->value_type) && ORSO_TYPE_IS_UNION(right_side_type)) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, expression_node->start.line);
            }

            if (orso_is_gc_type(expression_node->value_type) && !orso_is_gc_type(right_side_type)) {
                emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, expression_node->start.line);
            }

            OrsoOPCode set_op;
            OrsoOPCode set_union_op;
            bool is_local;
            i32 index;

            index = retrieve_variable(vm, compiler, &identifier_token, &is_local);
            if (is_local) {
                set_op = ORSO_OP_SET_LOCAL;
                set_union_op = ORSO_OP_SET_LOCAL_UNION;
            } else {
                set_op = ORSO_OP_SET_GLOBAL;
                set_union_op = ORSO_OP_SET_GLOBAL_UNION;
            }

            if (!ORSO_TYPE_IS_UNION(expression_node->value_type)) {
                emit_instruction3(set_op, compiler, index, chunk, expression_node->start.line);
            } else {
                if (orso_is_gc_type(expression_node->value_type)) {
                    if (is_local) {
                        const i32 object_stack_position = compiler->locals[index].object_stack_position;
                        ASSERT(object_stack_position >= 0, "must be on object stack.");
                        emit_instruction3(ORSO_OP_UPDATE_STACK_GC_TYPE, compiler, object_stack_position, chunk, expression_node->start.line);
                    } else {
                        const i32 gc_index = find_global_gc_index(vm, index + 1);
                        ASSERT(gc_index >= 0, "gc_index must exist.");
                        emit_instruction3(ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE, compiler, gc_index, chunk, expression_node->start.line);
                    }
                }

                emit_instruction3(set_union_op, compiler, index, chunk, expression_node->start.line);
            }
            break;
        }

        case EXPRESSION_IMPLICIT_CAST: {
            OrsoExpressionNode* operand = expression_node->expr.cast.operand;
            expression(vm, compiler, ast, operand, chunk);
            emit_type_convert(compiler, operand->value_type, expression_node->value_type, chunk, operand->start.line);
            break;
        }

        case EXPRESSION_BLOCK: {
            begin_scope(compiler);

            OrsoBlock* block = &expression_node->expr.block;
            OrsoType* return_value_type = gen_block(vm, compiler, ast, chunk, block, false, expression_node->end.line);

            end_scope(compiler, chunk, return_value_type, expression_node->end.line);
            break;
        }
        
        case EXPRESSION_IFELSE: {
            if (expression_node->expr.ifelse.loop_block) {
                emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, expression_node->start.line);
                emit_storage_type_convert(compiler, chunk, &OrsoTypeVoid, expression_node->value_type, expression_node->start.line);
            }

            u32 position_before_condition_evaluation = sb_count(chunk->code);

            OrsoExpressionNode* condition = expression_node->expr.ifelse.condition;
            expression(vm, compiler, ast, condition, chunk);

            OrsoOPCode jump_instruction = ORSO_OP_JUMP_IF_FALSE;
            if (expression_node->expr.ifelse.is_negated) {
                jump_instruction = ORSO_OP_JUMP_IF_TRUE;
            }

            i32 then_jump = emit_jump(jump_instruction, compiler, chunk, condition->end.line);

            /*
             * The then or else branch get ran once but during code-gen, both branches are processed
             * therefore before any branch is processed, I save the object and regular stack.
            */
            i32 stack_count = compiler->current_stack_size;
            i32 object_stack_count = compiler->current_object_stack_size;

            emit_pop(compiler, chunk, condition->value_type, condition->end.line);
            if (expression_node->expr.ifelse.loop_block) {
                emit_pop(compiler, chunk, expression_node->value_type, condition->start.line);
            }

            expression(vm, compiler, ast, expression_node->expr.ifelse.then, chunk);
            emit_storage_type_convert(compiler, chunk,
                    expression_node->expr.ifelse.then->value_type, expression_node->value_type,
                    expression_node->expr.ifelse.then->end.line);

            if (expression_node->expr.ifelse.loop_block) {
                emit_loop(compiler, chunk, expression_node->expr.ifelse.then->end.line, position_before_condition_evaluation);
            }

            i32 else_jump = emit_jump(ORSO_OP_JUMP, compiler, chunk, expression_node->expr.ifelse.then->end.line);

            patch_jump(chunk, then_jump);

            i32 then_stack_count = compiler->current_stack_size;
            i32 then_object_stack_size = compiler->current_object_stack_size;
            (void)then_stack_count;
            (void)then_object_stack_size;

            /*
             * In the else branch, I restore the old stack count and object stack count so that they can be
             * calculated properly
            */
            compiler->current_stack_size = stack_count;
            compiler->current_object_stack_size = object_stack_count;

            emit_pop(compiler, chunk, condition->value_type, condition->end.line);
            if (expression_node->expr.ifelse.loop_block) {
                emit_pop(compiler, chunk, expression_node->value_type, expression_node->expr.ifelse.then->end.line);
            }

            if (expression_node->expr.ifelse.else_) {
                expression(vm, compiler, ast, expression_node->expr.ifelse.else_, chunk);

                emit_storage_type_convert(compiler, chunk,
                        expression_node->expr.ifelse.else_->value_type, expression_node->value_type,
                        expression_node->expr.ifelse.else_->end.line);
            } else {
                emit_instruction(ORSO_OP_PUSH_0, compiler, chunk, expression_node->end.line);

                emit_storage_type_convert(compiler, chunk,
                        &OrsoTypeVoid, expression_node->value_type,
                        expression_node->end.line);
            }

            patch_jump(chunk, else_jump);

            ASSERT(then_stack_count == compiler->current_stack_size, "then and else branch should end up with the same stack size");
            ASSERT(then_object_stack_size == compiler->current_object_stack_size, "then and else branch should end up with same object stack size");

            break;
        }
        case EXPRESSION_CALL: {
            Token* callee = &expression_node->expr.call.callee;
            bool is_local;
            i32 index = retrieve_variable(vm, compiler, callee, &is_local);
            bool is_callee_union = ORSO_TYPE_IS_UNION(expression_node->expr.call.callee_type);

            if (is_local) {
                emit_instruction3(is_callee_union ? ORSO_OP_GET_LOCAL_UNION : ORSO_OP_GET_LOCAL,
                        compiler, index, chunk, callee->line);
            } else {
                emit_instruction3(is_callee_union ? ORSO_OP_GET_GLOBAL_UNION : ORSO_OP_GET_GLOBAL,
                        compiler, index, chunk, callee->line);
            }

            emit_instruction(ORSO_OP_PUSH_TOP_OBJECT, compiler, chunk, expression_node->start.line);

            if (is_callee_union) {
                emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, expression_node->start.line);
            }

            // if (ORSO_TYPE_IS_UNION(expression_node->value_type)) {
            //     emit_instruction3(ORSO_OP_UPDATE_STACK_GC_TYPE, compiler, compiler->current_object_stack_size - 1, chunk, expression_node->end.line);
            // }

            for (i32 i = 0; i < sb_count(expression_node->expr.call.arguments); i++) {
                OrsoExpressionNode* argument = expression_node->expr.call.arguments[i];
                expression(vm, compiler, ast, argument, chunk);

                OrsoType* parameter_type = expression_node->expr.call.callee_function_type->argument_types[i];
                emit_storage_type_convert(compiler, chunk, argument->value_type, parameter_type, expression_node->end.line);

            }

            OrsoFunctionType* overload_type = expression_node->expr.call.callee_function_type;
            emit_call(compiler, chunk, overload_type, expression_node->expr.call.callee.line);
            break;
        }

        case EXPRESSION_FUNCTION_DEFINITION: {
            function_expression(vm, compiler, ast, expression_node, chunk);
            break;
        }
        case EXPRESSION_FOR:
        case EXPRESSION_NONE: UNREACHABLE();
    }

#undef EMIT_NEGATE
#undef EMIT_NOT
#undef EMIT_BINARY_OP_F64
#undef EMIT_BINARY_OP_I64
#undef EMIT_BINARY_OP
}

static void statement(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoStatementNode* statement, Chunk* chunk) {
    switch (statement->type) {
        case ORSO_STATEMENT_EXPRESSION: {
            OrsoExpressionNode* expression_ = statement->stmt.expression;
            expression(vm, compiler, ast, expression_, chunk);

            emit_pop(compiler, chunk, expression_->value_type, statement->end.line);
            break;
        }
        
        case ORSO_STATEMENT_PRINT:
        case ORSO_STATEMENT_PRINT_EXPR: {
                expression(vm, compiler, ast, statement->stmt.expression, chunk);

                if (!ORSO_TYPE_IS_UNION(statement->stmt.expression->value_type)) {
                    emit_put_in_union(compiler, statement->stmt.expression->value_type, chunk, statement->start.line);
                }

                if (!orso_is_gc_type(statement->stmt.expression->value_type)) {
                    emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, statement->start.line);
                }

                Token start = statement->stmt.expression->start;
                Token end = statement->stmt.expression->end;

                OrsoString* expression_string = orso_new_string_from_cstrn(&vm->gc, start.start, (end.start + end.length) - start.start);

                OrsoSlot slot = ORSO_SLOT_P(expression_string, &OrsoTypeString);
                emit_constant(compiler, chunk, slot, start.line, true);

                OrsoSlot value_type = ORSO_SLOT_P(statement->stmt.expression->value_type, &OrsoTypeType);
                emit_constant(compiler, chunk, value_type, start.line, false);

                if (statement->type == ORSO_STATEMENT_PRINT_EXPR) {
                    emit_instruction(ORSO_OP_PRINT_EXPR, compiler, chunk, start.line);
                } else {
                    emit_instruction(ORSO_OP_PRINT, compiler, chunk, start.line);
                }
            break;
        }
        case ORSO_STATEMENT_RETURN: {
            expression(vm, compiler, ast, statement->stmt.expression, chunk);
            emit_storage_type_convert(compiler, chunk, 
                    statement->stmt.expression->value_type,
                    compiler->function->type->return_type, statement->stmt.expression->end.line);
            
            emit_return(compiler, chunk, compiler->function->type->return_type, statement->end.line);
            break;
        }
        case ORSO_STATEMENT_NONE: UNREACHABLE();
    }
}

static void default_value(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, Chunk* chunk, OrsoEntityDeclarationNode* entity_declaration) {
    OrsoType* conform_type = entity_declaration->type;
    bool is_conform_gc_type = orso_is_gc_type(entity_declaration->type);

    if (entity_declaration->expression != NULL) {
        OrsoExpressionNode* default_expression = entity_declaration->expression;
        expression(vm, compiler, ast, default_expression, chunk);

        if (ORSO_TYPE_IS_UNION(conform_type) && !ORSO_TYPE_IS_UNION(default_expression->value_type)) {
            emit_put_in_union(compiler, entity_declaration->expression->value_type, chunk, default_expression->start.line);
        } else if (!ORSO_TYPE_IS_UNION(conform_type) && ORSO_TYPE_IS_UNION(default_expression->value_type)) {
            emit_instruction(ORSO_OP_NARROW_UNION, compiler, chunk, default_expression->start.line);
        }

        // This is for local unions since they stay on the stack and have their value tracked
        if (is_conform_gc_type && !orso_is_gc_type(default_expression->value_type)) {
            emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, default_expression->start.line);
        }

    } else {
        if (ORSO_TYPE_IS_UNION(conform_type)) {
            ASSERT(orso_type_fits(conform_type, &OrsoTypeVoid), "default type only allowed for void type unions.");
            emit_instruction(ORSO_OP_PUSH_NULL_UNION, compiler, chunk, entity_declaration->end.line);

            if (is_conform_gc_type) {
                emit_instruction(ORSO_OP_PUSH_TOP_OBJECT_NULL, compiler, chunk, entity_declaration->end.line);
            }
        } else {
            ASSERT(entity_declaration->implicit_default_value_index >= 0, "if no expression, there must be an implicit value");
            OrsoSlot* default_value = &ast->folded_constants[entity_declaration->implicit_default_value_index];
            emit_constant(compiler, chunk, *default_value, entity_declaration->end.line, is_conform_gc_type);
        }
    }
}

static void entity_declaration(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoEntityDeclarationNode* variable_declaration, Chunk* chunk) {
    default_value(vm, compiler, ast, chunk, variable_declaration);
    
    define_entity(vm, compiler, chunk,
            variable_declaration->type, &variable_declaration->name,
            variable_declaration->start.line);
}

static void declaration(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoDeclarationNode* declaration, Chunk* chunk) {
    switch (declaration->type) {
        case ORSO_DECLARATION_STATEMENT: statement(vm, compiler, ast, declaration->decl.statement, chunk); break;
        case ORSO_DECLARATION_ENTITY: entity_declaration(vm, compiler, ast, declaration->decl.entity, chunk); break;
        case ORSO_DECLARATION_NONE: UNREACHABLE(); break;
    }
}

void orso_code_builder_init(OrsoCodeBuilder* builder, OrsoVM* vm, OrsoAST* ast) {
    builder->vm = vm;
    builder->ast = ast;
}

void orso_code_builder_free(OrsoCodeBuilder* builder) { 
    (void)builder;
}

OrsoFunction* orso_generate_expression_function(OrsoCodeBuilder* builder, OrsoExpressionNode* expression_node, bool literals_only) {
    Compiler compiler;
    OrsoFunctionType* function_type = (OrsoFunctionType*)orso_type_set_fetch_function(&builder->ast->type_set, expression_node->value_type, NULL, 0);

    compiler_init(&compiler, ORSO_FUNCTION_TYPE_SCRIPT, builder->vm, (OrsoType*)function_type);
    compiler.literals_only = literals_only;

    // The vm will put this guy on the guy.
    compiler.max_stack_size = compiler.current_stack_size = compiler.current_object_stack_size = 1;
    declare_local_function_definition(&compiler, compiler.function);

    //emit_constant(&compiler, top_chunk, ORSO_SLOT_P(compiler.function, compiler.function->type), 0, true);

    // Token empty = {.start = "", .length = 0};
    // declare_global_entity(builder->vm, &empty, 1, false, false);

    Chunk* top_chunk = &compiler.function->chunk;

    expression(builder->vm, &compiler, builder->ast, expression_node, top_chunk);

    emit_return(&compiler, top_chunk, expression_node->value_type, expression_node->start.line);

    OrsoFunction* function = compiler.function;

    compiler_free(&compiler);

    return function;
}

OrsoFunction* orso_generate_code(OrsoVM* vm, OrsoAST* ast) {
    Compiler compiler;
    OrsoFunctionType* function_type = (OrsoFunctionType*)orso_type_set_fetch_function(&ast->type_set, &OrsoTypeVoid, NULL, 0);
    compiler_init(&compiler, ORSO_FUNCTION_TYPE_SCRIPT, vm, (OrsoType*)function_type);

    // when a function is called it is placed on the stack. The caller does this... In this case,
    // the caller is the virtual machine. So we start at stack size 1 since it should be there when
    // the program starts.
    compiler.max_stack_size = compiler.current_stack_size = compiler.current_object_stack_size = 1;
    declare_local_function_definition(&compiler, compiler.function);

    Chunk* top_chunk = &compiler.function->chunk;

    for (i32 i = 0; i < sb_count(ast->declarations); i++) {
        declaration(vm, &compiler, ast, ast->declarations[i], top_chunk);
    }

    OrsoFunction* function = compiler_end(&compiler, top_chunk, ast->declarations[sb_count(ast->declarations) - 1]->end.line);

    compiler_free(&compiler);

    return function;
}
