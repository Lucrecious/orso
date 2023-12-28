#include "codegen.h"

#ifdef DEBUG_PRINT
#include "debug.h"
#endif

#include <stdarg.h>

#include "opcodes.h"
#include "instructions.h"
#include "sb.h"
#include "type_set.h"

typedef struct {
    Token name;
    i32 depth;
    byte slot_count;
    i32 stack_position;
} Local;

typedef enum {
    ORSO_FUNCTION_TYPE_FUNCTION,
    ORSO_FUNCTION_TYPE_SCRIPT,
} OrsoCompilerFunctionType;

typedef struct Compiler {
    OrsoFunction* function;
    OrsoCompilerFunctionType function_type;

    bool skip_function_definitions;

    OrsoFunction** functions_to_compile;

    Local* locals;
    i32 locals_count;
    i32 scope_depth;
    i32 current_stack_size;
    i32 max_stack_size;
} Compiler;

static i32 add_local(Compiler* compiler, Token name, i32 slot_count);

// A Token* is passed in instead of OrsoSymbol* because Token* cant be garbage collected and OrsoSymbol* can and would be.
static void compiler_init(Compiler* compiler, OrsoCompilerFunctionType function_type, OrsoVM* vm, OrsoFunction* function, OrsoType* creator_type) {
    ASSERT(function, "must be not null");

    // TODO: remove if possible, previously this was used to get the garbage collector but thats being
    //   removed if possible, previously this was used to get the garbage collector but thats being removed
    (void)vm; 

    compiler->functions_to_compile = NULL;
    compiler->function = NULL;
    compiler->function_type = function_type;

    compiler->locals = NULL;
    compiler->scope_depth = 0;
    compiler->locals_count = 0;

    compiler->current_stack_size = 0;
    compiler->max_stack_size = 0;

    compiler->function = function;
    compiler->function->signature = creator_type;
    compiler->skip_function_definitions = false;
}

static void compiler_free(Compiler* compiler) {
    sb_free(compiler->locals);
    compiler->locals = NULL;
    compiler->locals_count = 0;

    compiler->current_stack_size = 0;
    compiler->max_stack_size = 0;

    sb_free(compiler->functions_to_compile);
}

static void apply_stack_effects(Compiler* compiler, i32 stack_effects) {
    compiler->current_stack_size += stack_effects;
    compiler->max_stack_size = compiler->current_stack_size > compiler->max_stack_size
        ? compiler->current_stack_size : compiler->max_stack_size;
}

static void emit(Compiler* compiler, Chunk* chunk, i32 line, const int op_code, ...) {
    va_list args;
    va_start(args, op_code);

    chunk_write(chunk, op_code, line);

    u32 stack_effect = 0;

    switch(op_code) {
        case ORSO_OP_NO_OP:
        case ORSO_OP_NEGATE_I64:
        case ORSO_OP_NEGATE_F64:
        case ORSO_OP_LOGICAL_NOT: {
            stack_effect = 0;
            break;
        }

        case ORSO_OP_PUSH_1:
        case ORSO_OP_PUSH_0: {
            stack_effect = 1;
            break;
        }

        case ORSO_OP_POP:
        case ORSO_OP_ADD_I64:
        case ORSO_OP_SUBTRACT_I64:
        case ORSO_OP_MULTIPLY_I64:
        case ORSO_OP_DIVIDE_I64:
        case ORSO_OP_ADD_F64:
        case ORSO_OP_SUBTRACT_F64:
        case ORSO_OP_MULTIPLY_F64:
        case ORSO_OP_DIVIDE_F64:
        case ORSO_OP_I64_TO_F64:
        case ORSO_OP_F64_TO_I64:
        case ORSO_OP_EQUAL_I64:
        case ORSO_OP_LESS_I64:
        case ORSO_OP_GREATER_I64:
        case ORSO_OP_EQUAL_F64:
        case ORSO_OP_LESS_F64: 
        case ORSO_OP_GREATER_F64:
        case ORSO_OP_EQUAL_SYMBOL:
        case ORSO_OP_EQUAL_STRING:
        case ORSO_OP_CONCAT_STRING: {
            stack_effect = -1;
            break;
        }

        case ORSO_OP_POPN: {
            byte n = (byte)va_arg(args, long);
            chunk_write(chunk, n, line);

            stack_effect = -n;
            break;
        }

        case ORSO_OP_POP_SCOPE: {
            byte scope_size_slots = (byte)va_arg(args, long);
            byte block_value_slots = (byte)va_arg(args, long);
            chunk_write(chunk, scope_size_slots, line);
            chunk_write(chunk, block_value_slots, line);

            stack_effect = -scope_size_slots;
            break;
        }

        case ORSO_OP_GET_LOCAL_8BIT_ADDRESS:
        case ORSO_OP_SET_LOCAL_8BIT_ADDRESS:
        case ORSO_OP_SET_GLOBAL_8BIT_ADDRESS:
        case ORSO_OP_GET_GLOBAL_8BIT_ADDRESS:
        case ORSO_OP_CONSTANT_8BIT_ADDRESS: {
            byte index = (byte)va_arg(args, long);
            byte size = (byte)va_arg(args, long);

            chunk_write(chunk, index, line);
            chunk_write(chunk, size, line);

            if (op_code == ORSO_OP_SET_LOCAL_8BIT_ADDRESS || op_code == ORSO_OP_SET_GLOBAL_8BIT_ADDRESS) {
                stack_effect = 0;
            } else {
                stack_effect = orso_bytes_to_slots(size);
            }
            break;
        }

        case ORSO_OP_GET_LOCAL_16BIT_ADDRESS:
        case ORSO_OP_SET_LOCAL_16BIT_ADDRESS:
        case ORSO_OP_SET_GLOBAL_16BIT_ADDRESS:
        case ORSO_OP_GET_GLOBAL_16BIT_ADDRESS:
        case ORSO_OP_CONSTANT_16BIT_ADDRESS: {
            u32 index = va_arg(args, long);
            ASSERT(index < UINT16_MAX, "must be a short");

            byte size = (u32)va_arg(args, long);

            byte index1, index2;;
            ORSO_u16_to_u8s(index, index1, index2);

            chunk_write(chunk, index1, line);
            chunk_write(chunk, index2, line);
            chunk_write(chunk, size, line);

            if (op_code == ORSO_OP_SET_GLOBAL_16BIT_ADDRESS || op_code == ORSO_OP_SET_LOCAL_16BIT_ADDRESS) {
                stack_effect = 0;
            } else {
                stack_effect = orso_bytes_to_slots(size);
            }
            break;
        }

        case ORSO_OP_SET_GLOBAL_32BIT_ADDRESS:
        case ORSO_OP_GET_GLOBAL_32BIT_ADDRESS:
        case ORSO_OP_CONSTANT_32BIT_ADDRESS: {
            u32 index = va_arg(args, long);
            byte size = va_arg(args, long);

            byte index1, index2, index3, index4;
            ORSO_u32_to_u8s(index, index1, index2, index3, index4);

            chunk_write(chunk, index1, line);
            chunk_write(chunk, index2, line);
            chunk_write(chunk, index3, line);
            chunk_write(chunk, index4, line);
            chunk_write(chunk, size, line);

            if (op_code == ORSO_OP_SET_GLOBAL_32BIT_ADDRESS) {
                stack_effect = 0;
            } else {
                stack_effect = orso_bytes_to_slots(size);
            }
            break;
        }

        case ORSO_OP_PUT_IN_UNION: {
            OrsoType* type = va_arg(args, OrsoType*);
            chunk_write(chunk, (byte)orso_type_size_bytes(type), line);

            stack_effect = 0;
            break;
        }

        case ORSO_OP_NARROW_UNION: {
            byte byte_offset = (byte)va_arg(args, long);
            chunk_write(chunk, byte_offset ,line);
            stack_effect = -1;
            break;
        }

        case ORSO_OP_JUMP_IF_FALSE:
        case ORSO_OP_JUMP_IF_TRUE:
        case ORSO_OP_JUMP_IF_UNION_FALSE:
        case ORSO_OP_JUMP_IF_UNION_TRUE:
        case ORSO_OP_JUMP: {
            chunk_write(chunk, 0xFF, line);
            chunk_write(chunk, 0xFF, line);

            u64* code_index = va_arg(args, u64*);
            *code_index = (sb_count(chunk->code) - 2);

            stack_effect = 0;
            break;
        }

        case ORSO_OP_LOOP: {
            u32 loop_start = va_arg(args, u64);
            u32 offset = sb_count(chunk->code) - loop_start + 2;
            ASSERT(offset <= UINT16_MAX, "loop cant go back more than 2^16");
            byte a;
            byte b;
            ORSO_u16_to_u8s(offset, a, b);
            chunk_write(chunk, a, line);
            chunk_write(chunk, b, line);

            stack_effect = 0;
            break;
        }

        case ORSO_OP_CALL: {
            OrsoType* function_type = va_arg(args, OrsoType*);
            ASSERT(ORSO_TYPE_IS_FUNCTION(function_type), "must be function");

            i32 argument_slots = 0;
            for (i32 i = 0; i < function_type->data.function.argument_count; i++) {
                argument_slots += orso_type_slot_count(function_type->data.function.argument_types[i]);
            }

            byte a;
            byte b;
            ORSO_u16_to_u8s(argument_slots, a, b);
            chunk_write(chunk, a, line);
            chunk_write(chunk, b, line);

            stack_effect = -argument_slots - 1 + orso_type_slot_count(function_type->data.function.return_type);
            break;
        }

        case ORSO_OP_RETURN: {
            OrsoType* type = va_arg(args, OrsoType*);
            // TODO: Figure out more robust way of doing this instead of casting to byte
            chunk_write(chunk, (byte)orso_type_slot_count(type), line);

            stack_effect = -orso_type_slot_count(type);
            break;
        }

        case ORSO_OP_PRINT_EXPR:
        case ORSO_OP_PRINT: {
            stack_effect = -4;
            break;
        }
    }

    va_end(args);

    apply_stack_effects(compiler, stack_effect);
}

// TODO: Remove compiler from the parameters... instead try to bubble up the stack effect somehow
static void emit_constant(Compiler* compiler, Chunk* chunk, byte* data, i32 line, OrsoType* type)
{
    u32 size = orso_type_size_bytes(type);
    u32 address = CHUNK_ADD_CONSTANT(chunk, data, size, type);
    address *= sizeof(OrsoSlot);

    ASSERT(address < UINT32_MAX, "address is too high");

    if (address < UINT8_MAX) {
        emit(compiler, chunk, line, ORSO_OP_CONSTANT_8BIT_ADDRESS, (long)address, size);
    } else if (address < UINT16_MAX) {
        emit(compiler, chunk, line, ORSO_OP_CONSTANT_16BIT_ADDRESS, (long)address, size);
    } else {
        emit(compiler, chunk, line, ORSO_OP_CONSTANT_32BIT_ADDRESS, (long)address, size);
    }
}

static void emit_put_in_union(Compiler* compiler, Chunk* chunk, i32 line, OrsoType* type) {
    OrsoSlot slot = ORSO_SLOT_P(type);
    emit_constant(compiler, chunk, (byte*)&slot, line, &OrsoTypeType);
    emit(compiler, chunk, line, ORSO_OP_PUT_IN_UNION, (OrsoType*)type);
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

static void emit_entity_get(Compiler* compiler, u32 index, byte size_bytes, Chunk* chunk, i32 line, bool is_local) {
    index *= sizeof(OrsoSlot);
    if (is_local) {
        ASSERT(index <= UINT16_MAX, "must be short");
        emit(compiler, chunk, line, ORSO_OP_GET_LOCAL_16BIT_ADDRESS, (long)index, (long)size_bytes);
    } else {
        emit(compiler, chunk, line, ORSO_OP_GET_GLOBAL_32BIT_ADDRESS, (long)index, (long)size_bytes);
    }
}

static void emit_type_convert(Compiler* compiler, OrsoType* from_type, OrsoType* to_type, Chunk* chunk, i32 line) {
    bool include_bool = true;
    ASSERT(orso_type_is_number(from_type, include_bool) && orso_type_is_number(to_type, include_bool), "Implicit type conversion only works for number types right now.");
    if (orso_type_is_float(from_type) && orso_type_is_integer(to_type, include_bool)) {
        emit(compiler, chunk, line, ORSO_OP_F64_TO_I64);
    } else if (orso_type_is_integer(from_type, include_bool) && orso_type_is_float(to_type)) {
        emit(compiler, chunk, line, ORSO_OP_I64_TO_F64);
    } 
}

static void emit_storage_type_convert(Compiler* compiler, Chunk* chunk, OrsoType* source, OrsoType* destination, i32 line) {
    if (ORSO_TYPE_IS_UNION(source) && !ORSO_TYPE_IS_UNION(destination)) {
        emit(compiler, chunk, line, ORSO_OP_NARROW_UNION, orso_type_size_bytes(source));
    } else if (!ORSO_TYPE_IS_UNION(source) && ORSO_TYPE_IS_UNION(destination)) {
        emit_put_in_union(compiler, chunk, line, (OrsoType*)source);
    }
}

static void emit_pop(Compiler* compiler, Chunk* chunk, u32 pop_count, i32 line) {
    if (pop_count < 1) {
        return;
    }
    
    if (pop_count == 1) {
        emit(compiler, chunk, line, ORSO_OP_POP);
    } else {
        while (pop_count > 0) {
            byte batch = pop_count > UINT8_MAX ? UINT8_MAX : pop_count;
            emit(compiler, chunk, line, ORSO_OP_POPN, (long)batch);
            pop_count -= batch;
        }
    }
}

static OrsoFunction* compiler_end(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, Chunk* chunk, i32 line) {
    emit(compiler, chunk, line, ORSO_OP_PUSH_0);
    emit(compiler, chunk, line, ORSO_OP_RETURN, (OrsoType*)&OrsoTypeVoid);

    // compile left over functions
    // TODO: make this faster. this is super slow because i need a hashmap to look up the functions
    // faster. Or I need to figure out a way to decide whether or not i should compile a function
    // on the fly.
    {
        OrsoFunction** functions_to_compile = NULL;
        for (i32 i = 0; i < sb_count(compiler->functions_to_compile); i++) {
            sb_push(functions_to_compile, compiler->functions_to_compile[i]);
        }

        sb_free(compiler->functions_to_compile);
        compiler->functions_to_compile = NULL;

        for (i32 i = 0; i < sb_count(functions_to_compile); i++) {
            OrsoFunction* function = functions_to_compile[i];
            if (function->chunk.code != NULL) {
                continue;
            }

            for (i32 j = 0; j < sb_count(ast->function_definition_pairs); j++) {
                if (ast->function_definition_pairs[j].function != function) {
                    continue;
                }

                if (!ast->function_definition_pairs[j].ast_defintion->data.function.compilable) {
                    break;
                }

                orso_compile_function(vm, ast, function, ast->function_definition_pairs[j].ast_defintion);
            }
        }
    }

#ifdef DEBUG_PRINT
    chunk_disassemble(chunk, compiler->function->binded_name ?
            compiler->function->binded_name->text : "<anonymous at TODO: line and column>");
#endif

    return compiler->function;
}

#ifdef DEBUG_TRACE_EXECUTION
#define DECLARE_GLOBAL(vm, name, slot_count, type) add_global(vm, name, slot_count, type)
static i32 add_global(OrsoVM* vm, Token* name, i32 slot_count, OrsoType* type)
#else
#define DECLARE_GLOBAL(vm, name, slot_count, type) add_global(vm, name, slot_count)
static i32 add_global(OrsoVM* vm, Token* name, i32 slot_count)
#endif
{
    OrsoSymbol* identifier = orso_new_symbol_from_cstrn(name->start, name->length, &vm->symbols);

    OrsoSlot index_slot;
    ASSERT(!orso_symbol_table_get(&vm->globals.name_to_index, identifier, &index_slot), "double global definition");

    // Use the specific khash for this
    index_slot = ORSO_SLOT_I(sb_count(vm->globals.values));
    orso_symbol_table_set(&vm->globals.name_to_index, identifier, index_slot);

    for (i32 i = 0; i < slot_count; i++) {
#ifdef DEBUG_TRACE_EXECUTION
        sb_push(vm->globals.types, type);
#endif

        sb_push(vm->globals.values, ORSO_SLOT_I(0));
    }

    i32 index = index_slot.as.i;

    return index;
}

static i32 add_local(Compiler* compiler, Token name, i32 slot_count) {
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

    if (previous_local) {
        local->stack_position = previous_local->stack_position + previous_local->slot_count;
    } else {
        local->stack_position = 0;
    }

    ASSERT(local->stack_position == compiler->current_stack_size - slot_count,
            "The calculated stack position for the local must properly related to the tracked compiler stack");

    return compiler->locals_count - 1;
}

static FORCE_INLINE bool is_global_scope(Compiler* compiler) {
    return compiler->scope_depth == 0;
}

static i32 declare_local_entity(Compiler* compiler, Token* name, i32 slot_count) {
    return add_local(compiler, *name, slot_count);
}

static i32 retrieve_global_variable(OrsoVM* vm, Token* name) {
    OrsoSymbol* identifier = orso_new_symbol_from_cstrn(name->start, name->length, &vm->symbols);
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

static i64 retrieve_local_variable(Compiler* compiler, Token* name) {
    for (i64 i = compiler->locals_count - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiers_equal(name, &local->name)) {
            return i;
        }
    }

    return -1;
}

static u32 retrieve_variable(OrsoVM* vm, Compiler* compiler, Token* name, bool* is_local) {
    i64 index = retrieve_local_variable(compiler, name);
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

static void begin_scope(Compiler* compiler) {
    compiler->scope_depth++;
}

static void end_scope(Compiler* compiler, Chunk* chunk, OrsoType* block_value_type, i32 line) {
    compiler->scope_depth--;

    // pop all local object markers
    i32 total_local_slot_count = 0;
    while (compiler->locals_count > 0) {
        Local* local = &compiler->locals[compiler->locals_count - 1];
        if (local->depth <= compiler->scope_depth) {
            break;
        }

        total_local_slot_count += local->slot_count;
        compiler->locals_count--;
    }

    i32 block_value_slots = orso_type_slot_count(block_value_type);

    emit(compiler, chunk, line, ORSO_OP_POP_SCOPE, (long)total_local_slot_count, (long)block_value_slots);
}

static void declaration(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoASTNode* declaration, Chunk* chunk);
static void expression(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoASTNode* expression_node, Chunk* chunk);

static OrsoType* gen_block(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, Chunk* chunk, OrsoASTNode** block, i32 node_count, i32 end_line) {
    OrsoASTNode* final_expression_statement = node_count > 0 ? block[node_count - 1] : NULL;

    final_expression_statement = final_expression_statement && final_expression_statement->node_type != ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION ?
            NULL : final_expression_statement;

    for (i32 i = 0; i < node_count - (final_expression_statement != NULL); i++) {
        declaration(vm, compiler, ast, block[i], chunk);
    }

    OrsoType* return_value_type = &OrsoTypeInvalid;
    if (final_expression_statement) {
        OrsoASTNode* final_expression = final_expression_statement->data.expression;
        return_value_type = final_expression->value_type;
        expression(vm, compiler, ast, final_expression, chunk);
    } else {
        return_value_type = &OrsoTypeVoid;
        emit(compiler, chunk, end_line, ORSO_OP_PUSH_0);
    }

    return return_value_type;
}

static void define_entity(OrsoVM* vm, Compiler* compiler, Chunk* chunk, OrsoType* type, Token* name, i32 line) {
    if (is_global_scope(compiler)) {
        u32 byte_size = orso_type_size_bytes(type);
        u32 slot_size = orso_type_slot_count(type);

        u32 index = DECLARE_GLOBAL(vm, name, slot_size, type);
        index *= sizeof(OrsoSlot);
        emit(compiler, chunk, line, ORSO_OP_SET_GLOBAL_32BIT_ADDRESS, index, (long)byte_size);
        emit_pop(compiler, chunk, slot_size, line);
    } else {
        declare_local_entity(compiler, name, orso_type_slot_count(type));
    }
}

static void declare_local_function_definition(Compiler* compiler, OrsoFunction* function){ 
    // used in assert
    (void)function;
    // TODO: fix this assert, why does function->type need to be converted to type? Will it never be unresolved?
    ASSERT((OrsoType*)function->signature != &OrsoTypeUnresolved, "Must have resolved type");

    Token empty = { .start = "", .length = 0 };
    declare_local_entity(compiler, &empty, 1);
}

static void function_expression(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoASTNode* function_defintion_expression, Chunk* chunk) {
    ASSERT(ORSO_TYPE_IS_FUNCTION(function_defintion_expression->value_type), "must be function if calling this");

    OrsoFunction* stored_function = (OrsoFunction*)ast->folded_constants[function_defintion_expression->value_index].as.p;
    if (!compiler->skip_function_definitions && stored_function->chunk.code == NULL /*is not compiled yet*/) {
        orso_compile_function(vm, ast, stored_function, function_defintion_expression);
    }
    OrsoSlot function_value = ORSO_SLOT_P(stored_function);
    emit_constant(compiler, chunk, (byte*)&function_value, function_defintion_expression->start.line, stored_function->signature);
}

static void gen_primary(Compiler* compiler, Chunk* chunk, OrsoAST* ast, OrsoType* value_type, i32 value_index, i32 line) {
    ASSERT(value_index >= 0, "must be pointing to a contant value...");

    OrsoSlot* value = &ast->folded_constants[value_index];
    switch (value_type->kind) {
        case ORSO_TYPE_BOOL:
        case ORSO_TYPE_INT32:
        case ORSO_TYPE_INT64:
        case ORSO_TYPE_FLOAT32:
        case ORSO_TYPE_FLOAT64:
        case ORSO_TYPE_VOID: {
            if (value->as.i == 0) {
                emit(compiler, chunk, line, ORSO_OP_PUSH_0);
            } else if (value->as.i == 1) {
                emit(compiler, chunk, line, ORSO_OP_PUSH_1);
            } else {
                emit_constant(compiler, chunk, (byte*)value, line, value_type);
            }
            break;
        }
        
        case ORSO_TYPE_TYPE:
        case ORSO_TYPE_FUNCTION:
        case ORSO_TYPE_NATIVE_FUNCTION:
        case ORSO_TYPE_SYMBOL:
        case ORSO_TYPE_STRING:
        case ORSO_TYPE_UNION:
        case ORSO_TYPE_STRUCT: {
            emit_constant(compiler, chunk, (byte*)value, line, value_type);
            break;
        }

        default: UNREACHABLE();
    }
}

static void expression(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoASTNode* expression_node, Chunk* chunk) {
#define EMIT_BINARY_OP(OP, TYPE) do { \
    emit(compiler, chunk, operator.line, ORSO_OP_##OP##_##TYPE); \
} while(false)

#define EMIT_BINARY_OP_I64(OP) EMIT_BINARY_OP(OP, I64)
#define EMIT_BINARY_OP_F64(OP) EMIT_BINARY_OP(OP, F64)

#define EMIT_NOT() do { \
    emit(compiler, chunk, operator.line, ORSO_OP_LOGICAL_NOT); \
} while(false)

#define EMIT_NEGATE(TYPE) do { \
    emit(compiler, chunk, operator.line, ORSO_OP_NEGATE_##TYPE); \
} while (false)

// #ifdef DEBUG
//     bool requires_entity = expression_node->type == EXPRESSION_ASSIGNMENT || expression_node->type == EXPRESSION_ENTITY;
//     ASSERT(!compiler->literals_only || (expression_node->folded_value_index >= 0 || !requires_entity), "compiler is assuming folded values only for ALL expressions");
// #endif

    if (expression_node->value_index >= 0) {
        // TODO: for now going to do this super naively until I get a better hash table that is more generic that I can
        // use for more things. This is will be super slow but I just want to get it to work.
        if (!compiler->skip_function_definitions && ORSO_TYPE_IS_FUNCTION(expression_node->value_type)) {
            OrsoFunction* function = (OrsoFunction*)ast->folded_constants[expression_node->value_index].as.p;
            sb_push(compiler->functions_to_compile, function);
        }

        gen_primary(compiler, chunk, ast,
                expression_node->value_type,
                expression_node->value_index, expression_node->start.line);
        return;
    }

    switch(expression_node->node_type) {
        case ORSO_AST_NODE_TYPE_EXPRESSION_BINARY: {
            Token operator = expression_node->operator;
            OrsoASTNode* left = expression_node->data.binary.lhs;
            OrsoASTNode* right = expression_node->data.binary.rhs;

            switch (operator.type) {
                case TOKEN_OR:
                case TOKEN_AND: {
                    expression(vm, compiler, ast, left, chunk);

                    emit_storage_type_convert(compiler, chunk, left->value_type, expression_node->value_type, left->end.line);

                    OrsoOPCode jump_instruction;
                    if (ORSO_TYPE_IS_STRUCT(left->value_type)) {
                        jump_instruction = operator.type == TOKEN_AND ? ORSO_OP_NO_OP : ORSO_OP_JUMP;
                    } else if (ORSO_TYPE_IS_UNION(left->value_type)) {
                        jump_instruction = operator.type == TOKEN_AND ? ORSO_OP_JUMP_IF_UNION_FALSE : ORSO_OP_JUMP_IF_UNION_TRUE;
                    } else {
                        jump_instruction = operator.type == TOKEN_AND ? ORSO_OP_JUMP_IF_FALSE : ORSO_OP_JUMP_IF_TRUE;
                    }

                    u64 jump_rest;
                    unless (jump_instruction == ORSO_OP_NO_OP) {
                        emit(compiler, chunk, operator.line, jump_instruction, (u64*)&jump_rest);
                    }

                    i32 previous_stack_count = compiler->current_stack_size;
                    (void)previous_stack_count;

                    emit_pop(compiler, chunk, orso_type_slot_count(expression_node->value_type), right->start.line);

                    expression(vm, compiler, ast, right, chunk);

                    emit_storage_type_convert(compiler, chunk, right->value_type, expression_node->value_type, right->end.line);

                    unless (jump_instruction == ORSO_OP_NO_OP) {
                        patch_jump(chunk, jump_rest);
                    }

                    ASSERT(previous_stack_count == compiler->current_stack_size, "must be the as before jump.");
                    break;
                }

                default: {
                    expression(vm, compiler, ast, left, chunk);

                    if (ORSO_TYPE_IS_UNION(left->value_type)) {
                        emit(compiler, chunk, left->start.line, ORSO_OP_NARROW_UNION, orso_type_size_bytes(left->value_type));
                    }

                    expression(vm, compiler, ast, right, chunk);

                    if (ORSO_TYPE_IS_UNION(right->value_type)) {
                        emit(compiler, chunk, right->start.line, ORSO_OP_NARROW_UNION, orso_type_size_bytes(right->value_type));;
                    }

                    if (orso_type_is_integer(left->value_type_narrowed, true)) {
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

                    } else if (orso_type_is_float(left->value_type_narrowed)) {
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

                    } else if (left->value_type_narrowed == &OrsoTypeString) {
                        switch (operator.type) {
                            case TOKEN_PLUS: EMIT_BINARY_OP(CONCAT, STRING); break;
                            case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); EMIT_NOT(); break;
                            case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, STRING); break;
                            default: UNREACHABLE();
                        }

                    } else if (left->value_type_narrowed == &OrsoTypeSymbol) {
                        switch (operator.type) {
                            case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); break;
                            case TOKEN_BANG_EQUAL: EMIT_BINARY_OP(EQUAL, SYMBOL); EMIT_NOT(); break;
                            default: UNREACHABLE();
                        }
                    } else if (left->value_type_narrowed == &OrsoTypeType) {
                        switch (operator.type) {
                            case TOKEN_EQUAL_EQUAL: EMIT_BINARY_OP_I64(EQUAL); break;
                            case TOKEN_BANG_EQUAL: EMIT_BINARY_OP_I64(EQUAL); EMIT_NOT(); break;
                            default: UNREACHABLE();
                        }
                    } else {
                        UNREACHABLE();
                    }
                    break;
                }
            }
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_UNARY: {
            expression(vm, compiler, ast, expression_node->data.expression, chunk);
            emit_storage_type_convert(compiler, chunk, expression_node->data.expression->value_type, expression_node->value_type, expression_node->start.line);

            OrsoASTNode* unary = expression_node->data.expression;
            Token operator = expression_node->operator;

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
                    break;
                }

                case TOKEN_AMPERSAND:
                    //EMIT_NOT();
                    break;

                default: UNREACHABLE();
            }
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_GROUPING: {
            expression(vm, compiler, ast, expression_node->data.expression, chunk);
            emit_storage_type_convert(compiler, chunk, expression_node->data.expression->value_type, expression_node->value_type, expression_node->end.line);
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            gen_primary(compiler, chunk, ast,
                    expression_node->value_type,
                    expression_node->value_index, expression_node->start.line);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY: {
            Token identifier_token = expression_node->data.dot.identifier;
            bool is_local;
            u32 index = retrieve_variable(vm, compiler, &identifier_token, &is_local);
            emit_entity_get(compiler, index, orso_type_size_bytes(expression_node->value_type), chunk, expression_node->start.line, is_local);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_DOT: {
            expression(vm, compiler, ast, expression_node->data.dot.lhs, chunk);

            ASSERT(ORSO_TYPE_IS_STRUCT(expression_node->data.dot.lhs->value_type), "LHS must be a struct for now");

            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            ASSERT(expression_node->data.binary.lhs->node_type == ORSO_AST_NODE_TYPE_EXPRESSION_ENTITY, "left side must be an entity");
            Token identifier_token = expression_node->data.binary.lhs->start;
            
            expression(vm, compiler, ast, expression_node->data.binary.rhs, chunk);
            OrsoType* right_side_type = expression_node->data.binary.rhs->value_type;

            emit_storage_type_convert(compiler, chunk, right_side_type, expression_node->value_type, expression_node->start.line);

            bool is_local;
            u32 index;

            index = retrieve_variable(vm, compiler, &identifier_token, &is_local);
            if (is_local) {
                ASSERT(index <= UINT16_MAX, "must be short");
                emit(compiler, chunk, expression_node->start.line, ORSO_OP_SET_LOCAL_16BIT_ADDRESS, (long)index * sizeof(OrsoSlot), (long)orso_type_slot_count(expression_node->value_type));
            } else {
                emit(compiler, chunk, expression_node->start.line, ORSO_OP_SET_GLOBAL_32BIT_ADDRESS, (long)index * sizeof(OrsoSlot), (long)orso_type_slot_count(expression_node->value_type));
            }
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: {
            OrsoASTNode* operand = expression_node->data.expression;
            expression(vm, compiler, ast, operand, chunk);
            emit_type_convert(compiler, operand->value_type, expression_node->value_type, chunk, operand->start.line);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_BLOCK: {
            begin_scope(compiler);

            OrsoASTNode** block = expression_node->data.block;
            i32 node_count = sb_count(block);
            OrsoType* return_value_type = gen_block(vm, compiler, ast, chunk, block, node_count, expression_node->end.line);

            end_scope(compiler, chunk, return_value_type, expression_node->end.line);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_STATEMENT: {
            OrsoASTNode* block[1] = { expression_node->data.statement };
            gen_block(vm, compiler, ast, chunk, block, 1, expression_node->start.line);
            break;
        }
        
        case ORSO_AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            if (expression_node->data.branch.looping) {
                emit(compiler, chunk, expression_node->start.line, ORSO_OP_PUSH_0);
                emit_storage_type_convert(compiler, chunk, &OrsoTypeVoid, expression_node->value_type, expression_node->start.line);
            }

            u32 position_before_condition_evaluation = sb_count(chunk->code);

            OrsoASTNode* condition = expression_node->data.branch.condition;
            expression(vm, compiler, ast, condition, chunk);

            OrsoOPCode jump_instruction = ORSO_OP_JUMP_IF_FALSE;
            if (expression_node->data.branch.condition_negated) {
                jump_instruction = ORSO_OP_JUMP_IF_TRUE;
            }

            u64 then_jump;
            emit(compiler, chunk, condition->end.line, jump_instruction, (u64*)&then_jump);

            /*
             * The then or else branch get ran once but during code-gen, both branches are processed
             * therefore before any branch is processed, I save the object and regular stack.
            */
            i32 stack_count = compiler->current_stack_size;

            emit_pop(compiler, chunk, orso_type_slot_count(condition->value_type), condition->end.line);
            if (expression_node->data.branch.looping) {
                emit_pop(compiler, chunk, orso_type_slot_count(expression_node->value_type), condition->start.line);
            }

            expression(vm, compiler, ast, expression_node->data.branch.then_expression, chunk);
            emit_storage_type_convert(compiler, chunk,
                    expression_node->data.branch.then_expression->value_type, expression_node->value_type,
                    expression_node->data.branch.then_expression->end.line);

            if (expression_node->data.branch.looping) {
                emit(compiler, chunk, expression_node->data.branch.then_expression->end.line, ORSO_OP_LOOP, (u64)position_before_condition_evaluation);
            }

            u64 else_jump;
            emit(compiler, chunk, expression_node->data.branch.then_expression->end.line, ORSO_OP_JUMP, (u64*)(&else_jump));

            patch_jump(chunk, then_jump);

            i32 then_stack_count = compiler->current_stack_size;
            (void)then_stack_count;

            /*
             * In the else branch, I restore the old stack count and object stack count so that they can be
             * calculated properly
            */
            compiler->current_stack_size = stack_count;

            emit_pop(compiler, chunk, orso_type_slot_count(condition->value_type), condition->end.line);
            if (expression_node->data.branch.looping) {
                emit_pop(compiler, chunk, orso_type_slot_count(expression_node->value_type), expression_node->data.branch.then_expression->end.line);
            }

            if (expression_node->data.branch.else_expression) {
                expression(vm, compiler, ast, expression_node->data.branch.else_expression, chunk);

                emit_storage_type_convert(compiler, chunk,
                        expression_node->data.branch.else_expression->value_type, expression_node->value_type,
                        expression_node->data.branch.else_expression->end.line);
            } else {
                emit(compiler, chunk, expression_node->end.line, ORSO_OP_PUSH_0);

                emit_storage_type_convert(compiler, chunk,
                        &OrsoTypeVoid, expression_node->value_type,
                        expression_node->end.line);
            }

            patch_jump(chunk, else_jump);

            ASSERT(then_stack_count == compiler->current_stack_size, "then and else branch should end up with the same stack size");
            break;
        }
        case ORSO_AST_NODE_TYPE_EXPRESSION_CALL: {
            expression(vm, compiler, ast, expression_node->data.call.callee, chunk);
            emit_storage_type_convert(compiler, chunk, expression_node->value_type, expression_node->value_type_narrowed, expression_node->end.line);

            OrsoType* function_type = expression_node->data.call.callee->value_type_narrowed;

            for (i32 i = 0; i < sb_count(expression_node->data.call.arguments); i++) {
                OrsoASTNode* argument = expression_node->data.call.arguments[i];
                expression(vm, compiler, ast, argument, chunk);

                OrsoType* parameter_type = function_type->data.function.argument_types[i];
                emit_storage_type_convert(compiler, chunk, argument->value_type, parameter_type, expression_node->end.line);
            }

            OrsoType* overload_type = function_type;
            emit(compiler, chunk, expression_node->data.call.callee->start.line, ORSO_OP_CALL, (OrsoType*)overload_type);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            function_expression(vm, compiler, ast, expression_node, chunk);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT:
        case ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR: {
            expression(vm, compiler, ast, expression_node->data.expression, chunk);

            if (!ORSO_TYPE_IS_UNION(expression_node->data.expression->value_type)) {
                emit_put_in_union(compiler, chunk, expression_node->start.line, expression_node->data.expression->value_type);
            }

            Token start = expression_node->data.expression->start;
            Token end = expression_node->data.expression->end;

            OrsoString* expression_string = orso_new_string_from_cstrn(start.start, (end.start + end.length) - start.start);

            OrsoSlot slot = ORSO_SLOT_P(expression_string);
            emit_constant(compiler, chunk, (byte*)&slot, start.line, &OrsoTypeString);

            OrsoSlot value_type = ORSO_SLOT_P(expression_node->data.expression->value_type);
            emit_constant(compiler, chunk, (byte*)&value_type, start.line, &OrsoTypeType);

            if (expression_node->node_type == ORSO_AST_NODE_TYPE_EXPRESSION_PRINT_EXPR) {
                emit(compiler, chunk, start.line, ORSO_OP_PRINT_EXPR);
            } else {
                emit(compiler, chunk, start.line, ORSO_OP_PRINT);
            }

            emit(compiler, chunk, start.line, ORSO_OP_PUSH_0);
            break;
        }

        case ORSO_AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            ASSERT(false, "not implemented");
            break;
        }

        // function signatures MUST be resolved at compile time ALWAYS
        case ORSO_AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: UNREACHABLE();

        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN:
        case ORSO_AST_NODE_TYPE_UNDEFINED:
        case ORSO_AST_NODE_TYPE_DECLARATION: UNREACHABLE();
    }

#undef EMIT_NEGATE
#undef EMIT_NOT
#undef EMIT_BINARY_OP_F64
#undef EMIT_BINARY_OP_I64
#undef EMIT_BINARY_OP
}

static void default_value(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, Chunk* chunk, OrsoASTNode* entity_declaration) {
    OrsoType* conform_type = entity_declaration->value_type;

    if (entity_declaration->data.declaration.initial_value_expression != NULL) {
        OrsoASTNode* default_expression = entity_declaration->data.declaration.initial_value_expression;
        expression(vm, compiler, ast, default_expression, chunk);
        emit_storage_type_convert(compiler, chunk, default_expression->value_type, conform_type, default_expression->start.line);
    } else {
        if (ORSO_TYPE_IS_UNION(conform_type)) {
            ASSERT(orso_type_fits(conform_type, &OrsoTypeVoid), "default type only allowed for void type unions.");
            emit(compiler, chunk, entity_declaration->end.line, ORSO_OP_PUSH_0);
            emit_put_in_union(compiler, chunk, entity_declaration->end.line, conform_type);
        } else {
            ASSERT(entity_declaration->value_index >= 0, "if no expression, there must be an implicit value");
            OrsoSlot* default_value = &ast->folded_constants[entity_declaration->value_index];
            emit_constant(compiler, chunk, (byte*)default_value, entity_declaration->end.line, conform_type);
        }
    }
}

static void entity_declaration(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoASTNode* variable_declaration, Chunk* chunk) {
    ASSERT(variable_declaration->value_type != &OrsoTypeUnresolved, "all declarations must be resolved");
    default_value(vm, compiler, ast, chunk, variable_declaration);
    
    define_entity(vm, compiler, chunk,
            variable_declaration->value_type, &variable_declaration->start,
            variable_declaration->start.line);
}

static void declaration(OrsoVM* vm, Compiler* compiler, OrsoAST* ast, OrsoASTNode* declaration, Chunk* chunk) {
    switch (declaration->node_type) {
        case ORSO_AST_NODE_TYPE_STATEMENT_EXPRESSION: {
            OrsoASTNode* expression_ = declaration->data.expression;
            expression(vm, compiler, ast, expression_, chunk);

            emit_pop(compiler, chunk, orso_type_slot_count(expression_->value_type), declaration->end.line);
            break;
        }
        
        case ORSO_AST_NODE_TYPE_STATEMENT_RETURN: {
            if (declaration->data.expression) {
                expression(vm, compiler, ast, declaration->data.expression, chunk);
                emit_storage_type_convert(compiler, chunk, 
                        declaration->data.expression->value_type,
                        compiler->function->signature->data.function.return_type, declaration->data.expression->end.line);
            } else {
                emit(compiler, chunk, declaration->start.line, ORSO_OP_PUSH_0);
                emit_storage_type_convert(compiler, chunk, &OrsoTypeVoid, compiler->function->signature->data.function.return_type, declaration->start.line);
            }
            
            emit(compiler, chunk, declaration->end.line, ORSO_OP_RETURN, (OrsoType*)compiler->function->signature->data.function.return_type);
            break;
        }

        case ORSO_AST_NODE_TYPE_DECLARATION: {
            entity_declaration(vm, compiler, ast, declaration, chunk);
            break;
        }

        case ORSO_AST_NODE_TYPE_UNDEFINED:
        case ORSO_AST_NODE_TYPE_EXPRESSION_CASE:
            UNREACHABLE();
    }
}

void orso_code_builder_init(OrsoCodeBuilder* builder, OrsoVM* vm, OrsoAST* ast) {
    builder->vm = vm;
    builder->ast = ast;
}

void orso_code_builder_free(OrsoCodeBuilder* builder) { 
    (void)builder;
}

OrsoFunction* orso_generate_expression_function(OrsoCodeBuilder* builder, OrsoASTNode* expression_node, bool is_folding_time) {
    Compiler compiler;
    OrsoType* function_type = orso_type_set_fetch_function(&builder->ast->type_set, expression_node->value_type, NULL, 0);

    OrsoFunction* run_function = orso_new_function();

    compiler_init(&compiler, ORSO_FUNCTION_TYPE_SCRIPT, builder->vm, run_function, (OrsoType*)function_type);
    compiler.skip_function_definitions = !is_folding_time;

    // The vm will put this guy on the stack.
    compiler.max_stack_size = compiler.current_stack_size = 1;
    declare_local_function_definition(&compiler, compiler.function);

    Chunk* top_chunk = &compiler.function->chunk;

    expression(builder->vm, &compiler, builder->ast, expression_node, top_chunk);

    emit(&compiler, top_chunk, expression_node->start.line, ORSO_OP_RETURN, (OrsoType*)expression_node->value_type);

    compiler_end(builder->vm, &compiler, builder->ast, top_chunk, expression_node->end.line);

    OrsoFunction* function = compiler.function;

    compiler_free(&compiler);

    return function;
}

void orso_compile_function(OrsoVM* vm, OrsoAST* ast, OrsoFunction* function, OrsoASTNode* function_definition_expression) {
    Compiler function_compiler;
    compiler_init(&function_compiler, ORSO_FUNCTION_TYPE_FUNCTION, vm, function, function_definition_expression->value_type);

    // this is placed down by the caller
    function_compiler.max_stack_size = function_compiler.current_stack_size = 1;
    declare_local_function_definition(&function_compiler, function_compiler.function);

    begin_scope(&function_compiler);

    function_compiler.function->signature = function_definition_expression->value_type;
    Chunk* function_chunk = &function_compiler.function->chunk;

    OrsoASTFunction* function_definition = &function_definition_expression->data.function;

    for (i32 i = 0; i < sb_count(function_definition->parameter_nodes); i++) {
        OrsoASTNode* parameter = function_definition->parameter_nodes[i];
        apply_stack_effects(&function_compiler, orso_type_slot_count(parameter->value_type));
        define_entity(vm, &function_compiler, function_chunk,
                parameter->value_type, &parameter->start, parameter->start.line);
    }

    expression(vm, &function_compiler, ast, function_definition->block, function_chunk);

    compiler_end(vm, &function_compiler, ast, function_chunk, function_definition_expression->end.line);
}

OrsoFunction* orso_generate_code(OrsoVM* vm, OrsoAST* ast) {
    Compiler compiler;
    OrsoType* function_type = orso_type_set_fetch_function(&ast->type_set, &OrsoTypeVoid, NULL, 0);
    OrsoFunction* main_function = orso_new_function();
    compiler_init(&compiler, ORSO_FUNCTION_TYPE_SCRIPT, vm, main_function, (OrsoType*)function_type);

    // when a function is called it is placed on the stack. The caller does this... In this case,
    // the caller is the virtual machine. So we start at stack size 1 since it should be there when
    // the program starts.
    compiler.max_stack_size = compiler.current_stack_size = 1;
    declare_local_function_definition(&compiler, compiler.function);

    Chunk* top_chunk = &compiler.function->chunk;

    for (i32 i = 0; i < sb_count(ast->root->data.block); i++) {
        declaration(vm, &compiler, ast, ast->root->data.block[i], top_chunk);
    }

    OrsoFunction* function = compiler_end(vm, &compiler, ast, top_chunk, ast->root->end.line);

    // TODO: Am I somehow missing an opertunity to warn for unused functions by doing this?
    for (i32 i = 0; i < sb_count(ast->function_definition_pairs); i++) {
        OrsoFunction* function = ast->function_definition_pairs[i].function;
        if (function->chunk.code == NULL) {
            orso_compile_function(vm, ast, function, ast->function_definition_pairs[i].ast_defintion);
        }
    }

    compiler_free(&compiler);

    return function;
}
