#include "codegen.h"

#ifdef DEBUG_PRINT
#include "debug.h"
#endif

#include <stdarg.h>

#include "opcodes.h"
#include "instructions.h"
#include "type_set.h"

#include "tmp.h"

typedef struct {
    token_t name;
    i32 depth;
    byte slot_count;
    i32 stack_position;
} local_t;


typedef struct functions_t functions_t;
struct functions_t {
    function_t **items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct locals_t locals_t;
struct locals_t {
    local_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct compiler_t {
    function_t *function;

    bool skip_function_definitions;

    functions_t functions_to_compile;

    locals_t locals;
    i32 locals_count;
    i32 scope_depth;
    i32 current_stack_size;
    i32 max_stack_size;

    bool debug;
} compiler_t;

static i32 add_local(compiler_t* compiler, token_t name, i32 slot_count);

// A token_t* is passed in instead of symbol_t* because token_t* cant be garbage collected and symbol_t* can and would be.
static void compiler_init(compiler_t* compiler, vm_t* vm, function_t* function, type_t* creator_type) {
    ASSERT(function, "must be not null");

    // TODO: remove if possible, previously this was used to get the garbage collector but thats being
    //   removed if possible, previously this was used to get the garbage collector but thats being removed
    (void)vm; 

    compiler->debug = true;

    compiler->functions_to_compile = (functions_t){.allocator=&vm->allocator};
    compiler->function = NULL;

    compiler->locals = (locals_t){.allocator=&vm->allocator};
    compiler->scope_depth = 0;
    compiler->locals_count = 0;

    compiler->current_stack_size = 0;
    compiler->max_stack_size = 0;

    compiler->function = function;
    compiler->function->signature = creator_type;
    compiler->skip_function_definitions = false;
}

static void compiler_free(compiler_t* compiler) {
    compiler->locals.count = 0;
    compiler->locals_count = 0;

    compiler->current_stack_size = 0;
    compiler->max_stack_size = 0;
}

static void apply_stack_effects(compiler_t* compiler, i32 stack_effects) {
    compiler->current_stack_size += stack_effects;
    compiler->max_stack_size = compiler->current_stack_size > compiler->max_stack_size
        ? compiler->current_stack_size : compiler->max_stack_size;
}

typedef struct debug_info_t debug_info_t;
struct debug_info_t {
    type_t *type;
    size_t local_count;
};

static size_t emit(compiler_t *compiler, chunk_t *chunk, i32 line, void *op_code_, size_t op_code_size_bytes, debug_info_t *debug);

static void emit_pop_type(compiler_t *compiler, chunk_t *chunk, i32 line, u64 amount) {
    op_push_pop_type_t push_pop_type = {
        .op = OP_POP_TYPE_N,
        .data.n = amount
    };

    emit(compiler, chunk, line, &push_pop_type, sizeof(op_push_pop_type_t), NULL);
}

static void emit_push_type(compiler_t *compiler, chunk_t *chunk, i32 line, type_t *type) {
    op_push_pop_type_t push_pop_type = {
        .op = OP_PUSH_TYPE,
        .data.type = type,
    };

    emit(compiler, chunk, line, &push_pop_type, sizeof(op_push_pop_type_t), NULL);
}

static void emit_debug_instructions(compiler_t *compiler, chunk_t *chunk, i32 line, void *op_code_, debug_info_t *debug) {
    op_code_t *op_code = (op_code_t*)op_code_;

    #define emit_pop_type(AMOUNT) emit_pop_type(compiler, chunk, line, (AMOUNT))
    #define emit_push_type(TYPE) emit_push_type(compiler, chunk, line, (TYPE))

    switch (*op_code) {
        case OP_SET_LVALUE_BOOL:
        case OP_SET_LVALUE_I32:
        case OP_SET_LVALUE_F32:
        case OP_SET_LVALUE_SLOT:
        case OP_SET_LVALUE_BYTES:
        case OP_JUMP_IF_UNION_TRUE:
        case OP_JUMP_IF_UNION_FALSE:
        case OP_JUMP_IF_TRUE:
        case OP_JUMP_IF_FALSE:
        case OP_JUMP:
        case OP_LOOP:
            break;

        case OP_NO_OP: {
            break;
        }

        case OP_I64_TO_F64: {
            emit_pop_type(1);
            emit_push_type(&OrsoTypeFloat64);
            break;
        }

        case OP_F64_TO_I64: {
            emit_pop_type(1);
            emit_push_type(&OrsoTypeInteger64);
            break;
        }

        case OP_LOGICAL_NOT: {
            emit_pop_type(1);
            emit_push_type(&OrsoTypeBool);
            break;
        }

        case OP_NEGATE_F64:
        case OP_NEGATE_I64: {
            NOT_NULL(debug);

            emit_pop_type(1);
            emit_push_type(debug->type);
            break;
        }

        case OP_CONSTANT:
        case OP_LOCAL:
        case OP_GLOBAL:
        case OP_PUSH_GLOBAL_ADDRESS:
        case OP_PUSH_LOCAL_ADDRESS:
        case OP_PUSH_0:
        case OP_PUSH_1: {
            NOT_NULL(debug);

            emit_push_type(debug->type);
            break;
        }

        case OP_POPN:
        case OP_POP: {
            emit_pop_type(1);
            break;
        }

        case OP_POP_SCOPE: {
            NOT_NULL(debug);

            emit_pop_type(debug->local_count);
            emit_push_type(debug->type);
            break;
       }

        case OP_ADD_I64:
        case OP_SUBTRACT_I64:
        case OP_MULTIPLY_I64:
        case OP_DIVIDE_I64:
        case OP_ADD_F64:
        case OP_SUBTRACT_F64:
        case OP_MULTIPLY_F64:
        case OP_DIVIDE_F64:
        case OP_ADD_PTR_I64: {
            NOT_NULL(debug);

            emit_pop_type(2);
            emit_push_type(debug->type);
            break;
       }

        case OP_EQUAL_I64:
        case OP_LESS_I64:
        case OP_GREATER_I64:
        case OP_EQUAL_F64:
        case OP_LESS_F64:
        case OP_GREATER_F64:
        case OP_EQUAL_SYMBOL:
        case OP_EQUAL_STRING: {
            emit_pop_type(2);
            emit_push_type(&OrsoTypeBool);
            break;
        }

        case OP_CONCAT_STRING: {
            emit_pop_type(2);
            emit_push_type(&OrsoTypeString);
            break;
        }

        case OP_FIELD: {
            NOT_NULL(debug);

            emit_pop_type(1);
            emit_push_type(debug->type);
            break;
        }

        case OP_CALL: {
            NOT_NULL(debug);

            op_call_t *call = (op_call_t*)op_code;
            emit_pop_type(call->argument_slots + 1);
            emit_push_type(debug->type);
            break;
        }

        case OP_RETURN: {
            op_return_t *return_ = (op_return_t*)op_code;
            emit_pop_type(return_->size_slots);
            break;
        }

        case OP_PRINT:
        case OP_PRINT_EXPR: {
            emit_pop_type(3);
            break;
        }

        case OP_POP_TYPE_N:
        case OP_PUSH_TYPE: {
            break;
        }

        case OP_PUT_IN_UNION:
        case OP_NARROW_UNION: {
            UNREACHABLE();
            break;
        }
    }
}

static size_t emit(compiler_t *compiler, chunk_t *chunk, i32 line, void *op_code_, size_t op_code_size_bytes, debug_info_t *debug) {
    op_code_t *op_code = (op_code_t*)op_code_;
    u32 stack_effect = 0;

    switch (*op_code) {
        case OP_NO_OP: {
            break;
        }
        case OP_I64_TO_F64:
        case OP_F64_TO_I64:
        case OP_NEGATE_I64:
        case OP_NEGATE_F64:
        case OP_LOGICAL_NOT:
        case OP_JUMP_IF_UNION_TRUE:
        case OP_JUMP_IF_UNION_FALSE:
        case OP_JUMP_IF_TRUE:
        case OP_JUMP_IF_FALSE:
        case OP_JUMP:
        case OP_PUT_IN_UNION:
        case OP_LOOP: {
            stack_effect = 0;
            break;
        }

        case OP_PUSH_1:
        case OP_PUSH_0:
        case OP_PUSH_LOCAL_ADDRESS:
        case OP_PUSH_GLOBAL_ADDRESS: {
            stack_effect = 1;
            break;
        }

        case OP_POP:
        case OP_ADD_I64:
        case OP_SUBTRACT_I64:
        case OP_MULTIPLY_I64:
        case OP_DIVIDE_I64:
        case OP_ADD_F64:
        case OP_SUBTRACT_F64:
        case OP_MULTIPLY_F64:
        case OP_DIVIDE_F64:
        case OP_ADD_PTR_I64:
        case OP_EQUAL_I64:
        case OP_LESS_I64:
        case OP_GREATER_I64:
        case OP_EQUAL_F64:
        case OP_LESS_F64: 
        case OP_GREATER_F64:
        case OP_EQUAL_SYMBOL:
        case OP_EQUAL_STRING:
        case OP_CONCAT_STRING:
        case OP_SET_LVALUE_BYTES:
        case OP_SET_LVALUE_BOOL:
        case OP_SET_LVALUE_I32:
        case OP_SET_LVALUE_F32:
        case OP_SET_LVALUE_SLOT:
        case OP_NARROW_UNION: {
            stack_effect = -1;
            break;
        }

        case OP_POPN: {
            op_popn_t *popn = (op_popn_t*)op_code;
            stack_effect = -popn->n;
            break;
        }

        case OP_POP_SCOPE: {
            op_pop_scope_t *pop_scope = (op_pop_scope_t*)op_code;
            stack_effect = -pop_scope->scope_size_slots;
            break;
        }
        case OP_GLOBAL:
        case OP_LOCAL:
        case OP_CONSTANT: {
            op_location_t *location = (op_location_t*)op_code;
            stack_effect = bytes_to_slots(location->size_bytes);
            break;
        }

        case OP_FIELD: {
            op_field_t *field = (op_field_t*)op_code;
            stack_effect = field->value_size_bytes - field->size_bytes;
            break;
        }

        case OP_CALL: {
            op_call_t *call = (op_call_t*)op_code;
            u16 argument_slots = call->argument_slots;

            stack_effect = -argument_slots - 1;
            break;
        }

        case OP_RETURN: {
            op_return_t *return_ = (op_return_t*)op_code;
            stack_effect = -return_->size_slots;
            break;
        }

        case OP_PRINT:
        case OP_PRINT_EXPR: {
            NOT_NULL(debug);
            stack_effect = -2;
            stack_effect -= type_slot_count(debug->type);
            break;
        }

        case OP_POP_TYPE_N:
        case OP_PUSH_TYPE: {
            stack_effect = 0;
            break;
        }
    }

    for (size_t i = 0; i < op_code_size_bytes; ++i) {
        chunk_write(chunk, ((byte*)op_code)[i], line);
    }

    apply_stack_effects(compiler, stack_effect);

    if (compiler->debug) {
        emit_debug_instructions(compiler, chunk, line, op_code_, debug);
    }

    return chunk->code.count - op_code_size_bytes;
}

static void emit1(compiler_t *compiler, chunk_t *chunk, i32 line, const op_code_t op_code, debug_info_t *debug) {
    emit(compiler, chunk, line, (void*)&op_code, 1, debug);
}

static void emit_constant(compiler_t *compiler, chunk_t *chunk, byte *data, i32 line, type_t *type) {
    u32 size = type_size_bytes(type);
    u32 address = CHUNK_ADD_CONSTANT(chunk, data, size, type);
    address *= sizeof(slot_t);

    ASSERT(address < UINT32_MAX, "address is too high TDO: proper error");
    ASSERT(size < UINT16_MAX, "address too high TODO: proper error");

    op_location_t location = {
        .op = OP_CONSTANT,
        .index_slots = address,
        .size_bytes = size,
    };

    debug_info_t debug = {.type=type};
    emit(compiler, chunk, line, (op_code_t*)&location, sizeof(op_location_t), &debug);
}

static void emit_put_in_union(compiler_t *compiler, chunk_t *chunk, i32 line, type_t *type) {
    slot_t slot = SLOT_P(type);
    emit_constant(compiler, chunk, (byte*)&slot, line, &OrsoTypeType);
    {
        op_put_in_union_t put_in_union = {
            .op = OP_PUT_IN_UNION,
            .size_bytes = type_size_bytes(type)
        };

        debug_info_t debug = {.type=type};
        emit(compiler, chunk, line, &put_in_union, sizeof(op_put_in_union_t), &debug);
    }
}

static void patch_jump(chunk_t *chunk, size_t update_index) {
    size_t jump = chunk->code.count - (update_index + sizeof(op_jump_t));
    op_jump_t *jump_code = (op_jump_t*)(chunk->code.items + update_index);

    if (jump > UINT32_MAX) {
        ASSERT(false, "TODO");
    }

    jump_code->offset = (u32)jump;
}

static void emit_entity_get(compiler_t *compiler, u32 index, type_t *type, chunk_t *chunk, i32 line, bool is_local) {
    index *= sizeof(slot_t);
    ASSERT(index <= UINT32_MAX, "must be short");
    u32 size_bytes = type_size_bytes(type);
    ASSERT(size_bytes <= UINT16_MAX, "must be short");
    {
        op_location_t location = {
            .op = is_local ? OP_LOCAL : OP_GLOBAL,
            .index_slots = index,
            .size_bytes = (u16)size_bytes,
        };

        debug_info_t debug = {.type=type};
        emit(compiler, chunk, line, (op_code_t*)&location, sizeof(op_location_t), &debug);
    }
}

static void emit_type_convert(compiler_t *compiler, type_t* from_type, type_t* to_type, chunk_t* chunk, i32 line) {
    bool include_bool = true;
    ASSERT(type_is_number(from_type, include_bool) && type_is_number(to_type, include_bool), "Implicit type conversion only works for number types right now.");
    if (type_is_float(from_type) && type_is_integer(to_type, include_bool)) {
        emit1(compiler, chunk, line, OP_F64_TO_I64, NULL);
    } else if (type_is_integer(from_type, include_bool) && type_is_float(to_type)) {
        emit1(compiler, chunk, line, OP_I64_TO_F64, NULL);
    } 
}

static void emit_storage_type_convert(compiler_t *compiler, chunk_t *chunk, type_t *source, type_t *destination, i32 line) {
    if (TYPE_IS_UNION(source) && !TYPE_IS_UNION(destination)) {
        op_narrow_union_t narrow_union = {
            .op = OP_NARROW_UNION,
            .offset_bytes = type_size_bytes(source),
        };

        debug_info_t debug = {.type=source};
        emit(compiler, chunk, line, &narrow_union, sizeof(op_narrow_union_t), &debug);
    } else if (!TYPE_IS_UNION(source) && TYPE_IS_UNION(destination)) {
        emit_put_in_union(compiler, chunk, line, source);
    }
}

static void emit_pop_value(compiler_t* compiler, chunk_t* chunk, type_t *type, i32 line) {
    size_t pop_count = type_slot_count(type);

    if (pop_count < 1) {
        return;
    }
    
    if (pop_count == 1) {
        op_code_t op = OP_POP;
        emit(compiler, chunk, line, &op, sizeof(op_code_t), NULL);
    } else {
        while (pop_count > 0) {
            byte batch = pop_count > UINT8_MAX ? UINT8_MAX : pop_count;

            op_popn_t popn = {
                .op = OP_POPN,
                .n = batch,
            };
            emit(compiler, chunk, line, (op_code_t*)&popn, sizeof(op_popn_t), NULL);
            pop_count -= batch;
        }
    }
}

static function_t* compiler_end(vm_t* vm, compiler_t* compiler, ast_t* ast, chunk_t* chunk, i32 line) {
    debug_info_t debug = {.type=&OrsoTypeVoid};
    emit1(compiler, chunk, line, OP_PUSH_0, &debug);
    {
        op_return_t return_ = {
            .op = OP_RETURN,
            .size_slots = type_slot_count(&OrsoTypeVoid)
        };
        emit(compiler, chunk, line, &return_, sizeof(op_return_t), &debug);
    }

    // compile left over functions
    // TODO: make this faster. this is super slow because i need a hashmap to look up the functions
    // faster. Or I need to figure out a way to decide whether or not i should compile a function
    // on the fly.
    {
        functions_t functions_to_compile = {.allocator = &vm->allocator};
        for (size_t i = 0; i < compiler->functions_to_compile.count; i++) {
            array_push(&functions_to_compile, compiler->functions_to_compile.items[i]);
        }

        compiler->functions_to_compile = (functions_t){.allocator=&vm->allocator};

        for (size_t i = 0; i < functions_to_compile.count; i++) {
            function_t *function = functions_to_compile.items[i];
            if (function->chunk.code.items != NULL) {
                continue;
            }

            for (size_t j = 0; j < ast->function_definition_pairs.count; ++j) {
                if (ast->function_definition_pairs.items[j].function != function) {
                    continue;
                }

                if (!ast->function_definition_pairs.items[j].ast_defintion->data.function.compilable) {
                    break;
                }

                compile_function(vm, ast, function, ast->function_definition_pairs.items[j].ast_defintion);
            }
        }
    }

#ifdef DEBUG_PRINT
    chunk_disassemble(chunk, compiler->function->binded_name ?
            compiler->function->binded_name->text : "<anonymous at TODO: line and column>");
#endif

    return compiler->function;
}

#define DECLARE_GLOBAL(vm, name, slot_count, type) add_global(vm, name, slot_count, type)
static i32 add_global(vm_t *vm, token_t *name, i32 slot_count, type_t* type)
{
    symbol_t* identifier = orso_new_symbol_from_cstrn(name->start, name->length, &vm->symbols, &vm->allocator);

    slot_t index_slot;
    ASSERT(!symbol_table_get(&vm->globals.name_to_index, identifier, &index_slot), "double global definition");

    // Use the specific khash for this
    index_slot = SLOT_I(vm->globals.values.count);
    symbol_table_set(&vm->globals.name_to_index, identifier, index_slot);

    for (i32 i = 0; i < slot_count; i++) {
        array_push(&vm->globals.types, type);
        array_push(&vm->globals.values, SLOT_I(0));
    }

    i32 index = index_slot.as.i;

    return index;
}

static i32 add_local(compiler_t *compiler, token_t name, i32 slot_count) {
    ASSERT(slot_count > 0, "local must consume stack space");

    while (compiler->locals_count >= 0 && compiler->locals.count <= (size_t)compiler->locals_count) {
        array_push(&compiler->locals, (local_t){ .depth = 0 });
    }

    local_t *previous_local = compiler->locals_count > 0 ?
            &compiler->locals.items[compiler->locals_count - 1] : NULL;
    local_t *local = &compiler->locals.items[compiler->locals_count++];
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

static i32 declare_local_entity(compiler_t *compiler, token_t *name, i32 slot_count) {
    return add_local(compiler, *name, slot_count);
}

static i32 retrieve_global_variable(vm_t *vm, token_t *name) {
    symbol_t *identifier = orso_new_symbol_from_cstrn(name->start, name->length, &vm->symbols, &vm->allocator);
    slot_t value;
    if (!symbol_table_get(&vm->globals.name_to_index, identifier, &value)) {
        return -1;
    }

    return value.as.i;
}

static bool identifiers_equal(token_t *a, token_t *b) {
    if (a->length != b->length) {
        return false;
    }

    return memcmp(a->start, b->start, a->length) == 0;
}

static i64 retrieve_local_variable(compiler_t *compiler, token_t *name) {
    for (i64 i = compiler->locals_count - 1; i >= 0; i--) {
        local_t *local = &compiler->locals.items[i];
        if (identifiers_equal(name, &local->name)) {
            return i;
        }
    }

    return -1;
}

static u32 retrieve_variable(vm_t *vm, compiler_t *compiler, token_t *name, bool* is_local) {
    i64 index = retrieve_local_variable(compiler, name);
    if (index < 0) {
        index = retrieve_global_variable(vm, name);
        ASSERT(index >= 0, "global must be defined.");

        *is_local = false;
        return index;
    } else {
        index = compiler->locals.items[index].stack_position;

        *is_local = true;
        return index;
    }
}

static void begin_scope(compiler_t *compiler) {
    compiler->scope_depth++;
}

static void end_scope(compiler_t *compiler, chunk_t *chunk, type_t *block_value_type, i32 line) {
    --compiler->scope_depth;

    // pop all local object markers
    size_t total_local_slot_count = 0;
    size_t scope_local_count = 0;
    while (compiler->locals_count > 0) {
        local_t *local = &compiler->locals.items[compiler->locals_count - 1];
        if (local->depth <= compiler->scope_depth) {
            break;
        }

        total_local_slot_count += local->slot_count;
        --compiler->locals_count;
        ++scope_local_count;
    }

    size_t block_value_slots = type_slot_count(block_value_type);

    if (total_local_slot_count > UINT8_MAX || block_value_slots > UINT8_MAX) {
        printf("TODO: need to create code gen error");
        UNREACHABLE();
        return;
    }

    {
        op_pop_scope_t pop_scope = {
            .op = OP_POP_SCOPE,
            .scope_size_slots = (byte)total_local_slot_count,
            .value_size_slots = (byte)block_value_slots,
        };

        debug_info_t debug = {.type=block_value_type, .local_count=scope_local_count};
        emit(compiler, chunk, line, (op_code_t*)&pop_scope, sizeof(op_pop_scope_t), &debug);
    }
}

static void declaration(vm_t *vm, compiler_t *compiler, ast_t *ast, ast_node_t *declaration, chunk_t *chunk);
static void expression(vm_t *vm, compiler_t *compiler, ast_t *ast, ast_node_t *expression_node, chunk_t *chunk);

static type_t *gen_block(vm_t *vm, compiler_t *compiler, ast_t *ast, chunk_t *chunk, ast_nodes_t block, i32 node_count, i32 end_line) {
    ast_node_t *final_expression_statement = node_count > 0 ? block.items[node_count - 1] : NULL;

    final_expression_statement = final_expression_statement && final_expression_statement->node_type != AST_NODE_TYPE_STATEMENT_EXPRESSION ?
            NULL : final_expression_statement;

    for (i32 i = 0; i < node_count - (final_expression_statement != NULL); i++) {
        declaration(vm, compiler, ast, block.items[i], chunk);
    }

    type_t *return_value_type = &OrsoTypeInvalid;
    if (final_expression_statement) {
        ast_node_t *final_expression = final_expression_statement->data.expression;
        return_value_type = final_expression->value_type;
        expression(vm, compiler, ast, final_expression, chunk);
    } else {
        return_value_type = &OrsoTypeVoid;
        debug_info_t debug = {.type=&OrsoTypeVoid};
        emit1(compiler, chunk, end_line, OP_PUSH_0, &debug);
    }

    return return_value_type;
}

static u64 define_global_entity(vm_t *vm, token_t *name, type_t *type) {
    u32 slot_size = type_slot_count(type);
    u32 index = DECLARE_GLOBAL(vm, name, slot_size, type);
    return index;
}

static void declare_local_function_definition(compiler_t* compiler, function_t* function){ 
    // used in assert
    (void)function;
    // TODO: fix this assert, why does function->type need to be converted to type? Will it never be unresolved?
    ASSERT((type_t*)function->signature != &OrsoTypeUnresolved, "Must have resolved type");

    token_t empty = { .start = "", .length = 0 };
    declare_local_entity(compiler, &empty, 1);
}

static void function_expression(vm_t* vm, compiler_t* compiler, ast_t* ast, ast_node_t* function_defintion_expression, chunk_t* chunk) {
    ASSERT(TYPE_IS_FUNCTION(function_defintion_expression->value_type), "must be function if calling this");

    function_t* stored_function = (function_t*)ast->folded_constants.items[function_defintion_expression->value_index].as.p;
    if (!compiler->skip_function_definitions && stored_function->chunk.code.items == NULL /*is not compiled yet*/) {
        compile_function(vm, ast, stored_function, function_defintion_expression);
    }
    slot_t function_value = SLOT_P(stored_function);
    emit_constant(compiler, chunk, (byte*)&function_value, function_defintion_expression->start.line, stored_function->signature);
}

static void gen_primary(compiler_t *compiler, chunk_t *chunk, ast_t *ast, type_t *value_type, i32 value_index, i32 line) {
    ASSERT(value_index >= 0, "must be pointing to a contant value...");

    slot_t *value = &ast->folded_constants.items[value_index];
    switch (value_type->kind) {
        case TYPE_BOOL:
        case TYPE_INT32:
        case TYPE_INT64:
        case TYPE_FLOAT32:
        case TYPE_FLOAT64:
        case TYPE_VOID: {
            debug_info_t debug = {.type=value_type};
            if (value->as.i == 0) {
                emit1(compiler, chunk, line, OP_PUSH_0, &debug);
            } else if (value->as.i == 1) {
                emit1(compiler, chunk, line, OP_PUSH_1, &debug);
            } else {
                emit_constant(compiler, chunk, (byte*)value, line, value_type);
            }
            break;
        }
        
        case TYPE_TYPE:
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_SYMBOL:
        case TYPE_STRING:
        case TYPE_UNION:
        case TYPE_STRUCT: {
            emit_constant(compiler, chunk, (byte*)value, line, value_type);
            break;
        }

        default: UNREACHABLE();
    }
}

static void expression_lvalue(vm_t *vm, compiler_t *compiler, ast_t *ast, ast_node_t *lvalue_node, chunk_t *chunk) {
    switch (lvalue_node->node_type) {
        case AST_NODE_TYPE_EXPRESSION_DOT:
            expression_lvalue(vm, compiler, ast, lvalue_node->lvalue_node, chunk);

            ast_node_t *referencing_declaration = lvalue_node->data.dot.referencing_declaration;

            if (TYPE_IS_STRUCT(lvalue_node->data.dot.lhs->value_type)) {
                type_t* struct_type = lvalue_node->data.dot.lhs->value_type;
                token_t field_name = referencing_declaration->data.declaration.identifier;
                struct_field_t *field = NULL;
                for (i32 i = 0; i < struct_type->data.struct_.field_count; ++i) {
                    field = &struct_type->data.struct_.fields[i];
                    if ((size_t)field_name.length == strlen(field->name) && strncmp(field->name, field_name.start, field_name.length) == 0) {
                        break;
                    }
                    field = NULL;
                }
                ASSERT(field != NULL, "the field offset must be found at this point since it should have been found during analysis");

                if (field->offset > 0) {
                    slot_t offset = SLOT_U(field->offset);
                    emit_constant(compiler, chunk, (byte*)&offset, lvalue_node->data.dot.identifier.line, &OrsoTypeInteger64);
                    emit1(compiler, chunk, lvalue_node->data.dot.identifier.line, OP_ADD_PTR_I64, NULL);
                }
            } else {
                UNREACHABLE();
            }
            break;

        case AST_NODE_TYPE_EXPRESSION_ENTITY: {
            bool is_local;
            u32 index;

            token_t identifier_token = lvalue_node->data.dot.identifier;

            index = retrieve_variable(vm, compiler, &identifier_token, &is_local);
            index *= sizeof(slot_t);

            op_code_t push_code = is_local ? OP_PUSH_LOCAL_ADDRESS : OP_PUSH_GLOBAL_ADDRESS;
            ASSERT((!is_local || index <= UINT16_MAX), "local address must be short");
            op_push_address_t push_address = {
                .op = push_code,
                .index = (u16)index,
            };

            debug_info_t debug = {.type=lvalue_node->value_type};
            emit(compiler, chunk, identifier_token.line, &push_address, sizeof(op_push_address_t), &debug);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_GROUPING:
            expression_lvalue(vm, compiler, ast, lvalue_node->lvalue_node, chunk);
            break;

        case AST_NODE_TYPE_UNDEFINED:
        case AST_NODE_TYPE_DECLARATION:
        case AST_NODE_TYPE_STATEMENT_RETURN:
        case AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case AST_NODE_TYPE_EXPRESSION_PRINT:
        case AST_NODE_TYPE_EXPRESSION_PRINT_EXPR:
        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT:
        case AST_NODE_TYPE_EXPRESSION_BINARY:
        case AST_NODE_TYPE_EXPRESSION_UNARY:
        case AST_NODE_TYPE_EXPRESSION_CALL:
        case AST_NODE_TYPE_EXPRESSION_PRIMARY:
        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT:
        case AST_NODE_TYPE_EXPRESSION_BLOCK:
        case AST_NODE_TYPE_EXPRESSION_BRANCHING:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION:
        case AST_NODE_TYPE_EXPRESSION_STATEMENT:
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE:
        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER:
            UNREACHABLE();
            break;
    }
}

static op_code_t get_lvalue_op_by_type(type_t *type) {
    op_code_t lvalue_set = OP_NO_OP;
    switch(type->kind) {
        case TYPE_VOID: {
            lvalue_set = OP_NO_OP;
            break;
        }
        case TYPE_BOOL: {
            lvalue_set = OP_SET_LVALUE_BOOL;
            break;
        }
        case TYPE_FUNCTION:
        case TYPE_NATIVE_FUNCTION:
        case TYPE_POINTER:
        case TYPE_STRING:
        case TYPE_SYMBOL:
        case TYPE_TYPE:
        case TYPE_FLOAT64:
        case TYPE_INT64: {
            lvalue_set = OP_SET_LVALUE_SLOT;
            break;
        }
        case TYPE_INT32: {
            lvalue_set = OP_SET_LVALUE_I32;
            break;
        }
        case TYPE_FLOAT32: {
            lvalue_set = OP_SET_LVALUE_F32;
            break;
        }
        case TYPE_UNION:
        case TYPE_STRUCT: {
            lvalue_set = OP_SET_LVALUE_BYTES;
            break;
        }

        case TYPE_UNRESOLVED:
        case TYPE_UNDEFINED:
        case TYPE_INVALID: UNREACHABLE();
    }

    return lvalue_set;
}

static void expression(vm_t *vm, compiler_t *compiler, ast_t *ast, ast_node_t *expression_node, chunk_t *chunk) {
#define EMIT_BINARY_OP(OP, TYPE) do { \
    debug_info_t debug = {.type=expression_node->value_type}; \
    emit1(compiler, chunk, operator.line,OP_##OP##_##TYPE, &debug); \
} while(false)

#define EMIT_BINARY_OP_I64(OP) EMIT_BINARY_OP(OP, I64)
#define EMIT_BINARY_OP_F64(OP) EMIT_BINARY_OP(OP, F64)

#define EMIT_NOT() do { \
    debug_info_t debug = {.type=expression_node->value_type}; \
    emit1(compiler, chunk, operator.line, OP_LOGICAL_NOT, &debug); \
} while(false)

#define EMIT_NEGATE(TYPE) do { \
    debug_info_t debug = {.type=expression_node->value_type}; \
    emit1(compiler, chunk, operator.line, OP_NEGATE_##TYPE, &debug); \
} while (false)

    if (expression_node->value_index >= 0) {
        // TODO: for now going to do this super naively until I get a better hash table that is more generic that I can
        // use for more things. This is will be super slow but I just want to get it to work.
        if (!compiler->skip_function_definitions && TYPE_IS_FUNCTION(expression_node->value_type)) {
            function_t *function = (function_t*)ast->folded_constants.items[expression_node->value_index].as.p;
            array_push(&compiler->functions_to_compile, function);
        }

        gen_primary(compiler, chunk, ast,
                expression_node->value_type,
                expression_node->value_index, expression_node->start.line);
        return;
    }

    switch(expression_node->node_type) {
        case AST_NODE_TYPE_EXPRESSION_BINARY: {
            token_t operator = expression_node->operator;
            ast_node_t* left = expression_node->data.binary.lhs;
            ast_node_t* right = expression_node->data.binary.rhs;

            switch (operator.type) {
                case TOKEN_OR:
                case TOKEN_AND: {
                    expression(vm, compiler, ast, left, chunk);

                    op_code_t jump_instruction;
                    if (TYPE_IS_STRUCT(left->value_type)) {
                        jump_instruction = operator.type == TOKEN_AND ? OP_NO_OP : OP_JUMP;
                    } else if (TYPE_IS_UNION(left->value_type)) {
                        jump_instruction = operator.type == TOKEN_AND ? OP_JUMP_IF_UNION_FALSE : OP_JUMP_IF_UNION_TRUE;
                    } else {
                        jump_instruction = operator.type == TOKEN_AND ? OP_JUMP_IF_FALSE : OP_JUMP_IF_TRUE;
                    }

                    size_t jump_rest;
                    unless (jump_instruction == OP_NO_OP) {
                        op_jump_t jump = {
                            .op = jump_instruction,
                        };
                        jump_rest = emit(compiler, chunk, operator.line, (op_code_t*)&jump, sizeof(op_jump_t), NULL);
                    }

                    i32 previous_stack_count = compiler->current_stack_size;
                    (void)previous_stack_count;

                    emit_pop_value(compiler, chunk, expression_node->value_type, right->start.line);

                    expression(vm, compiler, ast, right, chunk);

                    unless (jump_instruction == OP_NO_OP) {
                        patch_jump(chunk, jump_rest);
                    }

                    ASSERT(previous_stack_count == compiler->current_stack_size, "must be the as before jump.");
                    break;
                }

                default: {
                    expression(vm, compiler, ast, left, chunk);

                    if (TYPE_IS_UNION(left->value_type)) {
                        op_narrow_union_t narrow_union = {
                            .op = OP_NARROW_UNION,
                            .offset_bytes = type_size_bytes(left->value_type)
                        };
                        debug_info_t debug = {.type=left->value_type};
                        emit(compiler, chunk, left->start.line, &narrow_union, sizeof(op_narrow_union_t), &debug);
                    }

                    expression(vm, compiler, ast, right, chunk);

                    if (TYPE_IS_UNION(right->value_type)) {
                        op_narrow_union_t narrow_union = {
                            .op = OP_NARROW_UNION,
                            .offset_bytes = type_size_bytes(right->value_type)
                        };
                        debug_info_t debug = {.type=right->value_type};
                        emit(compiler, chunk, right->start.line, &narrow_union, sizeof(op_narrow_union_t), &debug);
                    }

                    if (type_is_integer(left->value_type_narrowed, true)) {
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

                    } else if (type_is_float(left->value_type_narrowed)) {
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

        case AST_NODE_TYPE_EXPRESSION_UNARY: {
            expression(vm, compiler, ast, expression_node->data.expression, chunk);
            emit_storage_type_convert(compiler, chunk, expression_node->data.expression->value_type, expression_node->value_type, expression_node->start.line);

            ast_node_t* unary = expression_node->data.expression;
            token_t operator = expression_node->operator;

            switch (operator.type) {
                case TOKEN_MINUS: {
                    if (type_is_or_has_integer(unary->value_type, true)) {
                        EMIT_NEGATE(I64);
                    } else if (type_is_or_has_float(unary->value_type)) {
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

        case AST_NODE_TYPE_EXPRESSION_GROUPING: {
            expression(vm, compiler, ast, expression_node->data.expression, chunk);
            emit_storage_type_convert(compiler, chunk, expression_node->data.expression->value_type, expression_node->value_type, expression_node->end.line);
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_PRIMARY: {
            gen_primary(compiler, chunk, ast,
                    expression_node->value_type,
                    expression_node->value_index, expression_node->start.line);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ENTITY: {
            token_t identifier_token = expression_node->data.dot.identifier;
            bool is_local;
            u32 index = retrieve_variable(vm, compiler, &identifier_token, &is_local);
            emit_entity_get(compiler, index, expression_node->value_type, chunk, expression_node->start.line, is_local);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_DOT: {
            expression(vm, compiler, ast, expression_node->data.dot.lhs, chunk);

            ASSERT(TYPE_IS_STRUCT(expression_node->data.dot.lhs->value_type), "LHS must be a struct for now");

            u32 struct_size = type_size_bytes(expression_node->data.dot.lhs->value_type);
            struct_field_t* field = type_struct_find_field(expression_node->data.dot.lhs->value_type, expression_node->data.dot.identifier.start, expression_node->data.dot.identifier.length);
            ASSERT(field, "this must exist, otherwise static analyzer would have caught it");

            u32 field_offset = field->offset;
            size_t field_size = 0;

            ASSERT(struct_size < UINT16_MAX, "TODO");
            ASSERT(field_offset < UINT16_MAX, "TODO");
            ASSERT(field_size < UINT16_MAX, "TODO");

            switch (expression_node->value_type->kind) {
                case TYPE_VOID: {
                    field_size = 0;
                    break;
                }
                case TYPE_BOOL: {
                    field_size = sizeof(bool);
                    break;
                }
                case TYPE_FUNCTION:
                case TYPE_NATIVE_FUNCTION:
                case TYPE_POINTER:
                case TYPE_STRING:
                case TYPE_SYMBOL:
                case TYPE_TYPE:
                case TYPE_FLOAT64:
                case TYPE_INT64: {
                    field_size = sizeof(u64);
                    break;
                }
                case TYPE_INT32: {
                    field_size = sizeof(i32);
                    break;
                }
                case TYPE_FLOAT32: {
                    field_size = sizeof(f32);
                    break;
                }
                case TYPE_UNION:
                case TYPE_STRUCT: {
                    field_size = type_size_bytes(field->type);
                    break;
                }

                case TYPE_UNRESOLVED:
                case TYPE_UNDEFINED:
                case TYPE_INVALID: UNREACHABLE();
            }

            {
                op_field_t op_field = {
                    .op = OP_FIELD,
                    .value_size_bytes = (u16)struct_size,
                    .offset_bytes = (u16)field_offset,
                    .size_bytes = (u16)field_size,
                };

                debug_info_t debug = {.type=field->type};
                emit(compiler, chunk, expression_node->data.dot.identifier.line, (op_code_t*)&op_field, sizeof(op_field_t), &debug);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_ASSIGNMENT: {
            expression(vm, compiler, ast, expression_node->data.binary.rhs, chunk);
            type_t *right_side_type = expression_node->data.binary.rhs->value_type;

            emit_storage_type_convert(compiler, chunk, right_side_type, expression_node->value_type, expression_node->start.line);

            ASSERT(expression_node->data.binary.lhs == expression_node->lvalue_node || expression_node->data.binary.lhs->lvalue_node == expression_node->lvalue_node, "the lvalue for an assignment is always the lhs or its lvalue of the binary by convention");
            expression_lvalue(vm, compiler, ast, expression_node->lvalue_node, chunk);

            bool is_field = (expression_node->lvalue_node->node_type == AST_NODE_TYPE_EXPRESSION_DOT);

            debug_info_t debug = {.type=expression_node->value_type};
            if (is_field || TYPE_IS_STRUCT(expression_node->lvalue_node->value_type) || TYPE_IS_UNION(expression_node->lvalue_node->value_type)) {
                op_code_t lvalue_set = get_lvalue_op_by_type(expression_node->lvalue_node->value_type);

                if (lvalue_set != OP_NO_OP) {
                    op_set_lvalue_t set_lvalue = {
                        .op = lvalue_set,
                        .size_bytes = type_size_bytes(expression_node->value_type)
                    };
                    // only lvalue bytes takes argument, but it'll just be ignored for the other op codes.
                    emit(compiler, chunk, expression_node->data.binary.lhs->end.line, &set_lvalue, sizeof(op_set_lvalue_t), &debug);
                }
            } else {
                op_set_lvalue_t set_lvalue = {
                    .op = OP_SET_LVALUE_SLOT,
                    .size_bytes = type_size_bytes(expression_node->value_type)
                };
                emit(compiler, chunk, expression_node->data.binary.lhs->end.line, &set_lvalue, sizeof(op_set_lvalue_t), &debug);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_CAST_IMPLICIT: {
            ast_node_t* operand = expression_node->data.expression;
            expression(vm, compiler, ast, operand, chunk);
            emit_type_convert(compiler, operand->value_type, expression_node->value_type, chunk, operand->start.line);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_BLOCK: {
            begin_scope(compiler);

            ast_nodes_t block = expression_node->data.block;
            i32 node_count = block.count;
            type_t* return_value_type = gen_block(vm, compiler, ast, chunk, block, node_count, expression_node->end.line);

            end_scope(compiler, chunk, return_value_type, expression_node->end.line);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STATEMENT: {
            tmp_arena_t *tmp = allocator_borrow(); {
                ast_nodes_t block = {.allocator=tmp->allocator};
                array_push(&block, expression_node->data.statement);
                gen_block(vm, compiler, ast, chunk, block, 1, expression_node->start.line);
            } allocator_return(tmp);
            
            break;
        }
        
        case AST_NODE_TYPE_EXPRESSION_BRANCHING: {
            if (expression_node->data.branch.looping) {
                debug_info_t debug = {.type=&OrsoTypeVoid};
                emit1(compiler, chunk, expression_node->start.line, OP_PUSH_0, &debug);
                emit_storage_type_convert(compiler, chunk, &OrsoTypeVoid, expression_node->value_type, expression_node->start.line);
            }

            u32 position_before_condition_evaluation = chunk->code.count;

            ast_node_t *condition = expression_node->data.branch.condition;
            expression(vm, compiler, ast, condition, chunk);

            op_code_t jump_instruction = OP_JUMP_IF_FALSE;
            if (expression_node->data.branch.condition_negated) {
                jump_instruction = OP_JUMP_IF_TRUE;
            }

            size_t then_jump;
            {
                op_jump_t jump = {
                    .op = jump_instruction,
                };
                then_jump = emit(compiler, chunk, condition->end.line, (op_code_t*)&jump, sizeof(op_jump_t), NULL);
            }

            /*
             * The then or else branch get ran once but during code-gen, both branches are processed
             * therefore before any branch is processed, I save the object and regular stack.
            */
            i32 stack_count = compiler->current_stack_size;

            emit_pop_value(compiler, chunk, condition->value_type, condition->end.line);
            if (expression_node->data.branch.looping) {
                emit_pop_value(compiler, chunk, expression_node->value_type, condition->start.line);
            }

            expression(vm, compiler, ast, expression_node->data.branch.then_expression, chunk);
            emit_storage_type_convert(compiler, chunk,
                    expression_node->data.branch.then_expression->value_type, expression_node->value_type,
                    expression_node->data.branch.then_expression->end.line);

            if (expression_node->data.branch.looping) {
                op_loop_t loop = {
                    .op = OP_LOOP,
                    .offset = position_before_condition_evaluation,
                };
                emit(compiler, chunk, expression_node->data.branch.then_expression->end.line, &loop, sizeof(op_loop_t), NULL);
            }

            size_t else_jump;
            {
                op_jump_t jump = {
                    .op = OP_JUMP,
                };
                else_jump = emit(compiler, chunk, expression_node->data.branch.then_expression->end.line, (op_code_t*)&jump, sizeof(op_jump_t), NULL);
            }

            patch_jump(chunk, then_jump);

            i32 then_stack_count = compiler->current_stack_size;
            (void)then_stack_count;

            /*
             * In the else branch, I restore the old stack count and object stack count so that they can be
             * calculated properly
            */
            compiler->current_stack_size = stack_count;

            emit_pop_value(compiler, chunk, condition->value_type, condition->end.line);
            if (expression_node->data.branch.looping) {
                emit_pop_value(compiler, chunk, expression_node->value_type, expression_node->data.branch.then_expression->end.line);
            }

            if (expression_node->data.branch.else_expression) {
                expression(vm, compiler, ast, expression_node->data.branch.else_expression, chunk);

                emit_storage_type_convert(compiler, chunk,
                        expression_node->data.branch.else_expression->value_type, expression_node->value_type,
                        expression_node->data.branch.else_expression->end.line);
            } else {
                debug_info_t debug = {.type=&OrsoTypeVoid};
                emit1(compiler, chunk, expression_node->end.line, OP_PUSH_0, &debug);

                emit_storage_type_convert(compiler, chunk,
                        &OrsoTypeVoid, expression_node->value_type,
                        expression_node->end.line);
            }

            patch_jump(chunk, else_jump);

            ASSERT(then_stack_count == compiler->current_stack_size, "then and else branch should end up with the same stack size");
            break;
        }
        case AST_NODE_TYPE_EXPRESSION_CALL: {
            expression(vm, compiler, ast, expression_node->data.call.callee, chunk);
            emit_storage_type_convert(compiler, chunk, expression_node->value_type, expression_node->value_type_narrowed, expression_node->end.line);

            type_t *function_type = expression_node->data.call.callee->value_type_narrowed;

            for (size_t i = 0; i < expression_node->data.call.arguments.count; ++i) {
                ast_node_t *argument = expression_node->data.call.arguments.items[i];
                expression(vm, compiler, ast, argument, chunk);

                type_t *parameter_type = function_type->data.function.argument_types.items[i];
                emit_storage_type_convert(compiler, chunk, argument->value_type, parameter_type, expression_node->end.line);
            }

            type_t *overload_type = function_type;
            {
                u16 argument_slots = 0;
                for (size_t i = 0; i < overload_type->data.function.argument_types.count; ++i) {
                    type_t *arg_type = overload_type->data.function.argument_types.items[i];
                    argument_slots += type_slot_count(arg_type);
                }

                op_call_t call = {
                    .op = OP_CALL,
                    .argument_slots = argument_slots,
                };

                debug_info_t debug = {.type=overload_type->data.function.return_type};
                emit(compiler, chunk, expression_node->data.call.callee->start.line, &call, sizeof(op_call_t), &debug);

                emit_pop_value(compiler, chunk, overload_type->data.function.return_type, expression_node->data.call.callee->start.line);
            }
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_FUNCTION_DEFINITION: {
            function_expression(vm, compiler, ast, expression_node, chunk);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_PRINT:
        case AST_NODE_TYPE_EXPRESSION_PRINT_EXPR: {
            expression(vm, compiler, ast, expression_node->data.expression, chunk);

            token_t start = expression_node->data.expression->start;
            token_t end = expression_node->data.expression->end;

            OrsoString *expression_string = orso_new_string_from_cstrn(start.start, (end.start + end.length) - start.start, &vm->allocator);

            slot_t slot = SLOT_P(expression_string);
            emit_constant(compiler, chunk, (byte*)&slot, start.line, &OrsoTypeString);

            slot_t value_type = SLOT_P(expression_node->data.expression->value_type);
            emit_constant(compiler, chunk, (byte*)&value_type, start.line, &OrsoTypeType);

            {
                debug_info_t debug = {.type=expression_node->value_type};
                if (expression_node->node_type == AST_NODE_TYPE_EXPRESSION_PRINT_EXPR) {
                    emit1(compiler, chunk, start.line, OP_PRINT_EXPR, &debug);
                } else {
                    emit1(compiler, chunk, start.line, OP_PRINT, &debug);
                }
            }

            debug_info_t debug = {.type=&OrsoTypeVoid};
            emit1(compiler, chunk, start.line, OP_PUSH_0, &debug);
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_STRUCT_DEFINITION: {
            ASSERT(false, "not implemented");
            break;
        }

        case AST_NODE_TYPE_EXPRESSION_TYPE_INITIALIZER: {
            i32 stack_position = compiler->current_stack_size*sizeof(slot_t);

            type_t *type = get_folded_type(ast, expression_node->data.initiailizer.type->value_index);

            i32 index = zero_value(ast, type, ast->symbols);
            emit_constant(compiler, chunk, (byte*)&ast->folded_constants.items[index], expression_node->start.line, type);

            if (TYPE_IS_STRUCT(type)) {
                size_t arg_count = expression_node->data.initiailizer.arguments.count;
                for (size_t i = 0; i < arg_count; ++i) {
                    ast_node_t *arg = expression_node->data.initiailizer.arguments.items[i];
                    unless (arg) continue;

                    expression(vm, compiler, ast, arg, chunk);

                    struct_field_t *field = &type->data.struct_.fields[i];
                    {
                        op_push_address_t push_address = {
                            .op = OP_PUSH_LOCAL_ADDRESS,
                            .index = stack_position + field->offset,
                        };
                        type_t *field_ptr = type_set_fetch_pointer(vm->type_set, field->type);
                        debug_info_t debug = {.type=field_ptr};
                        emit(compiler, chunk, arg->start.line, &push_address, sizeof(op_push_address_t), &debug);
                    }

                    {
                        op_set_lvalue_t set_lvalue = {
                            .op = get_lvalue_op_by_type(field->type),
                            .size_bytes = type_size_bytes(type),
                        };
                        emit(compiler, chunk, arg->start.line, &set_lvalue, sizeof(op_set_lvalue_t), NULL);
                    }

                    emit_pop_value(compiler, chunk, field->type, arg->start.line);
                }
            } else {
                UNREACHABLE();
            }
            break;
        }

        // function signatures MUST be resolved at compile time ALWAYS
        case AST_NODE_TYPE_EXPRESSION_FUNCTION_SIGNATURE: UNREACHABLE();

        case AST_NODE_TYPE_STATEMENT_EXPRESSION:
        case AST_NODE_TYPE_STATEMENT_RETURN:
        case AST_NODE_TYPE_UNDEFINED:
        case AST_NODE_TYPE_DECLARATION: UNREACHABLE();
    }

#undef EMIT_NEGATE
#undef EMIT_NOT
#undef EMIT_BINARY_OP_F64
#undef EMIT_BINARY_OP_I64
#undef EMIT_BINARY_OP
}

static void set_global_entity_default_value(vm_t *vm, ast_t *ast, ast_node_t *entity_declaration, u32 global_index) {
    type_t* conform_type = entity_declaration->value_type;
    u32 slot_count = type_slot_count(conform_type);
    if (entity_declaration->data.declaration.initial_value_expression != NULL) {
        ast_node_t* default_expression = entity_declaration->data.declaration.initial_value_expression;
        ASSERT(!TYPE_IS_UNION(default_expression->value_type), "this should be a concrete type since its foldable");
        ASSERT(default_expression->foldable && default_expression->value_index >= 0, "TODO: this needs to be caught and thrown");

        u32 index = global_index;
        if (TYPE_IS_UNION(conform_type)) {
            vm->globals.values.items[index] = SLOT_P(default_expression->value_type);
            index++;
            slot_count--;
        } 
        
        for (u32 i = 0; i < slot_count; ++i) {
            vm->globals.values.items[index + i] = ast->folded_constants.items[default_expression->value_index + i];
        }
    } else {
        if (TYPE_IS_UNION(conform_type)) {
            ASSERT(type_fits(conform_type, &OrsoTypeVoid), "default type only allowed for void type unions.");
            vm->globals.values.items[global_index] = SLOT_P(&OrsoTypeVoid);
            // no need to set any other value since they should be 0
        } else {
            memcpy(&vm->globals.values.items[global_index], &ast->folded_constants.items[entity_declaration->value_index], type_size_bytes(conform_type));
        }
    }
}

static void set_local_entity_default_value(vm_t* vm, compiler_t* compiler, ast_t* ast, chunk_t* chunk, ast_node_t* entity_declaration) {
    type_t* conform_type = entity_declaration->value_type;

    if (entity_declaration->data.declaration.initial_value_expression != NULL) {
        ast_node_t* default_expression = entity_declaration->data.declaration.initial_value_expression;

        expression(vm, compiler, ast, default_expression, chunk);
        emit_storage_type_convert(compiler, chunk, default_expression->value_type, conform_type, default_expression->start.line);
    } else {
        if (TYPE_IS_UNION(conform_type)) {
            ASSERT(type_fits(conform_type, &OrsoTypeVoid), "default type only allowed for void type unions.");
            debug_info_t debug = {.type=conform_type};
            emit1(compiler, chunk, entity_declaration->end.line, OP_PUSH_0, &debug);
            emit_put_in_union(compiler, chunk, entity_declaration->end.line, &OrsoTypeVoid);
        } else {
            ASSERT(entity_declaration->value_index >= 0, "if no expression, there must be an implicit value");
            slot_t *default_value = &ast->folded_constants.items[entity_declaration->value_index];
            emit_constant(compiler, chunk, (byte*)default_value, entity_declaration->end.line, conform_type);
        }
    }
}

static void global_entity_declaration(vm_t* vm, ast_t* ast, ast_node_t* entity_declaration) {
    ASSERT(entity_declaration->value_type != &OrsoTypeUnresolved, "all declarations must be resolved");
    u32 index = define_global_entity(vm, &entity_declaration->start, entity_declaration->value_type);
    set_global_entity_default_value(vm, ast, entity_declaration, index);
}

static void local_entity_declaration(vm_t* vm, compiler_t* compiler, ast_t* ast, ast_node_t* variable_declaration, chunk_t* chunk) {
    ASSERT(variable_declaration->value_type != &OrsoTypeUnresolved, "all declarations must be resolved");
    set_local_entity_default_value(vm, compiler, ast, chunk, variable_declaration);
    declare_local_entity(compiler, &variable_declaration->start, type_slot_count(variable_declaration->value_type));
}

static void declaration(vm_t *vm, compiler_t *compiler, ast_t *ast, ast_node_t *declaration, chunk_t *chunk) {
    switch (declaration->node_type) {
        case AST_NODE_TYPE_STATEMENT_EXPRESSION: {
            ast_node_t* expression_ = declaration->data.expression;
            expression(vm, compiler, ast, expression_, chunk);

            emit_pop_value(compiler, chunk, expression_->value_type, declaration->end.line);
            break;
        }
        
        case AST_NODE_TYPE_STATEMENT_RETURN: {
            if (declaration->data.expression) {
                expression(vm, compiler, ast, declaration->data.expression, chunk);
                emit_storage_type_convert(compiler, chunk, 
                        declaration->data.expression->value_type,
                        compiler->function->signature->data.function.return_type, declaration->data.expression->end.line);
            } else {
                debug_info_t debug = {.type=&OrsoTypeVoid};
                emit1(compiler, chunk, declaration->start.line, OP_PUSH_0, &debug);
                emit_storage_type_convert(compiler, chunk, &OrsoTypeVoid, compiler->function->signature->data.function.return_type, declaration->start.line);
            }
            
            {
                op_return_t return_ = {
                    .op = OP_RETURN,
                    .size_slots = type_slot_count(compiler->function->signature->data.function.return_type),
                };

                debug_info_t debug = {.type=compiler->function->signature->data.function.return_type};
                emit(compiler, chunk, declaration->end.line, &return_, sizeof(op_return_t), &debug);
            }
            break;
        }

        case AST_NODE_TYPE_DECLARATION: {
            local_entity_declaration(vm, compiler, ast, declaration, chunk);
            break;
        }

        case AST_NODE_TYPE_UNDEFINED:
        case AST_NODE_TYPE_EXPRESSION_CASE:
            UNREACHABLE();
    }
}

void code_builder_init(code_builder_t *builder, vm_t *vm, ast_t *ast) {
    builder->vm = vm;
    builder->ast = ast;
}

void code_builder_free(code_builder_t *builder) { 
    (void)builder;
}

function_t *generate_expression_function(code_builder_t *builder, ast_node_t *expression_node, bool is_folding_time, arena_t *allocator) {
    compiler_t compiler;
    type_t *function_type = type_set_fetch_function(&builder->ast->type_set, expression_node->value_type, (types_t){0});

    function_t *run_function = orso_new_function(allocator);

    compiler_init(&compiler, builder->vm, run_function, (type_t*)function_type);
    compiler.skip_function_definitions = !is_folding_time;

    // The vm will put this guy on the stack.
    compiler.max_stack_size = compiler.current_stack_size = 1;
    declare_local_function_definition(&compiler, compiler.function);

    chunk_t *top_chunk = &compiler.function->chunk;

    expression(builder->vm, &compiler, builder->ast, expression_node, top_chunk);

    {
        op_return_t return_ = {
            .op = OP_RETURN,
            .size_slots = type_slot_count(expression_node->value_type),
        };
        
        debug_info_t debug = {.type=expression_node->value_type};
        emit(&compiler, top_chunk, expression_node->start.line, &return_, sizeof(op_return_t), &debug);
    }

    compiler_end(builder->vm, &compiler, builder->ast, top_chunk, expression_node->end.line);

    function_t *function = compiler.function;

    compiler_free(&compiler);

    return function;
}

void compile_function(vm_t *vm, ast_t *ast, function_t *function, ast_node_t *function_definition_expression) {
    compiler_t function_compiler;
    compiler_init(&function_compiler, vm, function, function_definition_expression->value_type);

    // this is placed down by the caller
    function_compiler.max_stack_size = function_compiler.current_stack_size = 1;
    declare_local_function_definition(&function_compiler, function_compiler.function);

    begin_scope(&function_compiler);

    function_compiler.function->signature = function_definition_expression->value_type;
    chunk_t *function_chunk = &function_compiler.function->chunk;

    ast_function_t *function_definition = &function_definition_expression->data.function;

    for (size_t i = 0; i < function_definition->parameter_nodes.count; i++) {
        ast_node_t *parameter = function_definition->parameter_nodes.items[i];
        apply_stack_effects(&function_compiler, type_slot_count(parameter->value_type));
        declare_local_entity(&function_compiler, &parameter->start, type_slot_count(parameter->value_type));
    }

    expression(vm, &function_compiler, ast, function_definition->block, function_chunk);

    compiler_end(vm, &function_compiler, ast, function_chunk, function_definition_expression->end.line);
}

#define MAIN_IDENTIFIER "main"

function_t *generate_code(vm_t *vm, ast_t *ast) {
    function_t *main_function = NULL;
    ast_node_t *main_declaration = NULL;

    for (size_t i = 0; i < ast->root->data.block.count; i++) {
        ast_node_t *declaration_ = ast->root->data.block.items[i];
        global_entity_declaration(vm, ast, declaration_);
        
        token_t identifier = declaration_->data.declaration.identifier;
        if (identifier.length != strlen(MAIN_IDENTIFIER)){
            continue;
        }
        
        if (strncmp(identifier.start, MAIN_IDENTIFIER, strlen(MAIN_IDENTIFIER)) != 0) {
            continue;
        }
            
        if (!TYPE_IS_FUNCTION(declaration_->value_type)) {
            // TODO: allow code generator to have errors, main must be a function type
            return NULL;
        }

        type_t *function_type = declaration_->value_type;
        unless (function_type->data.function.return_type->kind == TYPE_INT32
         || function_type->data.function.return_type->kind == TYPE_VOID) {
            // TODO: allow code generator to throw error here, main must return i32
            return NULL;
        }

        if (function_type->data.function.argument_types.count != 0) {
            // TODO: allow code gen to throw error here, main must have 0 args for now
            return NULL;
        }

        ASSERT(declaration_->value_index >= 0, "must be folded and have a value");

        main_declaration = declaration_;
        main_function = (function_t*)ast->folded_constants.items[declaration_->value_index].as.p;
        break;
    }

    if (main_declaration == NULL) {
        return NULL;
    }

    compile_function(vm, ast, main_function, main_declaration->data.declaration.initial_value_expression);

    // TODO: Am I somehow missing an opertunity to warn for unused functions by doing this?
    for (size_t i = 0; i < ast->function_definition_pairs.count; ++i) {
        function_t *function = ast->function_definition_pairs.items[i].function;
        if (function->chunk.code.items == NULL) {
            compile_function(vm, ast, function, ast->function_definition_pairs.items[i].ast_defintion);
        }
    }

    return main_function;
}
