#include "virtual_machine.h"

#include <stdio.h>

#include "codegen.h"
#include "instructions.h"
#include "opcodes.h"
#include "parser.h"
#include "static_analyzer.h"
#include "sb.h"
#include "type.h"
#include "type_set.h"

#if defined(DEBUG_TRACE_EXECUTION) || defined(DEBUG_PRINT_CODE)
#include "debug.h"
#endif

#include <time.h>

void vm_init(vm_t* vm, write_function_t write_fn, i32 stack_size) {
    vm->frame_count = 0;

    vm->type_set = NULL;

    i32 stack_size_slot_count = orso_bytes_to_slots(stack_size);

    vm->stack = ORSO_ALLOCATE_N(slot_t, stack_size_slot_count);
    vm->stack_top = vm->stack;

    vm->globals.values = NULL;

#ifdef DEBUG_TRACE_EXECUTION
    vm->stack_types = ORSO_ALLOCATE_N(OrsoType*, stack_size_slot_count);
    vm->globals.types = NULL;
#endif

    vm->write_fn = write_fn;

    orso_symbol_table_init(&vm->symbols);
    orso_symbol_table_init(&vm->globals.name_to_index);
}

void vm_free(vm_t* vm) {
    orso_symbol_table_free(&vm->symbols);
    orso_symbol_table_free(&vm->globals.name_to_index);

    free(vm->stack);
    vm->stack = NULL;
    vm->stack_top = NULL;

#ifdef DEBUG_TRACE_EXECUTION
    free(vm->stack_types);
    vm->stack_types = NULL;

    sb_free(vm->globals.types);
    vm->globals.types = NULL;
#endif

    sb_free(vm->globals.values);
    vm->globals.values = NULL;
}

#ifdef DEBUG_TRACE_EXECUTION
static FORCE_INLINE void push_i64(vm_t* vm, slot_t value, OrsoType* type) {
#else
static FORCE_INLINE void push_i64(vm_t* vm, slot_t value) {
#endif
    *vm->stack_top = value;
#ifdef DEBUG_TRACE_EXECUTION
    vm->stack_types[vm->stack_top - vm->stack] =  type;
#endif
    vm->stack_top++;
}

#ifdef DEBUG_TRACE_EXECUTION
#define RESERVE_STACK_SPACE(vm, slot_count, type) reserve_stack_space(vm, slot_count, type)
static FORCE_INLINE void reserve_stack_space(vm_t* vm, u32 slot_count, OrsoType* type) {
    for (u32 i = 0; i < slot_count; i++) {
        vm->stack_types[vm->stack_top - vm->stack + i] = &OrsoTypeInvalid;
    }
    vm->stack_types[vm->stack_top - vm->stack] = type;
#else
#define RESERVE_STACK_SPACE(vm, slot_count, type) reserve_stack_space(vm, slot_count)
static FORCE_INLINE void reserve_stack_space(vm_t* vm, u32 slot_count) {
#endif

    for (u32 i = 0; i < slot_count; i++) {
        vm->stack_top->as.i = 0;
        vm->stack_top++;
    }
}

void vm_push_object(vm_t* vm, OrsoObject* object) {
    *vm->stack_top = ORSO_SLOT_P(object);
#ifdef DEBUG_TRACE_EXECUTION
    vm->stack_types[vm->stack_top - vm->stack] =  object->type;
#endif
    vm->stack_top++;
}

static FORCE_INLINE slot_t pop(vm_t* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

static FORCE_INLINE void pop_n(vm_t* vm, u32 count) {
    vm->stack_top -= count;
}

static FORCE_INLINE slot_t* peek(vm_t* vm, i32 i) {
    return (vm->stack_top - (i + 1));
}

static void call(vm_t* vm, function_t* function, i32 argument_slots) {
    if (vm->frame_count == FRAMES_MAX) {
        // TODO
        UNREACHABLE();
    }

    call_frame_t* frame = &vm->frames[vm->frame_count++];
    frame->function = function;
    frame->ip = function->chunk.code;
    frame->slots = vm->stack_top - argument_slots - 1;
}

void vm_call(vm_t* vm, function_t* function) {
    i32 argument_slots = 0;
    for (i32 i = 0; i < function->signature->data.function.argument_count; i++) {
        argument_slots += orso_type_slot_count(function->signature->data.function.argument_types[i]);
    }
    call(vm, function, argument_slots);
}

static void call_object(vm_t* vm, OrsoObject* callee, i32 argument_slots) {
    if (ORSO_TYPE_IS_FUNCTION(callee->type)) {
        function_t* function = (function_t*)callee;
        call(vm, function, argument_slots);
        return;
    } else if (callee->type->kind == ORSO_TYPE_NATIVE_FUNCTION) {
        OrsoNativeFunction* function_obj = (OrsoNativeFunction*)callee;
        NativeFunction function = function_obj->function;
        function(vm->stack_top - argument_slots, vm->stack_top);

        i32 return_slot_size = orso_type_slot_count(function_obj->signature->data.function.return_type);
        for (i32 i = 0; i < return_slot_size; i++) {
            vm->stack_top[-(argument_slots + 1) + i] = vm->stack_top[i];
        }

        // arguments on the stack + function, but remove the amount used for the return items
        vm->stack_top -= (argument_slots + 1) - return_slot_size;
        return;
    }

    UNREACHABLE();
}

static void run(vm_t* vm, OrsoErrorFunction error_fn) {
    call_frame_t* frame = &vm->frames[vm->frame_count - 1];

#define READ_BYTE() *(frame->ip++)
#define READ_U24() ORSO_u8s_to_u24(READ_BYTE(), READ_BYTE(), READ_BYTE())
#define READ_U16() ORSO_u8s_to_u16(READ_BYTE(), READ_BYTE())
#define READ_U32() ORSO_u8s_to_u32(READ_BYTE(), READ_BYTE(), READ_BYTE(), READ_BYTE())
#define READ_TYPE_KIND() ORSO_u8s_to_TypeKind(READ_BYTE(), READ_BYTE())
#define READ_TYPE() ORSO_TYPE_SINGLE(\
    ORSO_u8s_to_u64(\
        READ_BYTE(), READ_BYTE(), READ_BYTE(), READ_BYTE(),\
        READ_BYTE(), READ_BYTE(), READ_BYTE(), READ_BYTE()))

#define PEEK(I) peek(vm, I)
#define POP() pop(vm)
#define POPN(N) pop_n(vm, N)
#ifdef DEBUG_TRACE_EXECUTION
#define PUSH(VALUE, TYPE) push_i64(vm, VALUE, TYPE)
#define SLOT_ADD_TYPE(SLOT, TYPE) (vm->stack_types[(vm->stack_top - 1) - SLOT] = TYPE)
#else
#define PUSH(VALUE, TYPE) push_i64(vm, VALUE)
#define SLOT_ADD_TYPE(SLOT, TYPE)
#endif

    (void)error_fn; // unused

#ifdef DEBUG_TRACE_EXECUTION
    printf("=== trace ===\n");
#endif

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        for (i32 i = 0; i < vm->globals.name_to_index.capacity; i++) {
            OrsoSymbolTableEntry* entry = &vm->globals.name_to_index.entries[i];
            if (entry->key == NULL) {
                continue;
            }

            u32 index = entry->value.as.u;
            printf("%s = ", entry->key->text);
            slot_t* value = vm->globals.values + index;
            orso_print_slot(value, vm->globals.types[index]);
            printf("\n");
        }

        printf("SLOTS = { ");
        i32 stack_size = vm->stack_top - vm->stack;
        for (i32 index = 0; index < stack_size;) {
            printf("[");
            orso_print_slot(vm->stack + index, vm->stack_types[index]);
            printf("]");

            // TODO: push field bytes and slot do not record type
            if (!ORSO_TYPE_IS_INVALID(vm->stack_types[index])) {
                index += orso_type_slot_count(vm->stack_types[index]);
            } else {
                index++;
            }
        }
        printf(" }\n");
        disassemble_instruction(&frame->function->chunk, frame->ip - frame->function->chunk.code);
        printf("\n");
#endif
        OrsoOPCode op_code = READ_BYTE();
        switch (op_code) {
            case ORSO_OP_NO_OP: break;

            case ORSO_OP_POP: POP(); break;

            case ORSO_OP_I64_TO_F64: *PEEK(0) = ORSO_SLOT_F((f64)PEEK(0)->as.i); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeFloat64); break;
            case ORSO_OP_F64_TO_I64: *PEEK(0) = ORSO_SLOT_I((i64)PEEK(0)->as.f); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeInteger64); break;

            case ORSO_OP_ADD_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i + b; break; }
            case ORSO_OP_SUBTRACT_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i - b; break; }
            case ORSO_OP_MULTIPLY_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i * b; break; }
            case ORSO_OP_DIVIDE_I64: { i64 b = POP().as.i; PEEK(0)->as.i = PEEK(0)->as.i / b; break; }

            case ORSO_OP_ADD_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f + b; break; }
            case ORSO_OP_SUBTRACT_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f - b; break; }
            case ORSO_OP_MULTIPLY_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f * b; break; }
            case ORSO_OP_DIVIDE_F64: { f64 b = POP().as.f; PEEK(0)->as.f = PEEK(0)->as.f / b; break; }

            case ORSO_OP_NEGATE_I64: PEEK(0)->as.i = -PEEK(0)->as.i; break;
            case ORSO_OP_NEGATE_F64: PEEK(0)->as.f = -PEEK(0)->as.f; break;

            case ORSO_OP_EQUAL_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i == b); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break; }
            case ORSO_OP_LESS_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i < b); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break; }
            case ORSO_OP_GREATER_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i > b); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break; }

            case ORSO_OP_EQUAL_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f == b); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break; }
            case ORSO_OP_LESS_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f < b); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break; }
            case ORSO_OP_GREATER_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f > b); SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break; }

            case ORSO_OP_ADD_PTR_I64: {
                i64 b = POP().as.i;
                PEEK(0)->as.p = (void*)(((byte*)PEEK(0)->as.p) + b);
                break;
            }

            case ORSO_OP_EQUAL_SYMBOL: { i32 result = (PEEK(1)->as.p == PEEK(0)->as.p); POP(); POP(); PUSH(ORSO_SLOT_I(result), &OrsoTypeBool); break; }

            case ORSO_OP_EQUAL_STRING: { i32 result = orso_string_equal(PEEK(1)->as.p, PEEK(0)->as.p); POP(); POP(); PUSH(ORSO_SLOT_I(result), &OrsoTypeBool); break; }
            case ORSO_OP_CONCAT_STRING: { PEEK(1)->as.p = orso_string_concat(PEEK(1)->as.p, PEEK(0)->as.p); POP(); break; } 

            case ORSO_OP_LOGICAL_NOT: PEEK(0)->as.i = !PEEK(0)->as.i; SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break;

            case ORSO_OP_PUSH_0: {
                const slot_t zero = ORSO_SLOT_I(0);
                PUSH(zero, &OrsoTypeInteger64);
                break;
            }
            case ORSO_OP_PUSH_1: {
                const slot_t one = ORSO_SLOT_I(1);
                PUSH(one, &OrsoTypeInteger64);
                break;
            }
 
            case ORSO_OP_PUSH_LOCAL_ADDRESS: {
                u16 index = READ_U16();
                PUSH(ORSO_SLOT_P(((byte*)frame->slots) + index), &OrsoTypeInteger64);
                break;
            }

            case ORSO_OP_PUSH_GLOBAL_ADDRESS: {
                u32 index = READ_U32();
                PUSH(ORSO_SLOT_P(((byte*)vm->globals.values) + index), &OrsoTypeInteger64);
                break;
            }

            case ORSO_OP_POP_SCOPE: {
                byte local_slot_count = READ_BYTE();
                byte block_value_slots = READ_BYTE();
                u32 stack_size = vm->stack_top - vm->stack;

                for (i32 i = 0; i < block_value_slots; i++) {
                    vm->stack[stack_size - (local_slot_count + block_value_slots) + i] = *(vm->stack_top - block_value_slots + i);
            #ifdef DEBUG_TRACE_EXECUTION
                    u32 type_index = (vm->stack_top - block_value_slots + i) - vm->stack;
                    vm->stack_types[stack_size - (local_slot_count + block_value_slots) + i] = *(vm->stack_types + type_index);
            #endif

                }

                POPN(local_slot_count);
                break;
            }

            case ORSO_OP_POPN: {
                POPN(READ_BYTE());
                break;
            }

#define PUSH_CONSTANT() do { \
    slot_t* current_top = vm->stack_top; \
    RESERVE_STACK_SPACE(vm, orso_bytes_to_slots(size), (frame->function->chunk.constant_types[index / sizeof(slot_t)])); \
    byte* constant = ((byte*)frame->function->chunk.constants) + index; \
    memcpy(current_top, constant, size); \
} while(0)

            case ORSO_OP_CONSTANT_8BIT_ADDRESS: {
                byte index = READ_BYTE();
                byte size = READ_BYTE();

                PUSH_CONSTANT();
                break;
            }

            case ORSO_OP_CONSTANT_16BIT_ADDRESS: {
                u32 index = READ_U16();
                byte size = READ_BYTE();

                PUSH_CONSTANT();
                break;
            }

            case ORSO_OP_CONSTANT_32BIT_ADDRESS: {
                u32 index = READ_U32();
                byte size = READ_BYTE();

                PUSH_CONSTANT();
                break;
            }
#undef PUSH_CONSTANT


#define PUSH_GLOBAL() do { \
    slot_t* current_top = vm->stack_top; \
    RESERVE_STACK_SPACE(vm, orso_bytes_to_slots(size), (vm->globals.types[index / sizeof(slot_t)])); \
    byte* global = ((byte*)vm->globals.values) + index; \
    memcpy(current_top, global, size); \
} while(0)

            case ORSO_OP_GET_GLOBAL_8BIT_ADDRESS: {
                u32 index = READ_BYTE();
                byte size = READ_BYTE();

                PUSH_GLOBAL();
                break;
            }

            case ORSO_OP_GET_GLOBAL_16BIT_ADDRESS: {
                u32 index = READ_U16();
                byte size = READ_BYTE();

                PUSH_GLOBAL();
                break;
            }

            case ORSO_OP_GET_GLOBAL_32BIT_ADDRESS: {
                u32 index = READ_U32();
                byte size = READ_BYTE();

                PUSH_GLOBAL();
                break;
            }
#undef PUSH_GLOBAL

#define PUSH_LOCAL() do { \
    slot_t* current_top = vm->stack_top; \
    RESERVE_STACK_SPACE(vm, orso_bytes_to_slots(size), \
        vm->stack_types[(frame->slots + (index / sizeof(slot_t))) - vm->stack]); \
    byte* local = ((byte*)frame->slots) + index; \
    memcpy(current_top, local, size); \
} while(0)

            case ORSO_OP_GET_LOCAL_8BIT_ADDRESS: {
                byte index = READ_BYTE();
                byte size = READ_BYTE();

                PUSH_LOCAL();
                break;
            }

            case ORSO_OP_GET_LOCAL_16BIT_ADDRESS: {
                u32 index = READ_U16();
                byte size = READ_BYTE();

                PUSH_LOCAL();
                break;
            }
#undef PUSH_LOCAL
            #ifdef DEBUG_TRACE_EXECUTION
            #define GET_FIELD_TYPE_TRACE(TYPE) u32 index = vm->stack_top - vm->stack; vm->stack_types[index] = &OrsoType##TYPE;
            #else
            #define GET_FIELD_TYPE_TRACE(TYPE)
            #endif
            #define ORSO_OP_GET_FIELD(CASE_TYPE, CAST_TYPE, TRACE_TYPE, UNION_ID) ORSO_OP_GET_FIELD_##CASE_TYPE: { \
                byte struct_size = READ_BYTE(); \
                byte field_offset = READ_BYTE();  \
                \
                byte struct_slot_size = orso_bytes_to_slots(struct_size); \
                slot_t* new_top = vm->stack_top - struct_slot_size; \
                \
                new_top->as.UNION_ID = *((CAST_TYPE*)((((byte*)new_top) + field_offset))); \
                \
                POPN(struct_slot_size - 1); \
                \
                GET_FIELD_TYPE_TRACE(TRACE_TYPE); \
                break;\
            } \
            break

            case ORSO_OP_GET_FIELD_VOID: {
                byte struct_size = READ_BYTE();

                byte struct_slot_size = orso_bytes_to_slots(struct_size);

                (vm->stack_top - struct_slot_size)->as.i = 0;

                POPN(struct_slot_size - 1);

            #ifdef DEBUG_TRACE_EXECUTION
                u32 index = vm->stack_top - vm->stack;
                vm->stack_types[index] = &OrsoTypeVoid;
            #endif
                break;
            }

            case ORSO_OP_GET_FIELD_SLOT: {
                byte struct_size = READ_BYTE();
                byte field_offset = READ_BYTE();

                byte struct_slot_size = orso_bytes_to_slots(struct_size);

                memmove(vm->stack_top - struct_slot_size, ((byte*)(vm->stack_top - struct_slot_size)) + field_offset, sizeof(slot_t));

                POPN(struct_slot_size - 1);

            #ifdef DEBUG_TRACE_EXECUTION
                u32 index = vm->stack_top - vm->stack;
                vm->stack_types[index] = &OrsoTypeInvalid; // TODO: Use better system to debug types
            #endif
                break;
            }

            case ORSO_OP_GET_FIELD_BYTES: {
                byte struct_size = READ_BYTE();
                byte field_offset = READ_BYTE();
                byte field_size = READ_BYTE();

                byte struct_slot_size = orso_bytes_to_slots(struct_size);

                ASSERT(field_size % sizeof(slot_t) == 0, "field size must be aligned to slots");
                memmove(vm->stack_top - struct_slot_size, ((byte*)(vm->stack_top - struct_slot_size)) + field_offset, field_size);

                byte field_slot_size = orso_bytes_to_slots(field_size);
                POPN(struct_slot_size - field_slot_size);

            #ifdef DEBUG_TRACE_EXECUTION
                u32 index = vm->stack_top - vm->stack;
                for (u32 i = 0; i < field_slot_size; i++) {
                    vm->stack_types[index + i] = &OrsoTypeInvalid; // TODO: use better system to keep track of types in slots
                }
            #endif
                break;
            }

            case ORSO_OP_GET_FIELD(BOOL, byte, Bool, i);

            case ORSO_OP_GET_FIELD(I32, i32, Integer32, i);

            case ORSO_OP_GET_FIELD(F32, f32, Float32, f);

            case ORSO_OP_SET_LVALUE_SLOT: {
                void* ptr = POP().as.p;
                slot_t value = *PEEK(0);
                *((slot_t*)ptr) = value;
                break;
            }
            case ORSO_OP_SET_LVALUE_BOOL: {
                void* ptr = POP().as.p;
                byte value = (byte)PEEK(0)->as.u;
                *((byte*)ptr) = value;
                break;
            }

            case ORSO_OP_SET_LVALUE_I32: {
                void* ptr = POP().as.p;
                i32 value = (i32)PEEK(0)->as.i;
                *((i32*)ptr) = value;
                break;
            }

            case ORSO_OP_SET_LVALUE_F32: {
                void* ptr = POP().as.p;
                f32 value  = (f32)PEEK(0)->as.f;
                *((f32*)ptr) = value;
                break;
            }

            case ORSO_OP_SET_LVALUE_BYTES: {
                byte size = READ_BYTE();
                void* ptr = POP().as.p;
                byte slots = orso_bytes_to_slots(size);

                memcpy(ptr, PEEK(slots - 1), size);
                break;
            }

            case ORSO_OP_PUT_IN_UNION: {
                byte size = READ_BYTE();

                type_t* type = (type_t*)(PEEK(0)->as.p);
                byte slot_size = orso_bytes_to_slots(size);
                memmove(PEEK(slot_size-1), PEEK(slot_size), slot_size * sizeof(slot_t));
                // for (u32 i = 0; i < slot_size; ++i) {
                //     *PEEK(i) = *PEEK(i + 1);
                // }
                PEEK(slot_size)->as.p = type;

            #ifdef DEBUG_TRACE_EXECUTION
                u32 stack_index = (vm->stack_top - slot_size - 1) - vm->stack;
                for (u32 i = 0; i < slot_size + 1; ++i) {
                    vm->stack_types[stack_index + i] = &OrsoTypeInvalid;
                }

                vm->stack_types[stack_index] = &OrsoTypeType;
                vm->stack_types[stack_index + 1] = type;
            #endif
                break;
            }

            case ORSO_OP_NARROW_UNION: {
                byte type_offset = READ_BYTE();
                ASSERT(type_offset % sizeof(slot_t), "must be perfectly slot size");
                byte slot_offset = type_offset / sizeof(slot_t);

            #ifdef DEBUG_TRACE_EXECUTION
                u32 stack_index = (vm->stack_top - (slot_offset - 1)) - vm->stack;

                vm->stack_types[stack_index] = (OrsoType*)vm->stack[stack_index].as.p;
            #endif

                memmove(PEEK(slot_offset - 1), PEEK(slot_offset - 2), type_offset - sizeof(slot_t));

                POP();
                break;
            }

            case ORSO_OP_JUMP_IF_UNION_FALSE:
            case ORSO_OP_JUMP_IF_FALSE: {
                u16 offset = READ_U16();
                if (orso_slot_is_falsey(*PEEK(0))) {
                    frame->ip += offset;
                }
                break;
            }

            case ORSO_OP_JUMP_IF_UNION_TRUE:
            case ORSO_OP_JUMP_IF_TRUE: {
                u16 offset = READ_U16();
                if (!orso_slot_is_falsey(*PEEK(0))) {
                    frame->ip += offset;
                }
                break;
            }

            case ORSO_OP_JUMP: {
                u16 offset = READ_U16();
                frame->ip += offset;
                break;
            }

            case ORSO_OP_LOOP: {
                u16 offset = READ_U16();
                frame->ip -= offset;
                break;
            }

            case ORSO_OP_CALL: {
                u16 argument_slots = READ_U16();
                call_object(vm, (OrsoObject*)PEEK(argument_slots)->as.p, argument_slots);

                frame = &vm->frames[vm->frame_count - 1];
                break;
            }

            case ORSO_OP_PRINT:
            case ORSO_OP_PRINT_EXPR: {
                type_t* type = (type_t*)POP().as.p; // pop expression type

                OrsoString* expression_string = (OrsoString*)(POP().as.p);

                byte size_slots = orso_type_slot_count(type);
                OrsoString* value_string = orso_slot_to_string(PEEK(size_slots - 1), type);

                if (vm->write_fn != NULL) {
                    if (op_code == ORSO_OP_PRINT_EXPR) {
                        vm->write_fn(expression_string->text);
                        vm->write_fn(" (");

                        const char type_str[128];
                        orso_type_to_cstrn(type, (char*)type_str, 128);

                        vm->write_fn(type_str);
                        vm->write_fn(") => ");
                        vm->write_fn(value_string->text);
                        vm->write_fn("\n");
                    } else {
                        vm->write_fn(value_string->text);
                        vm->write_fn("\n");
                    }
                }

                POPN(size_slots);
                break;
            }
            
            case ORSO_OP_RETURN: {
                byte result_size = READ_BYTE();
                for (i32 i = 0; i < result_size; i++) {
                    frame->slots[i] = *PEEK(result_size - i - 1);
                #ifdef DEBUG_TRACE_EXECUTION
                    vm->stack_types[(frame->slots + i) - vm->stack] = &OrsoTypeInvalid;
                #endif
                }

                vm->frame_count--;

                if (vm->frame_count == 0) {
                    return;
                }

                vm->stack_top = frame->slots + result_size;
                frame = &vm->frames[vm->frame_count - 1];
                break;
            }
        }
    }

#undef PUSH
#undef POPN
#undef POP
#undef PEEK
#undef READ_TYPE
#undef READ_TYPE_KIND
#undef READ_U16
#undef READ_U24
#undef READ_U32
#undef READ_BYTE
}


void vm_interpret(vm_t* vm, OrsoErrorFunction error_fn) {
    run(vm, error_fn);
}
