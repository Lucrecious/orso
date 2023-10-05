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

void orso_vm_init(OrsoVM* vm, OrsoWriteFunction write_fn) {
    vm->frame_count = 0;

    vm->type_set = NULL;

    vm->stack = NULL;
    vm->stack_top = NULL;

    vm->globals.values = NULL;

    vm->write_fn = write_fn;

    orso_symbol_table_init(&vm->symbols);
    orso_symbol_table_init(&vm->globals.name_to_index);
}

void orso_vm_free(OrsoVM* vm) {
    orso_symbol_table_free(&vm->symbols);
    orso_symbol_table_free(&vm->globals.name_to_index);

    vm->globals.values = NULL;
}

static FORCE_INLINE void push_i64(OrsoVM* vm, OrsoSlot value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

void orso_vm_push_object(OrsoVM* vm, OrsoObject* object) {
    *vm->stack_top = ORSO_SLOT_P(object, object->type);
    vm->stack_top++;
}

static FORCE_INLINE OrsoSlot pop(OrsoVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

static FORCE_INLINE void pop_n(OrsoVM* vm, u32 count) {
    vm->stack_top -= count;
}

static FORCE_INLINE OrsoSlot* peek(OrsoVM* vm, i32 i) {
    return (vm->stack_top - (i + 1));
}

static void call(OrsoVM* vm, OrsoFunction* function, i32 argument_slots) {
    if (vm->frame_count == FRAMES_MAX) {
        // TODO
        UNREACHABLE();
    }

    CallFrame* frame = &vm->frames[vm->frame_count++];
    frame->function = function;
    frame->ip = function->chunk.code;
    frame->slots = vm->stack_top - argument_slots - 1;
}

void orso_vm_call(OrsoVM* vm, OrsoFunction* function) {
    i32 argument_slots = 0;
    for (i32 i = 0; i < function->signature->data.function.argument_count; i++) {
        argument_slots += orso_type_slot_count(function->signature->data.function.argument_types[i]);
    }
    call(vm, function, argument_slots);
}

static void call_object(OrsoVM* vm, OrsoObject* callee, i32 argument_slots) {
    if (ORSO_TYPE_IS_FUNCTION(callee->type)) {
        OrsoFunction* function = (OrsoFunction*)callee;
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

static void run(OrsoVM* vm, OrsoErrorFunction error_fn) {
    CallFrame* frame = &vm->frames[vm->frame_count - 1];

#define READ_BYTE() *(frame->ip++)
#define READ_U24() ORSO_u8s_to_u24(READ_BYTE(), READ_BYTE(), READ_BYTE())
#define READ_U16() ORSO_u8s_to_u16(READ_BYTE(), READ_BYTE())
#define READ_TYPE_KIND() ORSO_u8s_to_TypeKind(READ_BYTE(), READ_BYTE())
#define READ_TYPE() ORSO_TYPE_SINGLE(\
    ORSO_u8s_to_u64(\
        READ_BYTE(), READ_BYTE(), READ_BYTE(), READ_BYTE(),\
        READ_BYTE(), READ_BYTE(), READ_BYTE(), READ_BYTE()))
#define PEEK(I) peek(vm, I)
#define POP() pop(vm)
#define POPN(N) pop_n(vm, N);
#define PUSH(VALUE) push_i64(vm, VALUE)
#ifdef DEBUG_TRACE_EXECUTION
#define SLOT_ADD_TYPE(SLOT, TYPE) SLOT->type = TYPE
#else
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
            OrsoSlot value = vm->globals.values[index];
            orso_print_slot(value, value.type);
            printf("\n");
        }

        printf("SLOTS = { ");
        for (OrsoSlot* slot = vm->stack; slot < vm->stack_top; slot++) {
            printf("[");
            orso_print_slot(*slot, slot->type);
            printf("]");
        }
        printf(" }\n");
        disassemble_instruction(&frame->function->chunk, frame->ip - frame->function->chunk.code);
        printf("\n");
#endif
        OrsoOPCode op_code = READ_BYTE();
        switch (op_code) {
            case ORSO_OP_NO_OP: break;

            case ORSO_OP_POP: POP(); break;

            case ORSO_OP_I64_TO_F64: *PEEK(0) = ORSO_SLOT_F((f64)PEEK(0)->as.i, &OrsoTypeFloat64); break;
            case ORSO_OP_F64_TO_I64: *PEEK(0) = ORSO_SLOT_I((i64)PEEK(0)->as.f, &OrsoTypeInteger64); break;

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

            case ORSO_OP_EQUAL_SYMBOL: { i32 result = (PEEK(1)->as.p == PEEK(0)->as.p); POP(); POP(); PUSH(ORSO_SLOT_I(result, &OrsoTypeBool)); break; }

            case ORSO_OP_EQUAL_STRING: { i32 result = orso_string_equal(PEEK(1)->as.p, PEEK(0)->as.p); POP(); POP(); PUSH(ORSO_SLOT_I(result, &OrsoTypeBool)); break; }
            case ORSO_OP_CONCAT_STRING: { PEEK(1)->as.p = orso_string_concat(PEEK(1)->as.p, PEEK(0)->as.p); POP(); break; } 

            case ORSO_OP_LOGICAL_NOT: PEEK(0)->as.i = !PEEK(0)->as.i; SLOT_ADD_TYPE(PEEK(0), &OrsoTypeBool); break;

            case ORSO_OP_PUSH_0: {
                const OrsoSlot zero = ORSO_SLOT_I(0, &OrsoTypeInteger64);
                PUSH(zero);
                break;
            }
            case ORSO_OP_PUSH_1: {
                const OrsoSlot one = ORSO_SLOT_I(1, &OrsoTypeInteger64);
                PUSH(one);
                break;
            }

            case ORSO_OP_PUSH_NULL_UNION: {
                const OrsoSlot void_type = ORSO_SLOT_P(&OrsoTypeVoid, &OrsoTypeType);
                const OrsoSlot null_value = ORSO_SLOT_I(0, &OrsoTypeVoid);
                PUSH(void_type);
                PUSH(null_value);
                break;
            }

            case ORSO_OP_POP_SCOPE: {
                byte local_slot_count = READ_BYTE();
                byte block_value_slots = READ_BYTE();
                u32 stack_size = vm->stack_top - vm->stack;

                for (i32 i = 0; i < block_value_slots; i++) {
                    vm->stack[stack_size - (local_slot_count + block_value_slots) + i] = *(vm->stack_top - block_value_slots + i);
                }

                POPN(local_slot_count);
                break;
            }

            case ORSO_OP_CONSTANT: {
                u32 index = READ_U24();
                PUSH(frame->function->chunk.constants[index]);
                break;
            }

            case ORSO_OP_CONSTANT_SHORT: {
                byte index = READ_BYTE();
                PUSH(frame->function->chunk.constants[index]);
                break;
            }

            case ORSO_OP_DEFINE_GLOBAL: {
                u32 index = READ_U24();
                vm->globals.values[index] = *PEEK(0);
                POP();
                break;
            }

            case ORSO_OP_DEFINE_GLOBAL_UNION: {
                u32 index = READ_U24();
                OrsoSlot type = *PEEK(1);
                vm->globals.values[index] = type;
                vm->globals.values[index + 1] = *PEEK(0);

                POP();
                POP();
                break;
            }

            case ORSO_OP_GET_GLOBAL: {
                u32 index = READ_U24();
                OrsoSlot slot = vm->globals.values[index];
                PUSH(slot);
                break;
            }

            case ORSO_OP_GET_GLOBAL_SHORT: {
                byte index = READ_BYTE();
                OrsoSlot slot = vm->globals.values[index];
                PUSH(slot);
                break;
            }

            case ORSO_OP_GET_LOCAL: {
                u32 index = READ_U24();
                OrsoSlot slot = frame->slots[index];
                PUSH(slot);
                break;
            }

            case ORSO_OP_GET_LOCAL_SHORT: {
                byte index = READ_BYTE();
                OrsoSlot slot = frame->slots[index];
                PUSH(slot);
                break;
            }

            case ORSO_OP_GET_GLOBAL_UNION: {
                u32 index = READ_U24();
                OrsoSlot type = vm->globals.values[index];
                OrsoSlot value = vm->globals.values[index + 1];

                PUSH(type);
                PUSH(value);
                break;
            }

            case ORSO_OP_GET_LOCAL_UNION: {
                u32 index = READ_U24();
                OrsoSlot type = frame->slots[index];
                OrsoSlot value = frame->slots[index + 1];

                PUSH(type);
                PUSH(value);
                break;
            }

            case ORSO_OP_SET_GLOBAL: {
                u32 index = READ_U24();
                vm->globals.values[index] = *PEEK(0);
                break;
            }

            case ORSO_OP_SET_LOCAL: {
                u32 index = READ_U24();
                frame->slots[index] = *PEEK(0);
                break;
            }

            case ORSO_OP_SET_GLOBAL_UNION: {
                OrsoSlot type = *PEEK(1);
                OrsoSlot value = *PEEK(0);

                u32 index = READ_U24();
                vm->globals.values[index] = type;
                vm->globals.values[index + 1] = value;
                break;
            }

            case ORSO_OP_SET_LOCAL_UNION: {
                OrsoSlot type = *PEEK(1);
                OrsoSlot value = *PEEK(0);

                u32 index = READ_U24();
                frame->slots[index] = type;
                frame->slots[index + 1] = value;
                break;
            }

            case ORSO_OP_PUT_IN_UNION: {
                OrsoType* type = (OrsoType*)POP().as.p;
                OrsoSlot value = POP();
                PUSH(ORSO_SLOT_P(type, &OrsoTypeType));
                PUSH(value);
                break;
            }

            case ORSO_OP_NARROW_UNION: {
                OrsoSlot value = POP();

                // pop type
                POP();
                PUSH(value);
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
                OrsoType* type = (OrsoType*)POP().as.p; // pop expression type

                OrsoString* expression_string = (OrsoString*)(PEEK(0)->as.p);
                OrsoType* union_type = (OrsoType*)PEEK(2)->as.p;
                ASSERT(!ORSO_TYPE_IS_UNION(union_type), "must be single type.");
                OrsoString* value_string = orso_slot_to_string(*PEEK(1), union_type);

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

                POPN(3);
                break;
            }
            
            case ORSO_OP_RETURN: {
                byte result_size = READ_BYTE();
                for (i32 i = 0; i < result_size; i++) {
                    frame->slots[i] = *PEEK(result_size - i - 1);
                }

                for (i32 i = 0; i < result_size; i++) {
                    POP();
                }

                vm->frame_count--;

                if (vm->frame_count == 0) {
                    //POP_PTR();
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
#undef READ_BYTE
}


void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn) {
    run(vm, error_fn);
}
