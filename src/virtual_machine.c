#include "virtual_machine.h"

#include <stdio.h>

#include "parser.h"
#include "codegen.h"
#include "static_analyzer.h"
#include "sb.h"
#include "type.h"

#if defined(DEBUG_TRACE_EXECUTION) || defined(DEBUG_PRINT_CODE)
#include "debug.h"
#endif

void orso_vm_init(OrsoVM* vm, OrsoWriteFunction write_fn) {
    orso_symbol_table_init(&vm->symbols);
    orso_gc_init(&vm->gc, vm);

    vm->write_fn = write_fn;
    vm->chunk = NULL;
    vm->stack = NULL;
    vm->stack_top = NULL;

    vm->object_stack = NULL;
    vm->object_stack_top = NULL;

    orso_symbol_table_init(&vm->globals.name_to_index);
    vm->globals.values = NULL;
    vm->globals.gc_values_indices = NULL;
}

void orso_vm_free(OrsoVM* vm) {
    orso_symbol_table_free(&vm->symbols);
    orso_symbol_table_free(&vm->globals.name_to_index);
    sb_free(vm->globals.values);
    sb_free(vm->globals.gc_values_indices);

    vm->globals.gc_values_indices = NULL;
    vm->globals.values = NULL;

    orso_gc_collect(&vm->gc);
}

static FORCE_INLINE void push_i64(OrsoVM* vm, OrsoSlot value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

static FORCE_INLINE void push_top_object(OrsoVM* vm) {
    vm->object_stack_top->is_object = true;
    vm->object_stack_top->index = (vm->stack_top - 1) - vm->stack;
    vm->object_stack_top++;
}

static FORCE_INLINE void push_top_object_null(OrsoVM* vm) {
    vm->object_stack_top->is_object = false;
    vm->object_stack_top->index = (vm->stack_top - 1) - vm->stack;
    vm->object_stack_top++;
}

static FORCE_INLINE OrsoSlot pop(OrsoVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

static FORCE_INLINE OrsoObject* pop_top_object(OrsoVM* vm) {
    vm->object_stack_top--;
    return (OrsoObject*)vm->stack[vm->object_stack_top->index].as.p;
}

static FORCE_INLINE OrsoSlot* peek(OrsoVM* vm, i32 i) {
    return (vm->stack_top - (i + 1));
}

static void run(OrsoVM* vm, OrsoErrorFunction error_fn) {
#define READ_BYTE() *(vm->ip++)
#define READ_U24() ORSO_u8s_to_u24(READ_BYTE(), READ_BYTE(), READ_BYTE())
#define READ_TYPE_KIND() ORSO_u8s_to_TypeKind(READ_BYTE(), READ_BYTE())
#define READ_TYPE() ORSO_TYPE_ONE(\
    ORSO_u8s_to_u64(\
        READ_BYTE(), READ_BYTE(), READ_BYTE(), READ_BYTE(),\
        READ_BYTE(), READ_BYTE(), READ_BYTE(), READ_BYTE()))
#define PEEK(I) peek(vm, I)
#define POP() pop(vm)
#define POP_TOP_OBJECT() pop_top_object(vm)
#define POP_PTR() pop_top_object(vm); pop(vm)
#define PUSH(VALUE) push_i64(vm, VALUE)
#define PUSH_TOP_OBJECT() push_top_object(vm)
#define PUSH_TOP_OBJECT_NULL() push_top_object_null(vm)
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
            orso_print_slot(value, value.type.one);
            printf("\n");
        }
        printf("PTR SLOTS = { ");
        for (OrsoGCValueIndex* index = vm->object_stack; index < vm->object_stack_top; index++) {
            printf("[");
            if (!index->is_object) {
                printf("!");
            }
            OrsoSlot slot = vm->stack[index->index];
            orso_print_slot(slot, slot.type.one);
            printf("]");
        }
        printf(" }\n");

        printf("ALL SLOTS = { ");
        for (OrsoSlot* slot = vm->stack; slot < vm->stack_top; slot++) {
            printf("[");
            orso_print_slot(*slot, slot->type.one);
            printf("]");
        }
        printf(" }\n");
        disassemble_instruction(vm->chunk, vm->ip - vm->chunk->code);
        printf("\n");
#endif
        OrsoOPCode op_code = READ_BYTE();
        switch (op_code) {
            case ORSO_OP_POP: POP(); break;
            case ORSO_OP_POP_TOP_OBJECT: POP_TOP_OBJECT(); break;
            case ORSO_OP_PUSH_TOP_OBJECT: PUSH_TOP_OBJECT(); break;
            case ORSO_OP_PUSH_TOP_OBJECT_NULL: PUSH_TOP_OBJECT_NULL(); break;

            case ORSO_OP_I64_TO_F64: *PEEK(0) = ORSO_SLOT_F((f64)PEEK(0)->as.i, ORSO_TYPE_ONE(ORSO_TYPE_FLOAT64)); break;
            case ORSO_OP_F64_TO_I64: *PEEK(0) = ORSO_SLOT_I((i64)PEEK(0)->as.f, ORSO_TYPE_ONE(ORSO_TYPE_INT64)); break;

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

            case ORSO_OP_EQUAL_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i == b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_LESS_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i < b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_GREATER_I64: { i64 b = POP().as.i; PEEK(0)->as.i = (PEEK(0)->as.i > b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }

            case ORSO_OP_EQUAL_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f == b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_LESS_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f < b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_GREATER_F64: { f64 b = POP().as.f; PEEK(0)->as.i = (PEEK(0)->as.f > b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }

            case ORSO_OP_EQUAL_SYMBOL: { i32 result = (PEEK(1)->as.p == PEEK(0)->as.p); POP_PTR(); POP_PTR(); PUSH(ORSO_SLOT_I(result, ORSO_TYPE_ONE(ORSO_TYPE_BOOL))); break; }

            case ORSO_OP_EQUAL_STRING: { i32 result = orso_string_equal(PEEK(1)->as.p, PEEK(0)->as.p); POP_PTR(); POP_PTR(); PUSH(ORSO_SLOT_I(result, ORSO_TYPE_ONE(ORSO_TYPE_BOOL))); break; }
            case ORSO_OP_CONCAT_STRING: { PEEK(1)->as.p = orso_string_concat(&vm->gc, PEEK(1)->as.p, PEEK(0)->as.p); POP_PTR(); break; } 

            case ORSO_OP_LOGICAL_NOT: PEEK(0)->as.i = !PEEK(0)->as.i; SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break;

            case ORSO_OP_PUSH_0: {
                const OrsoSlot zero = ORSO_SLOT_I(0, ORSO_TYPE_ONE(ORSO_TYPE_INT64));
                PUSH(zero);
                break;
            }
            case ORSO_OP_PUSH_1: {
                const OrsoSlot one = ORSO_SLOT_I(1, ORSO_TYPE_ONE(ORSO_TYPE_INT64));
                PUSH(one);
                break;
            }

            case ORSO_OP_PUSH_NULL_UNION: {
                const OrsoSlot void_type = ORSO_SLOT_U((u64)ORSO_TYPE_NULL, ORSO_TYPE_ONE(ORSO_TYPE_TYPE));
                const OrsoSlot null_value = ORSO_SLOT_I(0, ORSO_TYPE_ONE(ORSO_TYPE_NULL));
                PUSH(void_type);
                PUSH(null_value);
                break;
            }

            case ORSO_OP_CONSTANT: {
                u32 index = READ_U24();
                PUSH(vm->chunk->constants[index]);
                break;
            }

            case ORSO_OP_DEFINE_GLOBAL: {
                u32 index = READ_U24();
                vm->globals.values[index] = *PEEK(0);
                POP();
                break;
            }

            case ORSO_OP_SET_GLOBAL_GC_TYPE: {
                u32 index = READ_U24();
                vm->globals.gc_values_indices[index].is_object = true;
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

            case ORSO_OP_GET_LOCAL: {
                u32 index = READ_U24();
                OrsoSlot slot = vm->stack[index];
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
                OrsoSlot type = vm->stack[index];
                OrsoSlot value = vm->stack[index + 1];

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
                vm->stack[index] = *PEEK(0);
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
                vm->stack[index] = type;
                vm->stack[index + 1] = value;
                break;
            }

            case ORSO_OP_PUT_IN_UNION: {
                OrsoType type = ORSO_TYPE_ONE(READ_TYPE_KIND());

                if (orso_is_gc_type(type)) {
                    POP_TOP_OBJECT();
                }

                OrsoSlot value = POP();
                PUSH(ORSO_SLOT_U(type.one, ORSO_TYPE_ONE(ORSO_TYPE_TYPE)));
                PUSH(value);

                if (orso_is_gc_type(type)) {
                    PUSH_TOP_OBJECT();
                }
                break;
            }

            case ORSO_OP_NARROW_UNION: {
                OrsoSlot value = POP();
                OrsoType type = ORSO_TYPE_ONE(POP().as.u);

                if (orso_is_gc_type(type)) {
                    POP_TOP_OBJECT();
                }

                PUSH(value);

                if (orso_is_gc_type(type)) {
                    PUSH_TOP_OBJECT();
                }
                break;
            }

            case ORSO_OP_UPDATE_GLOBAL_UNION_GC_TYPE: {
                OrsoType type = ORSO_TYPE_ONE(PEEK(1)->as.u);
                u32 index = READ_U24();
                vm->globals.gc_values_indices[index].is_object = orso_is_gc_type(type);
                break;
            }

            case ORSO_OP_UPDATE_STACK_GC_TYPE: {
                OrsoType type = ORSO_TYPE_ONE(PEEK(1)->as.u);
                u32 index = READ_U24();
                vm->object_stack[index].is_object = orso_is_gc_type(type);
                break;
            }

            case ORSO_OP_PRINT_EXPR: {
                OrsoType type = ORSO_TYPE_ONE(POP().as.u); // pop expression type

                OrsoString* expression_string = (OrsoString*)(PEEK(0)->as.p);
                ASSERT(ORSO_TYPE_IS_SINGLE(ORSO_TYPE_ONE(PEEK(2)->as.u)), "must be single type.");
                OrsoTypeKind union_type = (OrsoTypeKind)PEEK(2)->as.u;
                OrsoString* value_string = orso_slot_to_string(&vm->gc, *PEEK(1), union_type);

                if (vm->write_fn != NULL) {
                    vm->write_fn(expression_string->text);
                    vm->write_fn(" (");

                    const char type_str[128];
                    orso_type_to_cstr(type, (char*)type_str);

                    vm->write_fn(type_str);
                    vm->write_fn(") => ");
                    vm->write_fn(value_string->text);
                    vm->write_fn("\n");
                }

                POP_PTR(); // pop expression string and pointer
                POP_PTR(); // pop value and union pointer
                POP(); // pop union type
                break;
            }
            
            case ORSO_OP_RETURN: return;
        }
    }

#undef PUSH_TOP_OBJECT_NULL
#undef PUSH_TOP_OBJECT
#undef PUSH
#undef POP_PTR
#undef POP_TOP_OBJECT
#undef POP
#undef PEEK
#undef READ_TYPE
#undef READ_TYPE_KIND
#undef READ_U24
#undef READ_BYTE
}


void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn) {
    run(vm, error_fn);
}
