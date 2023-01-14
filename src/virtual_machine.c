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
    *vm->object_stack_top = (vm->stack_top - 1)->p;
    vm->object_stack_top++;
}

static FORCE_INLINE OrsoSlot pop(OrsoVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

static FORCE_INLINE OrsoObject* pop_top_object(OrsoVM* vm) {
    vm->object_stack_top--;
    return *vm->object_stack_top;
}

static FORCE_INLINE OrsoSlot* peek(OrsoVM* vm, i32 i) {
    return (vm->stack_top - (i + 1));
}

static void run(OrsoVM* vm, OrsoErrorFunction error_fn) {
#define READ_INSTRUCTION() (vm->ip++)
#define PEEK(I) peek(vm, I)
#define POP() pop(vm)
#define POP_TOP_OBJECT() pop_top_object(vm)
#define POP_PTR() pop_top_object(vm); pop(vm)
#define PUSH(VALUE) push_i64(vm, VALUE)
#define PUSH_TOP_OBJECT() push_top_object(vm)
#ifdef DEBUG_TRACE_EXECUTION
#define SLOT_ADD_TYPE(SLOT, TYPE) SLOT->type = TYPE
#else
#define SLOT_ADD_TYPE(SLOT, TYPE)
#endif

    for (;;) {
        OrsoInstruction* instruction = READ_INSTRUCTION();
#ifdef DEBUG_TRACE_EXECUTION
        printf("PTR SLOTS = { ");
        for (OrsoObject** object = vm->object_stack; object < vm->object_stack_top; object++) {
            printf("[");
            OrsoSlot slot = ORSO_SLOT_P(*object, ORSO_TYPE_ONE((u64)(*object)->type_kind));
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
        disassemble_instruction(vm->chunk, instruction - vm->chunk->code);
        printf("\n");
#endif
        switch (instruction->op_code) {
            case ORSO_OP_POP: POP(); break;
            case ORSO_OP_POP_TOP_OBJECT: POP_TOP_OBJECT(); break;
            case ORSO_OP_PUSH_TOP_OBJECT: PUSH_TOP_OBJECT(); break;

            case ORSO_OP_I64_TO_F64: PEEK(0)->f = (f64)PEEK(0)->i; break;
            case ORSO_OP_F64_TO_I64: PEEK(0)->i = (i64)PEEK(0)->f; break;

            case ORSO_OP_ADD_I64: { i64 b = POP().i; PEEK(0)->i = PEEK(0)->i + b; break; }
            case ORSO_OP_SUBTRACT_I64: { i64 b = POP().i; PEEK(0)->i = PEEK(0)->i - b; break; }
            case ORSO_OP_MULTIPLY_I64: { i64 b = POP().i; PEEK(0)->i = PEEK(0)->i * b; break; }
            case ORSO_OP_DIVIDE_I64: { i64 b = POP().i; PEEK(0)->i = PEEK(0)->i / b; break; }

            case ORSO_OP_ADD_F64: { f64 b = POP().f; PEEK(0)->f = PEEK(0)->f + b; break; }
            case ORSO_OP_SUBTRACT_F64: { f64 b = POP().f; PEEK(0)->f = PEEK(0)->f - b; break; }
            case ORSO_OP_MULTIPLY_F64: { f64 b = POP().f; PEEK(0)->f = PEEK(0)->f * b; break; }
            case ORSO_OP_DIVIDE_F64: { f64 b = POP().f; PEEK(0)->f = PEEK(0)->f / b; break; }

            case ORSO_OP_NEGATE_I64: PEEK(0)->i = -PEEK(0)->i; break;
            case ORSO_OP_NEGATE_F64: PEEK(0)->f = -PEEK(0)->f; break;

            case ORSO_OP_EQUAL_I64: { i64 b = POP().i; PEEK(0)->i = (PEEK(0)->i == b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_LESS_I64: { i64 b = POP().i; PEEK(0)->i = (PEEK(0)->i < b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_GREATER_I64: { i64 b = POP().i; PEEK(0)->i = (PEEK(0)->i > b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }

            case ORSO_OP_EQUAL_F64: { f64 b = POP().f; PEEK(0)->i = (PEEK(0)->f == b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_LESS_F64: { f64 b = POP().f; PEEK(0)->i = (PEEK(0)->f < b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_GREATER_F64: { f64 b = POP().f; PEEK(0)->i = (PEEK(0)->f > b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break; }

            case ORSO_OP_EQUAL_SYMBOL: { i32 result = (PEEK(1)->p == PEEK(0)->p); POP_PTR(); POP_PTR(); PUSH(ORSO_SLOT_I(result, ORSO_TYPE_ONE(ORSO_TYPE_BOOL))); break; }

            case ORSO_OP_EQUAL_STRING: { i32 result = orso_string_equal(PEEK(1)->p, PEEK(0)->p); POP_PTR(); POP_PTR(); PUSH(ORSO_SLOT_I(result, ORSO_TYPE_ONE(ORSO_TYPE_BOOL))); break; }
            case ORSO_OP_CONCAT_STRING: { PEEK(1)->p = orso_string_concat(&vm->gc, PEEK(1)->p, PEEK(0)->p); POP_PTR(); break; } 

            case ORSO_OP_LOGICAL_NOT: PEEK(0)->i = !PEEK(0)->i; SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_ONE(ORSO_TYPE_BOOL)); break;

            case ORSO_OP_PUSH_0: {
                const OrsoSlot zero = ORSO_SLOT_I(0, instruction->constant.type);
                PUSH(zero);
                break;
            }
            case ORSO_OP_PUSH_1: {
                const OrsoSlot one = ORSO_SLOT_I(1, instruction->constant.type);
                PUSH(one);
                break;
            }

            case ORSO_OP_CONSTANT: PUSH(vm->chunk->constants[instruction->constant.index]); break;

            case ORSO_OP_DEFINE_GLOBAL: {
                vm->globals.values[instruction->constant.index] = *PEEK(0);
                POP();
                break;
            }

            case ORSO_OP_DEFINE_GLOBAL_UNION: {
                OrsoSlot type = *PEEK(1);
                vm->globals.values[instruction->constant.index] = type;
                vm->globals.values[instruction->constant.index + 1] = *PEEK(0);

                if (orso_is_gc_type(ORSO_TYPE_ONE(type.u))) {
                    POP_TOP_OBJECT();
                }

                POP();
                POP();
                break;
            }

            case ORSO_OP_GET_GLOBAL: {
                OrsoSlot slot = vm->globals.values[instruction->constant.index];
                PUSH(slot);
                break;
            }

            case ORSO_OP_GET_GLOBAL_UNION: {
                OrsoSlot type = vm->globals.values[instruction->constant.index];
                OrsoSlot value = vm->globals.values[instruction->constant.index + 1];

                PUSH(type);
                PUSH(value);

                if (orso_is_gc_type(ORSO_TYPE_ONE(type.u))) {
                    PUSH_TOP_OBJECT();
                }
                break;
            }

            case ORSO_OP_SET_GLOBAL: {
                vm->globals.values[instruction->constant.index] = *PEEK(0);
                break;
            }

            case ORSO_OP_SET_GLOBAL_UNION: {
                OrsoSlot type = *PEEK(1);
                OrsoSlot value = *PEEK(0);
                vm->globals.values[instruction->constant.index] = type;
                vm->globals.values[instruction->constant.index + 1] = value;
                break;
            }

            case ORSO_OP_PUT_IN_UNION: {
                OrsoType type = instruction->put_in_union.type;
                OrsoSlot value = POP();
                PUSH(ORSO_SLOT_U(type.one, ORSO_TYPE_TYPE));
                PUSH(value);
                break;
            }

            case ORSO_OP_UPDATE_GLOBAL_UNION_TYPE: {
                OrsoType type = ORSO_TYPE_ONE(PEEK(1)->u);
                OrsoGCValueIndex gc_value_index = vm->globals.gc_values_indices[instruction->constant.index];
                if (orso_is_gc_type(type)) {
                    vm->globals.gc_values_indices[instruction->constant.index].is_object = true;
                } else {
                    vm->globals.gc_values_indices[instruction->constant.index].is_object = false;
                }
                break;
            }

            case ORSO_OP_PRINT_EXPR: {
                OrsoString* expression_string;
                OrsoString* value_string;
                if (ORSO_TYPE_IS_SINGLE(instruction->print_expr.type)) {
                    expression_string = (OrsoString*)(PEEK(0)->p);
                    value_string = orso_slot_to_string(&vm->gc, *PEEK(1), instruction->print_expr.type.one);
                } else {
                    expression_string = (OrsoString*)(PEEK(0)->p);
                    // ASSERT that PEEK(2) is single type
                    OrsoTypeKind union_type = (OrsoTypeKind)PEEK(2)->u;
                    value_string = orso_slot_to_string(&vm->gc, *PEEK(1), union_type);
                }

                if (vm->write_fn != NULL) {
                    vm->write_fn(expression_string->text);
                    vm->write_fn(" (");

                    const char type_str[128];
                    orso_type_to_cstr(instruction->print_expr.type, type_str);

                    vm->write_fn(type_str);
                    vm->write_fn(") => ");
                    vm->write_fn(value_string->text);
                    vm->write_fn("\n");
                }

                POP_PTR();

                if (orso_is_gc_type(instruction->print_expr.type)) {
                    POP_PTR();
                } else {
                    POP();
                }

                if (ORSO_TYPE_IS_UNION(instruction->print_expr.type)) {
                    POP();
                }
                break;
            }
            
            case ORSO_OP_RETURN: return;
        }
    }

#undef SLOT_ADD_TYPE
#undef PUSH_TOP_OBJECT
#undef PUSH
#undef POP_TOP_OBJECT
#undef POP
#undef TOP_SLOT
#undef READ_INSTRUCTION
}


void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn) {
    run(vm, error_fn);
}

