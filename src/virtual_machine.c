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
    orso_symbol_table_init(&vm->stack_globals);
    orso_symbol_table_init(&vm->object_globals);
    orso_gc_init(&vm->gc, vm);

    vm->write_fn = write_fn;
    vm->chunk = NULL;
    vm->stack = NULL;
    vm->stack_top = NULL;

    vm->object_stack = NULL;
    vm->object_stack_top = NULL;
}

void orso_vm_free(OrsoVM* vm) {
    orso_symbol_table_free(&vm->symbols);
    orso_symbol_table_free(&vm->stack_globals);
    orso_symbol_table_free(&vm->object_globals);

    orso_gc_collect(&vm->gc);
}

static FORCE_INLINE void push_i64(OrsoVM* vm, OrsoSlot value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

static FORCE_INLINE void push_ptr(OrsoVM* vm, OrsoSlot value) {
    push_i64(vm, value);
    *vm->object_stack_top = &(*(vm->stack_top - 1));
    vm->object_stack_top++;

}

static FORCE_INLINE OrsoSlot pop(OrsoVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

static FORCE_INLINE OrsoSlot pop_ptr(OrsoVM* vm) {
    pop(vm);
    vm->object_stack_top--;
    return **vm->object_stack_top;
}

static FORCE_INLINE OrsoSlot* peek(OrsoVM* vm, i32 i) {
    return (vm->stack_top - (i + 1));
}

static void run(OrsoVM* vm, OrsoErrorFunction error_fn) {
#define READ_INSTRUCTION() (vm->ip++)
#define PEEK(I) peek(vm, I)
#define POP() pop(vm)
#define POP_PTR() pop_ptr(vm)
#define PUSH(VALUE) push_i64(vm, VALUE)
#define PUSH_PTR(VALUE) push_ptr(vm, VALUE)
#ifdef DEBUG_TRACE_EXECUTION
#define SLOT_ADD_TYPE(SLOT, TYPE) SLOT->type = TYPE
#else
#define SLOT_ADD_TYPE(SLOT, TYPE)
#endif

    for (;;) {
        OrsoInstruction* instruction = READ_INSTRUCTION();
#ifdef DEBUG_TRACE_EXECUTION
        printf("PTR SLOTS = { ");
        for (OrsoSlot** slot = vm->object_stack; slot < vm->object_stack_top; slot++) {
            printf("[");
            orso_print_slot(**slot, (*slot)->type);
            printf("]");
        }
        printf(" }\n");

        printf("ALL SLOTS = { ");
        for (OrsoSlot* slot = vm->stack; slot < vm->stack_top; slot++) {
            printf("[");
            orso_print_slot(*slot, slot->type);
            printf("]");
        }
        printf(" }\n");
        disassemble_instruction(vm->chunk, instruction - vm->chunk->code);
        printf("\n");
#endif
        switch (instruction->op_code) {
            case ORSO_OP_POP: POP(); break;
            case ORSO_OP_POP_PTR: POP_PTR(); break;
            //case ORSO_OP_PUSH_I64: PUSH(instruction->constant.index); break;

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

            case ORSO_OP_EQUAL_I64: { i64 b = POP().i; PEEK(0)->i = (PEEK(0)->i == b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break; }
            case ORSO_OP_LESS_I64: { i64 b = POP().i; PEEK(0)->i = (PEEK(0)->i < b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break; }
            case ORSO_OP_GREATER_I64: { i64 b = POP().i; PEEK(0)->i = (PEEK(0)->i > b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break; }

            case ORSO_OP_EQUAL_F64: { f64 b = POP().f; PEEK(0)->i = (PEEK(0)->f == b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break; }
            case ORSO_OP_LESS_F64: { f64 b = POP().f; PEEK(0)->i = (PEEK(0)->f < b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break; }
            case ORSO_OP_GREATER_F64: { f64 b = POP().f; PEEK(0)->i = (PEEK(0)->f > b); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break; }

            case ORSO_OP_EQUAL_SYMBOL: { i32 result = (PEEK(1)->p == PEEK(0)->p); POP_PTR(); POP_PTR(); PUSH(ORSO_SLOT_I(result, ORSO_TYPE_BOOL)); break; }

            case ORSO_OP_EQUAL_STRING: { i32 result = orso_string_equal(PEEK(1)->p, PEEK(0)->p); POP_PTR(); POP_PTR(); PUSH(ORSO_SLOT_I(result, ORSO_TYPE_BOOL)); break; }
            case ORSO_OP_CONCAT_STRING: { PEEK(1)->p = orso_string_concat(&vm->gc, PEEK(1)->p, PEEK(0)->p); POP_PTR(); break; } 

            case ORSO_OP_LOGICAL_NOT: PEEK(0)->i = !PEEK(0)->i; SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break;

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
            case ORSO_OP_CONSTANT_PTR: PUSH_PTR(vm->chunk->constants[instruction->constant.index]); break;

            case ORSO_OP_DEFINE_GLOBAL: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                orso_symbol_table_set(&vm->stack_globals, name, *PEEK(0));
                POP();
                break;
            }

            case ORSO_OP_DEFINE_GLOBAL_PTR: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                orso_symbol_table_set(&vm->object_globals, name, *PEEK(0));
                POP_PTR();
                break;
            }

            case ORSO_OP_GET_GLOBAL: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                OrsoSlot slot;
                orso_symbol_table_get(&vm->stack_globals, name, &slot);
                PUSH(slot);
                break;
            }

            case ORSO_OP_GET_GLOBAL_PTR: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                OrsoSlot slot;
                orso_symbol_table_get(&vm->object_globals, name, &slot);
                PUSH_PTR(slot);
                break;
            }

            case ORSO_OP_SET_GLOBAL: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                orso_symbol_table_set(&vm->stack_globals, name, *PEEK(0));
                break;
            }

            case ORSO_OP_SET_GLOBAL_PTR: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                orso_symbol_table_set(&vm->object_globals, name, *PEEK(0));
                break;
            }

            case ORSO_OP_PRINT_EXPR: {
                OrsoString* expression_string = (OrsoString*)(PEEK(0)->p);
                OrsoString* string = orso_slot_to_string(&vm->gc, *PEEK(1), instruction->print_expr.type);

                if (vm->write_fn != NULL) {
                    vm->write_fn(expression_string->text);
                    vm->write_fn(" => ");
                    vm->write_fn(string->text);
                }

                POP_PTR();

                if (orso_is_gc_type(instruction->print_expr.type)) {
                    POP_PTR();
                } else {
                    POP();
                }
                break;
            }
            
            case ORSO_OP_RETURN: return;
        }
    }

#undef SLOT_ADD_TYPE
#undef PUSH_PTR
#undef PUSH
#undef POP_PTR
#undef POP
#undef TOP_SLOT
#undef READ_INSTRUCTION
}


void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn) {
    run(vm, error_fn);
}

