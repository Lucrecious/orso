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

void orso_vm_init(OrsoVM* vm) {
    orso_symbol_table_init(&vm->symbols);
    orso_symbol_table_init(&vm->globals);
    orso_gc_init(&vm->gc, vm);

    vm->chunk = NULL;
    vm->stack = NULL;
    vm->stack_top = NULL;
}

void orso_vm_free(OrsoVM* vm) {
    orso_symbol_table_free(&vm->symbols);
    orso_symbol_table_free(&vm->globals);
}

static void FORCE_INLINE push_i64(OrsoVM* vm, OrsoSlot value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

static OrsoSlot FORCE_INLINE pop(OrsoVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

static FORCE_INLINE OrsoSlot* peek(OrsoVM* vm, i32 i) {
    return (vm->stack_top - (i + 1));
}

static void run(OrsoVM* vm, OrsoErrorFunction error_fn) {
#define READ_INSTRUCTION() (vm->ip++)
#define PEEK(I) peek(vm, I)
#define POP() pop(vm)
#define PUSH(VALUE) push_i64(vm, VALUE)
#ifdef DEBUG_TRACE_EXECUTION
#define SLOT_ADD_TYPE(SLOT, TYPE) SLOT->type = TYPE
#else
#define SLOT_ADD_TYPE(SLOT, TYPE)
#endif

    for (;;) {
        OrsoInstruction* instruction = READ_INSTRUCTION();
#ifdef DEBUG_TRACE_EXECUTION
        printf("SLOTS = { ");
        for (OrsoSlot* slot = vm->stack; slot < vm->stack_top; slot++) {
            printf("[");
            orso_print_slot(*slot, slot->type);
            printf("]");
        }
        printf(" }\n");
        disassemble_instruction(vm->chunk, instruction - vm->chunk->code);
#endif
        switch (instruction->op_code) {
            case ORSO_OP_POP: POP(); break;
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

            case ORSO_OP_EQUAL_STRING: { PEEK(1)->i = orso_string_equal(PEEK(1)->p, PEEK(0)->p); POP(); SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break; }
            case ORSO_OP_CONCAT_STRING: { PEEK(1)->p = orso_string_concat(&vm->gc, PEEK(1)->p, PEEK(0)->p); POP(); break; } 

            case ORSO_OP_LOGICAL_NOT: PEEK(0)->i = !PEEK(0)->i; SLOT_ADD_TYPE(PEEK(0), ORSO_TYPE_BOOL); break;

            case ORSO_OP_PUSH_0: {
                const OrsoSlot zero = {
                    .i = 0,
#ifdef DEBUG_TRACE_EXECUTION
                    .type = instruction->constant.type,
#endif
                };
                PUSH(zero);
                break;
            }
            case ORSO_OP_PUSH_1: {
                const OrsoSlot one = {
                    .i = 1,
#ifdef DEBUG_TRACE_EXECUTION
                    .type = instruction->constant.type,
#endif
                };
                PUSH(one);
                break;
            }

            case ORSO_OP_CONSTANT: PUSH(vm->chunk->constants[instruction->constant.index]); break;

            case ORSO_OP_DEFINE_GLOBAL: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                orso_symbol_table_set(&vm->globals, name, *PEEK(0));
                POP();
                break;
            }
            case ORSO_OP_GET_GLOBAL: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                OrsoSlot slot;
                orso_symbol_table_get(&vm->globals, name, &slot);
                PUSH(slot);
                break;
            }

            case ORSO_OP_SET_GLOBAL: {
                OrsoSymbol* name = (OrsoSymbol*)vm->chunk->constants[instruction->constant.index].p;
                orso_symbol_table_set(&vm->globals, name, *PEEK(0));
                break;
            }

            case ORSO_OP_PRINT_EXPR: {
                OrsoString* expression_string = (OrsoString*)POP().p;
                printf("%s => ", expression_string->text);

                OrsoSlot slot = POP();
                orso_print_slot(slot, instruction->print_expr.type);
                printf("\n");
                break;
            }
            
            case ORSO_OP_RETURN: return;
        }
    }

#undef PUSH
#undef POP
#undef TOP_SLOT
#undef READ_INSTRUCTION
}


void orso_vm_interpret(OrsoVM* vm, OrsoErrorFunction error_fn) {
    run(vm, error_fn);
}

