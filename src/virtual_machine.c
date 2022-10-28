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
    orso_symbol_table_init(&vm->symbol_table);
    orso_symbol_table_init(&vm->globals);

    vm->chunk = NULL;
    vm->stack = NULL;
    vm->stack_top = NULL;
}

void orso_vm_free(OrsoVM* vm) {
    orso_symbol_table_free(&vm->symbol_table);
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

static FORCE_INLINE OrsoSlot* get_top_slot(OrsoVM* vm) {
    return (vm->stack_top - 1);
}

static void run(OrsoVM* vm, OrsoErrorFunction error_fn) {
#define READ_INSTRUCTION() (vm->ip++)
#define TOP_SLOT get_top_slot(vm)
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

            case ORSO_OP_I64_TO_F64: TOP_SLOT->f = (f64)TOP_SLOT->i; break;
            case ORSO_OP_F64_TO_I64: TOP_SLOT->i = (i64)TOP_SLOT->f; break;

            case ORSO_OP_ADD_I64: { i64 b = POP().i; TOP_SLOT->i = TOP_SLOT->i + b; break; }
            case ORSO_OP_SUBTRACT_I64: { i64 b = POP().i; TOP_SLOT->i = TOP_SLOT->i - b; break; }
            case ORSO_OP_MULTIPLY_I64: { i64 b = POP().i; TOP_SLOT->i = TOP_SLOT->i * b; break; }
            case ORSO_OP_DIVIDE_I64: { i64 b = POP().i; TOP_SLOT->i = TOP_SLOT->i / b; break; }

            case ORSO_OP_ADD_F64: { f64 b = POP().f; TOP_SLOT->f = TOP_SLOT->f + b; break; }
            case ORSO_OP_SUBTRACT_F64: { f64 b = POP().f; TOP_SLOT->f = TOP_SLOT->f - b; break; }
            case ORSO_OP_MULTIPLY_F64: { f64 b = POP().f; TOP_SLOT->f = TOP_SLOT->f * b; break; }
            case ORSO_OP_DIVIDE_F64: { f64 b = POP().f; TOP_SLOT->f = TOP_SLOT->f / b; break; }

            case ORSO_OP_NEGATE_I64: TOP_SLOT->i = -TOP_SLOT->i; break;
            case ORSO_OP_NEGATE_F64: TOP_SLOT->f = -TOP_SLOT->f; break;

            case ORSO_OP_EQUAL_I64: { i64 b = POP().i; TOP_SLOT->i = (TOP_SLOT->i == b); SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break; }
            case ORSO_OP_LESS_I64: { i64 b = POP().i; TOP_SLOT->i = (TOP_SLOT->i < b); SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break; }
            case ORSO_OP_GREATER_I64: { i64 b = POP().i; TOP_SLOT->i = (TOP_SLOT->i > b); SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break; }

            case ORSO_OP_EQUAL_F64: { f64 b = POP().f; TOP_SLOT->i = (TOP_SLOT->f == b); SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break; }
            case ORSO_OP_LESS_F64: { f64 b = POP().f; TOP_SLOT->i = (TOP_SLOT->f < b); SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break; }
            case ORSO_OP_GREATER_F64: { f64 b = POP().f; TOP_SLOT->i = (TOP_SLOT->f > b); SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break; }

            case ORSO_OP_EQUAL_STRING: { ptr b = POP().p; TOP_SLOT->i = orso_string_equal(TOP_SLOT->p, b); SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break; }
            case ORSO_OP_CONCAT_STRING: { ptr b = POP().p; TOP_SLOT->p = orso_string_concat(TOP_SLOT->p, b); break; }

            case ORSO_OP_LOGICAL_NOT: TOP_SLOT->i = !TOP_SLOT->i; SLOT_ADD_TYPE(TOP_SLOT, ORSO_TYPE_BOOL); break;

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
                orso_symbol_table_set(&vm->globals, name, *TOP_SLOT);
                POP();
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

static bool compile(const char* source, Chunk* chunk, OrsoSymbolTable* symbol_table, OrsoErrorFunction error_fn) {
    OrsoAST ast;
    orso_ast_init(&ast);

    if (!orso_parse(&ast, source, symbol_table, error_fn)) {
        orso_ast_free(&ast);
        return false;
    }

#ifdef DEBUG_PRINT
    orso_ast_print(&ast, "unresolved");
#endif

    OrsoStaticAnalyzer analyzer;
    orso_static_analyzer_init(&analyzer, symbol_table, error_fn);

    bool resolved = orso_resolve_ast_types(&analyzer, &ast);

#ifdef DEBUG_PRINT
    orso_ast_print(&ast, "resolved");
#endif


    bool succeeded = resolved;

    if (succeeded) {
        succeeded = orso_generate_code(&ast, chunk);
    }

    orso_ast_free(&ast);

    return succeeded;
}

void orso_interpret(OrsoVM* vm, const char* source, OrsoErrorFunction error_fn) {
    Chunk chunk;
    chunk_init(&chunk);
    chunk.max_stack_size = 256;

    if (!compile(source, &chunk, &vm->symbol_table, error_fn)) {
        return;
    }

    vm->stack = ALLOCATE_N(OrsoSlot, chunk.max_stack_size);
    vm->stack_top = vm->stack;

    vm->chunk = &chunk;
    vm->ip = vm->chunk->code;

    run(vm, error_fn);

    free(vm->stack);
    chunk_free(&chunk);
    vm->stack_top = NULL;
}

