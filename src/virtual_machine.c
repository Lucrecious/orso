#include "virtual_machine.h"

#include <stdio.h>

#include "abstract_syntax_tree.h"
#include "codegen.h"
#include "static_analyzer.h"
#include "sb.h"
#include "type.h"

#if defined(DEBUG_TRACE_EXECUTION) || defined(DEBUG_PRINT_CODE)
#include "debug.h"
#endif

void orso_vm_init(OrsoVM* vm) {
    vm->chunk = NULL;
    vm->stack = NULL;
    vm->stack_top = NULL;
}

void orso_vm_free(OrsoVM* vm) {
}

static void FORCE_INLINE push(OrsoVM* vm, OrsoValue value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

static OrsoValue FORCE_INLINE pop(OrsoVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

static void FORCE_INLINE push_int(OrsoVM* vm, i64 value) {
    vm->stack_top->as_int = value;
    vm->stack_top++;
}

static void FORCE_INLINE push_float(OrsoVM* vm, f64 value) {
    vm->stack_top->as_float = value;
    vm->stack_top++;
}

static i64 FORCE_INLINE pop_int(OrsoVM* vm) {
    vm->stack_top--;
    return vm->stack_top->as_int;
}

static f64 FORCE_INLINE pop_float(OrsoVM* vm) {
    vm->stack_top--;
    return vm->stack_top->as_float;
}

static InterpretResult run(OrsoVM* vm) {
#define TOP_SLOT (vm->stack_top - 1)
#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm->chunk->constants[((u32)READ_BYTE() << 16) | ((u16)READ_BYTE() << 8) | READ_BYTE()])
#define BINARY_OP(op, type, fn_suffix) \
    do { \
        type b = pop_ ## fn_suffix(vm); \
        TOP_SLOT->as_ ## fn_suffix = TOP_SLOT->as_ ## fn_suffix  op b; \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (OrsoValue* slot = vm->stack; slot < vm->stack_top; slot++) {
            printf("[");
            print_value(*slot);
            printf("]");
        }
        printf("\n");
        disassemble_instruction(vm->chunk, (i32)(vm->ip - vm->chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                OrsoValue constant = READ_CONSTANT();
                push(vm, constant);
                break;
            }
            case OP_CONSTANT_LONG: {
                OrsoValue constant = READ_CONSTANT_LONG();
                push(vm, constant);
                break;
            }
            case OP_ZERO: {
                push_int(vm, 0);
                break;
            }
            case OP_ONE: {
                push_int(vm, 1);
                break;
            }

            case OP_NEGATE_INT: {
                TOP_SLOT->as_int = -TOP_SLOT->as_int;
                break;
            }
            case OP_NEGATE_DOUBLE: {
                TOP_SLOT->as_float = -TOP_SLOT->as_float;
                break;
            }

            case OP_ADD_INT: BINARY_OP(+, i64, int); break;
            case OP_SUBTRACT_INT: BINARY_OP(-, i64, int); break;
            case OP_MULTIPLY_INT: BINARY_OP(*, i64, int); break;
            case OP_DIVIDE_INT: BINARY_OP(/, i64, int); break;

            case OP_ADD_DOUBLE: BINARY_OP(+, f64, float); break;
            case OP_SUBTRACT_DOUBLE: BINARY_OP(-, f64, float); break;
            case OP_MULTIPLY_DOUBLE: BINARY_OP(*, f64, float); break;
            case OP_DIVIDE_DOUBLE: BINARY_OP(/, f64, float); break;

            case OP_INT_TO_DOUBLE:
                TOP_SLOT->as_float = (f64)TOP_SLOT->as_int;
                break;
            case OP_DOUBLE_TO_INT:
                TOP_SLOT->as_int = (i64)TOP_SLOT->as_float;
                break;
            
            case OP_NOT:
                TOP_SLOT->as_int = !(TOP_SLOT->as_int);
                break;
            
            case OP_EQUAL_INT: {
                i64 right = pop_int(vm);
                TOP_SLOT->as_int = (TOP_SLOT->as_int == right);
                break;
            }
            case OP_EQUAL_DOUBLE: {
                f64 right = pop_float(vm);
                TOP_SLOT->as_int = (TOP_SLOT->as_float == right);
                break;
            }
            
            case OP_LESS_INT: {
                i64 right = pop_int(vm);
                TOP_SLOT->as_int = (TOP_SLOT->as_int < right);
                break;
            }
            case OP_LESS_DOUBLE: {
                f64 right = pop_float(vm);
                TOP_SLOT->as_int = (TOP_SLOT->as_float < right);
                break;
            }
            
            case OP_GREATER_INT: {
                i64 right = pop_int(vm);
                TOP_SLOT->as_int = (TOP_SLOT->as_int > right);
                break;
            }
            case OP_GREATER_DOUBLE: {
                f64 right = pop_float(vm);
                TOP_SLOT->as_int = (TOP_SLOT->as_float > right);
                break;
            }

            case OP_RETURN: {
                print_value(pop(vm));
                printf("\n");
                return ORSO_INTERPRET_OK;
            }
        }
    }

#undef BINARY_OP
#undef READ_CONSTANT_LONG
#undef READ_CONSTANT
#undef READ_BYTE
#undef LAST_SLOT
}

static bool compile(const char* source, Chunk* chunk) {
    OrsoAST ast;
    if (!orso_parse_to_ast(source, &ast)) {
        orso_ast_free(&ast);
        return false;
    }

#ifdef DEBUG_PRINT_CODE
    orso_ast_print(&ast, "unresolved");
#endif

    orso_resolve_ast_types(&ast);

#ifdef DEBUG_PRINT_CODE
    orso_ast_print(&ast, "resolved");
#endif

    bool succeeded = true;
    succeeded &= ast.expression->value_type != ORSO_TYPE_UNRESOLVED;
    succeeded &= ast.expression->value_type != ORSO_TYPE_INVALID;

    if (succeeded) {
        succeeded = orso_generate_code(&ast, chunk);
    }

    orso_ast_free(&ast);

    return succeeded;
}

InterpretResult orso_interpret(OrsoVM* vm, const char* source) {
    Chunk chunk;
    chunk_init(&chunk);
    chunk.max_stack_size = 256;

    InterpretResult result;
    if (!compile(source, &chunk)) {
        result = ORSO_INTERPRET_COMPILE_ERROR;
    } else {
        vm->stack = ALLOCATE_N(OrsoValue, chunk.max_stack_size);
        vm->stack_top = vm->stack;

        vm->chunk = &chunk;
        vm->ip = vm->chunk->code;

        result = run(vm);

        free(vm->stack);
        vm->stack_top = NULL;
    }

    return result;
}

