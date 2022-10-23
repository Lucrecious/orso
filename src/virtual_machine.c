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

void savine_vm_init(SavineVM* vm) {
    vm->chunk = NULL;
    vm->stack = NULL;
    vm->stack_top = NULL;
}

void savine_vm_free(SavineVM* vm) {
}

void savine_vm_push(SavineVM* vm, SavineValue value) {
    *vm->stack_top = value;
    vm->stack_top++;
}

SavineValue savine_vm_pop(SavineVM* vm) {
    vm->stack_top--;
    return *vm->stack_top;
}

void savine_vm_push_int(SavineVM* vm, i64 value) {
    vm->stack_top->as_int = value;
    vm->stack_top++;
}

void savine_vm_push_float(SavineVM* vm, f64 value) {
    vm->stack_top->as_float = value;
    vm->stack_top++;
}

i64 savine_vm_pop_int(SavineVM* vm) {
    vm->stack_top--;
    return vm->stack_top->as_int;
}

f64 savine_vm_pop_float(SavineVM* vm) {
    vm->stack_top--;
    return vm->stack_top->as_float;
}

static InterpretResult run(SavineVM* vm) {
#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants[READ_BYTE()])
#define READ_CONSTANT_LONG() (vm->chunk->constants[((u32)READ_BYTE() << 16) | ((u16)READ_BYTE() << 8) | READ_BYTE()])
#define BINARY_OP(op, type, fn_suffix) \
    do { \
        type b = savine_vm_pop_ ## fn_suffix(vm); \
        (vm->stack_top - 1)->as_ ## fn_suffix = (vm->stack_top - 1)->as_ ## fn_suffix  op b; \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (SavineValue* slot = vm->stack; slot < vm->stack_top; slot++) {
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
                SavineValue constant = READ_CONSTANT();
                savine_vm_push(vm, constant);
                break;
            }
            case OP_CONSTANT_LONG: {
                SavineValue constant = READ_CONSTANT_LONG();
                savine_vm_push(vm, constant);
                break;
            }
            case OP_ZERO: {
                savine_vm_push_int(vm, 0);
                break;
            }
            case OP_ONE: {
                savine_vm_push_int(vm, 1);
                break;
            }

            case OP_NEGATE_INT: {
                (vm->stack_top - 1)->as_int = -(vm->stack_top - 1)->as_int;
                break;
            }
            case OP_NEGATE_DOUBLE: {
                (vm->stack_top - 1)->as_float = -(vm->stack_top - 1)->as_float;
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
                (vm->stack_top - 1)->as_float = (f64)(vm->stack_top - 1)->as_int;
                break;
            case OP_DOUBLE_TO_INT:
                (vm->stack_top - 1)->as_int = (i64)(vm->stack_top - 1)->as_float;
                break;

            case OP_RETURN: {
                print_value(savine_vm_pop(vm));
                printf("\n");
                return SAVINE_INTERPRET_OK;
            }
        }
    }

#undef BINARY_OP
#undef READ_CONSTANT_LONG
#undef READ_CONSTANT
#undef READBYTE
}

static bool compile(const char* source, Chunk* chunk) {
    SavineAST ast;
    if (!savine_parse_to_ast(source, &ast)) {
        savine_ast_free(&ast);
        return false;
    }

#ifdef DEBUG_PRINT_CODE
    savine_ast_print(&ast, "code");
#endif

    savine_resolve_ast_types(&ast);

    bool succeeded = true;
    succeeded &= ast.expression->value_type != SAVINE_TYPE_UNRESOLVED;
    succeeded &= ast.expression->value_type != SAVINE_TYPE_INVALID;

    if (succeeded) {
        succeeded = savine_generate_code(&ast, chunk);
    }

    savine_ast_free(&ast);

    return succeeded;
}

InterpretResult savine_interpret(SavineVM* vm, const char* source) {
    Chunk chunk;
    chunk_init(&chunk);
    chunk.max_stack_size = 256;

    InterpretResult result;
    if (!compile(source, &chunk)) {
        result = SAVINE_INTERPRET_COMPILE_ERROR;
    } else {
        vm->stack = ALLOCATE_N(SavineValue, chunk.max_stack_size);
        vm->stack_top = vm->stack;

        vm->chunk = &chunk;
        vm->ip = vm->chunk->code;

        result = run(vm);

        free(vm->stack);
        vm->stack_top = NULL;
    }

    return result;
}

