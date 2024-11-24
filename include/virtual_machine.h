#ifndef VIRTUAL_MACHINE_H_
#define VIRTUAL_MACHINE_H_

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "symbol_table.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct source_location_t source_location_t;
struct source_location_t {
    cstr_t file_path;
    size_t line;
    size_t column;
};

typedef struct {
    function_t *function;
    byte *ip;
    slot_t *slots;
} call_frame_t;

typedef struct vm_t {
    write_function_t write_fn;
    error_function_t error_fn;

    function_t *entry_point;

    type_table_t *type_set;

    call_frame_t frames[FRAMES_MAX];
    i32 frame_count;

    struct {
        symbol_table_t name_to_index;

        types_t types;
        slots_t values;
    } globals;

    symbol_table_t symbols;

    size_t stack_types_slot_count;
    types_t stack_types;
    
    slot_t *stack;
    slot_t *stack_top;

    arena_t allocator;
} vm_t;

void vm_init(vm_t *vm, write_function_t write_fn, error_function_t error_fn, i32 stack_size);
void vm_free(vm_t *vm);

void vm_call(vm_t *vm, function_t *function);
void vm_push_object(vm_t *vm, object_t *object);

void vm_print_stack(vm_t *vm);
void vm_disassemble_current_instruction(vm_t *vm);

bool vm_is_on_debug_instruction(vm_t *vm);

void vm_begin(vm_t *vm, function_t *entry_point);
bool vm_step(vm_t *vm);
bool vm_find_source_location(vm_t *vm, source_location_t *source_location);

void vm_interpret(vm_t *vm, error_function_t error_fn);

#endif
