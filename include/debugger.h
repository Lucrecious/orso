#ifndef DEBUGGER_H_
#define DEBUGGER_H_

#include "arena.h"
#include "stringt.h"
#include "vm.h"
#include "def.h"

typedef struct source_location_t source_location_t;
struct source_location_t {
    string_t file_path;
    texloc_t text_location;
};

typedef struct breakpoints_t breakpoints_t;
struct breakpoints_t {
    source_location_t *items;
    size_t count;
    size_t capacity;
    arena_t *allocator;
};

typedef struct debugger_t debugger_t;
struct debugger_t {
    arena_t *allocator;
    breakpoints_t breakpoints;
    arena_t step_allocator;
};

void debugger_init(debugger_t *debugger, arena_t *allocator);
bool debugger_step(debugger_t *debugger, vm_t *vm);
string_t disassemble_instruction(instruction_t in, arena_t *allocator);

source_location_t vm_find_source_location(vm_t *vm);

#endif
