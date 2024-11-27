#ifndef VM_H_
#define VM_H_

typedef enum op_type_t op_type_t;
enum op_type_t {
    OP_NOP_,
};

typedef struct instruction_t instruction_t;
struct instruction_t {
    op_type_t type;
};

typedef struct instructions_t instructions_t;
struct instructions_t {
    arena_t items;
    size_t count;
};

typedef struct vm_t vm_t;
struct vm_t {
    byte *memory;
};

void vm_init(vm_t *vm);

#endif

#ifdef VM2_IMPLEMENTATION

void vm_init(vm_t *vm) {
    (void)vm;
}

#endif
