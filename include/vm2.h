#ifndef VM2_H_
#define VM2_H_

typedef enum op_type_t op_type_t;
enum op_type_t {
    OP_NOP_,
};

typedef struct vm2_t vm2_t;
struct vm2_t {
    byte *memory;
};

void vm2_init(vm2_t *vm);


#endif

#ifdef VM2_IMPLEMENTATION

void vm2_init(vm2_t *vm) {
    (void)vm;
}

#endif
