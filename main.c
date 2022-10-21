#include <stdio.h>

#include "chunk.h"
#include "debug.h"
#include "virtual_machine.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    Chunk chunk;
    chunk.max_stack_size = 256;

    chunk_init(&chunk);

    chunk_write_constant(&chunk, 42, 123);
    chunk_write(&chunk, OP_NEGATE, 123);
    chunk_write_constant(&chunk, 10, 123);
    chunk_write(&chunk, OP_ADD, 123);
    chunk_write(&chunk, OP_RETURN, 123);

    SavineVM vm;
    savine_vm_init(&vm);
    
    savine_interpret(&vm, &chunk);

    savine_vm_free(&vm);
    chunk_free(&chunk);

    return 0;
}
