#include <stdio.h>

#include "chunk.h"
#include "debug.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    Chunk chunk;

    chunk_init(&chunk);

    i32 constant = chunk_add_constant(&chunk, 42);

    chunk_write(&chunk, OP_RETURN, 123);

    chunk_disassemble(&chunk, "my code");

    chunk_free(&chunk);

    return 0;
}
