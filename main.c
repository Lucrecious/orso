#include <stdio.h>

#include "chunk.h"
#include "debug.h"

#define PROJECT_NAME "savine"

int main(int argc, char **argv) {
    Chunk chunk;

    chunk_init(&chunk);

    chunk_write_constant(&chunk, 42, 123);
    chunk_write_constant(&chunk, 69, 123);
    chunk_write_constant(&chunk, 420, 123);
    chunk_write_constant(&chunk, 1, 123);
    chunk_write_constant(&chunk, 2, 123);
    chunk_write_constant(&chunk, 3, 123);
    chunk_write(&chunk, OP_RETURN, 123);

    chunk_disassemble(&chunk, "my code");

    chunk_free(&chunk);

    return 0;
}
