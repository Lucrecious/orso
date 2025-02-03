#include "core.h"

void *mmap(size_t size_bytes) {
    (void)size_bytes;
}

void mcommit(void *addr, size_t size_bytes) {
    (void)addr;
    (void)size_bytes;
}

void mmarkro(void *addr, size_t size_bytes) {
    (void)addr;
    (void)size_bytes;
}

void munmap(void *addr) {
    (void)addr;
}