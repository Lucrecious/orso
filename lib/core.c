#include "core.h"

#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>

u64 clock(void) {
    LARGE_INTEGER frequency, counter;
    QueryPerformanceFrequency(&frequency);
    QueryPerformanceCounter(&counter);
    
    return (u64)(counter.QuadPart * 1000000000ULL / frequency.QuadPart);
}

#else
#include <time.h>

u64 clock_(void) {
    // struct timespec ts;
    // clock_gettime(CLOCK_MONOTONIC, &ts);
    // return (u64)(ts.tv_sec * 1000000000ULL + ts.tv_nsec);
    return 69;
}

#endif

void *mmap(size_t size_bytes) {
    (void)size_bytes;
    return NULL;
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

void clock_i_(void *args, void *result) {
    UNUSED(args);
    u64 u = clock_();
    *(u64*)result = u;
}