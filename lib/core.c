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

u64 clock_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (u64)(ts.tv_sec * 1000000000ULL + ts.tv_nsec);
}

#endif

f64 ns2sec(u64 ns) {
    return ns*(f64)(1e-9);
}

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

void clock_ns_i_(void *args, void *result) {
    UNUSED(args);
    u64 u = clock_ns();
    *(u64*)result = u;
}

void ns2sec_i_(void *args, void *result) {
    u64 ns = *(u64*)args;
    f64 ret = ns2sec(ns);
    *(f64*)result = ret;
}