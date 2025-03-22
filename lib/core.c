#include "intrinsics.h"
#include "core.h"
#include <stdio.h>

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
#include <unistd.h>
#include <sys/mman.h>

u64 clock_ns(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (u64)((unsigned long long)ts.tv_sec * 1000000000ULL + (unsigned long long)ts.tv_nsec);
}

void *odlmreserve(size_t size) {
    void *addr = mmap(NULL, size, PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS,-1, 0);
    return addr;
}

bool_ odlmmarkro(void *addr, size_t size) {
    bool_ success = (u8)(mprotect(addr, size, PROT_READ) == 0);
    return success;
}

bool_ odlmmarkrw(void *addr, size_t size) {
    bool_ success = (bool_)(mprotect(addr, size, PROT_READ|PROT_WRITE) == 0);
    return success;
}

bool_ odlmfree(void *addr, size_t size) {
    bool_ success = (bool_)(munmap(addr, size) == 0);
    return success;
}

size_t odlmpagesize(void) {
    size_t pagesize = (size_t)getpagesize();
    return pagesize;
}

int odlreadint(void) {
    char buffer[100];
    int number;
    while(fgets(buffer, sizeof buffer, stdin) != NULL) {
        if(sscanf(buffer, "%d", &number) != 1) {
            return 0;
        }

        return number;
    }

    return 0;
}

void odlprintint(int n) {
    printf("%d", n);
}

void odlprintf64(f64 n) {
    printf("%g", n);
}

void odlprintu64(u64 n) {
    printf("%llu", n);
}

void odlprints64(s64 n) {
    printf("%lld", n);
}

void odlprintln(void) {
    printf("\n");
}

#endif

f64 ns2sec(u64 ns) {
    return ns*(f64)(1e-9);
}

#define XARG(name, type) type name = *(type*)(args); args += b2w(sizeof(type))*WORD_SIZE
#define XRET(type, c_fn_name, ...) type ret = c_fn_name(__VA_ARGS__); *(type*)result = ret
#define X(export_fn_name, c_fn_name, return_type, code, ...) void c_fn_name##_i_(void *args, void *result) { code; }

#include "intrinsic_fns.x"

#undef X
#undef XRET
#undef XARG
