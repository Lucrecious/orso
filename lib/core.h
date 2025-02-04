#ifndef CORE_H_
#define CORE_H_

#include "intrinsics.h"

u64 clock_ns(void);
f64 ns2sec(u64);

void *mmap(size_t size_bytes);
void mcommit(void *addr, size_t size_bytes);
void mmarkro(void *addr, size_t size_bytes);
void munmap(void *addr);

void clock_ns_i_(void *, void*);
void ns2sec_i_(void*, void*);


#endif
