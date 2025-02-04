#ifndef CORE_H_
#define CORE_H_

#include "intrinsics.h"

u64 clock_(void);

void *mmap(size_t size_bytes);
void mcommit(void *addr, size_t size_bytes);
void mmarkro(void *addr, size_t size_bytes);
void munmap(void *addr);

void clock_i_(void *, void*);


#endif
