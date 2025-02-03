#ifndef CORE_H_
#define CORE_H_

#include "intrinsics.h"

void *mmap(size_t size_bytes);
void mcommit(void *addr, size_t size_bytes);
void mmarkro(void *addr, size_t size_bytes);
void munmap(void *addr);


#endif
