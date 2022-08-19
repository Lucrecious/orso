#ifndef MATHUTILS_H_
#define MATHUTILS_H_

#include "def.h"

i32 inline __attribute__((always_inline)) imin(i32 a, i32 b) {
    return a < b ? a : b;
}

i32 inline __attribute__((always_inline)) imax(i32 a, i32 b)  {
    return a > b ? a : b;
}

#endif 
