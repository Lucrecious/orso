#ifndef DEF_H_
#define DEF_H_

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define FORCE_INLINE inline __attribute__((always_inline))

#define ALLOCATE(T) (T*)malloc(sizeof(T))

typedef uint8_t byte;
typedef int16_t i16;
typedef uint16_t u16;
typedef int32_t i32;
typedef uint32_t u32;
typedef int64_t i64;
typedef float f32;
typedef double f64;

#endif
