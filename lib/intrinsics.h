#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include <stdint.h>

typedef int8_t i8;
typedef uint8_t u8;
typedef int16_t i16;
typedef uint16_t u16;
typedef int32_t i32;
typedef uint32_t u32;
typedef int64_t i64;
typedef uint64_t u64;
typedef float f32;
typedef double f64;
typedef u8 bool_;

#define unless(condition) if (!(condition))
#define until(condition) while (!(condition))

#endif
