#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include <stdint.h>
#include <math.h>

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
typedef const char *cstr_t;

#define true 1
#define false 0

#define unless(condition) if (!(condition))
#define until(condition) while (!(condition))

#endif

#define div_(a, b) ((b) != 0 ? ((a)/(b)) : 0)
#define min_(a, b) ((a) < (b) ? (a) : (b))
#define max_(a, b) ((a) > (b) ? (a) : (b))
#define modd_(a, b)  (modd(a, b))
#define remd_(a, b) (remd(a, b))
#define modi_(a, b) (modi(a, b))
#define modu_(a, b) (modu(a, b))

f64 modd(f64 a, f64 b);
f64 remd(f64 a, f64 b);
i64 modi(i64 a, i64 b);
u64 modu(u64 a, u64 b);

#ifdef INTRINSICS_IMPLEMENTATION

f64 modd(f64 a, f64 b) {
    f64 b_ = b < 0 ? -b : b;
    return fmod(fmod(a, b) + b_, b_);
}

f64 remd(f64 a, f64 b) {
    return fmod(a, b);
}

i64 modi(i64 a, i64 b) {
    i64 b_ = b < 0 ? -b : b;
    return ((a%b) + b_) % b_;
}

u64 modu(u64 a, u64 b) {
    return a%b;
}

#undef INTRINSICS_IMPLEMENTATION
#endif
