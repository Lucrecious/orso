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

#define __IS_TWO_COMPLIMENT ((-1 & 3) == 3)
#if !__IS_TWO_COMPLIMENT
#error "compiler is only defined for machines that use 2's compliment"
#endif

#define true 1
#define false 0

#define unless(condition) if (!(condition))
#define until(condition) while (!(condition))

#endif

#define div_(a, b) ((b) != 0 ? ((a)/(b)) : 0)
#define divi_(a, b, imin) ((a == imin && b == -1) ? (imin) : (div_(a, b)))
#define opi_(a, b, i, u, op) ((i)(((u)a) op (u)b))
#define opu_(a, b, u, op) (((u)a) op ((u)b))
#define opf_(a, b, op) (((f32)a) op ((f32)b)) 
#define min_(a, b) ((a) < (b) ? (a) : (b))
#define max_(a, b) ((a) > (b) ? (a) : (b))
#define modd_(a, b)  (modd(a, b))
#define remd_(a, b) (remd(a, b))
#define modi_(a, b) (modi(a, b))
#define modu_(a, b) (modu(a, b))

i8 cast_i64_to_i8(i64 i);
i16 cast_i64_to_i16(i64 i);
i32 cast_i64_to_i32(i64 i);

u8 cast_u64_to_u8(u64 u);
u16 cast_u64_to_u16(u64 u);
u32 cast_u64_to_u32(u64 u);

f32 cast_f64_to_f32(f64 d);

f64 modd(f64 a, f64 b);
f64 remd(f64 a, f64 b);
i64 modi(i64 a, i64 b);
u64 modu(u64 a, u64 b);

#ifdef INTRINSICS_IMPLEMENTATION

#define clamp_(value, low, high) ((value) < (low) ? (low) : (((value) > (high) ? (high) : (value))))

i8 cast_i64_to_i8(i64 i) {
    return (i8)i;
}

i16 cast_i64_to_i16(i64 i) {
    return (i16)i;
}

i32 cast_i64_to_i32(i64 i) {
    return (i32)i;
}

u8 cast_u64_to_u8(u64 u) {
    return (u8)u;
}

u16 cast_u64_to_u16(u64 u) {
    return (u16)u;
}

u32 cast_u64_to_u32(u64 u) {
    return (u32)u;
}

f32 cast_f64_to_f32(f64 d) {
    return (f32)d;
}


#undef clamp_

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
