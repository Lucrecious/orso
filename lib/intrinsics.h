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
typedef const char *cstr_t;

#define true 1
#define false 0

#define unless(condition) if (!(condition))
#define until(condition) while (!(condition))

u64 divu(u64 a, u64 b);
i64 divi(i64 a, i64 b);
f64 divd(f64 a, f64 b);

u64 maxu(u64 a, u64 b);
u64 minu(u64 a, u64 b);

i64 maxi(i64 a, i64 b);
i64 mini(i64 a, i64 b);

f64 maxd(f64 a, f64 b);
f64 mind(f64 a, f64 b);

#endif

#ifdef INSTRINSICS_IMPLEMENTATION
#define div_(a, b) ((b) != 0 ? ((a)/(b)) : 0)

u64 divu(u64 a, u64 b) {
    return div_(a, b);
}

i64 divi(i64 a, i64 b) {
    return div_(a, b);
}

f64 divd(f64 a, f64 b) {
    return div_(a, b);
}

#undef div_

#define min_(a, b) ((a) < (b) ? (a) : (b))
#define max_(a, b) ((a) > (b) ? (a) : (b))

u64 minu(u64 a, u64 b) {
    return min_(a, b);
}

u64 maxu(u64 a, u64 b) {
    return max_(a, b);
}

i64 mini(i64 a, i64 b) {
    return min_(a, b);
}

i64 maxi(i64 a, i64 b) {
    return max_(a, b);
}
f64 mind(f64 a, f64 b) {
    return min_(a, b);
}

f64 maxd(f64 a, f64 b) {
    return max_(a, b);
}

#undef min_
#undef max_

#undef INSTRINSICS_IMPLEMENTATION
#endif
