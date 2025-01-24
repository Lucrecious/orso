#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include <stdint.h>
#include <stddef.h>
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

typedef unsigned int uint;

typedef const char *cstr_t;

#define i8_(lit) ((i8)lit)
#define u8_(lit) ((u8)lit)
#define i16_(lit) ((i16)lit)
#define u16_(lit) ((u16)lit)
#define i32_(lit) ((i32)lit)
#define u32_(lit) ((u32)lit)
#define f32_(lit) ((f32)lit)
#define i64_(lit) ((i64)lit)
#define u64_(lit) ((u64)lit)
#define f64_(lit) ((f64)lit)
#define int_(lit) ((int)lit)
#define uint_(lit) ((uint)lit)

#define fn_t_(var_name, return_type, ...) return_type (*var_name)(__VA_ARGS__)

typedef struct type_t type_t;
struct type_t {
    u64 i;
};

#define typeid(index) ((type_t){.i=(index)})
#define typeid_eq(t1, t2) ((t1).i == (t2).i)
#define typeid_nq(t1, t2) (!typeid_eq(t1, t2))

#define __IS_TWO_COMPLIMENT ((-1 & 3) == 3)
#if !__IS_TWO_COMPLIMENT
#error "compiler is only defined for machines that use 2's compliment"
#endif

#define true 1
#define false 0

#define unless(condition) if (!(condition))
#define until(condition) while (!(condition))

#endif

#define opi_(a, b, i, u, op) ((i)(((u)a) op (u)b))
#define opu_(a, b, u, op) ((u)(((u)a) op ((u)b)))
#define modd_(a, b)  (modd(a, b))
#define remd_(a, b) (remd(a, b))
#define modi_(a, b) (modi(a, b))
#define modu_(a, b) (modu(a, b))
#define div_(a, b) ((b) != 0 ? ((a)/(b)) : 0)

#define divi_(a, b, imin) ((a == imin && b == -1) ? (imin) : (div_(a, b)))

#define addi8_(a, b) opi_(a, b, i8, u8, +)
#define subi8_(a, b) opi_(a, b, i8, u8, -)
#define muli8_(a, b) opi_(a, b, i8, u8, *)
#define divi8_(a, b) divi_(a, b, INT8_MIN)
#define modi8_(a, b) modi_(a, b)
#define remi8_(a, b) (a % b)

#define addi16_(a, b) opi_(a, b, i16, u16, +)
#define subi16_(a, b) opi_(a, b, i16, u16, -)
#define muli16_(a, b) opi_(a, b, i16, u16, *)
#define divi16_(a, b) divi_(a, b, INT16_MIN)
#define modi16_(a, b) modi_(a, b)
#define remi16_(a, b) (a % b)

#define addi32_(a, b) opi_(a, b, i32, u32, +)
#define subi32_(a, b) opi_(a, b, i32, u32, -)
#define muli32_(a, b) opi_(a, b, i32, u32, *)
#define divi32_(a, b) divi_(a, b, INT32_MIN)
#define modi32_(a, b) modi_(a, b)
#define remi32_(a, b) (a % b)

#define addi64_(a, b) opi_(a, b, i64, u64, +)
#define subi64_(a, b) opi_(a, b, i64, u64, -)
#define muli64_(a, b) opi_(a, b, i64, u64, *)
#define divi64_(a, b) divi_(a, b, INT64_MIN)
#define modi64_(a, b) modi_(a, b)
#define remi64_(a, b) (a % b)

#define addu8_(a, b) opu_(a, b, u8, +)
#define subu8_(a, b) opu_(a, b, u8, -)
#define mulu8_(a, b) opu_(a, b, u8, *)
#define divu8_(a, b) div_(a, b)
#define modu8_(a, b) modi_(a, b)
#define remu8_(a, b) (a % b)

#define addu16_(a, b) opu_(a, b, u16, +)
#define subu16_(a, b) opu_(a, b, u16, -)
#define mulu16_(a, b) opu_(a, b, u16, *)
#define divu16_(a, b) div_(a, b)
#define modu16_(a, b) modi_(a, b)
#define remu16_(a, b) (a % b)

#define addu32_(a, b) opu_(a, b, u32, +)
#define subu32_(a, b) opu_(a, b, u32, -)
#define mulu32_(a, b) opu_(a, b, u32, *)
#define divu32_(a, b) div_(a, b)
#define modu32_(a, b) modi_(a, b)
#define remu32_(a, b) (a % b)

#define addu64_(a, b) opu_(a, b, u64, +)
#define subu64_(a, b) opu_(a, b, u64, -)
#define mulu64_(a, b) opu_(a, b, u64, *)
#define divu64_(a, b) div_(a, b)
#define modu64_(a, b) modu_(a, b)
#define remu64_(a, b) (a % b)

#define addf_(a, b) (((f32)a) + ((f32)b))
#define subf_(a, b) (((f32)a) - ((f32)b))
#define mulf_(a, b) (((f32)a) * ((f32)b))
#define divf_(a, b) div_(a, b)
#define modf_(a, b) modd_(a, b)
#define remf_(a, b) remd_(a, b)

#define addd_(a, b) (a + b)
#define subd_(a, b) (a - b)
#define muld_(a, b) (a * b)
#define divd_(a, b) div_(a, b)

#define min_(a, b) ((a) < (b) ? (a) : (b))
#define max_(a, b) ((a) > (b) ? (a) : (b))

#define cast(type, value) ((type)value)

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
