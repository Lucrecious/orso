#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include <stdint.h>
#include <stddef.h>
#include <math.h>

#define UNUSED(arg) ((void)arg)

typedef int8_t s8;
typedef uint8_t u8;
typedef int16_t s16;
typedef uint16_t u16;
typedef int32_t s32;
typedef uint32_t u32;
typedef int64_t s64;
typedef uint64_t u64;
typedef float f32;
typedef double f64;
typedef u8 bool_;

typedef unsigned int uint;

typedef const char *cstr_t;

typedef struct type_t type_t;
struct type_t {
    u64 i;
};

typedef struct word_t word_t;
struct word_t {
    union {
        s64 s;
        f64 d;
        void *p;
        u64 u;
        type_t t;
    } as;
};

#define WORD_SIZE sizeof(word_t)

#define b2w(size)  ((size + (WORD_SIZE-1)) / WORD_SIZE)

#define WORDI(value) ((word_t){.as.s=(value)})
#define WORDU(value) ((word_t){.as.u=(value)})
#define WORDD(value) ((word_t){.as.d=(value)})
#define WORDP(value) ((word_t){.as.p=(value)})
#define WORDT(value) ((word_t){.as.t=(value)})

#define s8_(lit) ((s8)lit)
#define u8_(lit) ((u8)lit)
#define s16_(lit) ((s16)lit)
#define u16_(lit) ((u16)lit)
#define s32_(lit) ((s32)lit)
#define u32_(lit) ((u32)lit)
#define f32_(lit) ((f32)lit)
#define s64_(lit) ((s64)lit)
#define u64_(lit) ((u64)lit)
#define f64_(lit) ((f64)lit)
#define int_(lit) ((int)lit)
#define uint_(lit) ((uint)lit)

#define fn_t_(var_name, return_type, ...) return_type (*var_name)(__VA_ARGS__)

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

#define adds8_(a, b) opi_(a, b, s8, u8, +)
#define subs8_(a, b) opi_(a, b, s8, u8, -)
#define muls8_(a, b) opi_(a, b, s8, u8, *)
#define divs8_(a, b) divi_(a, b, INT8_MIN)
#define mods8_(a, b) modi_(a, b)
#define rems8_(a, b) (a % b)

#define adds16_(a, b) opi_(a, b, s16, u16, +)
#define subs16_(a, b) opi_(a, b, s16, u16, -)
#define muls16_(a, b) opi_(a, b, s16, u16, *)
#define divs16_(a, b) divi_(a, b, INT16_MIN)
#define mods16_(a, b) modi_(a, b)
#define rems16_(a, b) (a % b)

#define adds32_(a, b) opi_(a, b, s32, u32, +)
#define subs32_(a, b) opi_(a, b, s32, u32, -)
#define muls32_(a, b) opi_(a, b, s32, u32, *)
#define divs32_(a, b) divi_(a, b, INT32_MIN)
#define mods32_(a, b) modi_(a, b)
#define rems32_(a, b) (a % b)

#define adds64_(a, b) opi_(a, b, s64, u64, +)
#define subs64_(a, b) opi_(a, b, s64, u64, -)
#define muls64_(a, b) opi_(a, b, s64, u64, *)
#define divs64_(a, b) divi_(a, b, INT64_MIN)
#define mods64_(a, b) modi_(a, b)
#define rems64_(a, b) (a % b)

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
s64 modi(s64 a, s64 b);
u64 modu(u64 a, u64 b);

#ifdef INTRINSICS_IMPLEMENTATION

f64 modd(f64 a, f64 b) {
    f64 b_ = b < 0 ? -b : b;
    return fmod(fmod(a, b) + b_, b_);
}

f64 remd(f64 a, f64 b) {
    return fmod(a, b);
}

s64 modi(s64 a, s64 b) {
    s64 b_ = b < 0 ? -b : b;
    return ((a%b) + b_) % b_;
}

u64 modu(u64 a, u64 b) {
    return a%b;
}

#undef INTRINSICS_IMPLEMENTATION
#endif
