#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <string.h>

#define CORE_MODULE_NAME "core"

#define UNUSED(arg) ((void)arg)

typedef signed char orschar;
typedef unsigned char oruchar;
typedef char orchar;
typedef int8_t ors8;
typedef uint8_t oru8;
typedef int16_t ors16;
typedef uint16_t oru16;
typedef int32_t ors32;
typedef uint32_t oru32;
typedef int64_t ors64;
typedef uint64_t oru64;
typedef float orf32;
typedef double orf64;
typedef oru8 orbool;

typedef int orint;
typedef unsigned int oruint;

typedef const char *orcstr_t;

typedef struct orstring_t orstring_t;
struct orstring_t {
    orcstr_t cstr;
    size_t length;
};

typedef struct ortype_t ortype_t;
struct ortype_t {
    oru64 i;
};

typedef struct orword_t orword_t;
struct orword_t {
    union {
        ors64 s;
        orf64 d;
        void *p;
        oru64 u;
        ortype_t t;
    } as;
};

#define WORD_SIZE sizeof(orword_t)

#define b2w(size)  ((size + (WORD_SIZE-1)) / WORD_SIZE)
#define b2w2b(size) (b2w(size)*WORD_SIZE)

#define WORDI(value) ((orword_t){.as.s=(value)})
#define WORDU(value) ((orword_t){.as.u=(value)})
#define WORDD(value) ((orword_t){.as.d=(value)})
#define WORDP(value) ((orword_t){.as.p=(value)})
#define WORDT(value) ((orword_t){.as.t=(value)})

#define _ors8(lit) ((ors8)lit)
#define _oru8(lit) ((oru8)lit)
#define _ors16(lit) ((ors16)lit)
#define _oru16(lit) ((oru16)lit)
#define _ors32(lit) ((ors32)lit)
#define _oru32(lit) ((oru32)lit)
#define _orf32(lit) ((orf32)lit)
#define _ors64(lit) ((ors64)lit)
#define _oru64(lit) ((oru64)lit)
#define _orf64(lit) ((orf64)lit)
#define _orint(lit) ((int)lit)
#define _oruint(lit) ((oruint)lit)

#define fn_t_(var_name, return_type, ...) return_type (*var_name)(__VA_ARGS__)

#define typeid(index) ((ortype_t){.i=(index)})
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

#define adds8_(a, b) opi_(a, b, ors8, oru8, +)
#define subs8_(a, b) opi_(a, b, ors8, oru8, -)
#define muls8_(a, b) opi_(a, b, ors8, oru8, *)
#define divs8_(a, b) divi_(a, b, INT8_MIN)
#define mods8_(a, b) modi_(a, b)
#define rems8_(a, b) (a % b)

#define adds16_(a, b) opi_(a, b, ors16, oru16, +)
#define subs16_(a, b) opi_(a, b, ors16, oru16, -)
#define muls16_(a, b) opi_(a, b, ors16, oru16, *)
#define divs16_(a, b) divi_(a, b, INT16_MIN)
#define mods16_(a, b) modi_(a, b)
#define rems16_(a, b) (a % b)

#define adds32_(a, b) opi_(a, b, ors32, oru32, +)
#define subs32_(a, b) opi_(a, b, ors32, oru32, -)
#define muls32_(a, b) opi_(a, b, ors32, oru32, *)
#define divs32_(a, b) divi_(a, b, INT32_MIN)
#define mods32_(a, b) modi_(a, b)
#define rems32_(a, b) (a % b)

#define adds64_(a, b) opi_(a, b, ors64, oru64, +)
#define subs64_(a, b) opi_(a, b, ors64, oru64, -)
#define muls64_(a, b) opi_(a, b, ors64, oru64, *)
#define divs64_(a, b) divi_(a, b, INT64_MIN)
#define mods64_(a, b) modi_(a, b)
#define rems64_(a, b) (a % b)

#define addu8_(a, b) opu_(a, b, oru8, +)
#define subu8_(a, b) opu_(a, b, oru8, -)
#define mulu8_(a, b) opu_(a, b, oru8, *)
#define divu8_(a, b) div_(a, b)
#define modu8_(a, b) modu_(a, b)
#define remu8_(a, b) (a % b)

#define addu16_(a, b) opu_(a, b, oru16, +)
#define subu16_(a, b) opu_(a, b, oru16, -)
#define mulu16_(a, b) opu_(a, b, oru16, *)
#define divu16_(a, b) div_(a, b)
#define modu16_(a, b) modu_(a, b)
#define remu16_(a, b) (a % b)

#define addu32_(a, b) opu_(a, b, oru32, +)
#define subu32_(a, b) opu_(a, b, oru32, -)
#define mulu32_(a, b) opu_(a, b, oru32, *)
#define divu32_(a, b) div_(a, b)
#define modu32_(a, b) modu_(a, b)
#define remu32_(a, b) (a % b)

#define addu64_(a, b) opu_(a, b, oru64, +)
#define subu64_(a, b) opu_(a, b, oru64, -)
#define mulu64_(a, b) opu_(a, b, oru64, *)
#define divu64_(a, b) div_(a, b)
#define modu64_(a, b) modu_(a, b)
#define remu64_(a, b) (a % b)

#define addf_(a, b) (((orf32)a) + ((orf32)b))
#define subf_(a, b) (((orf32)a) - ((orf32)b))
#define mulf_(a, b) (((orf32)a) * ((orf32)b))
#define divf_(a, b) div_(a, b)
#define modf_(a, b) modd_(a, b)
#define remf_(a, b) remd_(a, b)

#define addd_(a, b) (a + b)
#define subd_(a, b) (a - b)
#define muld_(a, b) (a * b)
#define divd_(a, b) div_(a, b)

#define addptr_(ptr, a) ((ptr) + (ors64)(a))
#define subptr_(ptr, a) ((ptr) - (ors64)(a))

#define min_(a, b) ((a) < (b) ? (a) : (b))
#define max_(a, b) ((a) > (b) ? (a) : (b))

#define cast(type, value) ((type)value)

#define __ormemcmp(a, b, size) (memcmp(a, b, size) == 0)

orf64 modd(orf64 a, orf64 b);
orf64 remd(orf64 a, orf64 b);
ors64 modi(ors64 a, ors64 b);
oru64 modu(oru64 a, oru64 b);

#ifdef INTRINSICS_IMPLEMENTATION

orf64 modd(orf64 a, orf64 b) {
    orf64 b_ = b < 0 ? -b : b;
    return fmod(fmod(a, b) + b_, b_);
}

orf64 remd(orf64 a, orf64 b) {
    return fmod(a, b);
}

ors64 modi(ors64 a, ors64 b) {
    ors64 b_ = b < 0 ? -b : b;
    return ((a%b) + b_) % b_;
}

oru64 modu(oru64 a, oru64 b) {
    return a%b;
}

#undef INTRINSICS_IMPLEMENTATION
#endif
