#ifndef INTRINSICS_H_
#define INTRINSICS_H_

#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <string.h>
#include <stdbool.h>

#define ORCORE_MODULE_NAME "core"

#define ORUNUSED(arg) ((void)(arg))

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
typedef bool orbool;
typedef ors64 orsint;

typedef int orint;
typedef unsigned int oruint;

typedef char *orcstr_t;

typedef struct orstring_t orstring_t;
struct orstring_t {
    orcstr_t cstr;
    orsint length;
};

typedef struct ortype_t ortype_t;
struct ortype_t {
    oru64 i;
};

typedef struct orany_t orany_t;
struct orany_t {
    ortype_t type;
    void *data;
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

#define ORWORD_SIZE sizeof(orword_t)

#define orb2w(size)  ((size + (ORWORD_SIZE-1)) / ORWORD_SIZE)
#define orb2w2b(size) (orb2w(size)*ORWORD_SIZE)

#define ORWORDI(value) ((orword_t){.as.s=(value)})
#define ORWORDU(value) ((orword_t){.as.u=(value)})
#define ORWORDD(value) ((orword_t){.as.d=(value)})
#define ORWORDP(value) ((orword_t){.as.p=(value)})
#define ORWORDT(value) ((orword_t){.as.t=(value)})

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
#define _orchar(lit) ((orchar)lit)

#define orfn_t_(var_name, return_type, ...) return_type (*var_name)(__VA_ARGS__)

#define ortypeid(index) ((ortype_t){.i=(index)})
#define ortypeid_eq(t1, t2) ((t1).i == (t2).i)
#define ortypeid_nq(t1, t2) (!ortypeid_eq(t1, t2))

#define __IS_TWO_COMPLIMENT ((-1 & 3) == 3)
#if !__IS_TWO_COMPLIMENT
#error "compiler is only defined for machines that use 2's compliment"
#endif

#endif

#define oropi(a, b, i, u, op) ((i)(((u)a) op (u)b))
#define oropu(a, b, u, op) ((u)(((u)a) op ((u)b)))
#define ormodd(a, b)  (ormodd_(a, b))
#define orremd(a, b) (orremd_(a, b))
#define ormodi(a, b) (ormodi_(a, b))
#define ormodu(a, b) (ormodu_(a, b))
#define ordiv(a, b) ((b) != 0 ? ((a)/(b)) : 0)

#define ordivi(a, b, imin) ((a == imin && b == -1) ? (imin) : (ordiv(a, b)))

#define oradds8(a, b) oropi(a, b, ors8, oru8, +)
#define orsubs8(a, b) oropi(a, b, ors8, oru8, -)
#define ormuls8(a, b) oropi(a, b, ors8, oru8, *)
#define ordivs8(a, b) ordivi(a, b, INT8_MIN)
#define ormods8(a, b) ormodi(a, b)
#define orrems8(a, b) (a % b)

#define oradds16(a, b) oropi(a, b, ors16, oru16, +)
#define orsubs16(a, b) oropi(a, b, ors16, oru16, -)
#define ormuls16(a, b) oropi(a, b, ors16, oru16, *)
#define ordivs16(a, b) ordivi(a, b, INT16_MIN)
#define ormods16(a, b) ormodi(a, b)
#define orrems16(a, b) (a % b)

#define oradds32(a, b) oropi(a, b, ors32, oru32, +)
#define orsubs32(a, b) oropi(a, b, ors32, oru32, -)
#define ormuls32(a, b) oropi(a, b, ors32, oru32, *)
#define ordivs32(a, b) ordivi(a, b, INT32_MIN)
#define ormods32(a, b) ormodi(a, b)
#define orrems32(a, b) (a % b)

#define oradds64(a, b) oropi(a, b, ors64, oru64, +)
#define orsubs64(a, b) oropi(a, b, ors64, oru64, -)
#define ormuls64(a, b) oropi(a, b, ors64, oru64, *)
#define ordivs64(a, b) ordivi(a, b, INT64_MIN)
#define ormods64(a, b) ormodi(a, b)
#define orrems64(a, b) (a % b)

#define oraddu8(a, b) oropu(a, b, oru8, +)
#define orsubu8(a, b) oropu(a, b, oru8, -)
#define ormulu8(a, b) oropu(a, b, oru8, *)
#define ordivu8(a, b) ordiv(a, b)
#define ormodu8(a, b) ormodu(a, b)
#define orremu8(a, b) (a % b)

#define oraddu16(a, b) oropu(a, b, oru16, +)
#define orsubu16(a, b) oropu(a, b, oru16, -)
#define ormulu16(a, b) oropu(a, b, oru16, *)
#define ordivu16(a, b) ordiv(a, b)
#define ormodu16(a, b) ormodu(a, b)
#define orremu16(a, b) (a % b)

#define oraddu32(a, b) oropu(a, b, oru32, +)
#define orsubu32(a, b) oropu(a, b, oru32, -)
#define ormulu32(a, b) oropu(a, b, oru32, *)
#define ordivu32(a, b) ordiv(a, b)
#define ormodu32(a, b) ormodu(a, b)
#define orremu32(a, b) (a % b)

#define oraddu64(a, b) oropu(a, b, oru64, +)
#define orsubu64(a, b) oropu(a, b, oru64, -)
#define ormulu64(a, b) oropu(a, b, oru64, *)
#define ordivu64(a, b) ordiv(a, b)
#define ormodu64(a, b) ormodu(a, b)
#define orremu64(a, b) (a % b)

#define oraddf(a, b) (((orf32)a) + ((orf32)b))
#define orsubf(a, b) (((orf32)a) - ((orf32)b))
#define ormulf(a, b) (((orf32)a) * ((orf32)b))
#define ordivf(a, b) ordiv(a, b)
#define ormodf(a, b) ormodd(a, b)
#define orremf(a, b) orremd(a, b)

#define oraddd(a, b) (a + b)
#define orsubd(a, b) (a - b)
#define ormuld(a, b) (a * b)
#define ordivd(a, b) ordiv(a, b)

#define oroffsetptr(ptr, a) ((ptr) + (ors64)(a))
#define orptrdiff(a, b) ((a) - (b))

#define ormin(a, b) ((a) < (b) ? (a) : (b))
#define ormax(a, b) ((a) > (b) ? (a) : (b))

#define orcast(type, value) ((type)value)

#define __ormemcmp(a, b, size) (memcmp(a, b, size) == 0)

orf64 ormodd_(orf64 a, orf64 b);
orf64 orremd_(orf64 a, orf64 b);
ors64 ormodi_(ors64 a, ors64 b);
oru64 ormodu_(oru64 a, oru64 b);

#ifdef INTRINSICS_IMPLEMENTATION

orf64 ormodd_(orf64 a, orf64 b) {
    orf64 b_ = b < 0 ? -b : b;
    return fmod(fmod(a, b) + b_, b_);
}

orf64 orremd_(orf64 a, orf64 b) {
    return fmod(a, b);
}

ors64 ormodi_(ors64 a, ors64 b) {
    ors64 b_ = b < 0 ? -b : b;
    return ((a%b) + b_) % b_;
}

oru64 ormodu_(oru64 a, oru64 b) {
    return a%b;
}

#undef INTRINSICS_IMPLEMENTATION
#endif
