#ifndef DEF_H_
#define DEF_H_

#ifndef DEBUG
#define DEBUG
#endif

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "intrinsics.h"

#define unless(condition) if (!(condition))
#define until(condition) while (!(condition))

#define FORCE_INLINE inline __attribute__((always_inline))

#define println(cstr) printf(cstr"\n");
#define printfln(fmt, ...) printf(fmt"\n", __VA_ARGS__)

#define megabytes(bytes) ((size_t)((bytes)*1000*1000))

typedef oru8 byte;
typedef void* ptr;

#define unless(condition) if (!(condition))
#define until(condition) while (!(condition))

#define len(a) sizeof(a) / sizeof(a[0]);

#define zero(ptr, type) memset(ptr, 0, sizeof(type))
#define zer0(type) ((type){0})

FORCE_INLINE ors32 mins32(ors32 a, ors32 b) {
  return a < b ? a : b;
}

FORCE_INLINE ors32 maxs32(ors32 a, ors32 b) {
  return a > b ? a : b;
}

FORCE_INLINE oru32 minu32(oru32 a, oru32 b) {
  return a < b ? a : b;
}

FORCE_INLINE oru32 maxu32(oru32 a, oru32 b) {
  return a > b ? a : b;
}

// Copied from Wren 
#ifdef DEBUG

  #include <stdio.h>
  
  #define ASSERT(condition, message)                                           \
      do                                                                       \
      {                                                                        \
        if (!(condition))                                                      \
        {                                                                      \
          fprintf(stderr, "[%s:%d] Assert failed in %s(): %s\n",               \
              __FILE__, __LINE__, __func__, message);                          \
          abort();                                                             \
        }                                                                      \
      } while (false)
  
  #define NOT_NULL(condition) ASSERT(condition, "cannot be null")

  #define MUST(condition) ASSERT(condition, #condition " failed")

  // Indicates that we know execution should never reach this point in the
  // program. In debug mode, we assert this fact because it's a bug to get here.
  //
  // In release mode, we use compiler-specific built in functions to tell the
  // compiler the code can't be reached. This avoids "missing return" warnings
  // in some cases and also lets it perform some optimizations by assuming the
  // code is never reached.
  #define UNREACHABLE()                                                        \
      do                                                                       \
      {                                                                        \
        fprintf(stderr, "[%s:%d] This code should not be reached in %s()\n",   \
            __FILE__, __LINE__, __func__);                                     \
        abort();                                                               \
      } while (false)

#else

  #define ASSERT(condition, message)
  #define MUST(condition)

  // Tell the compiler that this part of the code will never be reached.
  #if defined( _MSC_VER )
    #define UNREACHABLE() __assume(0)
  #elif (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5))
    #define UNREACHABLE() __builtin_unreachable()
  #else
    #define UNREACHABLE()
  #endif

#endif

#endif
