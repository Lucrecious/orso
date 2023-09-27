#ifndef DEF_H_
#define DEF_H_

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define FORCE_INLINE inline __attribute__((always_inline))

#define ORSO_ALLOCATE(T) (T*)malloc(sizeof(T))
#define ORSO_ALLOCATE_N(T, N) (T*)malloc(sizeof(T) * N)
#define ORSO_ALLOCATE_FLEX(T, N) (T*)malloc(sizeof(T) + N)

typedef uint8_t byte;
typedef int16_t i16;
typedef uint16_t u16;
typedef int32_t i32;
typedef uint32_t u32;
typedef int64_t i64;
typedef uint64_t u64;
typedef float f32;
typedef double f64;
typedef void* ptr;

FORCE_INLINE bool i32min(i32 a, i32 b) {
  return a < b ? a : b;
}

FORCE_INLINE bool i32max(i32 a, i32 b) {
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

  #define ASSERT(condition, message) do { } while (false)

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
