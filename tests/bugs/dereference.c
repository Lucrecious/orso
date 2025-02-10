#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"
#include "core.h"
#include "core.c"
typedef void(*fn_void)();
typedef void* p_void;
typedef p_void(*fn_size_t_p_void)(size_t);
typedef bool_(*fn_p_void_size_t_bool_)(p_void,size_t);
typedef size_t(*fn_size_t)();
typedef u64(*fn_u64)();
typedef int(*fn_int)();
typedef void(*fn_int_void)(int);
typedef void(*fn_void)();
typedef f64(*fn_u64_f64)(u64);
typedef u8* p_u8;
f64 ns2sec_odlfn1_(u64);

f64 ns2sec_odlfn1_(u64 ns_) {
  {
    f64 tmp2 = muld_((cast(f64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}



int expr(void) {
  _module_init_3();

  int tmp1; {
    p_u8 m_ = cast(p_u8, ((odlmreserve)(4llu)));

    ((odlmmarkrw)(cast(p_void, m_), 4llu));

    (*((addptr_((p_u8)m_, 3ll))) = int_(1));

    int a_ = cast(int, *(m_));

    int b_ = cast(int, *((addptr_((p_u8)m_, 1ll))));

    int c_ = cast(int, *((addptr_((p_u8)m_, 2ll))));

    int d_ = cast(int, *((addptr_((p_u8)m_, 3ll))));

    ((odlmfree)(cast(p_void, m_), 4llu));

    tmp1 = adds32_(adds32_(adds32_(a_, b_), c_), d_);
  };

  return tmp1;
}
