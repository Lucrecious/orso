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
typedef f64(*fn_f64_f64_f64)(f64,f64);
typedef int(*fn_int_int_int)(int,int);
f64 ns2sec_odlfn1_(u64);

f64 ns2sec_odlfn1_(u64 ns_) {
  {
    f64 tmp2 = muld_((cast(f64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}

f64 func1(f64, f64);
int func2(int, int);

f64 func1(f64 a_, f64 b_) {
  {
    f64 tmp3 = addd_(a_, b_);
    return tmp3;
  };
}

int func2(int a_, int b_) {
  {
    int tmp4 = adds32_(a_, b_);
    return tmp4;
  };
}


int expr(void) {
  _module_init_3();

  int tmp5; {

    ((func1)(1, 2));

    ((func2)(int_(1), int_(2)));

    ((func2)(int_(1), int_(2)));

    tmp5 = ((func2)(int_(1), int_(2)));
  };

  return tmp5;
}
