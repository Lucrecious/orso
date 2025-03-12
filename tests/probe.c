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
typedef struct arr_4_int arr_4_int; struct arr_4_int { int arr[4]; };
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
    int l_ = int_(1);

    arr_4_int x_ = ((arr_4_int){ .arr={l_, l_, l_, l_}});

    int y_ = int_(1);

    int *tmp3 = (x_.arr);
    int tmp4; {
      tmp4 = adds32_(y_, int_(1));
    };
    int *tmp2 = tmp3 + tmp4;
    int tmp5 = int_(5);
    tmp1 = (*(tmp2) = tmp5);
  };

  return tmp1;
}
