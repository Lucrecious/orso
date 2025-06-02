#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"
#include "core.h"
#include "core.c"
typedef void(*fn_void)();
typedef void* p_void;
typedef p_void(*fn_size_t_p_void)(size_t);
typedef orbool(*fn_p_void_size_t_bool_)(p_void,size_t);
typedef size_t(*fn_size_t)();
typedef oru64(*fn_u64)();
typedef int(*fn_int)();
typedef void(*fn_int_void)(int);
typedef void(*fn_void)();
typedef orf64(*fn_u64_f64)(oru64);
orf64 ns2sec_odlfn1_(oru64);

orf64 ns2sec_odlfn1_(oru64 ns_) {
  {
    orf64 tmp2 = muld_((cast(orf64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}



orbool expr(void) {
  _module_init_3();

  orbool tmp1; {
    orbool x_ = false;

    int tmp2; {
      if (x_) {
        tmp2 = int_(1);
      } else {
        tmp2 = int_(-1);
      }
    };
    orf64 a_ = cast(orf64, tmp2);

    tmp1 = (a_ == -1);
  };

  return tmp1;
}
