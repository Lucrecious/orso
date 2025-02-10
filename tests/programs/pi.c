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
typedef f64(*fn_int_f64)(int);
f64 ns2sec_odlfn1_(u64);

f64 ns2sec_odlfn1_(u64 ns_) {
  {
    f64 tmp2 = muld_((cast(f64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}

f64 calculate_pi_odlfn1_(int);

f64 calculate_pi_odlfn1_(int iterations_) {
  {
    f64 pi_ = 0;

    int n_ = int_(0);

    {
      while ((n_ < iterations_)) {
        {
          f64 tmp5 = pi_;
;
          f64 tmp7; {
            if ((rems32_(n_, int_(2)) == int_(0))) {
              tmp7 = 1;
            } else {
              tmp7 = -1;
            }
          };
          f64 tmp8 = (addd_(muld_(2, (cast(f64, n_))), 1));
          f64 tmp6 = divd_(tmp7, tmp8);
          f64 tmp4 = addd_(tmp5, tmp6);
          (pi_ = tmp4);

          (n_ = adds32_(n_, int_(1)));

          pi_;
        };
        continue3_:;
      }
      (pi_ = muld_(pi_, 4));
    } break2_:;

    f64 tmp9 = pi_;
;
    return tmp9;
  };
}


f64 expr(void) {
  _module_init_3();

  f64 tmp10; {
    fn_int_f64 calculate_pi_ = (calculate_pi_odlfn1_);

    tmp10 = 3.14159;
  };

  return tmp10;
}
