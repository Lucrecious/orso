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
typedef orf64(*fn_int_f64)(int);
orf64 ns2sec_odlfn1_(oru64);

orf64 ns2sec_odlfn1_(oru64 ns_) {
  {
    orf64 tmp2 = muld_((cast(orf64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}

orf64 calculate_pi_odlfn1_(int);

orf64 calculate_pi_odlfn1_(int iterations_) {
  {
    orf64 pi_ = 0;

    int n_ = int_(0);

    {
      while ((n_ < iterations_)) {
        {
          orf64 tmp5 = pi_;
;
          orf64 tmp7; {
            if ((rems32_(n_, int_(2)) == int_(0))) {
              tmp7 = 1;
            } else {
              tmp7 = -1;
            }
          };
          orf64 tmp8 = (addd_(muld_(2, (cast(orf64, n_))), 1));
          orf64 tmp6 = divd_(tmp7, tmp8);
          orf64 tmp4 = addd_(tmp5, tmp6);
          (pi_ = tmp4);

          (n_ = adds32_(n_, int_(1)));

          pi_;
        };
        continue3_:;
      }
      (pi_ = muld_(pi_, 4));
    } break2_:;

    orf64 tmp9 = pi_;
;
    return tmp9;
  };
}


orf64 expr(void) {
  _module_init_3();

  orf64 tmp10; {
    fn_int_f64 calculate_pi_ = (calculate_pi_odlfn1_);

    tmp10 = 3.14159;
  };

  return tmp10;
}
