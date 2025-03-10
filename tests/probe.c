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
typedef struct arr_2_s64 arr_2_s64; struct arr_2_s64 { s64 arr[2]; };
typedef struct arr_5_arr_2_s64 arr_5_arr_2_s64; struct arr_5_arr_2_s64 { arr_2_s64 arr[5]; };
typedef struct arr_10_s64 arr_10_s64; struct arr_10_s64 { s64 arr[10]; };
f64 ns2sec_odlfn1_(u64);

f64 ns2sec_odlfn1_(u64 ns_) {
  {
    f64 tmp2 = muld_((cast(f64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}



s64 expr(void) {
  _module_init_3();

  s64 tmp1; {

    s64 x_ = 69ll;

    arr_2_s64 tmp2 = (arr_2_s64){ .arr={1ll, 2ll} };
    arr_2_s64 tmp3 = (arr_2_s64){ .arr={3ll, 4ll} };
    s64 tmp5 = 5ll;
    s64 tmp6; {
      tmp6 = x_;
;
    };
    arr_2_s64 tmp4 = ((arr_2_s64){ .arr={tmp5, tmp6}});
;
    arr_5_arr_2_s64 d_ = ((arr_5_arr_2_s64){ .arr={tmp2, tmp3, tmp4, (arr_2_s64){ .arr={7ll, 8ll} }, (arr_2_s64){ .arr={9ll, 10ll} }}});
;

    arr_10_s64 v_ = (arr_10_s64){ .arr={0ll, 0ll, 0ll, 0ll, 0ll, 0ll, 0ll, 0ll, 0ll, 0ll} };

    {
      int i_ = int_(0);
      while ((i_ < int_(5))) {
        {
          int j_ = int_(0);
          while ((j_ < int_(2))) {
            {
              v_.arr[adds32_(muls32_(i_, int_(2)), j_)] = d_.arr[i_].arr[j_];

              d_.arr[i_].arr[j_] = d_.arr[i_].arr[j_];
            };
            continue10_:;
            (j_ = adds32_(j_, int_(1)));
          }
        } break9_:;
        continue8_:;
        (i_ = adds32_(i_, int_(1)));
      }
    } break7_:;

    tmp1 = (v_.arr[int_(5)]);
  };

  return tmp1;
}
