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
typedef type_t(*fn_type_t)();
f64 ns2sec_odlfn1_(u64);

f64 ns2sec_odlfn1_(u64 ns_) {
  {
    f64 tmp2 = muld_((cast(f64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}

type_t get_type_odlfn1_();

type_t get_type_odlfn1_() {
  {
    int x_ = ((odlreadint)());

    {
      if ((x_ == int_(0))) {
        type_t tmp2 = typeid(8);
        return tmp2;
      }
    };

    type_t tmp3 = typeid(17);
    return tmp3;
  };
}


type_t expr(void) {
  _module_init_3();

  type_t tmp4; {
    fn_type_t get_type_ = (get_type_odlfn1_);

    type_t int_or_float_ = typeid(17);

    int n_ = int_(1);

    tmp4 = typeid(17);
  };

  return tmp4;
}
