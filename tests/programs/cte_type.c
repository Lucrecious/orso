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
typedef ortype_t(*fn_type_t)();
orf64 ns2sec_odlfn1_(oru64);

orf64 ns2sec_odlfn1_(oru64 ns_) {
  {
    orf64 tmp2 = muld_((cast(orf64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}

ortype_t get_type_odlfn1_();

ortype_t get_type_odlfn1_() {
  {
    int x_ = ((odlreadint)());

    {
      if ((x_ == int_(0))) {
        ortype_t tmp2 = typeid(8);
        return tmp2;
      }
    };

    ortype_t tmp3 = typeid(17);
    return tmp3;
  };
}


ortype_t expr(void) {
  _module_init_3();

  ortype_t tmp4; {
    fn_type_t get_type_ = (get_type_odlfn1_);

    ortype_t int_or_float_ = typeid(17);

    int n_ = int_(1);

    tmp4 = typeid(17);
  };

  return tmp4;
}
