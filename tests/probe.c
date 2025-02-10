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
typedef bool_(*fn_int_int_bool_)(int,int);
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
bool_ date_odlfn2_(int, int);

type_t get_type_odlfn1_() {
  {
    int preference_ = ((odlreadint)());

    type_t tmp3; {
      if ((preference_ == int_(0))) {
        tmp3 = typeid(17);
      } else {
        tmp3 = typeid(7);
      }
    };
    return tmp3;
  };
}

bool_ date_odlfn2_(int a_, int b_) {
  {
    bool_ tmp4 = true;
    return tmp4;
  };
}


bool_ expr(void) {
  _module_init_3();

  bool_ tmp5; {
    type_t person_t_ = typeid(17);

    int bob_ = int_(0);

    int alice_ = int_(1);

    fn_type_t get_type_ = (get_type_odlfn1_);

    fn_int_int_bool_ date_ = (date_odlfn2_);

    tmp5 = ((date_odlfn2_)(int_(1), int_(0)));
  };

  return tmp5;
}
