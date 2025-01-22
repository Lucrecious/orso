#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef uint(*fn_uint_uint)(uint);

uint fib1_fn_(uint);

uint fib1_fn_(uint n_) {
  {
    {
      if ((n_ <= uint_(1))) {
        uint tmp2 = uint_(1);
        return tmp2;
      } else {
        {
          if ((n_ <= uint_(2))) {
            uint tmp3 = uint_(1);
            return tmp3;
          } else {
            uint tmp4 = addu32_(((fib1_fn_)(subu32_(n_, uint_(1)))), ((fib1_fn_)(subu32_(n_, uint_(2)))));
            return tmp4;
          }
        };
      }
    };
  };
}


uint expr(void) {
uint result_; {
    fn_uint_uint fib_ = (fib1_fn_);

    result_ = ((fib1_fn_)(uint_(5)));
  };

  return result_;
}
