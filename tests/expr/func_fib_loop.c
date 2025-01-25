#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int(*fn_int_int)(int);

int fib1_fn_(int);

int fib1_fn_(int n_) {
  {
    int a_ = int_(0);

    int b_ = int_(1);

    int tmp2; {
      while ((n_ > int_(1))) {
        {
          int t_ = a_;
;

          (a_ = b_);

          (b_ = adds32_(b_, t_));

          tmp2 = ((n_ = subs32_(n_, int_(1))));
        };
        continue4_:;
      }
      {
        tmp2 = b_;
;
      };
    } break3_:;
    return tmp2;
  };
}


int expr(void) {
int result_; {
    fn_int_int fib_ = (fib1_fn_);

    result_ = ((fib1_fn_)(int_(10)));
  };

  return result_;
}
