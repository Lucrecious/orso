#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int x_ = int_(0);

    int n_ = int_(10);

    (x_ = n_);

    int sum_ = int_(0);

    {
      if ((x_ == n_)) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    int d_; {
      int a_ = int_(1);

      int b_ = int_(2);

      int c_ = int_(3);

      d_ = ((a_ = (b_ = c_)));
    };

    {
      if ((d_ == int_(3))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    int e_; {
      int a_ = int_(1);

      int b_ = int_(2);

      int c_ = int_(3);

      int tmp2; {
        tmp2 = adds32_(adds32_(a_, b_), c_);
      };
      int tmp1 = (b_ = tmp2);
      e_ = (a_ = tmp1);
    };

    {
      if ((e_ == int_(9))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
