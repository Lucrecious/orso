#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    i32 x_ = 0;

    i32 n_ = 10;

    (x_ = n_);

    i32 sum_ = 0;

    {
      if ((x_ == n_)) {
        (sum_ = (sum_ + 1));
      }
    };

    i32 d_; {
      i32 a_ = 1;

      i32 b_ = 2;

      i32 c_ = 3;

      d_ = ((a_ = (b_ = c_)));
    };

    {
      if ((d_ == 3)) {
        (sum_ = (sum_ + 1));
      }
    };

    i32 e_; {
      i32 a_ = 1;

      i32 b_ = 2;

      i32 c_ = 3;

      i32 tmp2; {
        tmp2 = ((a_ + b_) + c_);
      };
      i32 tmp1 = (b_ = tmp2);
      e_ = (a_ = tmp1);
    };

    {
      if ((e_ == 9)) {
        (sum_ = (sum_ + 1));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
