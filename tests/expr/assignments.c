#include "intrinsics.h"
i64 expr(void) {
  i64 result_; {
    i64 x_ = 0;

    i64 n_ = 10;

    (x_ = n_);

    i64 sum_ = 0;

    {
      if (x_ == n_) {
        (sum_ = (sum_ + 1));
      }
    }

    i64 d_; {
      i64 a_ = 1;

      i64 b_ = 2;

      i64 c_ = 3;

      d_ = ((a_ = (b_ = c_)));
    }

    {
      if (d_ == 3) {
        (sum_ = (sum_ + 1));
      }
    }

    i64 e_; {
      i64 a_ = 1;

      i64 b_ = 2;

      i64 c_ = 3;

      i64 tmp2; {
        tmp2 = ((a_ + b_) + c_);
      }
      i64 tmp1 = (b_ = tmp2);
      e_ = (a_ = tmp1);
    }

    {
      if (e_ == 9) {
        (sum_ = (sum_ + 1));
      }
    }

    result_ = sum_;
  }
  return result_;
}
