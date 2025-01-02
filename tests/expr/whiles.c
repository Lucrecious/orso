#include "intrinsics.h"
i64 expr(void) {
  i64 result_; {
    i64 x_ = 0;

    i64 n_ = 10;

    i64 sum_ = 0;

    {
      while (x_ < n_) {
        {
          sum_ = (sum_ + x_);

          i64 tmp2; {
            tmp2 = x_;
          }
          i64 tmp3 = 1;
          i64 tmp1 = tmp2 + tmp3;
          result_ = (x_ = tmp1);
        }
      }
      result_ = sum_;
    }
  }
  return result_;
}
