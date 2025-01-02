#include "intrinsics.h"
i64 expr(void) {
i64 result_; {
    i64 x_ = 0;

    i64 n_ = 10;

    i64 sum_ = 0;

    {
      while (x_ < n_) {
        {
          (sum_ = (sum_ + x_));

          i64 tmp2; {
            tmp2 = x_;
;
          };
          i64 tmp3 = 1;
          i64 tmp1 = tmp2 + tmp3;
          (x_ = tmp1);
        };
      }
      sum_;
    };

    i64 count_ = 10;

    {
      while (((count_ = (count_ - 1))) > 0) {
        (sum_ = (sum_ + 1));
      }
      0;
    };

    (count_ = 10);

    i64 a_; {
      while (((count_ = (count_ - 1))) > 0) {
        a_ = (sum_ = (sum_ + 1));
      }
      a_ = 0;
    };

    {
      if (a_ == 0) {
        (sum_ = (sum_ - 65));
      } else {
        (sum_ = (sum_ + 1));
      }
    };

    i64 b_; {
      while (sum_ > 0) {
        b_ = (sum_ = (sum_ - 1));
      }
      b_ = ((sum_ = (sum_ + 1)));
    };

    {
      if (b_ == 1) {
        result_ = 1;
      } else {
        result_ = 0;
      }
    };
  };

  return result_;
}
