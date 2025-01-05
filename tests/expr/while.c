#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i64 expr(void) {
i64 result_; {
    i64 x_ = 0;

    i64 n_ = 10;

    i64 sum_ = 0;

    {
      while ((x_ < n_)) {
        {
          (sum_ = (sum_ + x_));

          i64 tmp4; {
            tmp4 = x_;
;
          };
          i64 tmp5 = 1;
          i64 tmp3 = tmp4 + tmp5;
          (x_ = tmp3);
        };
        continue2_:;
      }
      sum_;
    } break1_:;

    i64 count_ = 10;

    {
      while ((((count_ = (count_ - 1))) > 0)) {
        (sum_ = (sum_ + 1));
        continue7_:;
      }
      0;
    } break6_:;

    (count_ = 10);

    i64 a_; {
      while ((((count_ = (count_ - 1))) > 0)) {
        a_ = (sum_ = (sum_ + 1));
        continue9_:;
      }
      a_ = 0;
    } break8_:;

    {
      if ((a_ == 0)) {
        (sum_ = (sum_ - 65));
      } else {
        (sum_ = (sum_ + 1));
      }
    };

    i64 b_; {
      while ((sum_ > 0)) {
        b_ = (sum_ = (sum_ - 1));
        continue11_:;
      }
      b_ = ((sum_ = (sum_ + 1)));
    } break10_:;

    {
      if ((b_ == 1)) {
        result_ = 1;
      } else {
        result_ = 0;
      }
    };
  };

  return result_;
}
