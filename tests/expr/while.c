#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    i32 x_ = 0;

    i32 n_ = 10;

    i32 sum_ = 0;

    {
      while ((x_ < n_)) {
        {
          (sum_ = (sum_ + x_));

          i32 tmp4; {
            tmp4 = x_;
;
          };
          i32 tmp5 = 1;
          i32 tmp3 = tmp4 + tmp5;
          (x_ = tmp3);
        };
        continue2_:;
      }
      sum_;
    } break1_:;

    i32 count_ = 10;

    {
      while ((((count_ = (count_ - 1))) > 0)) {
        (sum_ = (sum_ + 1));
        continue7_:;
      }
      0;
    } break6_:;

    (count_ = 10);

    i32 a_; {
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

    i32 b_; {
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
