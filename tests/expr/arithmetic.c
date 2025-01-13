#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i64 expr(void) {
i64 result_; {
    i64 sum_ = 0;

    i64 a_ = 3;

    {
      if ((a_ == 3)) {
        (sum_ = (sum_ + 1));
      }
    };

    i64 tmp2; {
      tmp2 = 1;
    };
    i64 tmp3; {
      tmp3 = 2;
    };
    i64 tmp1 = tmp2 + tmp3;
    (a_ = tmp1);

    {
      if ((a_ == 3)) {
        (sum_ = (sum_ + 1));
      }
    };

    i64 tmp5; {
      tmp5 = 1;
    };
    i64 tmp6 = 2;
    i64 tmp4 = tmp5 + tmp6;
    (a_ = tmp4);

    {
      if ((a_ == 3)) {
        (sum_ = (sum_ + 1));
      }
    };

    i64 tmp8 = 1;
    i64 tmp9; {
      tmp9 = 2;
    };
    i64 tmp7 = tmp8 + tmp9;
    (a_ = tmp7);

    {
      if ((a_ == 3)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 7);

    {
      if ((a_ == 7)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 9);

    {
      if ((a_ == 9)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 1);

    {
      if ((a_ == 1)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 1);

    {
      if ((a_ == 1)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 1);

    a_;

    {
      if ((a_ == 1)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 4);

    {
      if ((a_ == 4)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = -4);

    {
      if ((a_ == -4)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 4);

    {
      if ((a_ == 4)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = -4);

    {
      if ((a_ == -4)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 6);

    {
      if ((a_ == 6)) {
        (sum_ = (sum_ + 1));
      }
    };

    i64 tmp11 = a_;
;
    i64 tmp12; {
      tmp12 = 1;
    };
    i64 tmp10 = div_(tmp11, tmp12);
    (a_ = tmp10);

    (a_ = div_(a_, 0));

    {
      if ((a_ == 0)) {
        (sum_ = (sum_ + 1));
      }
    };

    (a_ = 0);

    {
      if ((a_ == 0)) {
        (sum_ = (sum_ + 1));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
