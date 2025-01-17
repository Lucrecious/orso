#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    i32 sum_ = 0;

    i32 a_ = 3;

    {
      if ((a_ == 3)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    i32 tmp2; {
      tmp2 = 1;
    };
    i32 tmp3; {
      tmp3 = 2;
    };
    i32 tmp1 = addi32_(tmp2, tmp3);
    (a_ = tmp1);

    {
      if ((a_ == 3)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    i32 tmp5; {
      tmp5 = 1;
    };
    i32 tmp6 = 2;
    i32 tmp4 = addi32_(tmp5, tmp6);
    (a_ = tmp4);

    {
      if ((a_ == 3)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    i32 tmp8 = 1;
    i32 tmp9; {
      tmp9 = 2;
    };
    i32 tmp7 = addi32_(tmp8, tmp9);
    (a_ = tmp7);

    {
      if ((a_ == 3)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 7);

    {
      if ((a_ == 7)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 9);

    {
      if ((a_ == 9)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 1);

    {
      if ((a_ == 1)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 1);

    {
      if ((a_ == 1)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 1);

    a_;

    {
      if ((a_ == 1)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 4);

    {
      if ((a_ == 4)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = -4);

    {
      if ((a_ == -4)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 4);

    {
      if ((a_ == 4)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = -4);

    {
      if ((a_ == -4)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 6);

    {
      if ((a_ == 6)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    i32 tmp11 = a_;
;
    i32 tmp12; {
      tmp12 = 1;
    };
    i32 tmp10 = divi32_(tmp11, tmp12);
    (a_ = tmp10);

    (a_ = divi32_(a_, 0));

    {
      if ((a_ == 0)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    (a_ = 0);

    {
      if ((a_ == 0)) {
        (sum_ = addi32_(sum_, 1));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
