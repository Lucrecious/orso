#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int sum_ = int_(0);

    int a_ = int_(3);

    {
      if ((a_ == int_(3))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    int tmp2; {
      tmp2 = int_(1);
    };
    int tmp3; {
      tmp3 = int_(2);
    };
    int tmp1 = addi32_(tmp2, tmp3);
    (a_ = tmp1);

    {
      if ((a_ == int_(3))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    int tmp5; {
      tmp5 = int_(1);
    };
    int tmp6 = int_(2);
    int tmp4 = addi32_(tmp5, tmp6);
    (a_ = tmp4);

    {
      if ((a_ == int_(3))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    int tmp8 = int_(1);
    int tmp9; {
      tmp9 = int_(2);
    };
    int tmp7 = addi32_(tmp8, tmp9);
    (a_ = tmp7);

    {
      if ((a_ == int_(3))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(7));

    {
      if ((a_ == int_(7))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(9));

    {
      if ((a_ == int_(9))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(1));

    {
      if ((a_ == int_(1))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(1));

    {
      if ((a_ == int_(1))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(1));

    a_;

    {
      if ((a_ == int_(1))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(4));

    {
      if ((a_ == int_(4))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(-4));

    {
      if ((a_ == int_(-4))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(4));

    {
      if ((a_ == int_(4))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(-4));

    {
      if ((a_ == int_(-4))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(6));

    {
      if ((a_ == int_(6))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    int tmp11 = a_;
;
    int tmp12; {
      tmp12 = int_(1);
    };
    int tmp10 = divi32_(tmp11, tmp12);
    (a_ = tmp10);

    (a_ = divi32_(a_, int_(0)));

    {
      if ((a_ == int_(0))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = int_(0));

    {
      if ((a_ == int_(0))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
