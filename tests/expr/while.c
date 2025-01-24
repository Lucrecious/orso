#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int x_ = int_(0);

    int n_ = int_(10);

    int sum_ = int_(0);

    {
      while ((x_ < n_)) {
        {
          (sum_ = addi32_(sum_, x_));

          int tmp4; {
            tmp4 = x_;
;
          };
          int tmp5 = int_(1);
          int tmp3 = addi32_(tmp4, tmp5);
          (x_ = tmp3);
        };
        continue2_:;
      }
      sum_;
    } break1_:;

    int count_ = int_(10);

    {
      while ((((count_ = subi32_(count_, int_(1)))) > int_(0))) {
        (sum_ = addi32_(sum_, int_(1)));
        continue7_:;
      }
      int_(0);
    } break6_:;

    (count_ = int_(10));

    int a_; {
      while ((((count_ = subi32_(count_, int_(1)))) > int_(0))) {
        a_ = (sum_ = addi32_(sum_, int_(1)));
        continue9_:;
      }
      a_ = int_(0);
    } break8_:;

    {
      if ((a_ == int_(0))) {
        (sum_ = subi32_(sum_, int_(65)));
      } else {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    int b_; {
      while ((sum_ > int_(0))) {
        b_ = (sum_ = subi32_(sum_, int_(1)));
        continue11_:;
      }
      b_ = ((sum_ = addi32_(sum_, int_(1))));
    } break10_:;

    {
      if ((b_ == int_(1))) {
        result_ = int_(1);
      } else {
        result_ = int_(0);
      }
    };
  };

  return result_;
}
