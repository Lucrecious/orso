#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    bool_ a_ = true;

    bool_ b_ = false;

    int sum_ = int_(0);

    bool_ ab_ = (a_ && b_);

    {
      if (!(ab_)) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = true);

    (b_ = false);

    (ab_ = (false && ((b_ = true))));

    {
      if (!(b_)) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = true);

    (b_ = false);

    (ab_ = (a_ || b_));

    {
      if (ab_) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = false);

    (b_ = true);

    (ab_ = (a_ || b_));

    {
      if (ab_) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = false);

    (b_ = false);

    (ab_ = (a_ || b_));

    {
      if (!(ab_)) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (a_ = true);

    (b_ = true);

    (ab_ = (a_ || b_));

    {
      if (ab_) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (b_ = false);

    bool_ tmp2 = false;
    bool_ tmp3 = false;
;
    tmp2 = true;
    unless (tmp2) {
      tmp3 = ((b_ = true));
    };
    bool_ tmp1 = tmp2 || tmp3;
    (ab_ = tmp1);

    {
      if (!(b_)) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    (b_ = false);

    bool_ tmp5 = false;
    bool_ tmp6 = false;
;
    {
      bool_ x_ = false;

      bool_ y_ = true;

      tmp5 = (x_ || y_);
    };
    if (tmp5) tmp6 = ((b_ = true));
    bool_ tmp4 = tmp5 && tmp6;
    (ab_ = tmp4);

    {
      if (!(b_)) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
