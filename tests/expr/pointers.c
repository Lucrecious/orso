#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int* p_int;



int expr(void) {
int result_; {
    int sum_ = int_(0);

    int x_ = int_(42);

    p_int xp_ = &(x_);

    {
      if (true) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    p_int xp1_ = &(x_);

    {
      if (true) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    int dx_ = *(xp_);

    {
      if (true) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      if ((dx_ == int_(42))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    (*(xp_) = int_(69));

    {
      if ((x_ == int_(69))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      if ((*(xp_) == int_(69))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
