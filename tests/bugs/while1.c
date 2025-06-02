#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

ors32 expr(void) {
ors32 result_; {
    ors32 sum_ = -1;

    ors32 b_; {
      while ((sum_ > 0)) {
        b_ = (sum_ = (sum_ - 1));
        continue2_:;
      }
      b_ = ((sum_ = (sum_ + 1)));
    } break1_:;

    result_ = b_;
;
  };

  return result_;
}
