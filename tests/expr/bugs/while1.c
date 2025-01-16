#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    i32 sum_ = -1;

    i32 b_; {
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
