#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

s32 expr(void) {
s32 result_; {
    s32 sum_ = -1;

    s32 b_; {
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
