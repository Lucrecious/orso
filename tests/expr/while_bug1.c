#include "intrinsics.h"
i64 expr(void) {
i64 result_; {
    i64 sum_ = (0 - 1);

    i64 b_; {
      while (sum_ > 0) {
        b_ = (sum_ = (sum_ - 1));
      }
      b_ = ((sum_ = (sum_ + 1)));
    };

    result_ = b_;
;
  };

  return result_;
}
