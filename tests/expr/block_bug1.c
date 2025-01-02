#include "intrinsics.h"
i64 expr(void) {
  i64 result_; {
    i64 b_; {
      i64 a_; {
        {
          a_ = 5;
        }
      }

      b_ = a_;
    }

    result_ = b_;
  }
  return result_;
}
