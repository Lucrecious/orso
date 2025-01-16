#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    i32 b_; {
      i32 a_; {
        {
          a_ = 5;
        };
      };

      b_ = a_;
;
    };

    result_ = b_;
;
  };

  return result_;
}
