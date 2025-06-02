#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

ors32 expr(void) {
ors32 result_; {
    ors32 b_; {
      ors32 a_; {
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
