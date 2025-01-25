#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

s32 expr(void) {
s32 result_; {
    s32 sum_ = 0;

    s32 count_ = 5;

    s32 tmp1; {
      while ((count_ > 0)) {
        {
          tmp1 = ((count_ = (count_ - 1)));

          {
            if ((count_ == 3)) {
              tmp1 = 100;
              goto break2_;
            }
          };

        };
        continue3_:;
      }
      tmp1 = 0;
    } break2_:;
    (count_ = tmp1);

    {
      if ((count_ == 100)) {
        (sum_ = (sum_ + 1));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
