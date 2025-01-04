#include "intrinsics.h"
i64 expr(void) {
i64 result_; {
    i64 sum_ = 0;

    i64 count_ = 5;

    i64 tmp1; {
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
