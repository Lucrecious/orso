#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i64 expr(void) {
i64 result_; {
    i64 x_ = 2;

    {
      while ((x_ > 0)) {
        {
          i64 t_ = 5;

          (x_ = (x_ - 1));

          {
            if ((x_ == 1)) {
              ;
              goto break1_;
            }
          };

        };
        continue2_:;
      }
      0;
    } break1_:;

    i64 y_ = 10;

    result_ = y_;
;
  };

  return result_;
}
