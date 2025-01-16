#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    i32 x_ = 2;

    {
      while ((x_ > 0)) {
        {
          i32 t_ = 5;

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

    i32 y_ = 10;

    result_ = y_;
;
  };

  return result_;
}
