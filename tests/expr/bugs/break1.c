#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

s32 expr(void) {
s32 result_; {
    s32 x_ = 2;

    {
      while ((x_ > 0)) {
        {
          s32 t_ = 5;

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

    s32 y_ = 10;

    result_ = y_;
;
  };

  return result_;
}
