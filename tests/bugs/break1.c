#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

ors32 expr(void) {
ors32 result_; {
    ors32 x_ = 2;

    {
      while ((x_ > 0)) {
        {
          ors32 t_ = 5;

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

    ors32 y_ = 10;

    result_ = y_;
;
  };

  return result_;
}
