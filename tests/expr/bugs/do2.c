#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i64 expr(void) {
i64 result_; {
    i64 inner_before_ = 0;

    i64 inner_after_ = 0;

    {
      do {
        {
          while (true) {
            {
              {
                while (true) {
                  {
                    (inner_before_ = 1);

                    ;
                    goto continue2_;
                  };
                  continue6_:;
                }
                ;
              } break5_:;

              (inner_after_ = 1);

              ;
              goto break3_;
            };
            continue4_:;
          }
          ;
        } break3_:;
      continue2_:;
      } while(false); 
      ;
    } break1_:;

    result_ = (inner_after_ + inner_before_);
  };

  return result_;
}
