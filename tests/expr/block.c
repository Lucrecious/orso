#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    {
      i32 x_ = 10;

      i32 y_ = 20;

      i32 z_ = 30;

      ((x_ + y_) + z_);
    };

    i32 a_; {
      i32 x_ = 10;

      i32 y_ = 20;

      i32 z_ = 30;

      a_ = ((x_ + y_) + z_);
    };

    ;

    {
      i32 a_; {
        {
          a_ = 5;
        };
      };

      a_;
    };

    i32 b_; {
      i32 a_; {
        {
          a_ = 5;
        };
      };

      b_ = a_;
;
    };

    i32 c_; {
      {
        {
          {
            {
              c_ = 10;
            };
          };
        };
      };
    };

    result_ = c_;
;
  };

  return result_;
}
