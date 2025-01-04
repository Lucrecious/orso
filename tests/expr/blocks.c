#include "intrinsics.h"
i64 expr(void) {
i64 result_; {
    {
      i64 x_ = 10;

      i64 y_ = 20;

      i64 z_ = 30;

      ((x_ + y_) + z_);
    };

    i64 a_; {
      i64 x_ = 10;

      i64 y_ = 20;

      i64 z_ = 30;

      a_ = ((x_ + y_) + z_);
    };

    
    {
      i64 a_; {
        {
          a_ = 5;
        };
      };

      a_;
    };

    i64 b_; {
      i64 a_; {
        {
          a_ = 5;
        };
      };

      b_ = a_;
;
    };

    i64 c_; {
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
