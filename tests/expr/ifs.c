#include "intrinsics.h"
i64 expr(void) {
  i64 result_; {
    {
      if (1 < 2) {
        {
        }
      }
    }

    {
      if (3 < 4) {
        {
          5;
        }
      } else {
        {
          6;
        }
      }
    }

    i64 x_ = 0; {
      if (7 < 8) {
        {
          x_ = 9;
        }
      }
    }

    i64 y_; {
      if (10 < 11) {
        {
          y_ = 12;
        }
      } else {
        {
          y_ = 13;
        }
      }
    }

    result_ = y_;
  }
  return result_;
}
