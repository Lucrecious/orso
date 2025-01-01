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
      if (1 < 2) {
        {
        }
      } else {
        {
        }
      }
    }

    {
      if (1 < 2) {
        {
          3;
        }
      } else {
        {
        }
      }
    }

    {
      if (1 < 2) {
        {
        }
      } else {
        {
          3;
        }
      }
    }

    {
      if (1 < 2) {
        {
          3;
        }
      } else {
        {
          4;
        }
      }
    }

    result_ = 1;
  }
  return result_;
}
