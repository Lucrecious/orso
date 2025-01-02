#include "intrinsics.h"
i64 expr(void) {
  i64 result_; {
    {
      if (1 < 2) {
        ;
      }
    }

    {
      if (1 < 2) {
        ;
      }
    }

    {
      if (1 < 2) {
        {
          3;
        }
      }
    }

    {
      if (1 < 2) {
        0;
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

    {
      bool_ tmp1; {
        tmp1 = (1 < 2);
      }
      if (tmp1) {
        ;
      }
    }

    {
      bool_ tmp2; {
        tmp2 = (1 < 2);
      }
      if (tmp2) {
        ;
      }
    }

    {
      bool_ tmp3; {
        tmp3 = (1 < 2);
      }
      if (tmp3) {
        {
          3;
        }
      }
    }

    {
      bool_ tmp4; {
        tmp4 = (1 < 2);
      }
      if (tmp4) {
        {
          3;
        }
      } else {
        {
          4;
        }
      }
    }

    {
      if (1 < 2) {
        ;
      }
    }

    {
      if (1 < 2) {
        ;
      }
    }

    i64 c_ = 0; {
      if (1 < 2) {
        {
          c_ = 3;
        }
      }
    }

    i64 d_; {
      if (1 < 2) {
        d_ = 0;
      } else {
        {
          d_ = 3;
        }
      }
    }

    i64 e_; {
      if (1 < 2) {
        {
          e_ = 3;
        }
      } else {
        {
          e_ = 4;
        }
      }
    }

    {
      bool_ tmp5; {
        tmp5 = (1 < 2);
      }
      if (tmp5) {
        ;
      }
    }

    {
      bool_ tmp6; {
        tmp6 = (1 < 2);
      }
      if (tmp6) {
        ;
      }
    }

    i64 h_ = 0; {
      bool_ tmp7; {
        tmp7 = (1 < 2);
      }
      if (tmp7) {
        {
          h_ = 3;
        }
      }
    }

    i64 i_; {
      bool_ tmp8; {
        tmp8 = (1 < 2);
      }
      if (tmp8) {
        {
          i_ = 3;
        }
      } else {
        {
          i_ = 4;
        }
      }
    }

    result_ = 1;
  }
  return result_;
}
