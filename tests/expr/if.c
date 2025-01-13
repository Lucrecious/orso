#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i64 expr(void) {
i64 result_; {
    {
      if (true) {
        ;
      }
    };

    {
      if (true) {
        ;
      }
    };

    {
      if (true) {
        {
          3;
        };
      }
    };

    {
      if (true) {
        0;
      } else {
        {
          3;
        };
      }
    };

    {
      if (true) {
        {
          3;
        };
      } else {
        {
          4;
        };
      }
    };

    {
      bool_ tmp1; {
        tmp1 = true;
      };
      if (tmp1) {
        ;
      }
    };

    {
      bool_ tmp2; {
        tmp2 = true;
      };
      if (tmp2) {
        ;
      }
    };

    {
      bool_ tmp3; {
        tmp3 = true;
      };
      if (tmp3) {
        {
          3;
        };
      }
    };

    {
      bool_ tmp4; {
        tmp4 = true;
      };
      if (tmp4) {
        {
          3;
        };
      } else {
        {
          4;
        };
      }
    };

    {
      if (true) {
        ;
      }
    };

    {
      if (true) {
        ;
      }
    };

    i64 c_ = 0;
; {
      if (true) {
        {
          c_ = 3;
        };
      }
    };

    i64 d_; {
      if (true) {
        d_ = 0;
      } else {
        {
          d_ = 3;
        };
      }
    };

    i64 e_; {
      if (true) {
        {
          e_ = 3;
        };
      } else {
        {
          e_ = 4;
        };
      }
    };

    {
      bool_ tmp5; {
        tmp5 = true;
      };
      if (tmp5) {
        ;
      }
    };

    {
      bool_ tmp6; {
        tmp6 = true;
      };
      if (tmp6) {
        ;
      }
    };

    i64 h_ = 0;
; {
      bool_ tmp7; {
        tmp7 = true;
      };
      if (tmp7) {
        {
          h_ = 3;
        };
      }
    };

    i64 i_; {
      bool_ tmp8; {
        tmp8 = true;
      };
      if (tmp8) {
        {
          i_ = 3;
        };
      } else {
        {
          i_ = 4;
        };
      }
    };

    result_ = 1;
  };

  return result_;
}
