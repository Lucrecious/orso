#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
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
          int_(3);
        };
      }
    };

    {
      if (true) {
        int_(0);
      } else {
        {
          int_(3);
        };
      }
    };

    {
      if (true) {
        {
          int_(3);
        };
      } else {
        {
          int_(4);
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
          int_(3);
        };
      }
    };

    {
      bool_ tmp4; {
        tmp4 = true;
      };
      if (tmp4) {
        {
          int_(3);
        };
      } else {
        {
          int_(4);
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

    int c_ = int_(0);
; {
      if (true) {
        {
          c_ = int_(3);
        };
      }
    };

    int d_; {
      if (true) {
        d_ = int_(0);
      } else {
        {
          d_ = int_(3);
        };
      }
    };

    int e_; {
      if (true) {
        {
          e_ = int_(3);
        };
      } else {
        {
          e_ = int_(4);
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

    int h_ = int_(0);
; {
      bool_ tmp7; {
        tmp7 = true;
      };
      if (tmp7) {
        {
          h_ = int_(3);
        };
      }
    };

    int i_; {
      bool_ tmp8; {
        tmp8 = true;
      };
      if (tmp8) {
        {
          i_ = int_(3);
        };
      } else {
        {
          i_ = int_(4);
        };
      }
    };

    result_ = int_(1);
  };

  return result_;
}
