#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    {
      int x_ = int_(10);

      int y_ = int_(20);

      int z_ = int_(30);

      adds32_(adds32_(x_, y_), z_);
    };

    int a_; {
      int x_ = int_(10);

      int y_ = int_(20);

      int z_ = int_(30);

      a_ = adds32_(adds32_(x_, y_), z_);
    };

    ;

    {
      int a_; {
        {
          a_ = int_(5);
        };
      };

      a_;
    };

    int b_; {
      int a_; {
        {
          a_ = int_(5);
        };
      };

      b_ = a_;
;
    };

    int c_; {
      {
        {
          {
            {
              c_ = int_(10);
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
