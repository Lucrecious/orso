#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int x_ = int_(10);

    {
      while (true) {
        bool_ tmp3; {
          (x_ = subi32_(x_, int_(1)));

          tmp3 = (x_ > int_(0));
        };
        unless (tmp3) break;
        {
          true;
        };
        continue2_:;
      }
      false;
    } break1_:;

    result_ = x_;
;
  };

  return result_;
}
