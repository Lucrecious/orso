#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int sum_ = int_(0);

    {
      i8 a_ = i8_(-128);

      {
        if ((cast(int, a_) == int_(-128))) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };
    };

    {
      i16 a_ = i16_(10000);

      result_ = int_(0);
; {
        if ((cast(int, a_) == int_(10000))) {
          result_ = (sum_ = addi32_(sum_, int_(1)));
        }
      };
    };
  };

  return result_;
}
