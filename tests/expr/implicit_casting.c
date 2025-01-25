#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int sum_ = int_(0);

    {
      s8 a_ = s8_(-128);

      {
        if ((cast(int, a_) == int_(-128))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      s16 a_ = s16_(10000);

      {
        if ((cast(int, a_) == int_(10000))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      s32 a_ = s32_(100000);

      {
        if ((cast(int, a_) == int_(100000))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      s64 a_ = 1000000ll;

      {
        if ((a_ == 1000000ll)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      int a_ = int_(100000);

      {
        if ((a_ == int_(100000))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      u8 a_ = u8_(100);

      {
        if ((a_ == u8_(100))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      u16 a_ = u16_(10000);

      {
        if ((a_ == u16_(10000))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      u32 a_ = u32_(100000);

      {
        if ((a_ == u32_(100000))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      u64 a_ = 1000000llu;

      {
        if ((a_ == 1000000llu)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    {
      uint a_ = uint_(100000);

      {
        if ((a_ == uint_(100000))) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    result_ = sum_;
;
  };

  return result_;
}
