#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int sum_ = int_(0);

    s8 min8_ = s8_(128);

    s8 max8_ = s8_(127);

    {
      s8 a_ = min8_;
;

      (a_ = subs8_(a_, s8_(1)));

      {
        if ((a_ == max8_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = adds8_(a_, s8_(1)));

      {
        if ((a_ == min8_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    u8 minu8_ = u8_(0);

    u8 maxu8_ = u8_(255);

    {
      u8 a_ = minu8_;
;

      (a_ = subu8_(a_, u8_(1)));

      {
        if ((a_ == maxu8_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = addu8_(a_, u8_(1)));

      {
        if ((a_ == minu8_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    s16 min16_ = s16_(32768);

    s16 max16_ = s16_(32767);

    {
      s16 a_ = min16_;
;

      (a_ = subs16_(a_, s16_(1)));

      {
        if ((a_ == max16_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = adds16_(a_, s16_(1)));

      {
        if ((a_ == min16_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    u16 minu16_ = u16_(0);

    u16 maxu16_ = u16_(65535);

    {
      u16 a_ = minu16_;
;

      (a_ = subu16_(a_, u16_(1)));

      {
        if ((a_ == maxu16_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = addu16_(a_, u16_(1)));

      {
        if ((a_ == minu16_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    s32 min32_ = s32_(-2147483648);

    s32 max32_ = s32_(2147483647);

    {
      s32 a_ = min32_;
;

      (a_ = subs32_(a_, s32_(1)));

      {
        if ((a_ == max32_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = adds32_(a_, s32_(1)));

      {
        if ((a_ == min32_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    u32 minu32_ = u32_(0);

    u32 maxu32_ = u32_(4294967295);

    {
      u32 a_ = minu32_;
;

      (a_ = subu32_(a_, u32_(1)));

      {
        if ((a_ == maxu32_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = addu32_(a_, u32_(1)));

      {
        if ((a_ == minu32_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    s64 min64_ = INT64_MIN;

    s64 max64_ = 9223372036854775807ll;

    {
      s64 a_ = min64_;
;

      (a_ = subs64_(a_, 1ll));

      {
        if ((a_ == max64_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = adds64_(a_, 1ll));

      {
        if ((a_ == min64_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    u64 minu64_ = 0llu;

    u64 maxu64_ = 18446744073709551615llu;

    {
      u64 a_ = minu64_;
;

      (a_ = subu64_(a_, 1llu));

      {
        if ((a_ == maxu64_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };

      (a_ = addu64_(a_, 1llu));

      {
        if ((a_ == minu64_)) {
          (sum_ = adds32_(sum_, int_(1)));
        }
      };
    };

    result_ = sum_;
;
  };

  return result_;
}
