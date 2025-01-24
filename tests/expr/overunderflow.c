#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int sum_ = int_(0);

    i8 min8_ = i8_(128);

    i8 max8_ = i8_(127);

    {
      i8 a_ = min8_;
;

      (a_ = subi8_(a_, i8_(1)));

      {
        if ((a_ == max8_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addi8_(a_, i8_(1)));

      {
        if ((a_ == min8_)) {
          (sum_ = addi32_(sum_, int_(1)));
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
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addu8_(a_, u8_(1)));

      {
        if ((a_ == minu8_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };
    };

    i16 min16_ = i16_(32768);

    i16 max16_ = i16_(32767);

    {
      i16 a_ = min16_;
;

      (a_ = subi16_(a_, i16_(1)));

      {
        if ((a_ == max16_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addi16_(a_, i16_(1)));

      {
        if ((a_ == min16_)) {
          (sum_ = addi32_(sum_, int_(1)));
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
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addu16_(a_, u16_(1)));

      {
        if ((a_ == minu16_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };
    };

    i32 min32_ = i32_(-2147483648);

    i32 max32_ = i32_(2147483647);

    {
      i32 a_ = min32_;
;

      (a_ = subi32_(a_, i32_(1)));

      {
        if ((a_ == max32_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addi32_(a_, i32_(1)));

      {
        if ((a_ == min32_)) {
          (sum_ = addi32_(sum_, int_(1)));
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
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addu32_(a_, u32_(1)));

      {
        if ((a_ == minu32_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };
    };

    i64 min64_ = INT64_MIN;

    i64 max64_ = 9223372036854775807ll;

    {
      i64 a_ = min64_;
;

      (a_ = subi64_(a_, 1ll));

      {
        if ((a_ == max64_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addi64_(a_, 1ll));

      {
        if ((a_ == min64_)) {
          (sum_ = addi32_(sum_, int_(1)));
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
          (sum_ = addi32_(sum_, int_(1)));
        }
      };

      (a_ = addu64_(a_, 1llu));

      {
        if ((a_ == minu64_)) {
          (sum_ = addi32_(sum_, int_(1)));
        }
      };
    };

    result_ = sum_;
;
  };

  return result_;
}
