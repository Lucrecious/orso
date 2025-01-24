#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int check_ = int_(0);

    u8 a1_ = u8_(100);

    u8 a2_ = u8_(0);

    type_t t_ = typeid(10);

    {
      if (typeid_eq(typeid(10), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(10), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    i8 b1_ = i8_(100);

    i8 b2_ = i8_(0);

    (t_ = typeid(9));

    {
      if (typeid_eq(typeid(9), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(9), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    u16 c1_ = u16_(10000);

    u16 c2_ = u16_(0);

    (t_ = typeid(12));

    {
      if (typeid_eq(typeid(12), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(12), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    i16 d1_ = i16_(10000);

    i16 d2_ = i16_(0);

    (t_ = typeid(11));

    {
      if (typeid_eq(typeid(11), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(11), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    u32 e1_ = u32_(100000);

    u32 e2_ = u32_(0);

    (t_ = typeid(14));

    {
      if (typeid_eq(typeid(14), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(14), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    i32 f1_ = i32_(100000);

    i32 f2_ = i32_(0);

    (t_ = typeid(13));

    {
      if (typeid_eq(typeid(13), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(13), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    u64 g1_ = 100000000;

    u64 g2_ = 0;

    (t_ = typeid(16));

    {
      if (typeid_eq(typeid(16), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(16), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    i64 h1_ = 100000000;

    i64 h2_ = 0;

    (t_ = typeid(15));

    {
      if (typeid_eq(typeid(15), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(15), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    uint j1_ = uint_(100);

    uint j2_ = uint_(0);

    (t_ = typeid(18));

    {
      if (typeid_eq(typeid(18), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(18), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    int i1_ = int_(100);

    int i2_ = int_(0);

    int i3_ = int_(420);

    (t_ = typeid(17));

    {
      if (typeid_eq(typeid(17), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(17), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(17), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    size_t s1_ = 100;

    size_t s2_ = 0;

    (t_ = typeid(19));

    {
      if (typeid_eq(typeid(19), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    {
      if (typeid_eq(typeid(19), t_)) {
        (check_ = addi32_(check_, int_(1)));
      }
    };

    result_ = check_;
;
  };

  return result_;
}
