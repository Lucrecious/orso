#include "intrinsics.h"
i64 expr(void) {
  i64 result_; {
    (1 + 2);

    i64 tmp1; {
      tmp1 = 1;
    }
    i64 tmp2; {
      tmp2 = 2;
    }
    tmp1 + tmp2;

    i64 tmp3; {
      tmp3 = 1;
    }
    i64 tmp4 = 2;
    tmp3 + tmp4;

    i64 tmp5 = 1;
    i64 tmp6; {
      tmp6 = 2;
    }
    tmp5 + tmp6;

    (1 + (2 * 3));

    result_ = (((1 + 2)) * 3);
  }
  return result_;
}
