#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int* p_int;
typedef void(*fn_p_int_void)(p_int);

void mutate1_fn_(p_int);

void mutate1_fn_(p_int n_) {
  {
    (*(n_) = int_(42));

    return;
  };
}


int expr(void) {
int result_; {
    fn_p_int_void mutate_ = (mutate1_fn_);

    int x_ = int_(0);

    int y_ = int_(0);

    p_int tmp2; {
      p_int zp_ = &(x_);

      {
        if (true) {
          (zp_ = &(y_));
        }
      };

      tmp2 = zp_;
;
    };
;
    int tmp3 = int_(10);
;
    *tmp2 = tmp3;

    result_ = y_;
;
  };

  return result_;
}
