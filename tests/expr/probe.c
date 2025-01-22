#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int(*fn_int_int_int)(int,int);

int add1_fn_(int, int);

int add1_fn_(int a_, int b_) {
  {
    int tmp2 = addi32_(a_, b_);
    return tmp2;
  };
}


int expr(void) {
int result_; {
    fn_int_int_int add_ = (add1_fn_);

    fn_int_int_int tmp3 = (add1_fn_);
    int tmp4; {
      tmp4 = int_(5);
    };
    result_ = tmp3(tmp4, int_(6));
  };

  return result_;
}
