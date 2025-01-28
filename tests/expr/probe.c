#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int(*fn_int_int_int)(int,int);

int add1_fn_(int, int);

int add1_fn_(int a_, int b_) {
  {
    int tmp2 = adds32_(a_, b_);
    return tmp2;
  };
}


int expr(void) {
int result_; {
    fn_int_int_int add_ = (add1_fn_);

    result_ = int_(69);
  };

  return result_;
}
