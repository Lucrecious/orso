#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int(*fn_int_int_int)(int,int);
typedef int(*fn_int_int_int)(int,int);
typedef void(*fn_void)();

int add1_fn_(int, int);
int sub2_fn_(int, int);
void foo3_fn_();

int add1_fn_(int a_, int b_) {
  {
    int tmp4 = addi32_(a_, b_);
    return tmp4;
  };
}

int sub2_fn_(int a_, int b_) {
  {
    int tmp5 = subi32_(a_, b_);
    return tmp5;
  };
}

void foo3_fn_() {
  {
    return;
  };
}


int expr(void) {
int result_; {
    fn_int_int_int add_ = (add1_fn_);

    fn_int_int_int sub_ = (sub2_fn_);

    fn_void foo_ = (foo3_fn_);

    fn_int_int_int sum_ = (add1_fn_);

    fn_int_int_int sans_ = (sub2_fn_);

    (add1_fn_);

    (sub2_fn_);

    ;

    result_ = result_;
;
  };

  return result_;
}
