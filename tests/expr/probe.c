#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int(*fn_int_int)(int);

int foo1_fn_(int);
int tar2_fn_(int);
int bar3_fn_(int);

int foo1_fn_(int n_) {
  {
    {
      if ((n_ > int_(0))) {
        int tmp4 = int_(-2);
        return tmp4;
      }
    };

    int tmp5 = n_;
;
    return tmp5;
  };
}

int tar2_fn_(int n_) {
  {
    int tmp6 = ((bar3_fn_)(subs32_(n_, int_(1))));
    return tmp6;
  };
}

int bar3_fn_(int n_) {
  {
    {
      if ((n_ > int_(0))) {
        int tmp7 = ((foo1_fn_)(int_(-1)));
        return tmp7;
      }
    };

    int tmp8 = n_;
;
    return tmp8;
  };
}


int expr(void) {
int result_; {
    fn_int_int foo_ = (foo1_fn_);

    fn_int_int tar_ = (tar2_fn_);

    fn_int_int bar_ = (bar3_fn_);

    result_ = ((foo1_fn_)(int_(10)));
  };

  return result_;
}
