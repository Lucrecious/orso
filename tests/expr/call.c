#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();
typedef int(*fn_int_int_int)(int,int);

int add1_fn_(int, int);
int sub2_fn_(int, int);

int add1_fn_(int a_, int b_) {
  {
    int tmp3 = adds32_(a_, b_);
    return tmp3;
  };
}

int sub2_fn_(int a_, int b_) {
  {
    int tmp4 = subs32_(a_, b_);
    return tmp4;
  };
}


int expr(void) {
int result_; {
    fn_int_int_int add_ = (add1_fn_);

    fn_int_int_int sub_ = (sub2_fn_);

    int sum_ = int_(0);

    int a_ = ((add1_fn_)(int_(1), int_(2)));

    {
      if ((a_ == int_(3))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    fn_int_int_int tmp5; {
      if ((a_ == int_(3))) {
        tmp5 = (add1_fn_);
      } else {
        tmp5 = (sub2_fn_);
      }
    };
    int b_ = tmp5(int_(3), int_(4));

    {
      if ((b_ == int_(7))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    fn_int_int_int tmp6 = (add1_fn_);
    int tmp7; {
      tmp7 = int_(5);
    };
    int c_ = tmp6(tmp7, int_(6));

    {
      if ((c_ == int_(11))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    fn_int_int_int tmp8 = (add1_fn_);
    int tmp9 = int_(7);
    int tmp10; {
      tmp10 = int_(8);
    };
    int d_ = tmp8(tmp9, tmp10);

    {
      if ((d_ == int_(15))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    fn_int_int_int tmp11 = (add1_fn_);
    int tmp12; {
      tmp12 = int_(9);
    };
    int tmp13; {
      tmp13 = int_(10);
    };
    int e_ = tmp11(tmp12, tmp13);

    {
      if ((e_ == int_(19))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    fn_int_int_int tmp14; {
      if ((e_ == int_(19))) {
        tmp14 = (add1_fn_);
      } else {
        tmp14 = (sub2_fn_);
      }
    };
    int tmp15; {
      tmp15 = int_(11);
    };
    int tmp16; {
      tmp16 = int_(12);
    };
    int f_ = tmp14(tmp15, tmp16);

    {
      if ((e_ == int_(23))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
