#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int sum_ = int_(0);

    int count_ = int_(10);

    int odds_ = int_(0);

    int evens_ = int_(0);

    {
      while ((count_ > int_(0))) {
        {
          (count_ = subi32_(count_, int_(1)));

          int a_ = addi32_(divi32_(count_, int_(2)), int_(1));

          int b_ = divi32_((addi32_(count_, int_(1))), int_(2));

          {
            if ((a_ == b_)) {
              {
                (odds_ = addi32_(odds_, int_(1)));

                ;
                goto continue2_;
              };
            }
          };

          (evens_ = addi32_(evens_, int_(1)));
        };
        continue2_:;
      }
      {
        (count_ = int_(69));
      };
    } break1_:;

    {
      if ((evens_ == int_(5))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    {
      if ((odds_ == int_(5))) {
        (sum_ = addi32_(sum_, int_(1)));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
