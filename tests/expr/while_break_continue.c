#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int count_ = int_(10);

    int sum_ = int_(0);

    {
      until ((count_ <= int_(0))) {
        {
          (count_ = subs32_(count_, int_(1)));

          {
            if ((count_ == int_(5))) {
              ;
              goto break1_;
            }
          };

        };
        continue2_:;
      }
      int_(0);
    } break1_:;

    {
      if ((count_ == int_(5))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    int tmp3; {
      while ((count_ > int_(0))) {
        {
          tmp3 = ((count_ = subs32_(count_, int_(1))));

          {
            if ((count_ == int_(3))) {
              tmp3 = int_(100);
              goto break4_;
            }
          };

        };
        continue5_:;
      }
      tmp3 = int_(0);
    } break4_:;
    (count_ = tmp3);

    {
      if ((count_ == int_(100))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    (count_ = int_(10));

    int odds_ = int_(0);

    int evens_ = int_(0);

    {
      while ((count_ > int_(0))) {
        {
          (count_ = subs32_(count_, int_(1)));

          {
            if ((adds32_(divs32_(count_, int_(2)), int_(1)) == divs32_((adds32_(count_, int_(1))), int_(2)))) {
              {
                (odds_ = adds32_(odds_, int_(1)));

                ;
                goto continue7_;
              };
            }
          };

          (evens_ = adds32_(evens_, int_(1)));
        };
        continue7_:;
      }
      {
        (count_ = int_(69));
      };
    } break6_:;

    {
      if ((evens_ == int_(5))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      if ((odds_ == int_(5))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      if ((count_ == int_(69))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    (count_ = int_(10));

    int found2_ = int_(0);

    int found_others_ = int_(0);

    int tmp8; {
      while ((count_ > int_(0))) {
        {
          (count_ = subs32_(count_, int_(1)));

          {
            if ((count_ == int_(2))) {
              {
                (found2_ = int_(1));

                ;
                goto continue10_;
              };
            }
          };

          tmp8 = ((found_others_ = adds32_(found_others_, int_(1))));
        };
        continue10_:;
      }
      {
        tmp8 = int_(420);
      };
    } break9_:;
    (count_ = tmp8);

    {
      if ((found2_ == int_(1))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      if ((found_others_ == int_(9))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
