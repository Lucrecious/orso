#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

i32 expr(void) {
i32 result_; {
    i32 count_ = 10;

    i32 sum_ = 0;

    {
      until ((count_ <= 0)) {
        {
          (count_ = (count_ - 1));

          {
            if ((count_ == 5)) {
              ;
              goto break1_;
            }
          };

        };
        continue2_:;
      }
      0;
    } break1_:;

    {
      if ((count_ == 5)) {
        (sum_ = (sum_ + 1));
      }
    };

    i32 tmp3; {
      while ((count_ > 0)) {
        {
          tmp3 = ((count_ = (count_ - 1)));

          {
            if ((count_ == 3)) {
              tmp3 = 100;
              goto break4_;
            }
          };

        };
        continue5_:;
      }
      tmp3 = 0;
    } break4_:;
    (count_ = tmp3);

    {
      if ((count_ == 100)) {
        (sum_ = (sum_ + 1));
      }
    };

    (count_ = 10);

    i32 odds_ = 0;

    i32 evens_ = 0;

    {
      while ((count_ > 0)) {
        {
          (count_ = (count_ - 1));

          {
            if (((div_(count_, 2) + 1) == div_(((count_ + 1)), 2))) {
              {
                (odds_ = (odds_ + 1));

                ;
                goto continue7_;
              };
            }
          };

          (evens_ = (evens_ + 1));
        };
        continue7_:;
      }
      {
        (count_ = 69);
      };
    } break6_:;

    {
      if ((evens_ == 5)) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      if ((odds_ == 5)) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      if ((count_ == 69)) {
        (sum_ = (sum_ + 1));
      }
    };

    (count_ = 10);

    i32 found2_ = 0;

    i32 found_others_ = 0;

    i32 tmp8; {
      while ((count_ > 0)) {
        {
          (count_ = (count_ - 1));

          {
            if ((count_ == 2)) {
              {
                (found2_ = 1);

                ;
                goto continue10_;
              };
            }
          };

          tmp8 = ((found_others_ = (found_others_ + 1)));
        };
        continue10_:;
      }
      {
        tmp8 = 420;
      };
    } break9_:;
    (count_ = tmp8);

    {
      if ((found2_ == 1)) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      if ((found_others_ == 9)) {
        (sum_ = (sum_ + 1));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
