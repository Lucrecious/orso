#include "intrinsics.h"
i64 expr(void) {
i64 result_; {
    i64 count_ = 10;

    i64 sum_ = 0;

    {
      until (count_ <= 0) {
        {
          (count_ = (count_ - 1));

          {
            if (count_ == 5) {
              ;
              goto blockend1_;
            }
          };
        };
      }
      ;
      blockend1_:
    };

    {
      if (count_ == 5) {
        (sum_ = (sum_ + 1));
      }
    };

    i64 tmp2; {
      while (count_ > 0) {
        {
          (count_ = (count_ - 1));

          tmp2 = ; {
            if (count_ == 3) {
              tmp2 = 100;
              goto blockend3_;
            }
          };
        };
      }
      tmp2 = 0;
      blockend3_:
    };
    (count_ = tmp2);

    {
      if (count_ == 100) {
        (sum_ = (sum_ + 1));
      }
    };

    (count_ = 10);

    i64 odds_ = 0;

    i64 evens_ = 0;

    {
      while (count_ > 0) {
        {
          (count_ = (count_ - 1));

          {
            if (((count_ / 2) + 1) == (((count_ + 1)) / 2)) {
              {
                (odds_ = (odds_ + 1));

                ;
                goto blockend4_;
              };
            }
          };

          (evens_ = (evens_ + 1));
        };
      }
      {
        (count_ = 69);
      };
      blockend4_:
    };

    {
      if (evens_ == 5) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      if (odds_ == 5) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      if (count_ == 69) {
        (sum_ = (sum_ + 1));
      }
    };

    (count_ = 10);

    i64 found2_ = 0;

    i64 found_others_ = 0;

    i64 tmp5; {
      while (count_ > 0) {
        {
          (count_ = (count_ - 1));

          {
            if (count_ == 2) {
              {
                (found2_ = 1);

                ;
                goto blockend6_;
              };
            }
          };

          tmp5 = ((found_others_ = (found_others_ + 1)));
        };
      }
      {
        tmp5 = 420;
      };
      blockend6_:
    };
    (count_ = tmp5);

    {
      if (found2_ == 1) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      if (found_others_ == 9) {
        (sum_ = (sum_ + 1));
      }
    };

    result_ = sum_;
;
  };

  return result_;
}
