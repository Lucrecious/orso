#include "intrinsics.h"
i64 expr(void) {
i64 result_; {
    i64 sum_ = 0;

    i64 a_ = 0;

    {
      do {
        (a_ = 1);
      } while(false); 
      (a_ = 2);
    } blockend1_:;

    {
      if (a_ == 2) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      do {
        {
          (a_ = 1);
        };
      } while(false); 
      {
        (a_ = 2);
      };
    } blockend2_:;

    {
      if (a_ == 2) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      do {
        {
          (a_ = 1);

          ;
          goto blockend3_;
        };
      } while(false); 
      {
        (a_ = 2);
      };
    } blockend3_:;

    {
      if (a_ == 1) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      do {
        {
          (a_ = 1);

          break;
        };
      } while(false); 
      {
        (a_ = 3);
      };
    } blockend4_:;

    {
      if (a_ == 3) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      do {
        {
          (a_ = 2);

          break;
        };
      } while(false); 
      {
        do {
          {
            (a_ = 3);
          };
        } while(false); 
        {
          (a_ = 4);
        };
      } blockend6_:;
    } blockend5_:;

    {
      if (a_ == 4) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      do {
        {
          (a_ = 2);

          break;
        };
      } while(false); 
      {
        do {
          {
            (a_ = 3);

            ;
            goto blockend8_;
          };
        } while(false); 
        {
          (a_ = 4);
        };
      } blockend8_:;
    } blockend7_:;

    {
      if (a_ == 3) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      do {
        {
          while (a_ > 0) {
            {
              {
                if (a_ == 3) {
                  ;
                  goto blockend9_;
                }
              }
              (a_ = (a_ - 1));
            };
          }
          0;
        } blockend10_:;
      } while(false); 
      (a_ = 0);
    } blockend9_:;

    {
      if (a_ == 3) {
        (sum_ = (sum_ + 1));
      }
    };

    {
      do {
        {
          if (a_ == 3) {
            {
              {
                while (a_ > 0) {
                  {
                    {
                      if (a_ == 3) {
                        break;
                      }
                    }
                    (a_ = (a_ - 1));
                  };
                }
                {
                  (a_ = 10);
                };
              } blockend12_:;
            };
          }
        };
      } while(false); 
      {
        (a_ = 4);
      };
    } blockend11_:;

    {
      if (a_ == 4) {
        (sum_ = (sum_ + 1));
      }
    };

    i64 answer_; {
      do {
        {
          {
            do {
              {
                5;
                goto blockend14_;
              };
            } while(false); 
            0;
          } blockend14_:;

          answer_ = 10;
        };
      } while(false); 
      answer_ = 0;
    } blockend13_:;

    {
      if (answer_ == 0) {
        (sum_ = (sum_ + 1));
      }
    };

    i64 tmp15; {
      do {
        {
          ; {
            if (a_ == 4) {
              {
                tmp15 = (sum_ + 1);
                goto blockend16_;
              };
            }
          }        }      } while(false); 
      tmp15 = 0;
    } blockend16_:;
    (sum_ = tmp15);

    result_ = sum_;
;
  };

  return result_;
}
