#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"

typedef void(*fn_void)();



int expr(void) {
int result_; {
    int sum_ = int_(0);

    int a_ = int_(0);

    {
      do {
        (a_ = int_(1));
      continue2_:;
      } while(false); 
      (a_ = int_(2));
    } break1_:;

    {
      if ((a_ == int_(2))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      do {
        {
          (a_ = int_(1));
        };
      continue4_:;
      } while(false); 
      {
        (a_ = int_(2));
      };
    } break3_:;

    {
      if ((a_ == int_(2))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      do {
        {
          (a_ = int_(1));

          ;
          goto break5_;
        };
      continue6_:;
      } while(false); 
      {
        (a_ = int_(2));
      };
    } break5_:;

    {
      if ((a_ == int_(1))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      do {
        {
          (a_ = int_(1));

          ;
          goto continue8_;
        };
      continue8_:;
      } while(false); 
      {
        (a_ = int_(3));
      };
    } break7_:;

    {
      if ((a_ == int_(3))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      do {
        {
          (a_ = int_(2));

          ;
          goto continue10_;
        };
      continue10_:;
      } while(false); 
      {
        do {
          {
            (a_ = int_(3));
          };
        continue12_:;
        } while(false); 
        {
          (a_ = int_(4));
        };
      } break11_:;
    } break9_:;

    {
      if ((a_ == int_(4))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      do {
        {
          (a_ = int_(2));

          ;
          goto continue14_;
        };
      continue14_:;
      } while(false); 
      {
        do {
          {
            (a_ = int_(3));

            ;
            goto break15_;
          };
        continue16_:;
        } while(false); 
        {
          (a_ = int_(4));
        };
      } break15_:;
    } break13_:;

    {
      if ((a_ == int_(3))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      do {
        {
          while ((a_ > int_(0))) {
            {
              {
                if ((a_ == int_(3))) {
                  ;
                  goto break17_;
                }
              };

              (a_ = subs32_(a_, int_(1)));
            };
            continue20_:;
          }
          (a_ = int_(0));
        } break19_:;
      continue18_:;
      } while(false); 
      int_(0);
    } break17_:;

    {
      if ((a_ == int_(3))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    {
      do {
        {
          if ((a_ == int_(3))) {
            {
              {
                while ((a_ > int_(0))) {
                  {
                    {
                      if ((a_ == int_(3))) {
                        ;
                        goto continue22_;
                      }
                    };

                    (a_ = subs32_(a_, int_(1)));
                  };
                  continue24_:;
                }
                {
                  (a_ = int_(10));
                };
              } break23_:;
            };
          }
        };
      continue22_:;
      } while(false); 
      {
        (a_ = int_(4));
      };
    } break21_:;

    {
      if ((a_ == int_(4))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    int answer_; {
      do {
        {
          {
            do {
              {
                int_(5);
                goto break27_;
              };
            continue28_:;
            } while(false); 
            int_(0);
          } break27_:;

          answer_ = int_(10);
        };
      continue26_:;
      } while(false); 
      answer_ = int_(0);
    } break25_:;

    {
      if ((answer_ == int_(0))) {
        (sum_ = adds32_(sum_, int_(1)));
      }
    };

    int tmp29; {
      do {
        {
          ;
; {
            if ((a_ == int_(4))) {
              {
                tmp29 = adds32_(sum_, int_(1));
                goto break30_;
              };
            }
          };
        };
      continue31_:;
      } while(false); 
      tmp29 = int_(0);
    } break30_:;
    (sum_ = tmp29);

    result_ = sum_;
;
  };

  return result_;
}
