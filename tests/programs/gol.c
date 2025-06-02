#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"
#include "core.h"
#include "core.c"
typedef void(*fn_void)();
typedef void* p_void;
typedef p_void(*fn_size_t_p_void)(size_t);
typedef orbool(*fn_p_void_size_t_bool_)(p_void,size_t);
typedef size_t(*fn_size_t)();
typedef oru64(*fn_u64)();
typedef int(*fn_int)();
typedef void(*fn_int_void)(int);
typedef void(*fn_void)();
typedef orf64(*fn_u64_f64)(oru64);
typedef struct arr_5_u8 arr_5_u8; struct arr_5_u8 { oru8 arr[5]; };
typedef struct arr_5_arr_5_u8 arr_5_arr_5_u8; struct arr_5_arr_5_u8 { arr_5_u8 arr[5]; };
typedef arr_5_arr_5_u8* p_arr_5_arr_5_u8;
typedef void(*fn_p_arr_5_arr_5_u8_void)(p_arr_5_arr_5_u8);
typedef void(*fn_p_arr_5_arr_5_u8_u8_void)(p_arr_5_arr_5_u8,oru8);
typedef int(*fn_p_arr_5_arr_5_u8_int_int_int)(p_arr_5_arr_5_u8,int,int);
orf64 ns2sec_odlfn1_(oru64);

orf64 ns2sec_odlfn1_(oru64 ns_) {
  {
    orf64 tmp2 = muld_((cast(orf64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}

void func1(p_arr_5_arr_5_u8, oru8);
void render_board_odlfn2_(p_arr_5_arr_5_u8);
int neighbors_odlfn3_(p_arr_5_arr_5_u8, int, int);

void func1(p_arr_5_arr_5_u8 arr_, oru8 value_) {
  {
    {
      int w_ = int_(0);
      while ((w_ < int_(5))) {
        {
          {
            int h_ = int_(0);
            while ((h_ < int_(5))) {
              {
                (*(arr_)).arr[w_].arr[h_] = value_;
              };
              continue7_:;
              (h_ = adds32_(h_, int_(1)));
            }
          } break6_:;
        };
        continue5_:;
        (w_ = adds32_(w_, int_(1)));
      }
    } break4_:;
  };
}

void render_board_odlfn2_(p_arr_5_arr_5_u8 board_) {
  {
    {
      int h_ = int_(0);
      while ((h_ < int_(5))) {
        {
          {
            int w_ = int_(0);
            while ((w_ < int_(5))) {
              {
                int tile_ = cast(int, (*(board_)).arr[h_].arr[w_]);

                ((odlprintint)(tile_));
              };
              continue11_:;
              (w_ = adds32_(w_, int_(1)));
            }
          } break10_:;

          ((odlprintln)());
        };
        continue9_:;
        (h_ = adds32_(h_, int_(1)));
      }
    } break8_:;
  };
}

int neighbors_odlfn3_(p_arr_5_arr_5_u8 board_, int x_, int y_) {
  {
    int sum_ = int_(0);

    int xs_ = subs32_(x_, int_(1));

    int ys_ = subs32_(y_, int_(1));

    {
      int yi_ = int_(0);
      while ((yi_ < int_(3))) {
        {
          {
            int xi_ = int_(0);
            while ((xi_ < int_(3))) {
              {
                int xn_ = adds32_(xs_, xi_);

                int yn_ = adds32_(ys_, yi_);

                {
                  if (((xn_ == x_) && (yn_ == y_))) {
                    ;
                    goto continue15_;
                  }
                };

                {
                  if (((((xn_ < int_(0)) || (yn_ < int_(0))) || (xn_ >= int_(5))) || (yn_ >= int_(5)))) {
                    ;
                    goto continue15_;
                  }
                };

                (sum_ = adds32_(sum_, cast(int, (*(board_)).arr[yn_].arr[xn_])));
              };
              continue15_:;
              (xi_ = adds32_(xi_, int_(1)));
            }
          } break14_:;
        };
        continue13_:;
        (yi_ = adds32_(yi_, int_(1)));
      }
    } break12_:;

    int tmp16 = sum_;
;
    return tmp16;
  };
}


int expr(void) {
  _module_init_3();

  int tmp17; {
    do {
      {


        {
          if (false) {
            {
              ((odlprintint)(int_(0)));

              ((odlprintln)());

              tmp17 = int_(1);
              goto break18_;
            };
          }
        };


        arr_5_arr_5_u8 board__ = (arr_5_arr_5_u8){ .arr={(arr_5_u8){ .arr={u8_(0), u8_(0), u8_(0), u8_(0), u8_(0)} }, (arr_5_u8){ .arr={u8_(0), u8_(0), u8_(0), u8_(0), u8_(0)} }, (arr_5_u8){ .arr={u8_(0), u8_(0), u8_(0), u8_(0), u8_(0)} }, (arr_5_u8){ .arr={u8_(0), u8_(0), u8_(0), u8_(0), u8_(0)} }, (arr_5_u8){ .arr={u8_(0), u8_(0), u8_(0), u8_(0), u8_(0)} }} };

        arr_5_arr_5_u8 buffer__ = board__;
;

        p_arr_5_arr_5_u8 board_ = &(board__);

        p_arr_5_arr_5_u8 buffer_ = &(buffer__);

        ((odlprintint)(int_(1)));

        ((odlprintln)());

        ((odlprintln)());

        int initx_ = ((odlreadint)());

        int inity_ = ((odlreadint)());

        {
          until ((muls32_(initx_, inity_) == int_(0))) {
            {
              int x_ = subs32_(initx_, int_(1));

              int y_ = subs32_(inity_, int_(1));

              {
                if (((((x_ < int_(0)) || (y_ < int_(0))) || (x_ >= int_(5))) || (y_ >= int_(5)))) {
                  {
                    ((odlprintint)(int_(0)));

                    ((odlprintln)());
                  };
                } else {
                  {
                    (*(board_)).arr[y_].arr[x_] = u8_(1);

                    ((odlprintint)(int_(1)));

                    ((odlprintln)());

                  };
                }
              };

              ((odlprintln)());

              (initx_ = ((odlreadint)()));

              (inity_ = ((odlreadint)()));
            };
            continue21_:;
          }
        } break20_:;

        ((odlprintln)());


        ((render_board_odlfn2_)(board_));


        {
          while ((((odlreadint)()) != int_(0))) {
            {
              ((func1)(buffer_, u8_(0)));

              {
                int y_ = int_(0);
                while ((y_ < int_(5))) {
                  {
                    {
                      int x_ = int_(0);
                      while ((x_ < int_(5))) {
                        {
                          int n_ = ((neighbors_odlfn3_)(board_, x_, y_));

                          oru8 alive_ = ((*(board_)).arr[y_].arr[x_]);

                          {
                            if (((alive_ > u8_(0)) && (n_ < int_(2)))) {
                              (*(buffer_)).arr[y_].arr[x_] = u8_(0);
                            } else {
                              {
                                if (((alive_ > u8_(0)) && (((n_ == int_(2)) || (n_ == int_(3)))))) {
                                  (*(buffer_)).arr[y_].arr[x_] = u8_(1);
                                } else {
                                  {
                                    if (((alive_ > u8_(0)) && (n_ > int_(3)))) {
                                      (*(buffer_)).arr[y_].arr[x_] = u8_(0);
                                    } else {
                                      {
                                        if (((alive_ < u8_(1)) && (n_ == int_(3)))) {
                                          (*(buffer_)).arr[y_].arr[x_] = u8_(1);
                                        }
                                      };
                                    }
                                  };
                                }
                              };
                            }
                          };
                        };
                        continue27_:;
                        (x_ = adds32_(x_, int_(1)));
                      }
                    } break26_:;
                  };
                  continue25_:;
                  (y_ = adds32_(y_, int_(1)));
                }
              } break24_:;

              p_arr_5_arr_5_u8 tmp_ = board_;
;

              (board_ = buffer_);

              (buffer_ = tmp_);

              ((render_board_odlfn2_)(board_));

            };
            continue23_:;
          }
        } break22_:;

        tmp17 = int_(0);
      };
    continue19_:;
    } while(false); 
    tmp17 = int_(0);
  } break18_:;

  return tmp17;
}

int main() {
  expr();
}
