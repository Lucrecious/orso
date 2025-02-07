#define INTRINSICS_IMPLEMENTATION
#include "intrinsics.h"
#include "core.h"
#include "core.c"
typedef void(*fn_void)();
typedef void* p_void;
typedef p_void(*fn_size_t_p_void)(size_t);
typedef bool_(*fn_p_void_size_t_bool_)(p_void,size_t);
typedef size_t(*fn_size_t)();
typedef u64(*fn_u64)();
typedef int(*fn_int)();
typedef void(*fn_int_void)(int);
typedef void(*fn_void)();
typedef f64(*fn_u64_f64)(u64);
typedef u8* p_u8;
typedef void(*fn_p_u8_int_void)(p_u8,int);
typedef p_u8(*fn_p_u8_s64_p_u8)(p_u8,s64);
typedef p_u8(*fn_p_u8_int_int_int_p_u8)(p_u8,int,int,int);
typedef void(*fn_p_u8_int_int_void)(p_u8,int,int);
typedef int(*fn_p_u8_int_int_int_int_int)(p_u8,int,int,int,int);
f64 ns2sec_odlfn1_(u64);

f64 ns2sec_odlfn1_(u64 ns_) {
  {
    f64 tmp2 = muld_((cast(f64, ns_)), 1e-09);
    return tmp2;
  };
}


void _module_init_3(void) {
}

p_u8 at_odlfn1_(p_u8, s64);
p_u8 atxy_odlfn2_(p_u8, int, int, int);
int neighbors_odlfn5_(p_u8, int, int, int, int);
void board_clear_odlfn3_(p_u8, int);
void render_board_odlfn4_(p_u8, int, int);

p_u8 at_odlfn1_(p_u8 board_, s64 index_) {
  {
    p_u8 tmp6 = (addptr_((p_u8)board_, index_));
    return tmp6;
  };
}

p_u8 atxy_odlfn2_(p_u8 board_, int x_, int y_, int width_) {
  {
    p_u8 tmp7 = addptr_((p_u8)board_, cast(s64, adds32_(muls32_(y_, width_), x_)));
    return tmp7;
  };
}

void board_clear_odlfn3_(p_u8 board_, int count_) {
  {
    s64 i_ = 0ll;

    {
      while ((i_ < cast(s64, count_))) {
        {
          (*(((at_odlfn1_)(board_, i_))) = int_(0));

          (i_ = adds64_(i_, 1ll));
        };
        continue9_:;
      }
    } break8_:;

    return;
  };
}

void render_board_odlfn4_(p_u8 board_, int width_, int height_) {
  {
    int w_ = int_(0);

    int h_ = int_(0);

    {
      while ((h_ < height_)) {
        {
          {
            while ((w_ < width_)) {
              {
                int tile_ = cast(int, *(((atxy_odlfn2_)(board_, w_, h_, width_))));

                ((odlprintint)(tile_));

                (w_ = adds32_(w_, int_(1)));
              };
              continue13_:;
            }
          } break12_:;

          (w_ = int_(0));

          (h_ = adds32_(h_, int_(1)));

          ((odlprintln)());

        };
        continue11_:;
      }
    } break10_:;

    ((odlprintln)());

    return;
  };
}

int neighbors_odlfn5_(p_u8 board_, int x_, int y_, int w_, int h_) {
  {
    int sum_ = int_(0);

    int xi_ = int_(0);

    int yi_ = int_(0);

    int xs_ = subs32_(x_, int_(1));

    int ys_ = subs32_(y_, int_(1));

    {
      while ((yi_ < int_(3))) {
        {
          {
            while ((xi_ < int_(3))) {
              {
                int xn_ = adds32_(xs_, xi_);

                int yn_ = adds32_(ys_, yi_);

                (xi_ = adds32_(xi_, int_(1)));

                {
                  if (((xn_ == x_) && (yn_ == y_))) {
                    ;
                    goto continue17_;
                  }
                };

                {
                  if (((((xn_ < int_(0)) || (yn_ < int_(0))) || (xn_ >= w_)) || (yn_ >= h_))) {
                    ;
                    goto continue17_;
                  }
                };

                (sum_ = adds32_(sum_, cast(int, *(((atxy_odlfn2_)(board_, xn_, yn_, w_))))));
              };
              continue17_:;
            }
          } break16_:;

          (xi_ = int_(0));

          (yi_ = adds32_(yi_, int_(1)));
        };
        continue15_:;
      }
    } break14_:;

    int tmp18 = sum_;
;
    return tmp18;
  };
}


int expr(void) {
  _module_init_3();

  int tmp19; {
    do {
      {
        int width_ = ((odlreadint)());

        int height_ = ((odlreadint)());

        int tile_count_ = muls32_(width_, height_);

        {
          if ((tile_count_ == int_(0))) {
            {
              ((odlprintint)(int_(0)));

              ((odlprintln)());

              tmp19 = int_(1);
              goto break20_;
            };
          }
        };

        p_u8 board_ = cast(p_u8, ((odlmreserve)(cast(size_t, muls32_(tile_count_, int_(2))))));

        p_u8 buffer_ = addptr_((p_u8)board_, (cast(s64, tile_count_)));

        ((odlmmarkrw)(cast(p_void, board_), cast(size_t, muls32_(tile_count_, int_(2)))));

        ((odlprintint)(int_(1)));

        ((odlprintln)());

        ((odlprintln)());

        fn_p_u8_s64_p_u8 at_ = (at_odlfn1_);

        fn_p_u8_int_int_int_p_u8 atxy_ = (atxy_odlfn2_);

        fn_p_u8_int_void board_clear_ = (board_clear_odlfn3_);

        ((board_clear_odlfn3_)(board_, tile_count_));

        int initx_ = ((odlreadint)());

        int inity_ = ((odlreadint)());

        {
          until ((muls32_(initx_, inity_) == int_(0))) {
            {
              int x_ = subs32_(initx_, int_(1));

              int y_ = subs32_(inity_, int_(1));

              {
                if (((((x_ < int_(0)) || (y_ < int_(0))) || (x_ >= width_)) || (y_ >= height_))) {
                  {
                    ((odlprintint)(int_(0)));

                    ((odlprintln)());
                  };
                } else {
                  {
                    (*(((atxy_odlfn2_)(board_, x_, y_, width_))) = int_(1));

                    ((odlprintint)(int_(1)));

                    ((odlprintln)());

                  };
                }
              };

              ((odlprintln)());

              (initx_ = ((odlreadint)()));

              (inity_ = ((odlreadint)()));
            };
            continue23_:;
          }
        } break22_:;

        ((odlprintln)());

        fn_p_u8_int_int_void render_board_ = (render_board_odlfn4_);

        ((render_board_odlfn4_)(board_, width_, height_));

        fn_p_u8_int_int_int_int_int neighbors_ = (neighbors_odlfn5_);

        {
          while ((((odlreadint)()) != int_(0))) {
            {
              ((board_clear_odlfn3_)(buffer_, tile_count_));

              int x_ = int_(0);

              int y_ = int_(0);

              {
                while ((y_ < height_)) {
                  {
                    {
                      while ((x_ < width_)) {
                        {
                          int n_ = ((neighbors_odlfn5_)(board_, x_, y_, width_, height_));

                          u8 alive_ = *(((atxy_odlfn2_)(board_, x_, y_, width_)));

                          {
                            if (((alive_ > u8_(0)) && (n_ < int_(2)))) {
                              (*(((atxy_odlfn2_)(buffer_, x_, y_, width_))) = int_(0));
                            } else {
                              {
                                if (((alive_ > u8_(0)) && (((n_ == int_(2)) || (n_ == int_(3)))))) {
                                  (*(((atxy_odlfn2_)(buffer_, x_, y_, width_))) = int_(1));
                                } else {
                                  {
                                    if (((alive_ > u8_(0)) && (n_ > int_(3)))) {
                                      (*(((atxy_odlfn2_)(buffer_, x_, y_, width_))) = int_(0));
                                    } else {
                                      {
                                        if (((alive_ < u8_(1)) && (n_ == int_(3)))) {
                                          (*(((atxy_odlfn2_)(buffer_, x_, y_, width_))) = int_(1));
                                        }
                                      };
                                    }
                                  };
                                }
                              };
                            }
                          };

                          (x_ = adds32_(x_, int_(1)));
                        };
                        continue29_:;
                      }
                    } break28_:;

                    (x_ = int_(0));

                    (y_ = adds32_(y_, int_(1)));
                  };
                  continue27_:;
                }
              } break26_:;

              p_u8 tmp_ = board_;
;

              (board_ = buffer_);

              (buffer_ = tmp_);

              ((render_board_odlfn4_)(board_, width_, height_));

            };
            continue25_:;
          }
        } break24_:;

        ((odlmfree)(cast(p_void, board_), cast(size_t, muls32_(tile_count_, int_(2)))));

        tmp19 = int_(0);
      };
    continue21_:;
    } while(false); 
    tmp19 = int_(0);
  } break20_:;

  return tmp19;
}
