# The Orso Programming Language
Orso is a small, portable, statically-typed, expression-based language with powerful metaprogramming and compile-time evaluation tools.

It maintains a simple and terse syntax making it easy to learn.

## Who is this language for?
It's for my personal projects! 

I would recommend to *not* use this language for anything other than maybe a little bit of recreational programming (and even then, only when this is somewhat stable).

orso language is heavily inspired by Jai, so if you're looking for a far more powerful, stable and complete language, wait for that to be released.

This might be cool for education though. Aside from the dynamic FFI library (libffi, which hasn't been integrated yet), everything, including its arbitrary compile-time expression evaluation, is handwritten by me. In terms of code volume, the goal is to keep it under ~25K lines of code, which is similar to Lua's LoC.

The project implements a register-based virtual machine, a transpiler from my AST representation to C99, a static analyzer (type analysis, compile-time evaluation, and other semantic analysis), and more. For beginners, I think it's a great project to learn from since it's such a small language relative to others.

## Key Features
- Statically typed 
- Expression-based
- Generics (Inferred Function Definitions and Paramterized Struct Definitions)
- Arbitrary Expression Compile-Time Evaluation
- First-Class Types
- Manual Memory Mangement
- Easy C interop
- Small Syntax

## Syntax
Orso is expression-based which means almost all of its language constructs evaluate to some "value". In fact, the only language constructs that are not expressions are
variable declarations or compile-time value declarations. As a result, this makes the language quite flexible in regards to where expressions can be placed in the syntax.
Due to this, I've found that without an explicit way to separate expressions, the code is very hard to parse in a way that the user expects.
To migitate this, semicolons are required to signal the end of all expressions and declarations - to be frank, they're everywhere! This is the trade-off
that I think needs to be made to keep this language minimal and simple.

### Code Examples
#### Game of Life
This should give you a good idea of how a simple orso program may look like. There aren't any comments but, honestly, any beginner programmer should be able to understand it. If you're familiar with Odin and C, you're going to be right at home. There's really nothing crazy in here syntax wise.

```python
rl :: @load "./vendor/raylib.or";

board_x :: 32;
board_y :: 32;

grid_size :: [2]int.{board_x, board_y};
cell_size :: [2]int.{16, 16};

board_t :: [board_x][board_y]bool;

window_size :: grid_size * cell_size;

delta :: 0.15;

main :: () -> void {
    board_ := board_t.{};
    buffer_ := board_t.{};

    board := &board_;
    buffer := &buffer_;

    clear_board :: (board: &board_t) -> void {
        for i := 0; i < len(*board); ++i do
            for j := 0; j < len(board[0]); ++j {
                board[i][j] = false;
            };
    };

    mark  :: (board: &board_t, pos: [2]int, with := true) -> void {
        if pos[0] >= 0 and pos[0] < len(*board) and pos[1] >= 0 and pos[1] < len(board[0]) {
            board[pos[0]][pos[1]] = with;
        };
    };

    is_marked :: (board: &board_t, pos: [2]int) -> bool {
        while pos[0] < 0 do pos[0] += len(*board);
        while pos[1] < 0 do pos[1] += len(board[0]);

        pos[0] %= len(*board);
        pos[1] %= len(board[0]);

        marked := board[pos[0]][pos[1]];
        return marked;
    };

    count_neighbors :: (board: &board_t, x: int, y: int) -> int {
        nc := 0;

        for sx := x-1; sx < x+2; ++sx do
            for sy := y-1; sy < y+2; ++sy {
                if sx == x and sy == y then continue;
                nc += if is_marked(board, .{sx, sy}) then 1 else 0;
            };
        
        return nc;
    };

    update_board :: (dst: &board_t, src: &board_t) -> void {
        for x := 0; x < len(*src); ++x do
            for y := 0; y < len(src[0]); ++y {
                nc := count_neighbors(src, x, y);
                alive := src[x][y];
                if alive and nc < 2 then
                    dst[x][y] = false
                else if alive and (nc == 2 or nc == 3) then
                    dst[x][y] = true
                else if alive and nc > 3 then
                    dst[x][y] = false
                else if not alive and nc == 3 then
                    dst[x][y] = true;
            };
    };

    rl.init_window(window_size[0], window_size[1], "Game of Life");

    rl.set_target_fps(60);

    update := false;

    sec_left := delta;

    while not rl.window_should_close() {
        rl.begin_drawing();

            rl.clear_background(rl.raywhite);

            has_alive := false;

            for i := 0; i < grid_size[0]; ++i do
                for j := 0; j < grid_size[1]; ++j {
                    if not has_alive and board[i][j] then has_alive = true;

                    color := if board[i][j] then
                        rl.black
                    else
                        if i % 2 == j % 2 then
                            rl.white
                        else
                            rl.raywhite;

                    rl.draw_rectangle(.{i, j}*cell_size, cell_size, color);
                };


            if update and not has_alive then update = false;
            
            if rl.is_mouse_button_pressed(rl.mouse_button_left) {
                mouse_pos := rl.get_mouse_position()/cell_size;
                marked := board[mouse_pos[0]][mouse_pos[1]];
                mark(board, mouse_pos, not marked);
                
                update = false;
                sec_left = delta;
            };

            if rl.is_mouse_button_pressed(rl.mouse_button_right) {
                update = not update;
                sec_left = delta;
            };

            if update {
                if sec_left < 0 {
                    sec_left = delta;

                    tmp := buffer;
                    buffer = board;
                    board = tmp;

                    clear_board(board);
                    update_board(board, buffer);
                };

                sec_left -= rl.get_frame_time();
            };

        rl.end_drawing();
    };

    rl.close_window();
};
```

#### Dynamic Array

This is a good example of the generic syntax in orso. It's makes use of inferred function definitions and parameterized struct definitions.

```python
# usage
main :: () -> void {
  arr := make(int);
  push(&arr, 6);
  push(&arr, 9);

  *at(&arr, 0) = 4;
  x := *at(&arr, 1);

  free(&arr);
};

# parameters in a struct definition indicates values must be provided as compile-time constants
dynarr_t :: struct(u: type) {
    items := (&u).{};
    count := 0;
    capacity := 0;
};

# the `!` before a parameter in a function signature indicates that the respective argument
# must be provided as a compile-time constant at call-time
make :: (!u: type) -> dynarr_t(u) {
    arr := dynarr_t(u).{};
    return arr;
};

# the `!` before a type in a function signature indicates that the function definition is
# inferred at call-time during compilation
push :: (arr: &dynarr_t(u), item: !u) -> void {
    if arr.count >= arr.capacity {
        new_cap := if arr.capacity == 0 then 8 else arr.capacity*2;
        arr.items = (@icall "realloc", arr.items as &void,
            sizeof(u)*(arr.capacity as size_t), sizeof(u)*(new_cap as size_t)) as &u;
        arr.capacity = new_cap;
    };

    *(arr.items + (arr.count as ptrdiff_t)) = item;
    ++arr.count;
};

at :: (arr: &dynarr_t(!u), index := 0) -> &u {
    if index < 0 then index = arr.count - index;
    item := arr.items + (index as ptrdiff_t);
    return item;
};

free :: (arr: &dynarr_t(!u)) -> void {
    @icall "realloc", arr.items as &void, sizeof(u)*(arr.capacity as size_t), 0sz;
    *arr = .{};
};
```
