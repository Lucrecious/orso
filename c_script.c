#include <stdio.h>

struct Point {
    unsigned int x : 2;
    unsigned int y : 2;
};

struct Point my_point = { .x = 3, .y = 2 };

int get_x(struct Point* point) {
    return point->x;
}

int get_y(struct Point* point) {
    return point->y;
}

void set_x(struct Point* point, int value) {
    point->x = value;
}

void set_y(struct Point* point, int value) {
    point->y = value;
}

int main() {
    printf("Hello, World!\n");
    return 0;
}
