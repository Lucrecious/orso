#include <stdio.h>
#include <time.h>

int fib(int n) {
    if (n <= 1)
        return n;
    else
        return fib(n - 1) + fib(n - 2);
}

int main() {
    clock_t start = clock();
    printf("%d\n", fib(35));
    printf("%lf\n", (double)(clock() - start) / CLOCKS_PER_SEC);
    return 0;
}
