#include <stdio.h>
#include <time.h>

int main() {
    printf("doing work on globals:\n");
    clock_t start = clock();

    int x = 0;

    while (x < 1000000000) {
        x = x + 1;
    }

    clock_t end = clock();
    double time_spent = (double)(end - start) / CLOCKS_PER_SEC;

    printf("%f seconds\n", time_spent);

    printf("\ndoing work on locals:\n");

    {
        start = clock();
        x = 0;

        while (x < 1000000000) {
            x = x + 1;
        }

        end = clock();
        time_spent = (double)(end - start) / CLOCKS_PER_SEC;

        printf("%f seconds\n", time_spent);
    }

    return 0;
}
