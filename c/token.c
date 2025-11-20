// token.c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(void) {
    unsigned int seed = (unsigned int)time(NULL);
    if (seed == 0) seed = 123456789u;

    double wM = 0.34, wC = 0.33, wK = 0.33;
    int minLen = 2, maxLen = 24;

    // std-dev
    double stepStd = 0.15;

    // gene range
    double lo = -1.0, hi = 1.0;

    double addP = 0.25, remP = 0.20;

    // output as simple key=value lines to be read by Haskell stdin
    printf("SEED=%u\n", seed);
    printf("WM=%.6f\n", wM);
    printf("WC=%.6f\n", wC);
    printf("WK=%.6f\n", wK);
    printf("MINLEN=%d\n", minLen);
    printf("MAXLEN=%d\n", maxLen);
    printf("STEPSTD=%.6f\n", stepStd);
    printf("RANGE_LO=%.6f\n", lo);
    printf("RANGE_HI=%.6f\n", hi);
    printf("ADD_BASE=%.6f\n", addP);
    printf("REM_BASE=%.6f\n", remP);

    return 0;
}
