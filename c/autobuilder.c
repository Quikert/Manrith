// autobuilder.c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static double clamp(double lo, double hi, double x) {
    if (x < lo) return lo;
    if (x > hi) return hi;
    return x;
}

int main(void) {
    // defaults
    int sM = 0, sC = 0, sK = 0;
    int tM = 0, tC = 0, tK = 0;

    char buf[256];
    if (fgets(buf, sizeof(buf), stdin) == NULL) {
        // stdin
        sM = sC = sK = 0;
        tM = tC = tK = 0;
    } else {
        // tokens KEY=VALUE
        char *tok = strtok(buf, " \t\n\r");
        while (tok) {
            int val = 0;
            if (sscanf(tok, "SM=%d", &val) == 1) sM = val;
            else if (sscanf(tok, "SC=%d", &val) == 1) sC = val;
            else if (sscanf(tok, "SK=%d", &val) == 1) sK = val;
            else if (sscanf(tok, "TM=%d", &val) == 1) tM = val;
            else if (sscanf(tok, "TC=%d", &val) == 1) tC = val;
            else if (sscanf(tok, "TK=%d", &val) == 1) tK = val;
            tok = strtok(NULL, " \t\n\r");
        }
    }

    double rM = (tM == 0) ? 0.0 : (double)sM / (double)tM;
    double rC = (tC == 0) ? 0.0 : (double)sC / (double)tC;
    double rK = (tK == 0) ? 0.0 : (double)sK / (double)tK;

    double wM = clamp(0.05, 10.0, 0.34 * (0.8 + 0.4 * rM));
    double wC = clamp(0.05, 10.0, 0.33 * (0.8 + 0.4 * rC));
    double wK = clamp(0.05, 10.0, 0.33 * (0.8 + 0.4 * rK));

    // normalize
    double total = wM + wC + wK;
    if (total <= 1e-12) {
        wM = wC = wK = 1.0 / 3.0;
    } else {
        wM /= total; wC /= total; wK /= total;
    }

    // output as KEY=val lines
    printf("WM=%.6f\n", wM);
    printf("WC=%.6f\n", wC);
    printf("WK=%.6f\n", wK);
    return 0;
}
