#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

int main() {

    double area, pi, x;
    int n;

    n=1000;

    area=0.0;
    for (int i=0; i< n; i++) {
        x=(i+0.5)/n;
        #pragma omp atomic
        area+=4.0/(1.0+x*x);
    }

    pi=area/n;
    printf("Pi is %f\n",pi);

    return(0);
}
