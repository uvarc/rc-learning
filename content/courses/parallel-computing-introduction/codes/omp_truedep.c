#include <stdio.h>
#include <omp.h>

int main() {

    const int N=48;
    int a[N];

    a[0]=100;

#pragma omp parallel for 
    for (int i=1; i<N; i++) {
        a[i] = a[i-1] + 10;
    }

    for (int i=0; i<N; i++) {
        printf("%d %d\n",i,a[i]);
    }

}
