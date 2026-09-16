#include <stdio.h>
#include <omp.h>

int main() {

    const int N=48;
    int a[N];

    for (int i=0; i<N; i++) 
        a[i]=100-i;

#pragma omp parallel for 
    for (int i=0; i<N-1; i++) {
        a[i] = a[i+1] + 10;
    }

    for (int i=0; i<N; i++) {
        printf("%d %d\n",i,a[i]);
    }

}
