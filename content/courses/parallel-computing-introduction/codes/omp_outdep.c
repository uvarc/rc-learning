#include <stdio.h>
#include <omp.h>

int main() {

    const int N=48;
    int a[N];
    int x=5;

    a[N-1]=100;

#pragma omp parallel for 
    for (int i=0; i<N-1; i++) {
        a[i]=i;
        a[i+1]=x+i;
    }

    for (int i=0; i<N; i++) {
        printf("%d %d\n",i,a[i]);
    }

}
