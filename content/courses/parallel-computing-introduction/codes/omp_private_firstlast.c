#include <stdio.h>
#include <omp.h>

int main() {

    int x, y, z;
    int tid;

    int nthreads=4;

    x=10;

#pragma omp parallel for private(x) num_threads(nthreads)
    for (int i=0; i<nthreads; i++) {
        tid=omp_get_thread_num();
        x=1000*(tid+1);
        printf("Thread %d gets x= %d\n",tid, x);
    }
    
    printf("Outside parallel region x= %d\n", x);

    y=20;
#pragma omp parallel for firstprivate(y) num_threads(nthreads)
    for (int i=0; i<nthreads; i++) {
        tid=omp_get_thread_num();
        int w=y*100*(tid+1);
        printf("Thread %d gets w= %d\n",tid, w);
    }
    
    printf("Outside parallel region y= %d\n", y);

    z=30;
#pragma omp parallel for lastprivate(z) num_threads(nthreads)
    for (int i=0; i<nthreads; i++) {
        tid=omp_get_thread_num();
        z=3000*(tid+1);
        printf("Thread %d gets z= %d\n",tid, z);
    }
    
    printf("Outside parallel region z= %d\n", z);
}
