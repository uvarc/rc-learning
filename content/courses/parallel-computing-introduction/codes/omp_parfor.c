#include <stdio.h>
#include <omp.h>

int main() {

    int N, nthreads;
    int tid;

    N=40;
    nthreads=4;

#pragma omp parallel for num_threads(nthreads)
    for (int i=0; i<N; i++) {
        tid=omp_get_thread_num();
        printf("Thread %d runs i= %d\n",tid, i);
    }
}
