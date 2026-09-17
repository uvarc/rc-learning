#include <iostream>
#include <cstdio>
#include <omp.h>

using namespace std;

int main() {

    int tid, nthreads;

    #pragma omp parallel private(tid)
    {
    nthreads=omp_get_num_threads();
    tid=omp_get_thread_num();
    printf("Hello from thread %d of %d\n",tid, nthreads);
    //cout<<"Hello from thread"<<" "<<tid<<" of"<<nthreads<<"\n";
    }
}
