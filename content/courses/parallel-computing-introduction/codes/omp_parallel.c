#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int main(int argc, char *argv[]){

    #pragma omp parallel
    {
        int tid=omp_get_thread_num();
        printf("Hello from thread %d\n",tid);
    }
    
    return 0;
}
