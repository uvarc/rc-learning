#include <stdlib.h>
#include <stdio.h>
#include <omp.h>

int main() {

   const int M=10;
   int a[10]={0,2,4,6,1,2,3,4,5,6};
   int b[10]={9,4,8,8,4,6,7,8,9,3};
   float c[10]={0.};

   int low, high;

   #pragma omp parallel 
   {
   for (int i=0; i<M; i++) {
       low = a[i];
       high = b[i];

       if (low > high) {
          #pragma omp master
          printf ("Exiting (%d)\n", i);
          break;
       }

   #pragma omp for
       for (int j=low; j<=high; j++) {
          c[i] -= (float)a[j]/(float)b[j];
       }
   }

   }

   for (int i=0;i<M;i++) {
          printf ("i=%d c[i]=%f\n", i,c[i]);
   }

}

