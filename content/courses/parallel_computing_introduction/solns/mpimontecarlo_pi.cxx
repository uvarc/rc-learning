/* Computes pi by a Monte-Carlo method
 *
*/

#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <random>
#include <cmath>
#include <mpi.h>

using namespace std;

#define urand ((double)rand()/(double)RAND_MAX);

long long throwDarts(long long nThrows) {

   long long hits=0;
   for (int i=0; i<nThrows; ++i) {
       double x = urand;
       double y = urand;
       if ( sqrt(pow(x,2)+pow(y,2)) < 1.0 ) {
           hits++;
       }
   }
   return hits;
}

int main(int argc, char **argv) {

   int nprocs, rank;

   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   long long nThrows;

   if (argc != 2) {
      cout<<rank<<"Usage: nThrows\n";
      MPI_Finalize();
      return 0;
   }
   else {
      string steps=argv[1];
      stringstream ssteps;
      ssteps<<steps;
      ssteps>>nThrows;
   }

   //It's better to use C++ random-number generators, but that requires some
   //additional coding, and possibly templating, so let's keep it simple and 
   //fast for this example.
   srand(time(0));

// Strong scaling
   long long myThrows=nThrows/nprocs;
   if ( nThrows%nprocs!=0 ) {
      long long nExtra=nThrows%nprocs;   // dopey load balancing
      for (long long n=1;n<=nExtra;n++) {
         if (n-1==rank) myThrows++;
      }
   }

// For weak scaling
//  myThrows=nThrows
//
   long long myHits=throwDarts(myThrows);
   long long allHits;
   MPI_Reduce(&myHits,&allHits,1,MPI_LONG_LONG_INT,MPI_SUM,0,MPI_COMM_WORLD);

   double pi=4.*(double)allHits/(double)myThrows;
//weak scaling
//   double pi=(double)allHits/(double)myThrows*nprocs;

   if (rank==0) {
       cout<<"Pi is "<<setprecision(6)<<pi<<endl;
   }

   MPI_Finalize();

   return 0;

}
