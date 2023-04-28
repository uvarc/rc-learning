#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <cmath>
//For templated function objects
#include <functional>
#include <mpi.h>

using namespace std;

float f(float x) {
  return sin(x);
}

//Function pointer
//float trap(float a, float b, float h, int n, float (*function)(float)) {
//Templated function object
float trap(float a, float b, float h, int n, function<float(float)> f) {

   float integral = (f(a) + f(b))/2.0;
   float x=a;
   for (int i=0; i<n; ++i) {
       x+=h;
       integral += f(x);
   }

   integral *= h;
   return integral;
}

int main(int argc, char **argv) {
                                                                                
// Calculate a definite integral using trapezoid rule
                                                                                
    float  a, b;
    int    n;

    float  h, integral;
    float  x;

    int   nprocs, rank;
    float total;

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    bool shutdown=false;

    if (rank==0) {
        if (argc != 4) {
            cout<<"Usage: lower bound, upper bound, number of steps\n";
	    shutdown=true;
        }
        else {
           stringstream lb;
           lb<<argv[1];
           lb>>a;
           a=stof(argv[1]);

           stringstream ub;
           ub<<argv[1];
           ub>>b;
           b=stof(argv[2]);

           stringstream steps;
           steps<<argv[3];
           steps>>n;
        }
	//Not going to bother to redistribute this one
        if (!shutdown) {
	    if (n%nprocs != 0) {
	        cout<<"Number of processes must evenly divide number of steps\n";
	        shutdown=true;
	    }
	}
    }

    MPI_Bcast(&shutdown,1,MPI_CXX_BOOL,0,MPI_COMM_WORLD);
    if (shutdown) {
        MPI_Finalize();
	exit(1);
    }

    MPI_Bcast(&a,1,MPI_REAL,0,MPI_COMM_WORLD);
    MPI_Bcast(&b,1,MPI_REAL,0,MPI_COMM_WORLD);
    MPI_Bcast(&n,1,MPI_INTEGER,0,MPI_COMM_WORLD);

    h=(b-a)/n;

    int   local_n=n/nprocs;
    float local_a=a+rank*local_n*h;
    float local_b=b+rank*local_n*h;

    integral = trap(local_a,local_b,h,local_n,&f);

    MPI_Reduce(&integral,&total,1,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);

    if (rank==0) {
        cout<<" Result "<<total<<endl;
    }

}

