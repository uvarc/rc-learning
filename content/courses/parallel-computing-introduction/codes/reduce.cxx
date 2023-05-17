#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int ncount,nprocs,rank;
    int n;
    float total;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    ncount=100;

    cout<<setprecision(1)<<fixed;

    float *myvals=new float[ncount];
    // load data into local arrays
    
    myvals[0]=rank+1;
    for (int i=1;i<ncount;++i) {
	myvals[i]=myvals[i-1]+rank+1.;
    }

    float mysum=0.0;
    for (int i=0;i<ncount;++i) {
	mysum+=myvals[i];
    }

    MPI_Reduce(&mysum,&total,1,MPI_FLOAT,MPI_SUM,0,MPI_COMM_WORLD);

    for (int n=0;n<nprocs;++n) {
	MPI_Barrier(MPI_COMM_WORLD);
	if (n==rank) {
            cout<<rank<<":"<<mysum<<" "<<total<<endl;
	}
    }

    MPI_Finalize();

}
        

