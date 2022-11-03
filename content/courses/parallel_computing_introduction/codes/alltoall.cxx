#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int nprocs,rank;
    int n;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    float *recvals=new float[nprocs];

    cout<<setprecision(1)<<fixed;

    float *myvals=new float[nprocs];
    // load data into local arrays
    
    myvals[0]=(rank+1)*100.;
    for (int i=1;i<nprocs;++i) {
	myvals[i]=myvals[i-1]+1.;
    }

    MPI_Alltoall(myvals,1,MPI_FLOAT,recvals,1,MPI_FLOAT,MPI_COMM_WORLD);

    for (int n=0;n<nprocs;++n) {
	MPI_Barrier(MPI_COMM_WORLD);
	if (n==rank) {
            for (int i=0;i<nprocs;++i) {
                cout<<rank<<":"<<recvals[i]<<" ";
            }
            cout<<"\n";
	 }
    }

    MPI_Finalize();

}
        

