#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int ncount,nprocs,rank;
    int n;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    ncount=10;

    cout<<setprecision(1)<<fixed;

    float *myvals=new float[ncount];
    // load data into local arrays
    
    myvals[0]=(rank+1);
    for (int i=1;i<ncount;++i) {
	myvals[i]=myvals[i-1]+rank+1.;
    }

    MPI_Allgather(MPI_IN_PLACE,ncount,MPI_FLOAT,myvals,ncount,MPI_FLOAT,MPI_COMM_WORLD);

    int nprow=ncount;
    if (rank == 0) {
        n=0;
        for (int i=0;i<=nprocs-1;++i) {
	    for (int j=n;j<=n+nprow-1;++j) {
	        cout<<" "<<myvals[j]<<" ";
            }
           n=n+nprow;
	   cout<<"\n";
	}
	cout<<"\n";
    }

    MPI_Finalize();

}
        

