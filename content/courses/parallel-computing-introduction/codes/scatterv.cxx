#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int nprocs,rank;
    int n;
    float values[101];
    int displs[8];

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    //For illustration only, don't hardcode nprocs
    if (nprocs != 8) {
        cout << "Example requires 8 processes\n";
	exit(1);
    }

    for (int i=0;i<=100;++i) {
        values[i]=(float) i;
    }

    int nprow=101/nprocs;
    int leftover=101%nprocs;
    cout<<setprecision(1)<<fixed;
    if (rank == 0) {
        n=0;
        for (int i=0;i<nprocs;++i) {
	    for (int j=n;j<=n+nprow-1;++j) {
	        cout<<" "<<values[j]<<" ";
            }
           n=n+nprow;
	   cout<<"\n";
	}
	for (int i=n;i<=n+leftover-1;++i) {
	    cout<<values[i]<<" ";
        }
	cout<<"\n";
    }

    //Hand-distributing the numbers
    int sendcounts[]={12,12,11,12,13,9,10,8};
    int offsets[]={0,2,3,1,4,1,1,2};

    displs[0]=offsets[0];
    for (int i=1;i<nprocs;++i) {
        displs[i]=displs[i-1]+sendcounts[i-1]+offsets[i];
    }

    float *myvals=new float[sendcounts[rank]];

    MPI_Scatterv(values,sendcounts,displs,MPI_FLOAT,myvals,sendcounts[rank],MPI_FLOAT,0,MPI_COMM_WORLD);

    //Forces each process to write separately and in order
    //Printing here is to demonstrate how the data are distributed
    for (n=0;n<nprocs;++n) {
        MPI_Barrier(MPI_COMM_WORLD);
        if (n==rank) {
	    cout<<rank;
	    for (int i=0;i<=sendcounts[rank]-1;++i) {
		cout<<" "<<myvals[i]<<" ";
	    }
	    cout<<"\n";
	}
    }

    MPI_Finalize();

}
        

