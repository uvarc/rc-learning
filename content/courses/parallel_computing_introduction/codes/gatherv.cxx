#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int nprocs,rank;
    int n;
    float values[101], allvals[101];
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
        allvals[i]=0.;
    }

    cout<<setprecision(1)<<fixed;

    //Hand-distributing the numbers
    int sendcounts[]={12,12,11,12,13,9,10,8};
    int offsets[]={0,2,3,1,4,1,1,2};

    displs[0]=offsets[0];
    for (int i=1;i<nprocs;++i) {
        displs[i]=displs[i-1]+sendcounts[i-1]+offsets[i];
    }

    float *myvals=new float[sendcounts[rank]];
    // load data into local arrays
    int start_index=displs[rank];
    for (int i=0;i<sendcounts[rank];++i) {
	myvals[i]=values[start_index+i];
    }

    MPI_Gatherv(myvals,sendcounts[rank],MPI_FLOAT,allvals,sendcounts,displs,MPI_FLOAT,0,MPI_COMM_WORLD);

    int nprow=101/nprocs;
    int leftover=101%nprocs;
    if (rank == 0) {
        n=0;
        for (int i=0;i<nprocs;++i) {
	    for (int j=n;j<=n+nprow-1;++j) {
	        cout<<" "<<allvals[j]<<" ";
            }
           n=n+nprow;
	   cout<<"\n";
	}
	for (int i=n;i<=n+leftover-1;++i) {
	    cout<<allvals[i]<<" ";
        }
	cout<<"\n";
    }

    MPI_Finalize();

}
        

