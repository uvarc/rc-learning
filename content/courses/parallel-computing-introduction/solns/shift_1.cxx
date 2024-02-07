#include <iostream>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {
    int rank, nprocs;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if ( nprocs < 2 ) {
        cout<<"This program runs only on at least two processes\n";
        MPI_Finalize();
        exit(1);
    }

    float message=42.*rank;

    if ( rank == 0 ) {
	MPI_Recv(&message,1,MPI_FLOAT,1,0,MPI_COMM_WORLD,&status);
    }
    else if ( rank == nprocs-1) {
	MPI_Send(&message,1,MPI_FLOAT,nprocs-2,0,MPI_COMM_WORLD);
    }
    else {
	MPI_Send(&message,1,MPI_FLOAT,rank-1,0,MPI_COMM_WORLD);
	MPI_Recv(&message,1,MPI_FLOAT,rank+1,0,MPI_COMM_WORLD,&status);
    }

    if (rank==0) {
	 cout<<"First Case\n";
    }

    for (int n=0; n<nprocs; ++n) {
	MPI_Barrier(MPI_COMM_WORLD);
	if (rank==n) {
            cout<<rank<<" "<<42*(rank+1)<<" "<<message<<"\n";
	}
    }

    MPI_Finalize();

    exit(0);
}

