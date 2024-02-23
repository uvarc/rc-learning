#include <iostream>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {
    int rank, nprocs;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if ( nprocs%2 != 0 ) {
	cout<<"This program runs only on an even number of processes\n";
	MPI_Finalize();
	exit(1);
    }

    int message;

    int partner;
    int half=nprocs/2;
    if ( rank < half ) {
        partner=half+rank;
    }
    else {
	partner=rank-half;
    }

    if ( rank < half ) {
	MPI_Recv(&message,1,MPI_INT,partner,0,MPI_COMM_WORLD,&status);
	MPI_Send(&rank,1,MPI_INT,partner,0,MPI_COMM_WORLD);
    }
    else { 
	MPI_Send(&rank,1,MPI_INT,partner,0,MPI_COMM_WORLD);
	MPI_Recv(&message,1,MPI_INT,partner,0,MPI_COMM_WORLD,&status);
    }

    cout<<rank<<" "<<message<<"\n";

    MPI_Finalize();

    exit(0);
}

