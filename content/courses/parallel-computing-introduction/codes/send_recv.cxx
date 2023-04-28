#include <iostream>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {
    int rank, npes;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &npes);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if ( npes != 2 ) {
	cout<<"This program runs only on two processes\n";
	MPI_Finalize();
	exit(1);
    }

    float baton=0;

    if ( rank == 0 ) {
	MPI_Recv(&baton,1,MPI_FLOAT,1,0,MPI_COMM_WORLD,&status);
	baton+=1.;
	MPI_Send(&baton,1,MPI_FLOAT,1,0,MPI_COMM_WORLD);
    }
    else if ( rank == 1) {
	baton=12.;
	MPI_Send(&baton,1,MPI_FLOAT,0,0,MPI_COMM_WORLD);
	MPI_Recv(&baton,1,MPI_FLOAT,0,0,MPI_COMM_WORLD,&status);
    }

    cout<<rank<<" "<<baton<<"\n";

    MPI_Finalize();

    exit(0);
}

