#include <iostream>
#include <mpi.h>

using namespace std;

int main(int argc, char **argv) {

    int rank, nprocs, message, neighbor;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (nprocs < 2) {
        cout<<"This program works only for at least two processes\n";
	MPI_Finalize();
        return 1;
    }
    else if (nprocs%2 != 0) {
        cout<<"This program works only for an even number of processes\n";
	MPI_Finalize();
        return 2;
    }

    if (rank%2==0) {
        neighbor = rank+1;
    }
    else {
	neighbor = rank-1;
    }

    MPI_Send(&rank, 1, MPI_INT, neighbor, 0, MPI_COMM_WORLD);
    MPI_Recv(&message, 1, MPI_INT, neighbor, 0, MPI_COMM_WORLD, &status);
    cout<<rank<<" "<<message<<endl;
    MPI_Finalize();

    return 0;
}
