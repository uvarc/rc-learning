#include <iostream>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {
    int rank, npes;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &npes);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if ( rank == 0 ) {
        cout<<"Running on "<<npes<<"  Processes\n";
    }
    cout<<"Greetings from rank "<<rank<<"\n";

    MPI_Finalize();

}

