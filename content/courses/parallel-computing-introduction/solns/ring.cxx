#include <iostream>
#include <mpi.h>

using namespace std;

int main(int argc, char **argv)
{
    int nProcs;
    int rank;
    int baton = 42;
    MPI_Status status;
    double startTime;
    double stopTime;

    MPI_Init(&argc, &argv);

    MPI_Comm_size(MPI_COMM_WORLD, &nProcs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    if (nProcs > 1)
    {
        if (rank == 0) {
            startTime = MPI_Wtime();
            MPI_Send(&baton, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
            MPI_Recv(&baton, 1, MPI_INT, MPI_ANY_SOURCE,
            MPI_ANY_TAG, MPI_COMM_WORLD, &status);
            cout<<"Process "<<rank<<" has the baton.\n";
            stopTime = MPI_Wtime();
            cout<<"Elapsed time to pass a token around a ring of size "<<nProcs<<" was "<<stopTime-startTime<<"seconds\n";
            fflush(stdout);
        } else {
            MPI_Recv(&baton, 1, MPI_INT, MPI_ANY_SOURCE,
                                  MPI_ANY_TAG, MPI_COMM_WORLD, &status);
            cout<<"Process "<<rank<<" has the baton\n";
            fflush(stdout);
            MPI_Send(&baton, 1, MPI_INT, (rank + 1) % nProcs,
                               0, MPI_COMM_WORLD);
        }
    }

    MPI_Finalize();
    return 0;
}
