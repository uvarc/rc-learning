#include <iostream>
#include <mpi.h>

using namespace std;

int main(int argc, char* argv[]) {
    int provided;
    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
    cout<<"Requested "<<MPI_THREAD_MULTIPLE<<" Provided "<<provided<<endl;
    MPI_Finalize();

    exit(0);
}
