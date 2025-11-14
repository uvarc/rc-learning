#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int nprocs,rank;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    //extra spece for null terminator
    double message;
    if (rank==0) {
        message=42.;
    }

    MPI_Bcast(&message,1,MPI_DOUBLE,0,MPI_COMM_WORLD);

    cout<<rank<<" "<<message<<endl;

    MPI_Finalize();

}
        

