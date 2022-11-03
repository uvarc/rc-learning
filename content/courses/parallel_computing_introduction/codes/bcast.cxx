#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int nprocs,rank;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    char message[]="I have a secret";

    MPI_Bcast(message,1,MPI_CHAR,0,MPI_COMM_WORLD);

    cout<<rank<<" "<<message<<endl;

    MPI_Finalize();

}
        

