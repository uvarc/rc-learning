#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int nprocs,rank;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    int *values=new int[10];
    for (int i=1; i<=10; ++i){
        values[i-1]=i;
    }

    MPI_Bcast(values,10,MPI_INT,0,MPI_COMM_WORLD);

    cout<<rank<<" "<<values[0];
    for (int i=1; i<9; ++i) {
       cout<<" "<<values[i];
    }
    cout<<" "<<values[9]<<endl;

    MPI_Finalize();

}
        

