#include <iostream>
#include <cstring>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[])  {
    int  rank, new_comm_rank, nprocs;
    MPI_Comm   new_comm; 
    int color, key;

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    if (rank%2==0) {
        color=0;
    }
    else {
        color=1;
    }

    key=0;
    MPI_Comm_split(MPI_COMM_WORLD, color, key, &new_comm);
    MPI_Comm_rank (new_comm, &new_comm_rank);

    char message[16];
    strcpy(message,"               ");
    if (new_comm_rank==0) {
        if (rank%2==0) {
            strcpy(message,"The secret is 1");
         }
         else {
            strcpy(message,"The secret is 2");
         }
    }

    MPI_Bcast(message, strlen(message), MPI_CHAR, 0, new_comm);

    cout<<"rank "<<rank<<" new_rank "<<new_comm_rank<<" "<<message<<endl;

    MPI_Finalize();

return 0;
}
