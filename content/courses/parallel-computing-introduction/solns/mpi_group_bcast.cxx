#include <iostream>
#include <cstring>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[])  {
    int  rank, new_comm_rank, nprocs;
    MPI_Group  world_group, new_group; 
    MPI_Comm   new_comm; 
    int  ne, nevens;

    MPI_Init(&argc,&argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    int *flag=new int[nprocs];

    ne=0;
    for (int r=0; r<nprocs; r++) {
        if (r%2==0) {
            ne+=1;
            flag[r]=0;
        }
        else {
            flag[r]=1;
        }
    } 
    nevens=ne;

    int *evens=new int[nevens];

    ne=0;
    for (int r=0; r<nprocs; r++) {
        if (flag[r]==0) {
            evens[ne]=r;
            ne+=1;
        }
    }

    MPI_Comm_group(MPI_COMM_WORLD, &world_group);

    if (flag[rank]==0) {
        MPI_Group_incl(world_group, nevens, evens, &new_group);
    }
    else {
        MPI_Group_excl(world_group, nevens, evens, &new_group);
    }

    MPI_Comm_create(MPI_COMM_WORLD, new_group, &new_comm);
    MPI_Comm_rank (new_comm, &new_comm_rank);

    char message[16];
    strcpy(message,"               ");
    if (new_comm_rank==0) {
        if (flag[rank]==0) {
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
