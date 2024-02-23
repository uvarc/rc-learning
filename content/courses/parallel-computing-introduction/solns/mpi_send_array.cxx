#include <iostream>
#include "mpi.h"

using namespace std;

int main (int argc, char *argv[]) {

  int nelem=10;

  int rank, nprocs;
  MPI_Status status;

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);

  if (nprocs < 2) {
     cout<<"This program works only for at least two processes\n";
     MPI_Finalize();
     return 0;
  }
  else if (nprocs%2 != 0) {
     cout<<"This program works only for an even number of processes\n";
     MPI_Finalize();
     return 0;
  }


  double* u=new double[nelem]{0};
  double* w=new double[nelem]{0};

  for (int i=0; i<nelem; ++i) {
      u[i]=20.+i*rank;
  }

  int neighbor;
  int sendtag, recvtag;

  if (rank%2==0) {
     neighbor = rank+1;
     sendtag=1;
     recvtag=2;
  }
  else {
     neighbor = rank-1;
     sendtag=2;
     recvtag=1;
  }

  if (rank%2==0) {
      MPI_Sendrecv(u, nelem, MPI_DOUBLE,neighbor,sendtag,
                  w, nelem, MPI_DOUBLE,neighbor,recvtag,MPI_COMM_WORLD,&status);
  } else {
      MPI_Sendrecv(u, nelem, MPI_DOUBLE,neighbor,sendtag,
                  w, nelem, MPI_DOUBLE,neighbor,recvtag,MPI_COMM_WORLD,&status);
  }

  for (int i=0; i<nelem; ++i) {
     cout<<rank<<" "<<i<<" "<<u[i]<<" "<<w[i]<<endl;
  }

  MPI_Finalize();

  return 0;

}
