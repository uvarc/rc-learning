#include <mpi.h>
#include <stdio.h>
int main(int argc, char **argv) {
int me, np, q, sendto;
MPI_Status status;
MPI_Init(&argc, &argv);
MPI_Comm_size(MPI_COMM_WORLD, &np);
MPI_Comm_rank(MPI_COMM_WORLD, &me);
if (np%2==1) return 0;
if (me%2==1) {sendto = me-1;}
else {sendto = me+1;}
MPI_Send(&me, 1, MPI_INT, sendto, me, MPI_COMM_WORLD);
MPI_Recv(&q, 1, MPI_INT, sendto, sendto, MPI_COMM_WORLD, &status);
printf("Sent %d to proc %d, received %d from proc %d\n", me, sendto, q, sendto);MPI_Finalize();
return 0;
}
