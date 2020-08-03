#include <stdio.h>
#include "mpi.h"
int main(int argc, char *argv[]) {
	int rank, npes;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &npes);
	if ( rank == 0 ) {
		printf("Running on %d Processes\n",npes);
	}
 fflush(stdout);
	printf("Greetings from process %d\n",rank);
	MPI_Finalize();
}
