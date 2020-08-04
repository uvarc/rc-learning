/* Parallel Solution to Steady-State Heat Problem */

#include <iostream>
#include <cmath>
#include <ctime>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <string>
#include <mpi.h>

//If your N is too large, it will have problems allocating the necessary memory
#define N 500

#define MAX_ITER 100000000

double topBC=100;
double bottomBC=0.;
double edgeBC=100.;

void set_bcs(double **u, int nr, int nc, int rank, int npes);

using namespace std; 

int main (int argc, char *argv[]) {
  int i, j;
  int iterations = 0;
  double epsilon;
  double diff;            // Change in value 
  float time, etime;

  // Added for MPI
  int nr, nrl, nc;
  int rank, npes;
  MPI_Status status;
  int root=0, tag=0;
  int up, down;
  double gdiff;
  string fname;

  //Initialize MPI
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD,&npes);
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);

  // check number of parameters and read in epsilon 
  if (argc != 3) {
    if (rank==root) {
       printf ("USAGE:  %s <epsilon> <output-file>\n", argv[0]);
    }
    MPI_Finalize();
    return 1;
  }
  stringstream ssepsilon;
  ssepsilon<<argv[1];
  if (ssepsilon.fail()) {
    printf ("Error reading in epsilon value\n");
    return 2;
  }

  ssepsilon>>epsilon;
  if (rank==root) {
     cout<<"running until the difference is <="<<epsilon<<"\n";
  }

  diff = epsilon;

  //Usually we would read in nr and nc; they represent the global grid RxC;
  //For this case they are both N.
  nr=N;
  nc=N;

  //Weak scaling
  //nrl=nr;

  //Strong scaling
  if (nr%npes!=0) {
     MPI_Finalize();
     cout<<"Not an even division of the arrays.\n";
     return 3;
  }
  else {
     nrl=nr/npes;
  }

  //Find my neighbors
  if (rank==0) {
      up=MPI_PROC_NULL;
  }
  else {
      up=rank-1;
  }

  if (rank==npes-1) {
      down=MPI_PROC_NULL;
  }
  else {
      down=rank+1;
  }

  int nrows=nrl+2;
  int ncols=nc+2;
  double **u=new double*[nrows];  // Old values
  double **w=new double*[nrows];  // New values

  double *uptr=new double[(nrows)*(ncols)];
  double *wptr=new double[(nrows)*(ncols)];

  for (i=0;i<(nrl+2);++i,uptr+=ncols) 
     u[i] = uptr;
  for (i=0;i<(nrl+2);++i,wptr+=ncols) 
     w[i] = wptr;

  //Compute a mean of the boundary values as a first guess
  double mean=(2.*edgeBC+topBC+bottomBC)/4.0;

  // Initialize all values to mean
  for ( i = 0; i <= nrl+1; i++ )
    for (j = 0; j <= nc+1; j++ )
       u[i][j] = mean;
      

  //Set physical boundary conditions
  set_bcs(u,nrl,nc,rank,npes);

  //Walltime from process 0 is good enough for me 
  if (rank==root) { 
     time=MPI_Wtime();
  }

  // Compute steady-state solution 
  // Have to remove the condition on diff to keep processes in sync

  while ( iterations<MAX_ITER ) {
    diff = 0.0;  //reset to get maximum
    for (i=1; i<=nrl;i++)
      for (j=1;j<=nc;j++) {
          w[i][j] = (u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1])/4.0;
    	  if ( abs(w[i][j] - u[i][j]) >= diff )
	     diff = abs(w[i][j] - u[i][j]);
      }

      //Exchange halo values (one ghost row each side)
      MPI_Sendrecv(&w[1][1],nc, MPI_DOUBLE,up,tag,&w[nrl+1][1],
                            nc, MPI_DOUBLE,down,tag,MPI_COMM_WORLD,&status);

      MPI_Sendrecv(&w[nrl][1],nc,MPI_DOUBLE,down,tag,&w[0][1],
                              nc,MPI_DOUBLE,up,tag,MPI_COMM_WORLD,&status);

      //Find max of dff in all the processors.
      MPI_Allreduce(&diff,&gdiff,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD);

      if (gdiff <= epsilon) {
          break;
      }
      for (i=0; i<=nrl+1;i++) 
         for (j=0;j<=nc+1;j++)
            u[i][j] = w[i][j];

      set_bcs(u,nrl,nc,rank,npes);

      iterations++;
  } 

  if (rank==root) {
      cout<<"completed; iterations = "<<iterations<<" in "<<MPI_Wtime()-time<<"\n";
  }

  // Write solution to output file
  ofstream fp;
  fname+=argv[2]+to_string(rank);
  fp.open(fname,ios::out);
  for (i = 1; i <= nrl; i++) {
    for (j = 1; j <= nc; j++)
      fp<<u[i][j]<<" ";
    fp<<"\n";
  }
  fp.close();

  // All done!
  cout<<"wrote output file "<<fname<<"\n";

  MPI_Finalize();
}

void set_bcs(double **u, int nr, int nc, int rank, int npes) {
  /* Set boundary values.
   * This has an ice bath on the top edge.
   * Note: when plotting, 0,0 is the bottom.
   */

  if (rank==0) {
     for (int i=0;i<=nc+1;++i){
        u[0][i]=bottomBC;
     }
  }
  if (rank==npes-1) {
      for (int i=0;i<nc+1;++i){
        u[nr+1][i]=topBC;
      }
  }

  for (int i=0;i<=nr+1;++i){
      u[i][0]=edgeBC;
      u[i][nc+1]=edgeBC;
  }
}
