#include <iostream>
#include <cmath>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip>
#include <mpi.h>

#define MAX_ITER 10000000

void set_bcs(double **u, int nr, int nc, int rank, int nprocs);

using namespace std; 

int main (int argc, char *argv[]) {
  int i, j;
  double diff;            // Change in value 
  int N, M;
  int iterations = 0;
  int diffInterval;
  double epsilon;

  // Added for MPI
  int nrl, ncl;
  int rank, nprocs;
  MPI_Status status;
  int root=0, tag=0;
  int up, down;
  double gdiff;

  //Initialize MPI
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);

/* check number of parameters and read in epsilon, filename, optionally size */
  if (argc < 3) {
    printf ("USAGE:  %s epsilon output-file <N> <M>\n", argv[0]);
    MPI_Finalize();
    return 1;
  }
  else {
     stringstream ssepsilon;
     ssepsilon<<argv[1];
     if (ssepsilon.fail()) {
         printf ("Error reading in epsilon value\n");
         MPI_Finalize();
         return 2;
     }
     ssepsilon>>epsilon;

     if (argc==3) {
        N=500;
	    M=500;
     }
     if (argc==4) {
        stringstream ssnr;
        ssnr<<argv[3];
        if (ssnr.fail()) {
            printf ("Error converting row dimension \n");
            MPI_Finalize();
            return 2;
	    }
        ssnr>>N;
	    M=N;
     }
     if (argc==5) {
        stringstream ssnr;
	    ssnr<<argv[3];
        if (ssnr.fail()) {
            printf ("Error converting row dimension \n");
            MPI_Finalize();
            return 2;
	    }
        ssnr>>N;
        stringstream ssnc;
        ssnc<<argv[4];
        if (ssnc.fail()) {
            printf ("Error converting row dimension \n");
            MPI_Finalize();
            return 2;
	    }
        ssnc>>M;
     }
  }

  //Weak scaling
  //nrl=N;

  //Strong scaling
  if (N%nprocs!=0) {
     MPI_Finalize();
     cout<<"Not an even division of the arrays.\n";
     return 3;
  }
  else {
     nrl=N/nprocs;
  }

  //Both
  ncl=M;

  //Find my neighbors
  if (rank==0) {
      up=MPI_PROC_NULL;
  }
  else {
      up=rank-1;
  }

  if (rank==nprocs-1) {
      down=MPI_PROC_NULL;
  }
  else {
      down=rank+1;
  }

  int nrows=nrl+2;
  int ncols=ncl+2;
  double **u=new double*[nrows];  // Old values
  double **w=new double*[nrows];  // New values
  double **diffs=new double*[nrows];  // Diffs

  double *uptr=new double[(nrows)*(ncols)];
  double *wptr=new double[(nrows)*(ncols)];
  double *dptr=new double[(nrows)*(ncols)];

  for (i=0;i<nrows;++i,uptr+=ncols)
     u[i] = uptr;
  for (i=0;i<nrows;++i,wptr+=ncols)
     w[i] = wptr;
  for (i=0;i<nrows;++i,dptr+=ncols)
     diffs[i] = dptr;

  // Set physical boundary conditions (overwrites estimate on physical edges)
  set_bcs(u, nrl, ncl, rank, nprocs);

  diffInterval=1;

  double time0=MPI_Wtime();

  // Compute steady-state solution 
  // The diffs array is mostly to make it simpler to extend the testing interval

  int nrequests=4;
  MPI_Request requests[nrequests];
  MPI_Status status_arr[4];

  if (rank==0) {
      cout<<"Running until the difference is <="<<epsilon<<" with size "<<N<<"x"<<M<<"\n";
  }

  while ( iterations<=MAX_ITER ) {
     // reset diff each time through to get max abs diff
     // max_element doesn't work great for twod arrays and is often slow
     diff=.8*epsilon;  

     //Exchange halo values (one ghost row each side)
     MPI_Irecv(&u[nrl+1][0], ncl+2, MPI_DOUBLE, down, tag, MPI_COMM_WORLD, &requests[0]);
     MPI_Irecv(&u[0][0], ncl+2, MPI_DOUBLE, up, tag, MPI_COMM_WORLD, &requests[1]);

     MPI_Isend(&u[1][0], ncl+2, MPI_DOUBLE, up, tag, MPI_COMM_WORLD, &requests[2]);
     MPI_Isend(&u[nrl][0],ncl+1,MPI_DOUBLE, down, tag, MPI_COMM_WORLD, &requests[3]);

     MPI_Waitall(nrequests,requests,status_arr);

     for (i=1; i<=nrl;i++) {
        for (j=1;j<=ncl;j++) {
            w[i][j] = (u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1])/4.0;
     	    diffs[i][j] = abs(w[i][j] - u[i][j]);
         }
     }

     //Set halo values
     for (int j=0; j<=ncl+1; j++) {
         w[0][j]=u[0][j];
         w[nrl+1][j]=u[nrl+1][j];
     }
     for (int i=0;i<=nrl+1; i++) {
         w[i][0]=u[i][0];
         w[i][ncl+1]=u[i][ncl+1];
     }

     if (iterations%diffInterval==0) {
        for (i=1; i<=nrl;i++) {
           for (j=1;j<=ncl;j++) {
	       if (diff<diffs[i][j]) {
		   diff=diffs[i][j];
	       }
	   }
	 }
         //Find max of diff in all the processors.
         MPI_Allreduce(&diff,&gdiff,1,MPI_DOUBLE,MPI_MAX,MPI_COMM_WORLD);
         if (gdiff <= epsilon) 
             break;
     }

     for (i=0; i<=nrl+1;i++) {
        for (j=0;j<=ncl+1;j++) {
            u[i][j] = w[i][j];
        }
     }

     set_bcs(u, nrl, ncl, rank, nprocs);

     iterations++;
  } //end of computation 

  if (iterations>MAX_ITER) {
     if (rank==0) {
        cout<<"Warning: maximum iterations exceeded\n";
     }
  }

  double totalTime=(MPI_Wtime()-time0);
  if (rank==0) {
      cout << "Completed "<<iterations<<" iterations; time "<<totalTime<<endl;
  }
  // Write solution to output file
  ofstream fp;
  string fname=argv[2]+to_string(rank);
  fp.open(fname,ios::out);
  for (i = 1; i <= nrl; i++) {
     for (j = 1; j <= ncl; j++) {
        fp<<u[i][j]<<" ";
     }
     fp<<"\n";
  }
  fp.close();

  // All done!
  MPI_Finalize();

  return 0;
}

void set_bcs(double **u, int nr, int nc, int rank, int nprocs) {

  /* Set boundary values.
   * This has an ice bath on the top edge.
   * Note: when plotting, 0,0 is the bottom.
   */

  double topBC=0;
  double bottomBC=100.;
  double leftBC=100.;
  double rightBC=100.;

  if (rank==0) {
     for (int i=0;i<=nc+1;++i){
        u[0][i]=topBC;
     }
  }
  if (rank==nprocs-1) {
      for (int i=0;i<=nc+1;++i){
        u[nr+1][i]=bottomBC;
      }
  }

  for (int i=1;i<=nr;++i){
      u[i][0]=leftBC;
      u[i][nc+1]=rightBC;
  }

}

