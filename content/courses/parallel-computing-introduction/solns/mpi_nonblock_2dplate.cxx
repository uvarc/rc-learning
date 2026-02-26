#include <iostream> 
#include <cmath>
#include <string>
#include <sstream>
#include <fstream>
#include <iomanip> 
#include <mpi.h> 


#define MAX_ITER 10000000

void set_bcs(int nr, int nc, int nrows, int ncols, int* coords, double **u);

using namespace std; 

int main (int argc, char *argv[]) {

    double diff;            // Change in value
    int N, M;
    int iterations = 0;
    int diffInterval;
    double epsilon;

    // Added for MPI
    int nrows, ncols;
    int nrl, ncl;
    int rank, nprocs;
    int errcode;
    int up, down, right, left;
    int tag=0;
    double gdiff;

    //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    /* check number of parameters and read in epsilon, filename, optionally size */
   if (argc < 3) {
     printf ("USAGE:  %s epsilon output-file <N> <M>\n", argv[0]);
     MPI_Abort(MPI_COMM_WORLD,errcode);
     return 1;
   }
   else {
      stringstream ssepsilon;
      ssepsilon<<argv[1];
      if (ssepsilon.fail()) {
         printf ("Error reading in epsilon value\n");
         MPI_Abort(MPI_COMM_WORLD,errcode);
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
           cout<<"Error converting row dimension \n";
           MPI_Abort(MPI_COMM_WORLD,errcode);
           return 2;
         }
         ssnr>>N;
         M=N;
      }
      if (argc==5) {
        stringstream ssnr;
        ssnr<<argv[3];
        if (ssnr.fail()) {
            cout<<"Error converting row dimension \n";
            MPI_Abort(MPI_COMM_WORLD,errcode);
            return 2;
        }
        ssnr>>N;
        stringstream ssnc;
        ssnc<<argv[4];
        if (ssnc.fail()) {
            cout<<"Error converting row dimension \n";
            MPI_Abort(MPI_COMM_WORLD,errcode);
            return 2;
        }
        ssnc>>M;
     }
   }
   //For simplicity, we will limit this code to perfect square process count
   double rows=sqrt(nprocs);
   nrows=(int)rows;
   if ( nrows*nrows != nprocs ) {
       cout<<"This code requires a perfect square number of processes\n";
       MPI_Abort(MPI_COMM_WORLD,errcode);
       return 1;
   }
   else {
       ncols=nrows;
   }

   //Strong scaling
   //Weak scaling would set nrl=N and ncl=M
   if (N%nrows==0 && M%ncols==0) {
       nrl = N/nrows;
       ncl = M/ncols;
   }
   else {
       cout<<"Number of ranks should divide the total number of rows/cols evenly.\n";
       MPI_Abort(MPI_COMM_WORLD,errcode);
   }

    double **u=new double*[nrl+2];
    double **w=new double*[nrl+2];
    double **diffs=new double*[nrl+2];

    double *uptr=new double[(nrl+2)*(ncl+2)];
    double *wptr=new double[(nrl+2)*(ncl+2)];
    double *dptr=new double[(nrl+2)*(ncl+2)];

    for (int i=0;i<nrl+2;++i,uptr+=ncl+2) {
       u[i] = uptr;
    }
    for (int i=0;i<nrl+2;++i,wptr+=ncl+2) {
       w[i] = wptr;
    }
    for (int i=0;i<nrl+2;++i,dptr+=ncl+2) {
       diffs[i] = dptr;
    }

    //create Cartesian topology
    int ndims=2;
    int dims[2]={nrows,ncols};
    int periods[2]={0,0};
    int reorder=0;
    MPI_Comm grid_comm;
    int grid_rank;
    int grid_coords[2];

    MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, &grid_comm);
    MPI_Comm_rank(grid_comm, &grid_rank);
    MPI_Cart_coords(grid_comm, grid_rank, ndims, grid_coords);

    int direction=0;
    int displ=1;
    MPI_Cart_shift(grid_comm, direction, displ, &up, &down);

    direction=1;
    MPI_Cart_shift(grid_comm, direction, displ, &left, &right);

    //Set up the MPI type for columns
    MPI_Datatype col;
    MPI_Type_vector(nrl+2,1,ncl+2,MPI_DOUBLE,&col);
    MPI_Type_commit(&col);

    int nrequests=8;
    MPI_Request requests[nrequests];

    //Initialize solution matrices
    for ( int i = 0; i <= nrl+1; i++ ) {
         for (int j = 0; j <= ncl+1; j++ ) {
             u[i][j] = 0.;
             w[i][j] = 0.;
         }
    }
    set_bcs(nrl, ncl, nrows, ncols, grid_coords, u);

    diffInterval=1;

    double time0=MPI_Wtime();

  // Compute steady-state solution
  // The diffs array is mostly to make it simpler to extend the testing interval

  if (rank==0) {
      cout<<"Running until the difference is <="<<epsilon<<" with size "<<N<<"x"<<M<<"\n";
  }

  while ( iterations<=MAX_ITER ) {
     // reset diff each time through to get max abs diff
     // max_element doesn't work great for twod arrays and is often slow
     diff=.8*epsilon;

     MPI_Irecv(&u[0][1], ncl, MPI_DOUBLE, up, tag, MPI_COMM_WORLD, &requests[0]);
     MPI_Irecv(&u[nrl+1][1], ncl, MPI_DOUBLE, down, tag, MPI_COMM_WORLD, &requests[1]);
     MPI_Irecv(&u[0][0], 1, col, left, tag, MPI_COMM_WORLD, &requests[2]);
     MPI_Irecv(&u[0][ncl+1], 1, col, right, tag, MPI_COMM_WORLD, &requests[3]);

     MPI_Isend(&u[1][1], ncl, MPI_DOUBLE, up, tag, MPI_COMM_WORLD, &requests[4]);

     MPI_Isend(&u[nrl][1],ncl, MPI_DOUBLE, down, tag, MPI_COMM_WORLD, &requests[5]);
     MPI_Isend(&u[0][ncl], 1, col, right, tag, MPI_COMM_WORLD, &requests[6]);
     MPI_Isend(&u[0][1], 1, col, left, tag, MPI_COMM_WORLD, &requests[7]);

     MPI_Waitall(nrequests,requests,MPI_STATUS_IGNORE);

     for (int i=1; i<=nrl;i++) {
        for (int j=1;j<=ncl;j++) {
            w[i][j] = (u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1])/4.0;
            diffs[i][j] = abs(w[i][j] - u[i][j]);
         }
     }
     if (iterations%diffInterval==0) {
        for (int i=1; i<=nrl;i++) {
           for (int j=1;j<=ncl;j++) {
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

     //Set halo values
     /*
     */
     for (int j=0; j<=ncl+1; j++) {
         w[0][j]=u[0][j];
         w[nrl+1][j]=u[nrl+1][j];
     }
     for (int i=0;i<=nrl+1; i++) {
         w[i][0]=u[i][0];
         w[i][ncl+1]=u[i][ncl+1];
     }

     for (int i=0; i<=nrl+1;i++) {
        for (int j=0;j<=ncl+1;j++) {
            u[i][j] = w[i][j];
        }
     }

     set_bcs(nrl, ncl, nrows, ncols, grid_coords, u);

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
  string fname=argv[2]+to_string(grid_coords[0])+to_string(grid_coords[1]);
  fp.open(fname,ios::out);
  for (int i = 1; i <= nrl; i++) {
     for (int j = 1; j <= ncl; j++) {
        fp<<u[i][j]<<" ";
     }
     fp<<"\n";
  }
  fp.close();

  //Type_free not really necessary but good practice
   MPI_Type_free(&col);
   MPI_Comm_free(&grid_comm);
   MPI_Finalize();

   return 0;

}

void set_bcs(int nr, int nc, int nrows, int ncols, int* coords, double **u) {

  /* Set boundary values.
   * This has an ice bath on the top edge.
   * Note: when plotting, 0,0 is the bottom.
   */

  double topBC=0.;
  double bottomBC=100.;
  double leftBC=100.;
  double rightBC=100.;

  if (coords[0]==0) {
     for (int i=0;i<=nc+1;++i){
         u[0][i]=topBC;
     }
  } else if (coords[0]==nrows-1) {
      for (int i=0;i<=nc+1;++i){
         u[nr+1][i]=bottomBC;
      }
  }
  if (coords[1]==0) {
      for (int i=0;i<=nr+1;++i){
         u[i][0]=leftBC;
      }
  } else if (coords[1]==ncols-1) {
     for (int i=0;i<=nr+1;++i){
        u[i][nc+1]=rightBC;
     }
  }

}
