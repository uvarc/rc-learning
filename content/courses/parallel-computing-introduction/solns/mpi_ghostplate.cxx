#include <iostream>
#include <cmath>
#include <string>
#include <sstream>
#include <fstream> 
#include <iomanip>
#include <mpi.h>
   

#define MAX_ITER 10000000

void set_bcs(int nghosts, int nr, int nc, int nrows, int ncols, int* coords, double **u);

using namespace std; 

int main (int argc, char *argv[]) {


    double diff;            // Change in value
    int N, M;
    int iteration = 0;
    int diffInterval;
    double epsilon;


    // Added for MPI
    int nrows, ncols;
    int nrl, ncl;
    int rank, grid_rank, nprocs;
    int up, down, right, left;
    int tag=0;
    int nghosts, w_halo;
    int errcode;
    double gdiff;

    //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

   //check number of parameters and read in epsilon, filename, optionally size
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


    //Number of ghost zones
    nghosts=1;

    w_halo=nghosts*2;
    int nrl_total=nrl+w_halo;
    int ncl_total=ncl+w_halo;

    double **u=new double*[nrl_total];
    double *uptr=new double[(nrl_total)*(ncl_total)];

    double **w=new double*[nrl_total];
    double *wptr=new double[(nrl_total)*(ncl_total)];

    for (int i=0;i<nrl_total;++i,wptr+=ncl_total) {
       w[i] = wptr;
    }

    for (int i=0;i<nrl_total;++i,uptr+=ncl_total) {
       u[i] = uptr;
    }

    //Declare one-d diff array for faster access.
    int dsize = nrl*ncl;
    double *diffs = new double[dsize];

    for ( int i = 0; i < nrl_total; i++ ) {
         for (int j = 0; j < ncl_total; j++ ) {
             u[i][j] = 0.;
             w[i][j] = 0.;
         }
    }

    //Set up the topology assuming processes number left to right by row
    const int ndims=2;
    int dims[2]={nrows,ncols};
    int periods[2]={0,0};
    int reorder=0;
    MPI_Comm grid_comm;
    int grid_coords[2];

    MPI_Cart_create(MPI_COMM_WORLD, ndims, dims, periods, reorder, &grid_comm);
    MPI_Comm_rank(grid_comm, &grid_rank);
    MPI_Cart_coords(grid_comm, grid_rank, ndims, grid_coords);

    int direction=0;
    int displ=1;
    MPI_Cart_shift(grid_comm, direction, displ, &up, &down);

    direction=1;
    MPI_Cart_shift(grid_comm, direction, displ, &left, &right);

    set_bcs(nghosts, nrl, ncl, nrows, ncols, grid_coords, u);

    int starts[ndims]; 
    int subsizes[ndims];

    int sizes[]={nrl_total,ncl_total};

    MPI_Datatype sbuf_left,rbuf_right,sbuf_right,rbuf_left;
    MPI_Datatype sbuf_up,rbuf_down,sbuf_down,rbuf_up;

    //Set up the MPI type for row buffers
    //
    subsizes[0]=nghosts; subsizes[1]=ncl_total;

    starts[0]=nghosts; starts[1]=0;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&sbuf_up);
    MPI_Type_commit(&sbuf_up);

    starts[0]=nrl+nghosts; starts[1]=0;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&rbuf_down);
    MPI_Type_commit(&rbuf_down);

    starts[0]=nrl; starts[1]=0;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&sbuf_down);
    MPI_Type_commit(&sbuf_down);

    starts[0]=0; starts[1]=0;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&rbuf_up);
    MPI_Type_commit(&rbuf_up);

    //Set up MPI types for columns
    //
    subsizes[0]=nrl; subsizes[1]=nghosts;

    starts[0]=nghosts; starts[1]=ncl;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&sbuf_right);
    MPI_Type_commit(&sbuf_right);

    starts[0]=nghosts; starts[1]=0;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&rbuf_left);
    MPI_Type_commit(&rbuf_left);

    starts[0]=nghosts; starts[1]=nghosts;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&sbuf_left);
    MPI_Type_commit(&sbuf_left);

    starts[0]=nghosts; starts[1]=ncl+nghosts;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_DOUBLE,&rbuf_right);
    MPI_Type_commit(&rbuf_right);

    //Buffers set up

    MPI_Status status;

    int nrequests=8;
    MPI_Request requests[nrequests];

    diffInterval=1;

    if (rank==0) {
        cout<<"Running until the difference is <="<<epsilon<<" with size "<<N<<"x"<<M<<"\n";
    }

    // Compute steady-state solution
    double time0=MPI_Wtime();

    while ( iteration <= MAX_ITER ) {

        MPI_Irecv(&u[0][0], 1, rbuf_up, up, tag, grid_comm, &requests[0]);
        MPI_Irecv(&u[0][0], 1, rbuf_down, down, tag, grid_comm, &requests[1]);
        MPI_Irecv(&u[0][0], 1, rbuf_left, left, tag, grid_comm, &requests[2]);
        MPI_Irecv(&u[0][0], 1, rbuf_right, right, tag, grid_comm, &requests[3]);

        MPI_Isend(&u[0][0], 1, sbuf_down, down, tag, grid_comm, &requests[5]);
        MPI_Isend(&u[0][0], 1, sbuf_up, up, tag, grid_comm, &requests[4]);
        MPI_Isend(&u[0][0], 1, sbuf_right, right, tag, grid_comm, &requests[6]);
        MPI_Isend(&u[0][0], 1, sbuf_left, left, tag, grid_comm, &requests[7]);

        MPI_Waitall(nrequests,requests,MPI_STATUS_IGNORE);

        for (int i=nghosts; i<nrl+nghosts;i++) {
           for (int j=nghosts;j<ncl+nghosts;j++) {
               w[i][j] = (u[i-1][j] + u[i+1][j] + u[i][j-1] + u[i][j+1])/4.0;
               diffs[(i-nghosts)*ncl+j-nghosts] = abs(w[i][j] - u[i][j]);
           }
        }

        if (iteration%diffInterval==0) {
             diff=diffs[0];
             for (int i=1;i<dsize;i++) {
                 if (diff<diffs[i]) {
                     diff=diffs[i];
                 }
             }
        }

        //Find max of diff in all the processors.
        MPI_Allreduce(&diff,&gdiff,1,MPI_DOUBLE,MPI_MAX,grid_comm);
        if (gdiff <= epsilon)
             break;

        //Update u
        for (int i=0; i<nrl_total; ++i){
            for (int j=0; j<ncl_total; ++j) {
                 u[i][j]=w[i][j];
            }
        }

        //Reset halo values
        for (int i=0;i<nghosts;i++) {
            for (int j=0; j<ncl_total; j++) {
                w[i][j]=u[i][j];
                w[nrl+nghosts+i][j]=u[nrl+nghosts+i][j];
            }
        }
        for (int i=0;i<nrl_total;i++) {
            for (int j=0;j<nghosts;j++) {
                w[i][j]=u[i][j];
                w[i][ncl+nghosts+i]=u[i][ncl+nghosts+i];
            }
        }

        set_bcs(nghosts, nrl, ncl, nrows, ncols, grid_coords, u);

        iteration++;
    } //end of computation

    if (iteration>MAX_ITER) {
         if (rank==0) {
            cout<<"Warning: maximum iteration exceeded\n";
         }
    }


    double totalTime=(MPI_Wtime()-time0);
    if (rank==0) {
        cout << "Completed "<<iteration<<" iterations; time "<<totalTime<<endl;
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
    MPI_Type_free(&sbuf_up);
    MPI_Type_free(&sbuf_down);
    MPI_Type_free(&rbuf_up);
    MPI_Type_free(&rbuf_down);
    MPI_Type_free(&sbuf_left);
    MPI_Type_free(&sbuf_right);
    MPI_Type_free(&rbuf_left);
    MPI_Type_free(&rbuf_right);

    MPI_Comm_free(&grid_comm);
    MPI_Finalize();

    return 0;

}


void set_bcs(int nghosts,int nr, int nc, int nrows, int ncols, int* coords, double **u) {

  /* Set boundary values.
   * This has an ice bath on the top edge.
   * Note: when plotting, 0,0 is the bottom.
   */

  int nr_total=nr+2*nghosts;
  int nc_total=nc+2*nghosts;

  double topBC=0.;
  double bottomBC=100.;
  double leftBC=100.;
  double rightBC=100.;

  if (coords[0]==0) {
      for (int i=0; i<nghosts; ++i) {
          for (int j=0;j<nc_total;++j){
             u[i][j]=topBC;
          }
      }
  } else if (coords[0]==nrows-1) {
      for (int i=nr+nghosts; i<nr_total; ++i) {
          for (int j=0;j<nc_total;++j){
                u[i][j]=bottomBC;
          }
      }
  }

  if (coords[1]==0) {
       for (int i=0; i<nr_total; ++i){
           for (int j=0; j<nghosts; ++j) {
               u[i][j]=leftBC;
           }
       }
  } else if (coords[1]==ncols-1) {
       for (int i=0; i<nr_total; ++i) {
           for (int j=nc+nghosts;j<nc_total;++j){
               u[i][j]=rightBC;
           }
       }
  }

}
