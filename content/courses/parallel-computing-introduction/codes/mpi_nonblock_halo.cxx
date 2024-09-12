#include <iostream>
#include <mpi.h>

using namespace std;

int main (int argc, char *argv[]) {

    int i, j;
    int N;

    double topBC=100;
    double bottomBC=0.;
    double edgeBC=100.;

    // Added for MPI
    int nr, nrl, nc, ncl;
    int rank, nprocs;
    MPI_Status status;
    int root=0, tag=0;
    int up, down;

     //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

   //Usually we would read in nr and nc; they represent the global grid RxC;
   //For this case they are both N.
    N=500;
    nr=N;
    nc=N;

    //Use ncl for clarity
    ncl=nc;

    //Weak scaling
    //nrl=nr;

    //Strong scaling
    if (nr%nprocs!=0) {
        MPI_Finalize();
        cout<<"Not an even division of the arrays.\n";
        return 0;
    } 
    else {
    nrl=nr/nprocs;
    }

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
    //Two extra rows and columns for boundary conditions
    int nrows=nrl+2;
    int ncols=ncl+2;
    double **w=new double*[nrows];
    double *wptr=new double[(nrows)*(ncols)];

    for (i=0;i<(nrl+2);++i,wptr+=ncols)
       w[i] = wptr;

    if (rank==0) {
       for (int i=0;i<=ncl+1;++i){
          w[0][i]=bottomBC;
       }
    }
    if (rank==nprocs-1) {
        for (int i=0;i<ncl+1;++i){
            w[nrl+1][i]=topBC;
       }
    }

    for (int i=0;i<=nrl+1;++i){
        w[i][0]=edgeBC;
        w[i][ncl+1]=edgeBC;
    }

    //Initialize interior only
    for ( i = 1; i <= nrl; i++ ) {
         for (j = 1; j <= ncl; j++ ) {
             w[i][j] = 50.;
         }
    }

    MPI_Request requests[4];
    int nrequests=4;

    MPI_Irecv(&w[nrl+1][1], ncl, MPI_DOUBLE, down, tag, MPI_COMM_WORLD, &requests[0]);
    MPI_Irecv(&w[0][1], ncl, MPI_DOUBLE, up, tag, MPI_COMM_WORLD, &requests[1]);

    MPI_Isend(&w[1][1], ncl, MPI_DOUBLE, up, tag, MPI_COMM_WORLD, &requests[2]);
    MPI_Isend(&w[nrl][1],ncl,MPI_DOUBLE, down, tag, MPI_COMM_WORLD, &requests[3]);

    MPI_Status status_arr[4];

    MPI_Waitall(nrequests,requests,status_arr);

    //Spot-check results
    for (i=0;i<nprocs;++i) {
        if (i==rank) {
	    cout<<i<<" "<<w[0][ncl/2]<<" "<<w[nrl+1][ncl/2]<<endl;
        }
    }	

    MPI_Finalize();

}

