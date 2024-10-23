#include <iostream>
#include <iomanip>
#include <mpi.h>

using namespace std;

int main (int argc, char *argv[]) {

    int i, j;
    int N;

   // Added for MPI
    int nr, nc;
    int rank, nprocs;
    int root=0, tag=0;
    int src, dest;

     //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    N = nprocs;
    nr = N+2;
    nc = N;

    double **w=new double*[nr];
    double *wptr=new double[(nr)*(nc)];

    for (i=0;i<nr;++i,wptr+=nc) {
       w[i] = wptr;
    }

    for ( i = 0; i < nr; i++ ) {
         for (j = 0; j < nc; j++ ) {
             w[i][j] = 0.;
         }
    }

    double **u=new double*[nr];
    double *uptr=new double[(nr)*(nc)];

    for (i=0;i<nr;++i,uptr+=nc) {
       u[i] = uptr;
    }

    double counter=1.;
    for ( i = 0; i < nr; i++ ) {
         for (j = 0; j < nc; j++ ) {
             u[i][j] = counter;
             counter++;
         }
    }

    //#Cyclic sending
    if (rank == nprocs-1) {
        src=rank-1;
        dest=0;
    }
    else if (rank==0) {
        src=nprocs-1;
        dest=1;
    }
    else {
        src=rank-1;
        dest=rank+1;
    }

    //These values pick a total of nc (ncount) items, one item
    //(blocklength) taken for each nr (stride) items

    //The length of the column is the number of rows
    int ncount=nr;
    //The number of items picked from each stride is 1
    int blocklength=1;
    //The length of the row is the number of columns
    int stride=nc;

    MPI_Datatype cols;
    MPI_Type_vector(ncount,blocklength,stride,MPI_DOUBLE,&cols);
    MPI_Type_commit(&cols);

    int nrequests=2;
    MPI_Request requests[nrequests];

    if (rank==0) {
        MPI_Irecv(&w[0][0], 1, cols, src, tag, MPI_COMM_WORLD, &requests[0]);
        MPI_Isend(&u[0][0], 1, cols, dest, tag, MPI_COMM_WORLD, &requests[1]);
    }
    else if (rank==nprocs-1) {
        MPI_Irecv(&w[0][nprocs-1], 1, cols, src, tag, MPI_COMM_WORLD, &requests[0]);
        MPI_Isend(&u[0][nprocs-1], 1, cols, dest, tag, MPI_COMM_WORLD, &requests[1]);
    }
    else {
        MPI_Irecv(&w[0][rank], 1, cols, src, tag, MPI_COMM_WORLD, &requests[0]);
        MPI_Isend(&u[0][rank], 1, cols, dest, tag, MPI_COMM_WORLD, &requests[1]);
    }

    MPI_Status status_arr[nrequests];
    MPI_Waitall(nrequests,requests,status_arr);


    MPI_Type_free(&cols);

    //Try to print neatly

    //U is the same for each rank in this example

    if (rank==0) {
        cout<<"U"<<endl;
        for (i=0;i<nr;i++) {
            for (j=0;j<nc;j++) {
                cout<<setprecision(6)<<u[i][j]<<" ";
            }
            cout<<endl;
        }
    }

    MPI_Barrier(MPI_COMM_WORLD);
    cout<<endl;
    MPI_Barrier(MPI_COMM_WORLD);
    cout<<"W for rank "<<rank<<endl;
    for (i=0;i<nr;i++) {
        for (j=0;j<nc;j++) {
            cout<<setprecision(6)<<w[i][j]<<" ";
        }
        cout<<endl;
    }
    cout<<endl;

    MPI_Finalize();

}
