#include <iostream>
#include <cstring>
#include <mpi.h>

using namespace std;

int main (int argc, char *argv[]) {

    int i, j;
    int N;

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
    N=12;
    nr=N;
    nc=N;

    //Use ncl for clarity
    ncl=nc;

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

    cout<<"Topology "<<rank<<" "<<up<<" "<<down<<endl;

    int nrows=nrl+2; int ncols=ncl+2; 
    double **w=new double*[nrows];
    double *wptr=new double[(nrows)*(ncols)];

    for (int i=0;i<nrl+2;++i,wptr+=ncl+2) {
       w[i] = wptr;
    }

    //Initialize 

    for (int i=0; i<=nrl+1; ++i) {
        for (int j=0; j<=ncl+1; ++j) {
            w[i][j]=50.;
        }
    }

    for ( int j=0; j<=ncl+1; ++j ) {
        w[1][j]=(double)(rank+1)*2.;
        w[nrl][j]=(double)(rank+1)*2.5;
    }

    double topBC=0.;
    double bottomBC=200.;
    double leftBC=100.;
    double rightBC=100.;

    for (int i=0;i<=nrl+1;++i){
        w[i][0]=leftBC;
        w[i][ncl+1]=rightBC;
    }

    if (rank==0) {
       for (int i=0;i<=ncl+1;++i){
          w[0][i]=topBC;
       }
    }
    if (rank==nprocs-1) {
        for (int i=0;i<+ncl+1;++i){
            w[nrl+1][i]=bottomBC;
       }
    }

    //starting
    //This forces the output to show one rank at a time. It's not efficient.
    int uwsize=(nrl+2)*(ncl+2);
    double *u =  new double[uwsize];
    memset(u,0.,uwsize);
    int position;

    MPI_Barrier(MPI_COMM_WORLD);

    if ( rank == 0 ) {
        cout<<"---W Before for rank 0---"<<endl;
        for (int i=0;i<nrl+2;i++) {
            for (int j=0;j<ncl+2;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<endl;
        }
        for (int n=1;n<nprocs;++n) {

            memset(u,0.,uwsize);
            MPI_Recv(u,uwsize,MPI_DOUBLE,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,&status);
            cout<<"--Before for rank "<<status.MPI_SOURCE<<endl;
            position=0;
            for (int i=0;i<=nrl+1;i++) {
                for (int j=0;j<=ncl+1;j++) {
                    cout<<u[position++]<<" ";
                }
                cout<<endl;
            }
        }
    }
    else {

        // Pack the 2D array into the buffer
        cout<<"Size "<<uwsize<<" "<<nrl<<" "<<ncl<<" "<<nrl+2<<" "<<ncl+2<<endl;
        position=0;
        for (int i=0; i<=nrl+1; i++) {
            for (int j=0;j<=ncl+1; j++) {
                u[position++]=w[i][j];
            }
        }

        MPI_Send(u,uwsize, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);

    }

    MPI_Barrier(MPI_COMM_WORLD);
    //end of test section


    MPI_Sendrecv(&w[1][0],ncl+2, MPI_DOUBLE,up,tag,&w[nrl+1][0],
                          ncl+2, MPI_DOUBLE,down,tag,MPI_COMM_WORLD,&status);

    MPI_Sendrecv(&w[nrl][0],ncl+2,MPI_DOUBLE,down,tag,&w[0][0],
                             ncl+2,MPI_DOUBLE,up,tag,MPI_COMM_WORLD,&status);

       //Print results
    MPI_Barrier(MPI_COMM_WORLD);

    if ( rank == 3 ) {
        cout<<"---After for rank --"<<endl;
        for (int i=0;i<nrl+2;i++) {
            for (int j=0;j<ncl+2;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<endl;
        }

    }
    MPI_Barrier(MPI_COMM_WORLD);

    if ( rank == 0 ) {
        cout<<"---After for rank 0---"<<endl;
        for (int i=0;i<nrl+2;i++) {
            for (int j=0;j<ncl+2;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<endl;
        }
        for (int n=1;n<nprocs;++n) {

            memset(u,0.,uwsize);
            MPI_Recv(u,uwsize,MPI_DOUBLE,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,&status);
            cout<<"--After for rank "<<status.MPI_SOURCE<<endl;
            position=0;
            for (int i=0;i<=nrl+1;i++) {
                for (int j=0;j<=ncl+1;j++) {
                    cout<<u[position++]<<" ";
                }
                cout<<endl;
            }
        }
    }
    else {

        // Pack the 2D array into the buffer
        position=0;
        for (int i=0; i<=nrl+1; i++)
            for (int j=0;j<=ncl+1; j++)
                u[position++]=w[i][j];

        MPI_Send(u,uwsize, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);

    }

    MPI_Finalize();

}

