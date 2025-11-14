
#include <iostream> 
#include <iomanip> 
#include <cstring>
#include <mpi.h> 

using namespace std; 

/*#This example exchanges data among four rectangular domains with halos.  Most real codes use squares, but we want to illustrate how to use different dimensions.  Divide up the processes.  Either we require a perfect square, or we must specify how to distribute by row/column.  In a realistic program,
the process distribution (either the total, for a perfect square, or
the rows/columns) would be read in and we would need to check that the number
of processes requested is consistent with the decomposition. */

int main (int argc, char *argv[]) {

    int N, M;

    // Added for MPI
    int nrl, ncl;
    int rank, nprocs;
    int up, down, right, left;
    int tag=0;

    //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    N = 8;
    M = 12;

    int nproc_rows=2;
    int nproc_cols=3;

    if (nproc_rows*nproc_cols != nprocs) {
        cout<<"Number of rows times columns does not equal nprocs\n";
        return 1;
    }

    if (N%nproc_rows==0 && M%nproc_cols==0) {
        nrl = N/nproc_rows;
        ncl = M/nproc_cols;
    }
    else {
        cout<<"Number of ranks should divide the number of rows evenly.\n";
        return 2;
    }

    double **w=new double*[nrl+2];
    double *wptr=new double[(nrl+2)*(ncl+2)];

    for (int i=0;i<nrl+2;++i,wptr+=ncl+2) {
       w[i] = wptr;
    }

    double counter=1.;
    for ( int i = 0; i <= nrl+1; i++ ) {
         for (int j = 0; j <= ncl+2; j++ ) {
             w[i][j] = (rank+1)*counter;
             counter++;
         }
    }


    //Set up the topology assuming processes number left to right by row
    int my_row=rank/nproc_cols;
    int my_col=rank%nproc_cols;

    //Boundary conditions
    double topBC=0.;
    double bottomBC=200.;
    double rightBC=100.;
    double leftBC=100.;

    if (my_col==0) {
        for (int i=0;i<=nrl+1;++i){
            w[i][0]=leftBC;
        }
    }
    if (my_col==nproc_cols-1) {
        for (int i=0;i<=nrl+1;++i){
            w[i][ncl+1]=rightBC;
        }
    }

    if (my_row==0) {
       for (int i=0;i<=ncl+1;++i){
          w[0][i]=topBC;
       }
    }
    if (my_row==nproc_rows-1) {
        for (int i=0;i<=ncl+1;++i){
            w[nrl+1][i]=bottomBC;
       }
    }

    //Find my neighbors
    if (my_row==0) {
        up=MPI_PROC_NULL;
    }
    else {
        up=rank-nproc_cols;
    }

    if (my_row==nproc_rows-1) {
        down=MPI_PROC_NULL;
    }
    else {
        down= rank+nproc_cols;
    }

    if (my_col==0) {
        left=MPI_PROC_NULL;
    }
    else {
        left= rank-1;
    }

    if (my_col==nproc_cols-1) {
        right=MPI_PROC_NULL;
    }
    else {
        right = rank+1;
    }

    cout<<"Topology "<<rank<<" "<<up<<" "<<down<<" "<<left<<" "<<right<<endl;

    MPI_Status status;

    cout<<"Topology "<<rank<<" "<<up<<" "<<down<<" "<<left<<" "<<right<<endl;
    //Force showing all of one rank array at a time.  Not efficient.
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
        position=0;
        for (int i=0; i<=nrl+1; i++) 
            for (int j=0;j<=ncl+1; j++) 
                u[position++]=w[i][j];

        MPI_Send(u,uwsize, MPI_DOUBLE, 0, rank, MPI_COMM_WORLD);

    }
    MPI_Barrier(MPI_COMM_WORLD);


    //Set up the MPI type for columns
    MPI_Datatype col;
    MPI_Type_vector(nrl,1,ncl+2,MPI_DOUBLE,&col);
    MPI_Type_commit(&col);


    MPI_Sendrecv(&w[1][0],ncl+2, MPI_DOUBLE,up,tag,&w[nrl+1][0],
                          ncl+2, MPI_DOUBLE,down,tag,MPI_COMM_WORLD,&status);

    MPI_Sendrecv(&w[nrl][0],ncl+2,MPI_DOUBLE,down,tag,&w[0][0],
                            ncl+2,MPI_DOUBLE,up,tag,MPI_COMM_WORLD,&status);

    MPI_Sendrecv(&w[1][ncl],1,col,right,tag,&w[1][0],1,col,left,                                                           tag, MPI_COMM_WORLD, &status);
    MPI_Sendrecv(&w[1][1],1,col,left,tag,&w[1][ncl+1],1,col,right,                                                         tag, MPI_COMM_WORLD, &status);


    //Print results

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
    MPI_Barrier(MPI_COMM_WORLD);
    //end checking section

    
    //Type_free not really necessary but good practice
    MPI_Type_free(&col);

    MPI_Finalize();

    return 0;

}

