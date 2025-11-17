
#include <iostream> 
#include <iomanip> 
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
    int rank, grid_rank, nprocs;
    int up, down, right, left;
    int tag=0;

    //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    N = 8;
    M = 12;

    int rows=2;
    int cols=3;

    if (rows*cols != nprocs) {
        cout<<"Number of rows times columns does not equal nprocs\n";
        return 1;
    }

    if (N%rows==0 && M%cols==0) {
        nrl = N/rows;
        ncl = M/cols;
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
    int ndims=2;
    int dims[2]={rows,cols};
    int periods[2]={1,0};
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

    cout<<"topo "<<grid_rank<<" "<<up<<" "<<down<<" "<<left<<" "<<right<<endl;

    //Boundary conditions
    double topBC=0.;
    double bottomBC=200.;
    double rightBC=100.;
    double leftBC=100.;

    if (grid_coords[0]==0) {
       for (int i=0;i<=ncl+1;++i){
          w[0][i]=topBC;
       }
    }
    if (grid_coords[0]==rows-1) {
        for (int i=0;i<=ncl+1;++i){
            w[nrl+1][i]=bottomBC;
       }
    }

    if (grid_coords[1]==0) {
        for (int i=0;i<=nrl+1;++i){
            w[i][0]=leftBC;
        }
    }
    if (grid_coords[1]==cols-1) {
        for (int i=0;i<=nrl+1;++i){
            w[i][ncl+1]=rightBC;
        }
    }

    //Set up the MPI type for columns
    MPI_Datatype col;
    MPI_Type_vector(nrl,1,ncl+2,MPI_DOUBLE,&col);
    MPI_Type_commit(&col);

    MPI_Status status;

    MPI_Sendrecv(&w[1][0],ncl+2, MPI_DOUBLE,up,tag,&w[nrl+1][0],
                          ncl+2, MPI_DOUBLE,down,tag,grid_comm,&status);

    MPI_Sendrecv(&w[nrl][0],ncl+2,MPI_DOUBLE,down,tag,&w[0][0],
                            ncl+2,MPI_DOUBLE,up,tag,grid_comm,&status);

    MPI_Sendrecv(&w[1][ncl],1,col,right,tag,&w[1][0],1,col,left,                                             tag, grid_comm, &status);
    MPI_Sendrecv(&w[1][1],1,col,left,tag,&w[1][ncl+1],1,col,right,                                           tag, grid_comm, &status);


    //Verification. Usually done with plots in real codes.
    //Simplified from earlier examples, we'll just spot-check
    //Same original values
    //
    double *u=new double[(nrl+2)*(ncl+2)];
    int uwsize=(nrl+2)*(ncl+2);
    memset(u,0.,uwsize);
    int position;

    MPI_Barrier(grid_comm);
    if ( grid_rank == 0 ) {
        MPI_Recv(u,uwsize,MPI_DOUBLE,1,1,grid_comm,&status);
        cout<<"Ranks 0 and 1  Check columns"<<endl;
        position=0;
        for (int i=0;i<nrl+2;i++) {
            for (int j=0;j<ncl+2;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<" | ";
            for (int j=0;j<ncl+2;j++) {
                cout<<u[position++]<<" ";
            }
            cout<<endl;
        }
        cout<<endl;
    }

    if ( grid_rank == 1 ) {
        position=0;
        for (int i=0; i<=nrl+1; i++)
            for (int j=0;j<=ncl+1; j++)
                u[position++]=w[i][j];

        MPI_Send(u,uwsize,MPI_DOUBLE,0,1,grid_comm);

    }
    MPI_Barrier(grid_comm);

    //Check columns on other side
    //
    memset(u,0.,uwsize);
    if ( grid_rank == 1 ) {
        MPI_Recv(u,uwsize,MPI_DOUBLE,2,2,grid_comm,&status);
        cout<<"Ranks 1 and 2 Check columns"<<endl;
        position=0;
        for (int i=0;i<nrl+2;i++) {
            for (int j=0;j<ncl+2;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<" | ";
            for (int j=0;j<ncl+2;j++) {
                cout<<u[position++]<<" ";
            }
            cout<<endl;
        }
        cout<<endl;
    }

    if ( grid_rank == 2 ) {
        position=0;
        for (int i=0; i<=nrl+1; i++)
            for (int j=0;j<=ncl+1; j++)
                u[position++]=w[i][j];

        MPI_Send(u,uwsize,MPI_DOUBLE,1,2,grid_comm);

    }
    MPI_Barrier(grid_comm);

    //Check periodic row exchanges
    //
    memset(u,0.,uwsize);
    if ( grid_rank == 0 ) {
        MPI_Recv(u,uwsize,MPI_DOUBLE,3,3,grid_comm,&status);
        cout<<"Ranks 0 and 3  Check rows including periodic exchange"<<endl;
        for (int i=0;i<nrl+2;i++) {
            for (int j=0;j<ncl+2;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<endl;
        }

        cout<<"-=---------------------------------------------------"<<endl;

        position=0;
        for (int i=0;i<nrl+2;i++) {
            for (int j=0;j<ncl+2;j++) {
                cout<<u[position++]<<" ";
            }
            cout<<endl;
        }
        cout<<endl;
    }

    if ( grid_rank == 3 ) {
        position=0;
        for (int i=0; i<=nrl+1; i++)
            for (int j=0;j<=ncl+1; j++)
                u[position++]=w[i][j];

        MPI_Send(u,uwsize,MPI_DOUBLE,0,3,grid_comm);

    }
    MPI_Barrier(grid_comm);
    //end checking section

    //Type_free not really necessary but good practice
    MPI_Type_free(&col);

    MPI_Comm_free(&grid_comm);

    MPI_Finalize();

    return 0;

}
