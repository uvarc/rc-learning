#include <cstring>
#include <iostream> 
#include <iomanip> 
#include <mpi.h> 

using namespace std; 

/*#This example exchanges data among four rectangular domains with halos.  Most real codes use squares, but we want to illustrate how to use different dimensions. 
*/

int main (int argc, char *argv[]) {

    int N, M;

    // Added for MPI
    int nrl, ncl;
    int rank, grid_rank, nprocs;
    int up, down, right, left;
    int tag=0;
    int nghosts, w_halo;

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

    nghosts=2;

    w_halo=nghosts*2;
    int nrl_total=nrl+w_halo;
    int ncl_total=ncl+w_halo;
    double **w=new double*[nrl_total];
    double *wptr=new double[(nrl_total)*(ncl_total)];

    for (int i=0;i<nrl_total;++i,wptr+=ncl_total) {
       w[i] = wptr;
    }

    double counter=1.;
    for ( int i = 0; i < nrl_total; i++ ) {
         for (int j = 0; j < ncl_total; j++ ) {
             w[i][j] = (rank+1)*counter;
             counter++;
         }
    }

    //Set up the topology assuming processes number left to right by row
    const int ndims=2;
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
    double topBC=100.;
    double bottomBC=400.;
    double rightBC=350.;
    double leftBC=350.;

    if (grid_coords[0]==0) {
       for (int i=0; i<nghosts; ++i) {
           for (int j=0;j<ncl_total;++j){
               w[i][j]=topBC;
           }
       }
    }
    if (grid_coords[0]==rows-1) {
        for (int i=nrl+nghosts; i<nrl_total; ++i) {
            for (int j=0;j<ncl_total;++j){
                w[i][j]=bottomBC;
            }
       }
    }

    if (grid_coords[1]==0) {
        for (int i=0; i<nrl_total; ++i){
            for (int j=0; j<nghosts; ++j) {
                w[i][j]=leftBC;
            }
        }
    }

    if (grid_coords[1]==cols-1) {
        for (int i=0;i<nrl_total;++i){
            for (int j=ncl+nghosts; j<ncl_total; ++j) {
                w[i][j]=rightBC;
            }
        }
    }


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

    //We declared our array as an array of pointers, so we must align the 
    //beginning pointer for MPI at the start of the actual data

    MPI_Sendrecv(&(w[0][0]),1,sbuf_up,up,tag,&(w[0][0]),1,rbuf_down,down,tag,grid_comm,&status);
    MPI_Sendrecv(&(w[0][0]),1,sbuf_down,down,tag,&(w[0][0]),1,rbuf_up,up,tag,grid_comm,&status);

    MPI_Sendrecv(&(w[0][0]),1,sbuf_right,right,tag,&(w[0][0]),1,rbuf_left,left,tag,grid_comm,&status);
    MPI_Sendrecv(&(w[0][0]),1,sbuf_left,left,tag,&(w[0][0]),1,rbuf_right,right,tag,grid_comm,&status);


    //Verification. Usually done with plots in real codes.
    //Simplified from earlier examples, we'll just spot-check
    //
    double *u=new double[(nrl_total)*(ncl_total)];
    int uwsize=(nrl_total)*(ncl_total);
    memset(u,0.,uwsize);
    int position;

    MPI_Barrier(grid_comm);
    if ( grid_rank == 0 ) {
        MPI_Recv(u,uwsize,MPI_DOUBLE,1,1,grid_comm,&status);
        cout<<"Ranks 0 and 1  Check columns"<<endl;
        position=0;
        for (int i=0;i<nrl_total;i++) {
            for (int j=0;j<ncl_total;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<" | ";
            for (int j=0;j<ncl_total;j++) {
                cout<<u[position++]<<" ";
            }
            cout<<endl;
        }
        cout<<endl;
    }

    if ( grid_rank == 1 ) {
        position=0;
        for (int i=0; i<nrl_total; i++)
            for (int j=0;j<ncl_total; j++)
                u[position++]=w[i][j];

        MPI_Send(u,uwsize,MPI_DOUBLE,0,1,grid_comm);
        cout<<"Rank 1 sent to rank 0 "<<endl;

    }
    MPI_Barrier(grid_comm);


    //Check columns on other side
    //
    memset(u,0.,uwsize);
    if ( grid_rank == 1 ) {
        MPI_Recv(u,uwsize,MPI_DOUBLE,2,2,grid_comm,&status);
        cout<<"Ranks 1 and 2 Check columns"<<endl;
        position=0;
        for (int i=0;i<nrl_total;i++) {
            for (int j=0;j<ncl_total;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<" | ";
            for (int j=0;j<ncl_total;j++) {
                cout<<u[position++]<<" ";
            }
            cout<<endl;
        }
        cout<<endl;
    }

    if ( grid_rank == 2 ) {
        position=0;
        for (int i=0; i<nrl_total; i++)
            for (int j=0;j<ncl_total; j++)
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
        for (int i=0;i<nrl_total;i++) {
            for (int j=0;j<ncl_total;j++) {
                cout<<w[i][j]<<" ";
            }
            cout<<endl;
        }

        cout<<"-=---------------------------------------------------"<<endl;

        position=0;
        for (int i=0;i<nrl_total;i++) {
            for (int j=0;j<ncl_total;j++) {
                cout<<u[position++]<<" ";
            }
            cout<<endl;
        }
        cout<<endl;
    }

    if ( grid_rank == 3 ) {
        position=0;
        for (int i=0; i<nrl_total; i++)
            for (int j=0;j<ncl_total; j++)
                u[position++]=w[i][j];

        MPI_Send(u,uwsize,MPI_DOUBLE,0,3,grid_comm);

    }
    MPI_Barrier(grid_comm);
    //end checking section

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
