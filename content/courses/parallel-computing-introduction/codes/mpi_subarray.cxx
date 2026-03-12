#include <cstring>
#include <iostream> 
#include <iomanip> 
#include <mpi.h> 

using namespace std; 

// MPI subarray example

int main (int argc, char *argv[]) {

    int nprocs, rank;
    int tag=0;
    int root=0;

    //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    //Run with two processes only
    if ( nprocs != 2 ) {
        if ( rank==root) { 
            cout<<"Please run with two processes"<<endl; 
            MPI_Finalize();
            exit(1);
        }
    }

    int nrl=6;
    int ncl=8;

    int **w=new int*[nrl];
    int *wptr=new int[(nrl)*(ncl)];

    for (int i=0;i<nrl;++i,wptr+=ncl) {
       w[i] = wptr;
    }

    if (rank==root) {
        for ( int i = 0; i < nrl; i++ ) {
             for (int j = 0; j < ncl; j++ ) {
                 w[i][j] = i*100+j;
             }
        }
    }
    else {
        for ( int i = 0; i < nrl; i++ ) {
             for (int j = 0; j < ncl; j++ ) {
                 w[i][j] = 0;
             }
        }
    }

    //Set up subarrays
    //
    int ndims=2;
    int sizes[]={nrl,ncl};

    int starts[ndims]; 
    int subsizes[ndims];

    subsizes[0]=3; subsizes[1]=4;

    MPI_Datatype sendtype, recvtype;

    starts[0]=2; starts[1]=3;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_INT,&sendtype);
    MPI_Type_commit(&sendtype);

    starts[0]=3; starts[1]=2;
    MPI_Type_create_subarray(ndims,sizes,subsizes,starts,MPI_ORDER_C,MPI_INT,&recvtype);
    MPI_Type_commit(&recvtype);

    //Buffers set up

    // Send type from root to rank 1
    if ( rank==root ) {
        cout<<"Rank "<<rank<<" sending array:"<<endl;
        for (int i=0;i<nrl;i++) {
            for (int j=0;j<ncl;j++) {
                cout<<setfill('0')<<setw(3)<<w[i][j]<<" ";
            }
            cout<<endl;
        }
        MPI_Send(&w[0][0], 1, sendtype, 1, tag, MPI_COMM_WORLD);
    }
    else {
        MPI_Recv(&w[0][0], 1, recvtype, root, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
        cout<<" Rank "<<rank<<" received subarray: "<<endl;
        for (int i=0;i<nrl;i++) {
            for (int j=0;j<ncl;j++) {
                cout<<setfill('0')<<setw(3)<<w[i][j]<<" ";
            }
            cout<<endl;
        }
    }

    //Type_free not really necessary but good practice
    MPI_Type_free(&sendtype);
    MPI_Type_free(&recvtype);

    MPI_Finalize();

    return 0;

}
