#include <cstring>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <string>
#include <mpi.h>

using namespace std;

int main (int argc, char *argv[]) {

    // Declarations for MPI
    int rank, nprocs;
    int errcode;
    MPI_Status mpi_stat;
    MPI_Info info;
    MPI_Offset offset;
    MPI_File fh;
    int root=0, tag=0;
    int mpi_err;

    // Check number of parameters and read in filename
    if (argc < 2) {
       printf ("USAGE:  %s output-file\n", argv[0]);
    exit(1);
    }
    const char *fname=argv[1];

    //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    // Hard-code sizes so we can see what we're doing
    // Row/column layout for ranks
    int nrows=4;
    int ncols=4;

    int nrl = 4;
    int ncl = 4;

    int N=nrl*nrows;
    int M=nrl*ncols;

    //Set up the topology
    int lrow=rank/ncols;
    int lcol=rank%ncols;

    int **loc_u=new int*[nrl];
    int *luptr=new int[(nrl)*(ncl)];

    for (int i=0;i<nrl;++i,luptr+=ncl) {
       loc_u[i] = luptr;
    }

    for ( int i = 0; i < nrl; i++ ) {
        for (int j = 0; j < ncl; j++ ) {
             loc_u[i][j] = rank+1;
         }
    }

    int lusize=nrl*ncl;

    //Write each segment to conventional file (the "old way")
    ofstream fp;
    string myfile=argv[1]+to_string(rank);
    fp.open(myfile,ios::out);
    for (int i = 0; i < nrl; i++) {
        for (int j = 0; j < ncl; j++) {
            fp<<loc_u[i][j]<<" ";
        }
        fp<<"\n";
    }
    fp.close();

    int gdims[]={N,M};
    int ldims[]={nrl,ncl};
    int starts[]={ncl*lrow,nrl*lcol};
    cout<<"Starts for rank "<<rank<<" are  "<<starts[0]<<" "<<starts[1]<<endl;

    MPI_Datatype locarray;
    MPI_Type_create_subarray(2,gdims,ldims,starts,MPI_ORDER_C,MPI_INT,&locarray);
    MPI_Type_commit(&locarray);

    int amode=MPI_MODE_CREATE | MPI_MODE_WRONLY;
    mpi_err=MPI_File_open(MPI_COMM_WORLD,fname,amode,MPI_INFO_NULL,&fh);

    if (mpi_err != MPI_SUCCESS) {
        cout<<"Unable to open file for writing\n";
        MPI_Finalize();
        exit(2);
    }

    offset=0;
    MPI_File_set_view(fh, offset, MPI_INT, locarray, "native", MPI_INFO_NULL);

    MPI_File_write_all(fh,&loc_u[0][0], lusize , MPI_INT, &mpi_stat);
    MPI_File_close(&fh);

    //All processes read file
    amode=MPI_MODE_RDONLY;
    mpi_err=MPI_File_open(MPI_COMM_WORLD,fname,amode,MPI_INFO_NULL,&fh);
        if ( mpi_err != MPI_SUCCESS) {
       MPI_Finalize();
       cout<<"Unable to open MPI file for reading\n";
       exit(2);
    }

    int **rbuf=new int*[nrl];
    int *rptr=new int[(nrl)*(ncl)];

    for (int i=0;i<nrl;++i,rptr+=ncl) {
       rbuf[i] = rptr;
    }

    for ( int i = 0; i < nrl; i++ ) {
        for (int j = 0; j < ncl; j++ ) {
             rbuf[i][j] = 0;
         }
    }

    MPI_File_set_view(fh, offset, MPI_INT, locarray, "native", MPI_INFO_NULL);
    MPI_File_read_all(fh, &rbuf[0][0], lusize, MPI_INT, &mpi_stat);
    MPI_File_close(&fh);

    //Usual trick to print one rank at a time
    int position;
    int *u =  new int[lusize];

    if ( rank == 0 ) {
        cout<<"Result for rank 0\n";
        for (int i=0;i<nrl;i++) {
            for (int j=0;j<ncl;j++) {
                cout<<rbuf[i][j]<<" ";
            }
            cout<<endl;
        }
        for (int n=1;n<nprocs;++n) {
           memset(u,0.,lusize);
           MPI_Recv(u,lusize,MPI_INT,MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,&mpi_stat);
           cout<<"Result for rank "<<mpi_stat.MPI_SOURCE<<endl;
           position=0;
           for (int i=0;i<nrl;i++) {
               for (int j=0;j<ncl;j++) {
                   cout<<u[position++]<<" ";
               }
               cout<<endl;
           }
       }
    }
    else {
        // Pack the 2D array into the buffer
        position=0;
        for (int i=0; i<nrl; i++)
            for (int j=0;j<ncl; j++)
                u[position++]=rbuf[i][j];

        MPI_Send(u,lusize, MPI_INT, 0, rank, MPI_COMM_WORLD);
    }

    MPI_Type_free(&locarray);

    MPI_Finalize();

}
