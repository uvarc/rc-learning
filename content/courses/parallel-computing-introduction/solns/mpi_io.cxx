#include <cstring>
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <mpi.h>

using namespace std;

int main (int argc, char *argv[]) {

    // Declarations for MPI
    int rank, nprocs;
    int errcode;
    MPI_Status status;
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

    double my_val=(rank+1)*4.0*atan(1.0);

    int amode=MPI_MODE_CREATE | MPI_MODE_WRONLY;
    mpi_err=MPI_File_open(MPI_COMM_WORLD,fname,amode,MPI_INFO_NULL,&fh);

    if (mpi_err != MPI_SUCCESS) {
        MPI_Finalize();
        exit(2);
    }

    int tsize;
    MPI_Type_size(MPI_DOUBLE,&tsize);

    offset=rank*tsize;
    MPI_File_write_at(fh,offset,&my_val,1,MPI_DOUBLE,&mpi_stat);
    MPI_File_close(&fh);

    //All processes read entire file
    amode=MPI_MODE_RDONLY;
    mpi_err=MPI_File_open(MPI_COMM_WORLD,fname,amode,MPI_INFO_NULL,&fh);
    if ( mpi_err != MPI_SUCCESS) {
       MPI_Finalize();
       cout<<"Unable to open MPI file for reading\n";
       exit(2);
    }
    MPI_Offset fsize;
    MPI_File_get_size(fh,&fsize);

    int nvals=fsize/tsize;
    double fbuf[nvals];

    MPI_File_read_all(fh, fbuf, nvals, MPI_DOUBLE, &mpi_stat);
    MPI_File_close(&fh);
    cout<<"Full file at rank "<<rank<<" ";
    for (int i=0; i<nvals; ++i) {
        cout<<fbuf[i]<<" ";
    }
    cout<<endl;

    //Read back in as MPI file
    amode=MPI_MODE_RDONLY;
    mpi_err=MPI_File_open(MPI_COMM_WORLD,fname,amode,MPI_INFO_NULL,&fh);

    if (mpi_err != MPI_SUCCESS) {
        MPI_Finalize();
        cout<<"Unable to open MPI file for reading\n";
        exit(2);
    }

    double u;
    offset=rank*tsize;
    MPI_File_read_at(fh,offset,&u,1,MPI_DOUBLE,&mpi_stat);

    cout<<"Rank "<<rank<<" "<<u<<endl;;

    MPI_File_close(&fh);

    MPI_Finalize();

}
