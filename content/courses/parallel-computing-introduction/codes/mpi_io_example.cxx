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

    int ord=int('A')+rank;
    char my_char=(char)ord;

    //Fun with pointers (sometimes fh is dereferenced, sometimes not)
    int amode=MPI_MODE_CREATE | MPI_MODE_WRONLY;
    mpi_err=MPI_File_open(MPI_COMM_WORLD,fname,amode,MPI_INFO_NULL,&fh);

    if (mpi_err != MPI_SUCCESS) {
        MPI_Finalize();
        exit(2);
    }

    int nreps=20;
    for (int i=0;i<nreps;i++) {
        offset=rank+i*nprocs;
        MPI_File_write_at(fh,offset,&my_char,1,MPI_CHAR,&mpi_stat);
    }
    MPI_File_close(&fh);

    // Read back in as ordinary file
    // We don't care about efficiency here
    if (rank==root) {
       ifstream fp(fname, ios::in);
       string sstr((istreambuf_iterator<char>(fp)), istreambuf_iterator<char>());
    cout<<sstr<<endl;
    fp.close();
    }

    //All processes read entire file
    //Blocking so will wait for root to finish above read
    amode=MPI_MODE_RDONLY;
    mpi_err=MPI_File_open(MPI_COMM_WORLD,fname,amode,MPI_INFO_NULL,&fh);
    if ( mpi_err != MPI_SUCCESS) {
       MPI_Finalize();
       cout<<"Unable to open MPI file for reading\n";
       exit(2);
    }
    MPI_Offset fsize;
    int tsize;
    MPI_File_get_size(fh,&fsize);
    MPI_Type_size(MPI_CHAR,&tsize);
    if (rank==root) {
        cout<<"File size is "<<fsize<<" Type size is "<<tsize<<" bytes\n";
    }
    int nchar=fsize/tsize;
    char fbuf[nchar];

    MPI_File_read_all(fh, fbuf, nchar, MPI_CHAR, &mpi_stat);
    MPI_File_close(&fh);
    cout<<"Rank "<<rank<<" ";
    for (int i=0; i<nchar; ++i) {
        cout<<fbuf[i];
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

    char rbuf[nreps];
    offset=0;
    for (int i=0;i<nreps;i++) {
        offset=rank+i*nprocs;
        MPI_File_read_at(fh,offset,&rbuf[i],1,MPI_CHAR,&mpi_stat);
    }

    cout<<"Rank "<<rank<<" ";
    for (int i=0; i<nreps; ++i) {
        cout<<rbuf[i];
    }
    cout<<endl;

    MPI_File_close(&fh);

    MPI_Finalize();

}
