#include <cstring>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <string>
#include <mpi.h>
#include <hdf5.h>

#define FAIL -1

using namespace std;

int main (int argc, char *argv[]) {

    // Declarations for MPI
    int rank, nprocs;
    int nrows, ncols;

    // Check number of parameters and read in base filename
    if (argc < 2) {
       printf ("USAGE:  %s output-file <nrows> <ncols>\n", argv[0]);
    exit(1);
    }

    string filename=argv[1];
    filename+=".h5";

    if (argc == 2) {
       nrows=4;
       ncols=4;
    }
    else if (argc == 3) {
       nrows=atoi(argv[2]);
       ncols=nrows;
    } 
    else if (argc == 4) {
       nrows=atoi(argv[2]);
       ncols=atoi(argv[3]);
    }

    //Initialize MPI
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    if (nrows*ncols != nprocs) {
        cout<<"Number of rows times columns does not equal nprocs\n";
        MPI_Finalize();
    return 1;
    }

    // Hard-code sizes so we can see what we're doing

    int nrl = 4;
    int ncl = 4;

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

    // Global size of array
    const hsize_t N = (hsize_t)(nrows*nrl);
    const hsize_t M = (hsize_t)(ncols*ncl);

    const int ndims=2;
    // Location of local array within global (like the fileview)
    hsize_t s0=(hsize_t)(ncl*lrow);
    hsize_t s1=(hsize_t)(nrl*lcol);
    hsize_t start[ndims]={s0,s1};
    // Size of each slab
    hsize_t count[ndims] ={(hsize_t)nrl,(hsize_t)ncl};
    // Stride through the data on each subslab
    hsize_t stride[ndims]={1,1};

    if (rank == 0) {
        cout<<"Parallel HDF5 write example\n";
        cout<<"Number of processes:"<< nprocs<<"\n";
        cout<<"Global array size  :"<<N<<"x"<<M<<"\n";
        cout<<"Output file        :"<<filename<<"\n";
    }

    // Fill local arrays
    for (hsize_t i=0; i<nrl; i++) {
        for (hsize_t j=0; j<ncl; j++) {
            loc_u[i][j] = (i*stride[0]+start[0])*100+(j*stride[1]+start[1]+1);

        }
    }

    const char *dataset_name = "twod-array";

    //Write the file.  Mostly taken from HDF5 parallel example set
    //https://github.com/HDFGroup/hdf5/tree/develop/HDF5Examples

    // Declare variables
    hid_t    fid1;                                /* HDF5 file IDs */
    hid_t    acc_tpl1;                            /* File access templates */
    hid_t    sid1;                                /* Dataspace ID */
    hid_t    file_dataspace;                      /* File dataspace ID */
    hid_t    mem_dataspace;                       /* memory dataspace ID */
    hid_t    dataset_id;                          /* Dataset ID */

    herr_t ret; /* Generic return value */

    /*
     * START AN HDF5 FILE
     * -------------------*/
    /* set up file access template with parallel IO access. */
    acc_tpl1 = H5Pcreate(H5P_FILE_ACCESS);
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl1, MPI_COMM_WORLD, MPI_INFO_NULL);
    if ( ret != FAIL ) {
        cout<<"H5Pset_fapl_mpio succeeded\n";
    }

     /*
     * OPTIONAL: It is generally recommended to set collective
     *           metadata writes on FAPL but this is not strictly necessary
     */
    H5Pset_coll_metadata_write(acc_tpl1, true);

    /* create the file collectively */
    fid1 = H5Fcreate(filename.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl1);
    if ( fid1 != FAIL ) {
        cout<<"H5Fcreate succeeded\n";
    }

    /* Release file-access template */
    ret = H5Pclose(acc_tpl1);

    /* set up dimensionality object */
    sid1 = H5Screate_simple(ndims, count, NULL);
    if ( sid1 != FAIL ) {
        cout<<"H5Screate_simple succeeded\n";
    }

    /* create a dataset collectively */
    dataset_id = H5Dcreate2(fid1, dataset_name, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if ( sid1 != FAIL ) {
        cout<<"H5create2 succeeded\n";
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space(dataset_id);
    if ( file_dataspace != FAIL ) {
        cout<<"H5Dget_space succeeded\n";
    }

    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL);
    if ( file_dataspace != FAIL ) {
        cout<<"H5Sselect_hyperslab succeeded\n";
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple(ndims, count, NULL);
    if ( file_dataspace != FAIL ) {
        cout<<"H5Screate_simple succeeded\n";
    }

    /* write data independently */
    ret = H5Dwrite(dataset_id, H5T_NATIVE_INT, mem_dataspace, file_dataspace, H5P_DEFAULT, loc_u);
    if ( ret != FAIL ) {
        cout<<"H5Dwrite succeeded\n";
    }

    MPI_Finalize();

}
