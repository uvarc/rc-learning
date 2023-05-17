#include <iostream>
#include <iomanip>
#include "mpi.h"

using namespace std;

int main(int argc, char *argv[]) {

    int nprocs,rank;
    int sendcount, n;
    float values[100];

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    //For illustration only, don't hardcode nprocs
    if (100%nprocs !=0) {
	if (rank==0) {
            cout << "For this simple example, nprocs must evenly divide 100\n";
	}
	exit(1);
    }
    else {
        sendcount=100/nprocs;
    }

    for(int i=0;i<100;++i) {
        values[i]=i+1;
    }

    cout<<setprecision(1)<<fixed;

    float *myvals=new float[sendcount];

    MPI_Scatter(values,sendcount,MPI_FLOAT,myvals,sendcount,MPI_FLOAT,0,MPI_COMM_WORLD);

    //Forces each process to write separately and in order
    //Printing here is to demonstrate how the data are distributed
    for (n=0;n<nprocs;++n) {
        MPI_Barrier(MPI_COMM_WORLD);
        if (n==rank) {
	    cout<<rank<<":";
	    for (int i=0;i<sendcount;++i) {
		cout<<" "<<myvals[i]<<" ";
	    }
	    cout<<"\n";
	}
    }

    MPI_Finalize();

}
        

