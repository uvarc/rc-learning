#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>
#include <random>
#include <cmath>
#include <vector>
#include "mpi.h"


using namespace std;

int random_int(int n,int m) {
    //quick and stupid way, using full C++ machinery is better but complex
    return n+rand()%m;
}

double do_work() {
    //hardcoded bounds for convenience
    int nsteps=random_int(10000,30000);
    float result=0.;
    for (int i=0; i<nsteps; ++i) { 
        result+=i;
    }
    return result;
}

int main(int argc, char **argv) {

    int nprocs, rank;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    //Don't use rand and srand if good random numbers are needed
    unsigned int seed = (unsigned) time(NULL);
    //taken from some University course site
    unsigned int my_seed = (seed & 0xFFFFFFF0) | (rank + 1);
    srand(my_seed);

    vector<float> results(nprocs);

    int done=0;
    float result;
    int sender;

    if (rank==0) {
        for (int i=1; i<nprocs; ++i) {
            MPI_Recv(
	    sender=
	    results[sender]=result;
            done=1;
            MPI_Send(&done,1,MPI_INT,sender,0,MPI_COMM_WORLD);
	}
    } else {
        for(int n=1; n<nprocs; ++n) {
            if (rank==n) {
                result=do_work();
                MPI_Send(
                MPI_Recv(
	    }
	}
    }

    float total=0;
    if (rank==0) {
	for (int i=1; i<nprocs; ++i ) {
            total+=results[i];
	}
        cout << "The final result is "<<total<<"\n";
    }

    MPI_Finalize();

}
