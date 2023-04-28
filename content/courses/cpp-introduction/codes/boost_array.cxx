#include <iostream>
#include <ctime>
#include <boost/multi_array.hpp>

using namespace std;

int main(int argc, char*argv[])

{

    int nrows;
    int ncols;

//  Iterations are to make the time measurable
    const int ITERATIONS = 1000;

    time_t startTime,endTime;

// Set the array dimensions
    nrows=500;
    ncols=500;

// Create the boost array
    typedef boost::multi_array<double, 2> Array2D;
    Array2D boostArray(boost::extents[nrows][ncols]);

// Create the C array
    double **C_Array;
//

//------------------Measure boost----------------------------------------------

    startTime= time(NULL);

    for (int i = 0; i < ITERATIONS; ++i) {
        for (int x = 0; x <nrows; ++x) {
            for (int y = 0; y <ncols; ++y) {
                boostArray[x][y] = 2.345;
            }
        }
    }

    endTime= time(NULL);

    cout<<"[Boost] Elapsed time: "<<(endTime-startTime)/1000.0<<" sec\n";
//------------------Measure native---------------------------------------------

    C_Array= new double* [nrows];
    for (int i=0;i<nrows;++i) {
        C_Array[i]=new double[ncols];
    }

    startTime= time(NULL);

    for (int i = 0; i < ITERATIONS; ++i) {
        for (int x = 0; x <nrows; ++x) {
            for (int y = 0; y <ncols; ++y) {
                C_Array[x][y] = 2.345;
            }
        }
    }

    endTime= time(NULL);

    cout<<"[C style] Elapsed time: "<<(endTime-startTime)/1000.0<<" sec\n";

    return 0;
}

