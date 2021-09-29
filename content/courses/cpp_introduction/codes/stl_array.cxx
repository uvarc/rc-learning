#include <iostream>
#include <ctime>
#include <array>

using namespace std;

int main(int argc, char*argv[])

{

// Create the array
    array<float,3> A={ 1.,2.,3. };

//------------------Measure boost----------------------------------------------

    for (int x = 0; x <3; ++x) {
        A[x] = 2.345;
    }

    return 0;
}

