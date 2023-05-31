#include <iostream>
#include <vector>
#include <exception>

using namespace std;

int main() {
    float x=10.;
    float y=0.;
    try { 
        if (y != 0) {
	    float z=x/y;
        } else {
            throw 10;
        }
    }
    catch (int e) {
       cout << "An exception occurred, error "<<e<<"\n";
    }

    try {
        if (y != 0) {
	    float z=x/y;
        }
	else {
	    throw "Exception";
        }
    }
    catch (...) {
       cout << "An exception occurred\n";
    }

    vector<double> v{1.,2.,3.,4.,5.};

    try {
      cout << "Last item "<<v.at(6)<<endl;
    }
    catch (const out_of_range &e) {
       cout << "Out of range \n";
    }
}


