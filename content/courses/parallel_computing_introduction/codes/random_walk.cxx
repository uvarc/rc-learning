/* Solves a random\-walk problem by a brute\-force method.
*
*/

#include <iostream>
#include <string>
#include <sstream>
#include <random>
#include <cmath>

using namespace std;

int main(int argc, char **argv) { 

    random_device rd;
    mt19937 rng(rd());
    uniform_int_distribution<int> choice(1,4);

    long long N;
    if (argc != 2) {
        cout<<"0,0,0\n";
        return 1;
    }
    else {
        string steps=argv[1];
        stringstream ssteps;
        ssteps<<steps;
        ssteps>>N;
    }

    double x=0;
    double y=0;
    int direction;

    for (long long i=1; i<=N; ++i) {
        direction=choice(rng);
        switch (direction) {
            case 1:
                x+=1.;
                break;
            case 2:
                x-=1.;
                break;
            case 3:
                y+=1.;
                break;
            case 4:
                y-=1.;
                break;
        }
    }

    double eucDist=sqrt(x*x+y*y);

    cout<<N<<","<<sqrt((double)N)<<","<<eucDist<<"\n";
    return 0;

}

