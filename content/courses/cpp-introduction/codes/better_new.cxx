#include <cstdlib>
#include <iostream>

int main() {

    int   N=10;

    float *x= new float[N];
    std::cout<<"New:"<<x<<"\n";
        for (int i=0;i<N;++i) {
            std::cout<<x[i]<<" ";
    }
    std::cout<<"\n";

    delete [] x ;

    return 0;
}
