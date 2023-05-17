#include <cstdlib>
#include <iostream>

int main() {

    int   N=10;
    float *x;

    x=(float *) std::malloc(N*sizeof(float));
    std::cout<<"Malloc:"<<x<<"\n";
        for (int i=0;i<N;++i) {
            std::cout<<x[i]<<"\n";
    }

    return 0;
}
