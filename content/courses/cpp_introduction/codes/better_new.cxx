#include <cstdlib>
#include <iostream>

int main() {

    int   N=10;

    float *x=(float *) new float[N];
    std::cout<<"New:"<<x<<"\n";
        for (int i=0;i<N;++i) {
            std::cout<<x[i]<<"\n";
    }

    delete x ;

    return 0;
}
