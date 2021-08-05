#include <cstdlib>
#include <iostream>

int main() {

    int   N=10;

    for (int i=1;i<=5;++i) {
        float *x= new float[N];
        std::cout<<"New:"<<x<<"\n";
    }

    return 0;
}
